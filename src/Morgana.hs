{-# LANGUAGE NoImplicitPrelude #-}
module Morgana where

import           Control.Lens
import           Control.Zipper
import           Network (listenOn, withSocketsDo, accept, PortID(..), Socket)
import           Protolude hiding (from, (&), to)
import           System.IO (hSetBuffering, hFlush, BufferMode(..))
import           Morgana.Types
import           Unsafe
import qualified Data.List as List
import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import qualified Language.PureScript as P


allMatchesInModule :: P.Module -> Int -> Int -> [Match]
allMatchesInModule modul l c = allMatchesInModule' modul (SSpan "<morgana>" (SPos l c) (SPos l c))

allMatchesInModule' :: P.Module -> SSpan -> [Match]
allMatchesInModule' (P.Module _ _ _ declarations _) sp = foldMap extractor declarations
  where
    extractor :: P.Declaration -> [Match]
    extractor = matcher
      where
        (matcher, _, _, _, _) =
          P.everythingOnValues
          (<>)
          (\case (P.PositionedDeclaration (view convertSpan -> sourceSpan') _ d) ->
                  [DeclarationMatch sourceSpan' d | isWithin sp sourceSpan']
                 _ -> [])
          (\case (P.PositionedValue (view convertSpan -> sourceSpan') _ e) ->
                  [ExprMatch sourceSpan' e | isWithin sp sourceSpan']
                 _ -> [])
          (\case (P.PositionedBinder (view convertSpan -> sourceSpan') _ b) ->
                  [BinderMatch sourceSpan' b | isWithin sp sourceSpan']
                 _ -> [])
          (\case ca@(P.CaseAlternative binders body)
                  | Just start <- view convertSpan <$> (getSpanFromBinder =<< head binders)
                  , Just end <- view (convertSpan.ssEnd) <$> (getSpanFromGuardedExpr =<< lastMay body)
                  -> let sourceSpan' = SSpan (start^.ssFile) (start^.ssStart) end
                      in [CaseAlternativeMatch sourceSpan' ca | isWithin sp sourceSpan']
                 _ -> [])
          (\case (P.PositionedDoNotationElement (view convertSpan -> sourceSpan') _ dne) ->
                  [DoNotationMatch sourceSpan' dne | isWithin sp sourceSpan']
                 _ -> [])

unsafeParseModule :: Text -> P.Module
unsafeParseModule t = snd . fromRight $ P.parseModuleFromFile identity ("hi", toS t)
  where
    fromRight :: Either a b -> b
    fromRight = unsafeFromJust . rightToMaybe

isWithin :: SSpan -> SSpan -> Bool
isWithin span1 span2 =
    span2^.ssStart <= span1^.ssStart && span1^.ssEnd <= span2^.ssEnd

commandProcessor :: (MonadState SState m, MonadIO m) => Command -> (Response -> m ()) -> m ()
commandProcessor command respond = case command of
  Pos fp l c -> do
    file' <- liftIO (T.readFile (toS fp))
    let modul = unsafeParseModule file'
    case zipper (allMatchesInModule modul l c) & within traverse <&> rightmost of
      Nothing -> do
        put Waiting
        respond (Message "Didn't match")
      Just z -> do
        put (SelectingState (Selecting file' modul z (SPos l c)))
        respond =<< respondSpan
  Narrow -> do
    _SelectingState.selectingMatches %= tug rightward
    respond =<< respondSpan
  Widen -> do
    freeVars <- gets (map freeVariablesInSelection . (preview _SelectingState))
    traceShowM freeVars
    _SelectingState.selectingMatches %= tug leftward
    respond =<< respondSpan
  FindOccurrences fp l c -> do
    file <- liftIO (T.readFile (toS fp))
    let modul = unsafeParseModule file
    case zipper (allMatchesInModule modul l c) & within traverse <&> rightmost of
      Nothing -> do
        put Waiting
        respond (Message "Didn't match")
      Just z -> do
        -- let occurrences = selectingFindOccurrences (Selecting file modul z)
        let occurrences =
              maybeToList (evalState findBinderForSelection (Selecting file modul z (SPos l c)))
        respond (Spans occurrences)
  Rename fp l c newName -> do
    file <- liftIO (T.readFile (toS fp))
    let modul = unsafeParseModule file
    case zipper (allMatchesInModule modul l c) & within traverse <&> rightmost of
      Nothing -> do
        put Waiting
        respond (Message "Didn't match")
      Just z ->
        let
          result = flip evalState (Selecting file modul z (SPos l c)) $ do
            occurrences <- selectingFindOccurrences
            binder' <- findBinderForSelection
            -- traceM ("Occurrences: " <> show occurrences)
            -- traceM ("Binder': " <> show binder')
            case binder' of
              Nothing -> pure []
              Just binder1 -> do
                -- traceShowM binder1
                flip filterM occurrences $ \sspan -> do
                  setSuccess <- setCursor (sspan^.ssStart)
                  if setSuccess
                    then
                      findBinderForSelection <&> (Just binder1 ==)
                    else
                      do traceM "Something went wrong here" $> False
        in
          respond (Edits (computeChangeset newName result))

setCursor :: SPos -> State Selecting Bool
setCursor (SPos l c)= do
  modul <- use selectingModule
  case zipper (allMatchesInModule modul l c) & within traverse <&> rightmost of
    Nothing -> pure False
    Just z -> do
      selectingMatches .= z
      selectingCursor .= SPos l c
      pure True

getIdentAtPoint :: (MonadReader Selecting m) => m (Maybe Text)
getIdentAtPoint = do
  selectionMaybe <- view (selectingMatches . focus)
  pure $ case selectionMaybe of
    BinderMatch _ (P.VarBinder (P.Ident ident)) -> Just ident
    ExprMatch _ (P.Var (P.Qualified Nothing (P.Ident ident))) -> Just ident
    DeclarationMatch _ (P.ValueDeclaration (P.Ident ident) _ _ _) -> Just ident
    _ -> Nothing

selectingFindOccurrences :: State Selecting [SSpan]
selectingFindOccurrences = do
  selectionMaybe <- use (selectingMatches . focus)
  let i = case selectionMaybe of
            BinderMatch _ (P.VarBinder (P.Ident ident)) -> ident
            ExprMatch _ (P.Var (P.Qualified Nothing (P.Ident ident))) -> ident
            _ -> "NotFound"
  selectingMatches %= leftmost
  current <- use (selectingMatches . focus)
  case current of
    DeclarationMatch _ d ->
      let x = if isBoundIn i d
              then findOccurrences i d
              else findOccurrences i d
      in pure x
    _ ->
      pure []

computeChangeset :: Text -> [SSpan] -> [(SSpan, Text)]
computeChangeset newText spans =
  List.groupBy ((==) `on` view startLine) spans
  <&> sortOn (view ssStart)
  <&> snd . List.mapAccumL (\offset next -> (offset + characterDifference next, next & columns +~ offset)) 0
  & fold
  <&> (, newText)
  where
    characterDifference span =  T.length newText - (span^.endColumn - span^.startColumn)

findBindersInGuardedExpr :: Text -> P.GuardedExpr -> [SSpan]
findBindersInGuardedExpr i (P.GuardedExpr guards _) = foldMap go guards
  where
    go (P.ConditionGuard _) = []
    go (P.PatternGuard binder _) = findBindersForInBinder i binder


findBindersFor :: Text -> P.Declaration -> [SSpan]
findBindersFor ident decl = execState (matcherDecl decl) []
  where
    (matcherDecl, _, _) = P.everywhereOnValuesTopDownM
      (\case d@(P.PositionedDeclaration sourceSpan' _ (P.ValueDeclaration (P.Ident ident') _ _ _))
               | ident == ident'-> modify (cons (valueDeclarationToBinderSpan
                                                 (sourceSpan'^.convertSpan)
                                                 ident)) $> d
             d -> pure d)
      pure
      (\case b@(P.PositionedBinder sourceSpan' _ (P.VarBinder (P.Ident ident')))
               | ident == ident'-> modify (cons (sourceSpan'^.convertSpan)) $> b
             b -> pure b)

findBindersForInBinder :: Text -> P.Binder -> [SSpan]
findBindersForInBinder ident binder = execState (matcherBinder binder) []
  where
    (_, _, matcherBinder) = P.everywhereOnValuesTopDownM
      (\case d@(P.PositionedDeclaration sourceSpan' _ (P.ValueDeclaration (P.Ident ident') _ _ _))
               | ident == ident'-> modify (cons
                                           (valueDeclarationToBinderSpan
                                            (sourceSpan'^.convertSpan)
                                            ident)) $> d
             d@(P.ValueDeclaration _ _ binders _) -> do
               modify ((++) (foldMap (findBindersForInBinder ident) binders)) $> d
             d -> pure d)
      pure
      (\case b@(P.PositionedBinder sourceSpan' _ (P.VarBinder (P.Ident ident')))
               | ident == ident'-> modify (cons (sourceSpan'^.convertSpan)) $> b
             b -> pure b)

valueDeclarationToBinderSpan :: SSpan -> Text -> SSpan
valueDeclarationToBinderSpan s i =
  s & endLine.~(s^.startLine) & endColumn .~ (s^.startColumn + T.length i)

findBinderForSelection :: State Selecting (Maybe SSpan)
findBinderForSelection = do
  selectingMatches %= rightmost
  i <- gets getIdentAtPoint
  z <- use selectingMatches
  case i of
    Just i' ->
      pure (asum $ (map (isBoundInMatch i') (upward z ^. focus & reverse)))
    Nothing -> traceM "Failed findBinderSelection" *> pure Nothing

isBoundInMatch :: Text -> Match -> Maybe SSpan
isBoundInMatch i m = trace ("\n" <> showWithoutSpans m) $ case m of
  DeclarationMatch s decl -> case decl of
    P.ValueDeclaration (P.Ident ident ) _ binders _
      | ident == i -> Just (valueDeclarationToBinderSpan s ident)
      | otherwise -> listToMaybe (foldMap (findBindersForInBinder i) binders)
    P.BoundValueDeclaration binder _ ->
      listToMaybe (findBindersForInBinder i binder)
    P.PositionedDeclaration sspan _ d ->
      isBoundInMatch i (DeclarationMatch (sspan^.convertSpan) d)
    _ -> Nothing
  ExprMatch _ expr -> case expr of
    P.Abs binder _ ->
      listToMaybe (findBindersForInBinder i binder)
    P.Let decls _ ->
      listToMaybe (mapMaybe (isValueDecl i) decls)
    P.Do els ->
      els
       & foldMap (\case
                       (P.PositionedDoNotationElement _ _ (P.DoNotationBind binder _)) -> findBindersForInBinder i binder
                       _ -> mempty)
       & listToMaybe
    P.PositionedValue sspan _ expr' ->
      isBoundInMatch i (ExprMatch (sspan^.convertSpan) expr')
    _ ->
      Nothing
  CaseAlternativeMatch _ alternative ->
    (alternative
      & P.caseAlternativeResult
      & foldMap (findBindersInGuardedExpr i)
      & listToMaybe)
    <|>
    (alternative
       & P.caseAlternativeBinders
       & foldMap (findBindersForInBinder i)
       & listToMaybe)
  _ -> Nothing

isValueDecl :: Text -> P.Declaration -> Maybe SSpan
isValueDecl ident (P.PositionedDeclaration sourceSpan _ (P.ValueDeclaration (P.Ident i) _ _ _))
  | i == ident = Just (valueDeclarationToBinderSpan (sourceSpan^.convertSpan) i)
isValueDecl ident (P.PositionedDeclaration _ _ (P.BoundValueDeclaration binder _)) =
  listToMaybe (findBindersForInBinder ident binder)
isValueDecl _ _ = Nothing

isBoundIn :: Text -> P.Declaration -> Bool
isBoundIn ident = not . null . findBindersFor ident

findOccurrences :: Text -> P.Declaration -> [SSpan]
findOccurrences ident = matcher
  where
    (matcher, _, _, _, _) =
      P.everythingOnValues
      (<>)
      (\case (P.PositionedDeclaration sourceSpan' _ (P.ValueDeclaration (P.Ident ident') _ _ _)) ->
               [valueDeclarationToBinderSpan (sourceSpan'^.convertSpan) ident | ident == ident']
             _ -> [])
      (\case (P.PositionedValue sourceSpan' _ (P.Var (P.Qualified Nothing (P.Ident ident')))) ->
               [sourceSpan'^.convertSpan | ident == ident']
             _ -> [])
      (\case (P.PositionedBinder sourceSpan' _ (P.VarBinder (P.Ident ident'))) ->
               [sourceSpan'^.convertSpan | ident == ident']
             _ -> [])
      (const [])
      (const [])

-- Accepting and responding to commands

respondSpan :: (MonadState SState m, MonadIO m) => m Response
respondSpan = do
  s <- get
  case s of
    SelectingState (Selecting file' _ selections _) ->
      let
        sp = selections^.focus.matchSpan
      in
        do
          case selections^.focus of
            BinderMatch _ binder ->
              traceShowM binder
            DeclarationMatch _ decl ->
              traceShowM decl
            ExprMatch _ expr ->
              traceShowM expr
            DoNotationMatch _ expr ->
              traceShowM expr
            CaseAlternativeMatch _ caseAlt ->
              traceShowM caseAlt
          pure (Span file' sp)
    Waiting ->
      pure (Message "Gief me da file!")

sockHandler :: (MonadState SState m, MonadIO m) => Socket -> m ()
sockHandler sock = do
  (h, _, _) <- liftIO (accept sock)
  liftIO (hSetBuffering h LineBuffering)
  forever $ do
    line <- liftIO (T.hGetLine h)
    case simpleParse line of
      Just command ->
        commandProcessor command (respond' h)
      Nothing ->
        liftIO (T.hPutStrLn h "Parse failure")
    liftIO (putText line)
    liftIO (hFlush h)

simpleParse :: Text -> Maybe Command
simpleParse t =
  case T.words t of
    ["w"] -> Just Widen
    ["n"] -> Just Narrow
    ["o", fp, x, y] -> FindOccurrences fp <$> readMaybe (toS x) <*> readMaybe (toS y)
    ["r", fp, x, y, newName] -> Rename fp <$> readMaybe (toS x) <*> readMaybe (toS y) <*> pure newName
    ["s", fp, x, y] -> Pos fp <$> readMaybe (toS x) <*> readMaybe (toS y)
    _ -> Nothing

newtype Emacs a = Emacs { runEmacs :: StateT SState IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadState SState)

respond' :: (MonadIO m) => Handle -> Response -> m ()
respond' h r = case r of
  Message t ->
    liftIO (T.hPutStrLn h ("m: " <> t))
  Span _ ss ->
    liftIO (T.hPutStrLn h ("s: " <> answerSS ss))
  Spans sourceSpans ->
    liftIO (T.hPutStrLn h ("ss: " <> T.unwords (map answerSS sourceSpans)))
  Edits edits ->
    liftIO (T.hPutStrLn h ("ed: " <> T.unwords (map answerEdit edits)))
  where
    answerSS (SSpan _ (SPos x1 y1) (SPos x2 y2)) = T.unwords (map show [x1, y1, x2, y2])
    answerEdit (ss, text) = answerSS ss <> " " <> text

-- MAIN --
main :: IO ()
main = withSocketsDo $ do
    args <- getArgs
    let port = fromIntegral $ fromMaybe 5678 (readMaybe =<< headMay args :: Maybe Int)
    sock <- listenOn $ PortNumber port
    putText $ "Listening on " <> show port
    void $ runStateT (runEmacs (forever (sockHandler sock))) Waiting
