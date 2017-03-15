{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
module Morgana where

import           Control.Lens
import           Control.Zipper
import           Network (listenOn, withSocketsDo, accept, PortID(..), Socket)
import           Protolude hiding (from, (&), to)
import           System.IO (hSetBuffering, hFlush, BufferMode(..))
import           Unsafe
import qualified Data.List as List
import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import qualified Language.PureScript as P

data Match where
  DeclarationMatch :: SSpan -> P.Declaration       -> Match
  ExprMatch        :: SSpan -> P.Expr              -> Match
  BinderMatch      :: SSpan -> P.Binder            -> Match
  DoNotationMatch  :: SSpan -> P.DoNotationElement -> Match
  CaseMatch        :: SSpan -> P.CaseAlternative   -> Match
  deriving (Show)

-- | Helpers for debug tracing
stripPositionsDecl :: P.Declaration -> P.Declaration
stripPositionsBinder :: P.Binder -> P.Binder
stripPositionsExpr :: P.Expr -> P.Expr
(stripPositionsDecl, stripPositionsExpr, stripPositionsBinder) = P.everywhereOnValues
      (\case (P.PositionedDeclaration _ _ d) -> d
             d -> d)
      (\case e@(P.PositionedValue _ _ (P.Let{})) -> e
             (P.PositionedValue _ _ e) -> e
             e -> e)
      (\case (P.PositionedBinder _ _ b) -> b
             b -> b)

showWithoutSpans :: Match -> Text
showWithoutSpans m = case m of
  DeclarationMatch _ d -> show (stripPositionsDecl d)
  ExprMatch _ e -> show (stripPositionsExpr e)
  BinderMatch _ b -> show (stripPositionsBinder b)
  DoNotationMatch _ d -> show d
  CaseMatch _ c -> show c

getSpanFromBinder :: P.Binder -> Maybe P.SourceSpan
getSpanFromBinder (P.PositionedBinder ss _ _) = Just ss
getSpanFromBinder _ = Nothing
getSpanFromGuardedExpr :: P.GuardedExpr -> Maybe P.SourceSpan
getSpanFromGuardedExpr (P.GuardedExpr _ (P.PositionedValue ss _ _)) = Just ss
getSpanFromGuardedExpr _ = Nothing

type File = Text

data SSpan = SSpan
  { _ssFile :: !Text
  , _ssStart :: !SPos
  , _ssEnd :: !SPos
  } deriving (Show, Eq)

data SPos = SPos
  { _spLine :: !Int
  , _spColumn :: !Int
  } deriving (Show, Eq)

makeLenses ''SPos
makeLenses ''SSpan

startLine, startColumn, endLine, endColumn :: Lens' SSpan Int
startLine = ssStart.spLine
startColumn = ssStart.spColumn
endLine = ssEnd.spLine
endColumn = ssEnd.spColumn

fields2 :: Lens' s a -> Lens' s a -> Traversal' s a
fields2 f1 f2 f s = (\v1 v2 -> s & f1 .~ v1 & f2 .~ v2) <$> f (s ^. f1) <*> f (s ^. f2)

columns :: Traversal' SSpan Int
columns = fields2 startColumn endColumn

lines :: Traversal' SSpan Int
lines = fields2 startLine endLine

instance Ord SSpan where
  compare s1 s2
    | s1^.ssFile == s2^.ssFile = (compare `on` view ssStart) s1 s2
    | otherwise = (compare `on` view ssFile) s1 s2

instance Ord SPos where
  compare (SPos x1 y1) (SPos x2 y2)
    | x1 < x2 = LT
    | x1 == x2 && y1 < y2 = LT
    | x1 == x2 && y1 == y2 = EQ
    | otherwise = GT

convertPos :: Iso' P.SourcePos SPos
convertPos = iso (\(P.SourcePos l c) -> SPos l c) (\(SPos l c) -> P.SourcePos l c)

convertSpan :: Iso' P.SourceSpan SSpan
convertSpan =
  iso
  (\(P.SourceSpan fn start end) -> (SSpan (T.pack fn) (start^.convertPos) (end^.convertPos)))
  (\(SSpan fn start end) -> P.SourceSpan (toS fn) (start^. from convertPos) (end^. from convertPos))

matchSpan :: Lens' Match SSpan
matchSpan = lens getSP setSP
  where
    getSP :: Match -> SSpan
    getSP (DeclarationMatch sp _) = sp
    getSP (ExprMatch sp _) = sp
    getSP (BinderMatch sp _) = sp
    getSP (DoNotationMatch sp _) = sp
    getSP (CaseMatch sp _) = sp

    setSP :: Match -> SSpan -> Match
    setSP (DeclarationMatch _ d) sp = DeclarationMatch sp d
    setSP (ExprMatch _ d) sp = ExprMatch sp d
    setSP (BinderMatch _ d) sp = BinderMatch sp d
    setSP (DoNotationMatch _ d) sp = DoNotationMatch sp d
    setSP (CaseMatch _ d) sp = CaseMatch sp d

instance Eq Match where
  (==) = (==) `on` view matchSpan

instance Ord Match where
  compare = compare `on` view matchSpan

extractor :: SPos -> P.Declaration -> [Match]
extractor sp = matcher
  where
    (matcher, _, _, _, _) =
      P.everythingOnValues
      (<>)
      (\case (P.PositionedDeclaration (view convertSpan -> sourceSpan') _ d) ->
               [DeclarationMatch sourceSpan' d | matches sp sourceSpan']
             _ -> [])
      (\case (P.PositionedValue (view convertSpan -> sourceSpan') _ e) ->
               [ExprMatch sourceSpan' e | matches sp sourceSpan']
             _ -> [])
      (\case (P.PositionedBinder (view convertSpan -> sourceSpan') _ b) ->
               [BinderMatch sourceSpan' b | matches sp sourceSpan']
             _ -> [])
      (\case ca@(P.CaseAlternative binders body)
               | Just start <- view convertSpan <$> (getSpanFromBinder =<< head binders)
               , Just end <- view (convertSpan.ssEnd) <$> (getSpanFromGuardedExpr =<< lastMay body)
               -> let sourceSpan' = SSpan (start^.ssFile) (start^.ssStart) end
                  in [CaseMatch sourceSpan' ca | matches sp sourceSpan']
             _ -> [])
      (\case (P.PositionedDoNotationElement (view convertSpan -> sourceSpan') _ dne) ->
               [DoNotationMatch sourceSpan' dne | matches sp sourceSpan']
             _ -> [])

allMatches :: P.Module -> Int -> Int -> [Match]
allMatches (P.Module _ _ _ d _) l c = concatMap (extractor (SPos l c)) d

fromRight :: Either a b -> b
fromRight = unsafeFromJust . rightToMaybe

unsafeParseModule :: Text -> P.Module
unsafeParseModule t = snd . fromRight $ P.parseModuleFromFile identity ("hi", toS t)

matches :: SPos -> SSpan -> Bool
matches pos span =
    span^.ssStart <= pos && pos <= span^.ssEnd

data Selecting =
  Selecting
    { _selectingFile    :: Text
    , _selectingModule  :: P.Module
    , _selectingMatches :: Top :>> [Match] :>> Match
    , _selectingCursor  :: SPos
    }

makeLenses ''Selecting

data SState
  = Waiting
  | SelectingState Selecting

makePrisms ''SState

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

data Response
  = Message Text
  | Span Text SSpan
  | Spans [SSpan]
  | Edits [(SSpan, Text)]


data Command
  = Pos Text Int Int
  | Widen
  | Narrow
  | FindOccurrences Text Int Int
  | Rename Text Int Int Text

commandProcessor :: (MonadState SState m, MonadIO m) => Command -> (Response -> m ()) -> m ()
commandProcessor command respond = case command of
  Pos fp l c -> do
    file' <- liftIO (T.readFile (toS fp))
    let modul = unsafeParseModule file'
    case zipper (allMatches modul l c) & within traverse <&> rightmost of
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
    _SelectingState.selectingMatches %= tug leftward
    respond =<< respondSpan
  FindOccurrences fp l c -> do
    file <- liftIO (T.readFile (toS fp))
    let modul = unsafeParseModule file
    case zipper (allMatches modul l c) & within traverse <&> rightmost of
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
    case zipper (allMatches modul l c) & within traverse <&> rightmost of
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
  case zipper (allMatches modul l c) & within traverse <&> rightmost of
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
      | otherwise -> listToMaybe (concatMap (findBindersForInBinder i) binders)
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
       & concatMap (\case
                       (P.PositionedDoNotationElement _ _ (P.DoNotationBind binder _)) -> findBindersForInBinder i binder
                       _ -> mempty)
       & listToMaybe
    P.PositionedValue sspan _ expr' ->
      isBoundInMatch i (ExprMatch (sspan^.convertSpan) expr')
    _ ->
      Nothing
  CaseMatch _ alternative ->
    alternative
       & P.caseAlternativeBinders
       & concatMap (findBindersForInBinder i)
       & listToMaybe
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
            CaseMatch _ caseAlt ->
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

-- MAIN --

main :: IO ()
main = withSocketsDo $ do
    args <- getArgs
    let port = fromIntegral $ fromMaybe 5678 (readMaybe =<< headMay args :: Maybe Int)
    sock <- listenOn $ PortNumber port
    putText $ "Listening on " <> show port
    void $ runStateT (runEmacs (forever (sockHandler sock))) Waiting
