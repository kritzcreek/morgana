{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE GADTs                      #-}
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
import           Protolude hiding (from, (&))
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
  deriving (Show)

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

    setSP :: Match -> SSpan -> Match
    setSP (DeclarationMatch _ d) sp = DeclarationMatch sp d
    setSP (ExprMatch _ d) sp = ExprMatch sp d
    setSP (BinderMatch _ d) sp = BinderMatch sp d
    setSP (DoNotationMatch _ d) sp = DoNotationMatch sp d

instance Eq Match where
  (==) = (==) `on` view matchSpan

instance Ord Match where
  compare = compare `on` view matchSpan

extractor :: P.SourcePos -> P.Declaration -> [Match]
extractor sp = matcher
  where
    (matcher, _, _, _, _) =
      P.everythingOnValues
      (<>)
      (\case (P.PositionedDeclaration sourceSpan' _ d) ->
               [DeclarationMatch (sourceSpan' ^. convertSpan) d | matches sp sourceSpan']
             _ -> [])
      (\case (P.PositionedValue sourceSpan' _ e) ->
               [ExprMatch (sourceSpan' ^. convertSpan) e | matches sp sourceSpan']
             _ -> [])
      (\case (P.PositionedBinder sourceSpan' _ b) ->
               [BinderMatch (sourceSpan' ^. convertSpan) b | matches sp sourceSpan']
             _ -> [])
      (const [])
      (\case (P.PositionedDoNotationElement sourceSpan' _ dne) ->
               [DoNotationMatch (sourceSpan' ^. convertSpan) dne | matches sp sourceSpan']
             _ -> [])

allMatches :: P.Module -> Int -> Int -> [Match]
allMatches (P.Module _ _ _ d _) l c = concatMap (extractor (P.SourcePos l c)) d

fromRight :: Either a b -> b
fromRight = unsafeFromJust . rightToMaybe

unsafeParseModule :: Text -> P.Module
unsafeParseModule t = snd . fromRight $ P.parseModuleFromFile identity ("hi", toS t)

matches :: P.SourcePos -> P.SourceSpan -> Bool
matches pos span =
  let
    pos' = pos^.convertPos
    span' = span^.convertSpan
  in
    span'^.ssStart <= pos' && pos' <= span'^.ssEnd

data Selecting =
  Selecting
    { _selectingFile    :: Text
    , _selectingModule  :: P.Module
    , _selectingMatches :: Top :>> [Match] :>> Match
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
        put (SelectingState (Selecting file' modul z))
        respond =<< respondSpan
  Narrow -> do
    -- [match] <- uses (selectingMatches . focus . matchSpan . Control.Lens.to pure) traceShowId
    _SelectingState.selectingMatches %= tug rightward
    respond =<< respondSpan
  Widen -> do
    _SelectingState.selectingMatches %= tug leftward
    respond =<< respondSpan
  FindOccurrences fp l c -> do
    file' <- liftIO (T.readFile (toS fp))
    let modul = unsafeParseModule file'
    case zipper (allMatches modul l c) & within traverse <&> rightmost of
      Nothing -> do
        put Waiting
        respond (Message "Didn't match")
      Just z -> do
        let occurrences = selectingFindOccurrences (Selecting file' modul z)
        respond (Spans occurrences)
  Rename fp l c newName -> do
    file' <- liftIO (T.readFile (toS fp))
    let modul = unsafeParseModule file'
    case zipper (allMatches modul l c) & within traverse <&> rightmost of
      Nothing -> do
        put Waiting
        respond (Message "Didn't match")
      Just z -> do
        let occurrences = selectingFindOccurrences (Selecting file' modul z)
        respond (Edits (computeChangeset newName occurrences))

selectingFindOccurrences :: Selecting -> [SSpan]
selectingFindOccurrences selecting = flip evalState selecting $ do
  selectionMaybe <- use (selectingMatches . focus)
  let i = case selectionMaybe of
            BinderMatch _ (P.VarBinder (P.Ident ident)) -> ident
            ExprMatch _ (P.Var (P.Qualified Nothing (P.Ident ident))) -> ident
            _ -> "NotFound"
  traceM ("ident: " <> i)
  selectingMatches %= leftmost
  current <- use (selectingMatches . focus)
  traceShowM current
  case current of
    DeclarationMatch _ d ->
      let x = if isBoundIn i d
              then findOccurrences i d
              else []
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
findBindersFor ident decl = execState (matcher decl) []
  where
    (matcher, _, _) = P.everywhereOnValuesTopDownM
      (\case b@(P.PositionedDeclaration sourceSpan' _ (P.ValueDeclaration (P.Ident ident') _ _ _))
               | ident == ident'-> modify (cons (sourceSpan'^.convertSpan)) $> b
             b -> pure b)
      pure
      (\case b@(P.PositionedBinder sourceSpan' _ (P.VarBinder (P.Ident ident')))
               | ident == ident'-> modify (cons (sourceSpan'^.convertSpan)) $> b
             b -> pure b)

isBoundIn :: Text -> P.Declaration -> Bool
isBoundIn ident =  not . null . matcher
  where
    (matcher, _, _, _, _) =
      P.everythingOnValues
      (<>)
      (\case (P.PositionedDeclaration sourceSpan' _ (P.ValueDeclaration (P.Ident ident') _ _ _)) ->
               [sourceSpan' | ident == ident']
             _ -> [])
      (const [])
      (\case (P.PositionedBinder sourceSpan' _ (P.VarBinder (P.Ident ident'))) ->
               [sourceSpan' | ident == ident']
             _ -> [])
      (const [])
      (const [])

findOccurrences :: Text -> P.Declaration -> [SSpan]
findOccurrences ident = matcher
  where
    (matcher, _, _, _, _) =
      P.everythingOnValues
      (<>)
      (\case (P.PositionedDeclaration sourceSpan' _ (P.ValueDeclaration (P.Ident ident') _ _ _)) ->
               [let s = sourceSpan'^.convertSpan in s & endColumn .~ (s^.startColumn + T.length ident') | ident == ident']
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
    SelectingState (Selecting file' _ selections) ->
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
