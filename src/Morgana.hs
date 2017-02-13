{-# LANGUAGE NoImplicitPrelude            #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings            #-}
{-# LANGUAGE FlexibleContexts             #-}
{-# LANGUAGE TypeOperators                #-}
{-# LANGUAGE TemplateHaskell              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving   #-}
module Morgana where

import           Control.Lens hiding ((&))
import           Control.Zipper
import           Network (listenOn, withSocketsDo, accept, PortID(..), Socket)
import           Protolude
import           System.IO (hSetBuffering, hFlush, BufferMode(..))
import           Unsafe
import qualified Data.Text           as T
import qualified Data.Text.IO as T
import qualified Language.PureScript as P

data Match where
  DeclarationMatch :: P.SourceSpan -> P.Declaration       -> Match
  ExprMatch        :: P.SourceSpan -> P.Expr              -> Match
  BinderMatch      :: P.SourceSpan -> P.Binder            -> Match
  DoNotationMatch  :: P.SourceSpan -> P.DoNotationElement -> Match
  deriving (Show)

sourceSpan :: Lens Match Match P.SourceSpan P.SourceSpan
sourceSpan = lens getSP setSP
  where
    getSP :: Match -> P.SourceSpan
    getSP (DeclarationMatch sp _) = sp
    getSP (ExprMatch sp _) = sp
    getSP (BinderMatch sp _) = sp
    getSP (DoNotationMatch sp _) = sp

    setSP :: Match -> P.SourceSpan -> Match
    setSP (DeclarationMatch _ d) sp = DeclarationMatch sp d
    setSP (ExprMatch _ d) sp = ExprMatch sp d
    setSP (BinderMatch _ d) sp = BinderMatch sp d
    setSP (DoNotationMatch _ d) sp = DoNotationMatch sp d

instance Eq Match where
  (==) = (==) `on` view sourceSpan

instance Ord Match where
  compare = compare `on` view sourceSpan

extractor :: P.SourcePos -> P.Declaration -> [Match]
extractor sp = matcher
  where
    (matcher, _, _, _, _) =
      P.everythingOnValues
      (<>)
      (\case (P.PositionedDeclaration sourceSpan' _ d) ->
               [DeclarationMatch sourceSpan' d | matches sp sourceSpan']
             _ -> [])
      (\case (P.PositionedValue sourceSpan' _ e) ->
               [ExprMatch sourceSpan' e | matches sp sourceSpan']
             _ -> [])
      (\case (P.PositionedBinder sourceSpan' _ b) ->
               [BinderMatch sourceSpan' b | matches sp sourceSpan']
             _ -> [])
      (const [])
      (\case (P.PositionedDoNotationElement sourceSpan' _ dne) ->
               [DoNotationMatch sourceSpan' dne | matches sp sourceSpan']
             _ -> [])

allMatches :: Text -> Int -> Int -> [Match]
allMatches t l c = concatMap (extractor (P.SourcePos l c)) (decls t)

slice :: Text -> P.SourceSpan -> [Text]
slice t (P.SourceSpan _ (P.SourcePos l1 c1) (P.SourcePos l2 c2)) =
  let (l:ls) = take (l2 - l1 + 1) . drop (l1 - 1) $ T.lines t
  in  T.drop (c1 - 1) l : fromMaybe [] (initMay ls) ++ maybeToList (T.take c2 <$> lastMay ls)

fromRight :: Either a b -> b
fromRight = unsafeFromJust . rightToMaybe

decls :: Text -> [P.Declaration]
decls t = d
  where
    P.Module _ _ _ d _ = snd . fromRight $ P.parseModuleFromFile identity ("hi", toS t)

matches :: P.SourcePos -> P.SourceSpan -> Bool
matches pos (P.SourceSpan _ start end)
  | start == pos || end == pos = True
  | (start `isBefore` pos) && (end `isBehind` pos) = True
  | otherwise = False

isBefore, isBehind :: P.SourcePos -> P.SourcePos -> Bool
isBefore (P.SourcePos x1 y1) (P.SourcePos x2 y2)
  | x1 < x2 = True
  | x1 == x2 && y1 < y2 = True
  | otherwise = False

isBehind x y = isBefore y x

-- INTERPRETERS

data Selecting =
  Selecting
    { _selectingFile :: Text
    , _selectingMatches :: Top :>> [Match] :>> Match
    }
makeLenses ''Selecting

data SState
  = Waiting
  | SelectingState Selecting

makePrisms ''SState
newtype Terminal a = Terminal { runTerminal :: StateT SState IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadState SState)

instance Respond Terminal where
  respond h (Message t) = liftIO (T.hPutStrLn h ("Message: " <> t))
  respond h (Span f sp) =
    liftIO (T.hPutStrLn h (T.unlines (slice f sp)))

newtype Emacs a = Emacs { runEmacs :: StateT SState IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadState SState)

instance Respond Emacs where
  respond h r = case r of
    Message t ->
      liftIO (T.hPutStrLn h ("m: " <> t))
    Span _ ss ->
      liftIO (T.hPutStrLn h ("s: " <> answerSS ss))
    Spans sourceSpans ->
      liftIO (T.hPutStrLn h ("ss: " <> T.unwords (map answerSS sourceSpans)))
    where
      answerSS (P.SourceSpan _ (P.SourcePos x1 y1) (P.SourcePos x2 y2)) = T.unwords (map show [x1, y1, x2, y2])

class Monad m => Respond m where
  respond :: Handle -> Response -> m ()

data Response
  = Message Text
  | Span Text P.SourceSpan
  | Spans [P.SourceSpan]

data Command
  = Pos Text Int Int
  | Widen
  | Narrow
  | FindOccurences Text Int Int

sockHandler :: (MonadState SState m, MonadIO m, Respond m) => Socket -> m ()
sockHandler sock = do
  (h, _, _) <- liftIO (accept sock)
  liftIO (hSetBuffering h LineBuffering)
  commandProcessor h

commandProcessor :: (MonadState SState m, MonadIO m, Respond m) => Handle -> m ()
commandProcessor h = do
  line <- liftIO (T.hGetLine h)
  liftIO (putText line)
  case simpleParse line of
      Just (Pos fp l c) -> do
        file' <- liftIO (T.readFile (toS fp))
        case zipper (allMatches file' l c) & within traverse <&> rightmost of
          Nothing -> do
            put Waiting
            respond h (Message "Didn't match")
          Just z -> do
            put (SelectingState (Selecting file' z))
            respond' h
      Just Narrow -> do
        -- [match] <- uses (selectingMatches . focus . sourceSpan . Control.Lens.to pure) traceShowId
        _SelectingState.selectingMatches %= tug rightward
        respond' h
      Just Widen -> do
        _SelectingState.selectingMatches %= tug leftward
        respond' h
      Just (FindOccurences fp l c) -> do
        file' <- liftIO (T.readFile (toS fp))
        case zipper (allMatches file' l c) & within traverse <&> rightmost of
          Nothing -> do
            put Waiting
            respond h (Message "Didn't match")
          Just z -> do
            let occurrences = evalState findOccurrences (Selecting file' z)
            respond h (Spans occurrences)
      Nothing -> liftIO (T.hPutStrLn h "Parse failure")
  liftIO (hFlush h)
  commandProcessor h

findOccurrences :: State Selecting [P.SourceSpan]
findOccurrences = do
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
      let x = traceShowId (extractOccurrences i d)
      in pure x
    _ ->
      pure []

extractOccurrences :: Text -> P.Declaration -> [P.SourceSpan]
extractOccurrences ident = matcher
  where
    (matcher, _, _, _, _) =
      P.everythingOnValues
      (<>)
      (const [])
      (\case (P.PositionedValue sourceSpan' _ (P.Var (P.Qualified Nothing (P.Ident ident')))) ->
               [sourceSpan' | ident == ident']
             _ -> [])
      (\case (P.PositionedBinder sourceSpan' _ (P.VarBinder (P.Ident ident'))) ->
               [sourceSpan' | ident == ident']
             _ -> [])
      (const [])
      (const [])

slice' :: Zipper h i Match -> Text -> Text
slice' z f = z ^. focus . sourceSpan & slice f & T.unlines

respond' :: (MonadState SState m, Respond m) => Handle -> m ()
respond' h = do
  s <- get
  case s of
    SelectingState (Selecting file' selections) ->
      let
        sp = selections^.focus.sourceSpan
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
          respond h (Span file' sp)
    Waiting -> respond h (Message "Gief me da file!")

simpleParse :: Text -> Maybe Command
simpleParse t =
  case T.words t of
    ["w"] -> Just Widen
    ["n"] -> Just Narrow
    ["o", fp, x, y] -> FindOccurences fp <$> readMaybe (toS x) <*> readMaybe (toS y)
    ["s", fp, x, y] -> Pos fp <$> readMaybe (toS x) <*> readMaybe (toS y)
    _ -> Nothing

-- MAIN --

main :: IO ()
main = withSocketsDo $ do
    args <- getArgs
    let port = fromIntegral $ fromMaybe 5678 (readMaybe =<< headMay args :: Maybe Int)
        isEmacs = True -- args ^. ix 1 == "emacs"
    sock <- listenOn $ PortNumber port
    putText $ "Listening on " <> show port
    void $ if isEmacs
           then runStateT (runEmacs (forever (sockHandler sock))) Waiting
           else runStateT (runTerminal (forever (sockHandler sock))) Waiting
