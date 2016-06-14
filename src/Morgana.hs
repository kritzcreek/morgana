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
import qualified Data.Text           as T
import qualified Data.Text.IO as T
import qualified Language.PureScript as P
import           Network (listenOn, withSocketsDo, accept, PortID(..), Socket)
import           Protolude
import           System.IO (hSetBuffering, hFlush, BufferMode(..))
import           Unsafe

data Match where
  DeclarationMatch :: P.SourceSpan -> P.Declaration -> Match
  ExprMatch        :: P.SourceSpan -> P.Expr        -> Match
  BinderMatch      :: P.SourceSpan -> P.Binder      -> Match
  deriving (Show)

getSP :: Match -> P.SourceSpan
getSP (DeclarationMatch sp _) = sp
getSP (ExprMatch sp _) = sp
getSP (BinderMatch sp _) = sp

getMatchType :: Match -> MatchType
getMatchType m = case m of
 DeclarationMatch _ _ -> Declaration
 ExprMatch _ _ -> Expression
 BinderMatch _ _ -> Expression

data MatchType where
  Expression :: MatchType
  Declaration :: MatchType
  deriving (Show, Eq)

instance Eq Match where
  (==) = (==) `on` getSP

instance Ord Match where
  compare = compare `on` getSP

extractor :: P.SourcePos -> P.Declaration -> [Match]
extractor sp = matcher
  where
    (matcher, _, _, _, _) =
      P.everythingOnValues
      (<>)
      (\case (P.PositionedDeclaration sourceSpan _ d) ->
               [DeclarationMatch sourceSpan d | matches sp sourceSpan]
             _ -> [])
      (\case (P.PositionedValue sourceSpan _ e) ->
               [ExprMatch sourceSpan e | matches sp sourceSpan]
             _ -> [])
      (\case (P.PositionedBinder sourceSpan _ b) ->
               [BinderMatch sourceSpan b | matches sp sourceSpan]
             _ -> [])
      (const [])
      (const [])

allMatches :: Text -> Int -> Int -> [Match]
allMatches t l c = concatMap (extractor (P.SourcePos l c)) (decls t)

slice :: Text -> P.SourceSpan -> [Text]
slice t (P.SourceSpan _ (P.SourcePos l1 c1) (P.SourcePos l2 c2)) =
  let (l:ls) = take (l2 - l1 + 1) . drop (l1 - 1) $ T.lines t
  in  T.drop (c1 - 1) l : fromMaybe [] (initMay ls) ++ maybeToList (T.take c2 <$> lastMay ls)

fromRight :: Either a b -> b
fromRight = fromJust . rightToMaybe

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

newtype Terminal a = Terminal { runTerminal :: StateT SState IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadState SState)

instance Respond Terminal where
  respond h (Message t) = liftIO (T.hPutStrLn h ("Message: " <> t))
  respond h (Span _ f sp) =
    liftIO (T.hPutStrLn h (T.unlines (slice f sp)))

newtype Emacs a = Emacs { runEmacs :: StateT SState IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadState SState)

instance Respond Emacs where
  respond h r = case r of
    Message t ->
      liftIO (T.hPutStrLn h ("m: " <> t))
    Span _ _ (P.SourceSpan _ (P.SourcePos x1 y1) (P.SourcePos x2 y2)) ->
      liftIO (T.hPutStrLn h ("s: " <> T.unwords (map show [x1, y1, x2, y2])))

class Monad m => Respond m where
  respond :: Handle -> Response -> m ()


data Response
  = Message Text
  | Span MatchType Text P.SourceSpan

data SState
  = Waiting
  | Selecting
    { _selectingFile :: Text
    , _selectingMatches :: Top :>> [Match] :>> Match
    }

makeLenses ''SState

data Command
  = Pos Text Int Int
  | Widen
  | Narrow

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
            put (Selecting file' z)
            respond' h
      Just Narrow -> do
        selectingMatches %= tug rightward
        respond' h
      Just Widen -> do
        selectingMatches %= tug leftward
        respond' h
      Nothing -> liftIO (T.hPutStrLn h "Parse failure")
  liftIO (hFlush h)
  commandProcessor h

slice' :: Zipper h i Match -> Text -> Text
slice' z f = z ^. focus & getSP & slice f & T.unlines

respond' :: (MonadState SState m, MonadIO m, Respond m) => Handle -> m ()
respond' h = do
  s <- get
  case s of
    Selecting file' selections ->
      let
        selection = selections ^. focus
        sourceSpan = getSP selection
        selectionType = getMatchType selection
      in
        respond h (Span selectionType file' sourceSpan)
    Waiting -> respond h (Message "Gief me da file!")

simpleParse :: Text -> Maybe Command
simpleParse t =
  case T.words t of
    ["w"] -> Just Widen
    ["n"] -> Just Narrow
    ["s", fp, x, y] -> Pos fp <$> readMaybe (toS x) <*> readMaybe (toS y)
    _ -> Nothing

-- MAIN --

main :: IO ()
main = withSocketsDo $ do
    args <- getArgs
    let port = fromIntegral $ fromMaybe 5678 (readMaybe =<< headMay args :: Maybe Int)
        isEmacs = args ^. ix 1 == "emacs"
    sock <- listenOn $ PortNumber port
    putText $ "Listening on " <> show port
    void $ if isEmacs
           then runStateT (runEmacs (forever (sockHandler sock))) Waiting
           else runStateT (runTerminal (forever (sockHandler sock))) Waiting
