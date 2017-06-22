{-# LANGUAGE NoImplicitPrelude #-}
module Morgana.Types where

import           Protolude hiding (from, (&), to)

import           Control.Lens
import           Control.Zipper      (Top, (:>>))
import qualified Data.Text           as T
import qualified Language.PureScript as P

data Match where
  DeclarationMatch     :: P.Declaration                -> Match
  ExprMatch            :: SSpan -> P.Expr              -> Match
  BinderMatch          :: SSpan -> P.Binder            -> Match
  DoNotationMatch      :: SSpan -> P.DoNotationElement -> Match
  CaseAlternativeMatch :: SSpan -> P.CaseAlternative   -> Match
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

data Response
  = Message Text
  | Span SSpan
  | Spans [SSpan]
  | Edits [(SSpan, Text)]

data Command
  = Pos Text Int Int
  | Widen
  | Narrow
  | FindOccurrences Text Int Int
  | Rename Text Int Int Text
  | ExtractFunction Text

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

getSpanFromBinder :: P.Binder -> Maybe P.SourceSpan
getSpanFromBinder (P.PositionedBinder ss _ _) = Just ss
getSpanFromBinder _ = Nothing
getSpanFromGuardedExpr :: P.GuardedExpr -> Maybe P.SourceSpan
getSpanFromGuardedExpr (P.GuardedExpr _ (P.PositionedValue ss _ _)) = Just ss
getSpanFromGuardedExpr _ = Nothing

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

instance Eq Match where
  (==) = (==) `on` view matchSpan

instance Ord Match where
  compare = compare `on` view matchSpan

showWithoutSpans :: Match -> Text
showWithoutSpans m = case m of
  DeclarationMatch d -> show d
  ExprMatch _ e -> show e
  BinderMatch _ b -> show b
  DoNotationMatch _ d -> show d
  CaseAlternativeMatch _ c -> show c

matchSpan :: Lens' Match SSpan
matchSpan = lens getSP setSP
  where
    getSP :: Match -> SSpan
    getSP (DeclarationMatch d) = P.declSourceSpan d ^. convertSpan
    getSP (ExprMatch sp _) = sp
    getSP (BinderMatch sp _) = sp
    getSP (DoNotationMatch sp _) = sp
    getSP (CaseAlternativeMatch sp _) = sp

    setSP :: Match -> SSpan -> Match
    -- TODO: Make a proper setter, or don't actually provide a full lens
    setSP (DeclarationMatch d) _ = DeclarationMatch d
    setSP (ExprMatch _ d) sp = ExprMatch sp d
    setSP (BinderMatch _ d) sp = BinderMatch sp d
    setSP (DoNotationMatch _ d) sp = DoNotationMatch sp d
    setSP (CaseAlternativeMatch _ d) sp = CaseAlternativeMatch sp d
