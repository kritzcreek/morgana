module MorganaTest where

hello = 1 + 2

whatever :: Int
whatever (Just {hello, whatever: x}) hello =
   do hello x x + show x
      hello x x

      rofl <- x test
      -- x
      x {hello}
      lol
      rofl
      q

      pure {x: x}
   where
     X q = let a = b in q rofl
     X lol = x

double :: Int -> Int -> Int
double x = x + x

declarationLevelBinder x = x

declarationLevelBinder x = do x

declarationLevelPattern (X x) = x

letBinder y =
  let
    x = y
  in
    x

whereBinder y = x
  where
    x = y

wherePattern y = x
  where
    (X x) = x
    (Y k) = x

caseBinder = case _ of
  x -> x

casePattern = case _ of
  X x -> x
  Y x -> x

doBinder = do
  x <- y
  x

doPattern = do
  X x <- y
  x

lambdaBinder = \x -> x \x -> x

lambdaPattern = \(X x) -> x

caseLetNestedBinder = case _ of
  X x ->
    let x = y in x
  x -> x

asd = asdvfc

  -- (delete-all-overlays)
