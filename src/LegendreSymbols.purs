-- This module calculates Legendre symbols; for an odd prime p, the Legendre
-- symbol (a/p) is defined via:
--
-- (a/p) =
--    { 0    if a is a multiple of p
--    { 1    if a is a square modulo p (and a is not a multiple of p)
--    { -1   if a is not a square modulo p (and also not a multiple of p)
--
-- We may use the following facts for computing Legendre symbols:

-- RuleEvalZero: (0/p) can be seen from the definition to be 0.
--
-- RuleEvalOne: 1 is always a square modulo p, so (1/p) = 1 for all p.
--
-- RuleModulo: If a ≡ b mod p, then (a/b) = (b/p).

-- RuleMultiplicative: (ab/p) = (a/p)(b/p), so we can split the symbol (a/p)
-- into (q1^e1/p)(q2^e2/p)...(qk^ek/p) using a's prime factorisation.
--
-- RulePower: A consequence of RuleMultiplicative; (a^k/p) = (a/p)^k, which is
-- equal to 1 if k is even, regardless of a. In practice we will only make use
-- of this fact for powers of primes.
--
-- RuleEvalMinusOne: As a consequence of Euler's Criterion, we have that
-- (-1/p) =
--   { 1     if p ≡ 1 mod 4
--   { -1    if p ≡ 3 mod 4
--
-- RuleEvalTwo: As a consequence of Gauss's Lemma, we have that
-- (2/p) =
--   { 1     if p ≡ ±1 mod 8
--   { -1    if p ≡ ±3 mod 8
--
-- RuleReciprocity: "Law of Quadratic reciprocity." Let p,q be distinct odd
-- primes. Then
--
-- (p/q) = i(q/p), where
--   i =
--    { -1    if p ≡ q ≡ -1 mod 4,
--    { 1     otherwise.
--

module LegendreSymbols where

import Prelude
import Data.Maybe (Maybe(..), fromJust)
import Data.String as String
import Data.Either (Either(..))
import Data.Monoid (class Monoid)
import Data.Foldable (product, all, any, foldMap)
import Data.ModularArithmetic (Z, mkZ, runZ, isPrime, primeFactors, enumerate)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Tuple.Nested ((/\))
import Data.List (List(..), (:))
import Data.List as List
import Data.Typelevel.Num (class Pos, reifyIntP)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Partial.Unsafe (unsafePartial)

data Expr = Expr Int (List (Tuple Int Int))

-- A `Reductions` consists of an initial expression followed by a list of
-- reduction steps (together with the rule being applied).
type Reductions =
  { initial :: Expr
  , steps :: List (Tuple ReductionRule Expr)
  }

derive instance eqExpr :: Eq Expr
derive instance ordExpr :: Ord Expr
derive instance genericExpr :: Generic Expr _

instance showExpr :: Show Expr where
  show = genericShow

data ReductionRule
  = RuleEvalZero
  | RuleEvalOne
  | RuleModulo
  | RuleMultiplicative
  | RulePower
  | RuleEvalMinusOne
  | RuleEvalTwo
  | RuleReciprocity

derive instance eqReductionRule :: Eq ReductionRule
derive instance ordReductionRule :: Ord ReductionRule
derive instance genericReductionRule :: Generic ReductionRule _

instance showReductionRule :: Show ReductionRule where
  show = genericShow

-- The `Expr` type represents an integer symbolically, so we may add and
-- multiply them. However, we're usually only interested in multiplying `Expr`
-- values, so we give them a Semigroup instance which does this.
instance semigroupExpr :: Semigroup Expr where
  append (Expr i1 xs) (Expr i2 ys) =
    Expr (i1 * i2) (xs <> ys)

instance monoidExpr :: Monoid Expr where
  mempty = Expr one Nil

single :: Tuple Int Int -> Expr
single t = Expr one (pure t)

lit :: Int -> Expr
lit i = Expr i Nil

-- A version of `mod` which returns a value whose sign is the same as the
-- divisor, instead of that of the dividend.
emod :: Int -> Int -> Int
emod x m =
  ((x `mod` m) + m) `mod` m

by = flip Tuple

-- Attempt to reduce a single Legendre symbol (a/p), returning an integer
-- (either 1, 0, or -1) multiplied by a (possibly empty) list of simpler
-- Legendre symbols (b1/p)(b2/p)... etc. This function also returns the
-- reduction rule used.
reduce :: Tuple Int Int -> Tuple ReductionRule Expr
reduce (Tuple a p)
  | not (0 <= a && a < p) =
      single (Tuple (a `emod` p) p) `by` RuleModulo

  | a == 0 =
      lit 0 `by` RuleEvalZero

  | a == 1 =
      lit 1 `by` RuleEvalOne

  | a == p - 1 =
      let
        e = if p `mod` 4 == 1
              then lit 1
              else lit (-1)
      in
        e `by` RuleEvalMinusOne

  | a == 2 =
      let
        r = p `mod` 8
        e = if r == 1 || r == 7
              then lit 1
              else lit (-1)
      in
        e `by` RuleEvalTwo

  | isPrime a =
      let
        e = if p `mod` 4 == 3 && a `mod` 4 == 3
              then lit (-1) <> single (Tuple p a)
              else single (Tuple p a)
      in
        e `by` RuleReciprocity

  | Just (Tuple q k) <- asPrimePower a =
      let
        e = if k `mod` 2 == 0
              then lit 1
              else single (Tuple q p)
      in
        e `by` RulePower

  | otherwise =
      -- a is neither prime nor a power of a prime, so it can be written as a
      -- product of prime powers with at least two distinct primes occuring in
      -- this product. Therefore, we can split it according to these prime
      -- power factors (and make progress).
      let
        e = foldMap (\b -> single (Tuple b p)) (primePowerFactors a)
      in
        e `by` RuleMultiplicative

reduceExpr :: Expr -> Either Int (Tuple ReductionRule Expr)
reduceExpr = case _ of
  Expr 0 _ ->
    Left 0
  Expr i Nil ->
    Left i
  Expr i (x:xs) ->
    Right (map (_ <> Expr i xs) (reduce x))

isFullyReduced :: Expr -> Boolean
isFullyReduced = case _ of
  Expr _ Nil ->
    true
  _ ->
    false

primePowerFactors :: Int -> List Int
primePowerFactors = bunchUp <<< primeFactors
  where
  bunchUp = map product <<< List.group

-- If an integer can be written in the form p^k, return this p and this k;
-- otherwise return Nothing.
asPrimePower :: Int -> Maybe (Tuple Int Int)
asPrimePower n =
  case primeFactors n of
    Nil ->
      Nothing
    (p:ps) ->
      if all (_ == p) ps
        then Just (Tuple p (List.length ps + 1))
        else Nothing

-- Render an Expr as Latex math code.
exprToLatex :: Expr -> String
exprToLatex = renderExpr pairLatex
  where
  pairLatex (Tuple a p) =
    "\\left( \\frac{" <> show a <> "}{" <> show p <> "} \\right)"

-- Render an Expr as a plain string.
exprToString :: Expr -> String
exprToString = renderExpr pairPlain
  where
  pairPlain (Tuple a p) =
    "(" <> show a <> "/" <> show p <> ")"

-- Render an Expr as a string, given a function for rendering a single Legendre
-- symbol (a/p).
renderExpr :: (Tuple Int Int -> String) -> Expr -> String
renderExpr f = case _ of
  Expr i Nil ->
    show i
  Expr i xs ->
    litStr i <> foldMap f xs
    where
    litStr i =
      case i of
        1  -> ""
        -1 -> "-"
        x  -> show x

-- Return the next n reductions of the given Expr; if there are less than n
-- reductions remaining to be made, then just return all remaining reductions.
reduceN :: Int -> Expr -> Reductions
reduceN n expr =
  { initial: expr
  , steps: List.reverse (go Nil n expr)
  }
  where
  go acc n expr =
    if n <= 0
      then
        acc
      else
        case reduceExpr expr of
          Right step ->
            go (step:acc) (n-1) (snd step)
          Left _ ->
            acc

-- If `p` is an odd prime, the expression `legendreSymbol a p` evaluates to the
-- Legendre symbol (a/p). Otherwise, `legendreSymbol a p` is not defined.
legendreSymbol :: Int -> Int -> Int
legendreSymbol a p = go (single (Tuple a p))
  where
  go expr =
    case reduceExpr expr of
      Right (Tuple _ expr') -> go expr'
      Left i -> i

-- Calculate a Legendre symbol by brute force. Just for testing.
legendreSymbol' :: Int -> Int -> Int
legendreSymbol' a p =
  if a `mod` p == 0
    then 0
    else if reifyIntP p hasSquareRoot
           then 1
           else -1
  where
  square :: forall a. Ring a => a -> a
  square x = x * x

  hasSquareRoot :: forall p. Pos p => p -> Boolean
  hasSquareRoot _ =
    any (_ == mkZ a)
        (map square (enumerate :: _ _ (Z p)))
