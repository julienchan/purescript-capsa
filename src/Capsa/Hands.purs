module Capsa.Hands
  ( handRank
  , compareHandRank
  ) where

import Prelude

import Data.Enum (succ)
import Data.Foldable (elem, maximum, foldl, foldr)
import Data.Function (on)
import Data.List (List(Nil), (:), length, sortBy, groupBy, nubBy, sort, head, take, filter, concat, drop, singleton)
import Data.List.NonEmpty as LN
import Data.List.Partial as LP
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))

import Partial (crashWith)
import Partial.Unsafe (unsafePartial)

import Capsa.Types (Card, HandRank(..), Rank(..), rank, suit)


-- | compare hand rank of two cards
compareHandRank :: List Card -> List Card -> Ordering
compareHandRank xs ys =
  let
    Tuple h1 xs' = handRank xs
    Tuple h2 ys' = handRank ys
  in
    case compare h1 h2 of
        EQ -> case h1 of
        Straight      -> maximum xs' `compare` maximum ys'
        StraightFlush -> maximum xs' `compare` maximum ys'
        Quads         -> head xs' `compare` head ys'
        FullHouse     -> head xs' `compare` head ys'
        _             -> EQ
      GT -> GT
      LT -> LT

-- | Determine the hand rank of the given card.
handRank :: List Card -> Tuple HandRank (List Card)
handRank xs = maybe ifNotFlush ifFlush (maybeFlush xs)
  where
    ifFlush ys = maybe (Tuple Flush $ take 5 ys) checkRoyalStraighFlush (maybeStraight ys)
    ifNotFlush = maybe (checkGroups xs) (Tuple Straight) (maybeStraight xs)

maybeFlush :: List Card -> Maybe (List Card)
maybeFlush xs = do
    mxs <- head $ sortByLength $ groupBySuit $ sortBySuit xs
    if LN.length mxs >= 5
      then pure (LN.toList mxs)
      else Nothing
  where
    groupBySuit = groupBy (eq `on` suit)
    sortBySuit  = sortBy (comparing suit <> flip compare)

-- | Check if the given card is straight
maybeStraight :: List Card -> Maybe (List Card)
maybeStraight cs = do
    xs <- head $ sortByLen $ groupBySucc $ sort cs'
    if length xs >= 5
      then pure (lastN' 5 xs)
      else maybeWheel cs'
  where
    sortByLen = sortBy (flip (comparing length) <> flip compare)
    cs'  = nubBy ((==) `on` rank) cs

sortByLength :: forall a. Ord a => List (NonEmptyList a) -> List (NonEmptyList a)
sortByLength = sortBy (flip (comparing LN.length) <> flip compare)

-- | Check [A, 2, 3, 4, 5] straight.
maybeWheel :: List Card -> Maybe (List Card)
maybeWheel cs = if length cs' == 5 then Just cs' else Nothing
  where
    cs' = filter (flip elem [Ace, Two, Three, Four, Five] <<< rank) cs

checkGroups :: List Card -> Tuple HandRank (List Card)
checkGroups h = Tuple hr cs
  where
    gs = sortByLength $ groupBy ((==) `on` rank) $ sort h
    cs = take 5 $ concat $ map LN.toList gs
    hr = case map LN.length gs of
      (4:_)    -> Quads
      (3:2:_)  -> FullHouse
      (3:_)    -> Triples
      (2:2:_)  -> TwoPair
      (2:_)    -> Pair
      _        -> HighCard

-- | RoyalStraightFlush [10, J, Q, K, A] else is StraightFlush. Note: this function
-- | should be called after checking the cards is flush and straight.
checkRoyalStraighFlush :: List Card -> Tuple HandRank (List Card)
checkRoyalStraighFlush xs =
    if (map rank xs) == highestStraight
        then Tuple RoyalStraightFlush xs
        else Tuple StraightFlush xs
  where
    highestStraight = (Ten : Jack : Queen : King : Ace : Nil)

groupBySucc :: List Card -> List (List Card)
groupBySucc = foldr f Nil
  where
    f a Nil = singleton (singleton a)
    f a xs@(x:xs') =
        if succ (rank a) == (rank <$> head x)
            then (a : x) : xs'
            else singleton a : xs

lastN' :: forall a. Int -> List a -> List a
lastN' n = foldl (\xs _ -> unsafePartial (crashWith "Capsa.Hands lastN" (LP.tail xs))) <*> drop n