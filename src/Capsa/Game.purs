module Capsa.Game where

import Prelude

import Data.List (List(Nil), (:))

import Capsa.Types (Card)
import Capsa.Hands (compareHandRank)


-- | Chinesse poker or known in indonesia as Capsa, a 2-4 person game, each player
-- | receives a 13-card (standard 52 card deck). Each player then has to divide their cards
-- | into three poker hands. The back must be the highest-ranking hand and the front
-- | the lowest-ranking hand (Straight and Flush do not count in the 3 cards hand)
data TheFront  = TheFront Card Card Card
data TheMiddle = TheMiddle Card Card Card Card Card
data TheBack   = TheBack Card Card Card Card Card

data CardSets = CardSets TheFront TheMiddle TheBack

isValidCardSets :: CardSets -> Boolean
isValidCardSets (CardSets (TheFront c1 c2 c3) (TheMiddle c4 c5 c6 c7 c8) (TheBack c9 c10 c11 c12 c13)) =
  	compareHandRank bs ms >= EQ && compareHandRank ms fs >= EQ
  where
    fs = (c1:c2:c3:Nil)
    ms = (c4:c5:c6:c7:c8:Nil)
    bs = (c9:c10:c11:c12:c13:Nil)