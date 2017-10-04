module Capsa.Types where

import Prelude

import Data.Enum (class Enum, class BoundedEnum, toEnum, fromEnum, Cardinality(..), enumFromTo)
import Data.Function (on)
import Data.List (List)
import Data.Maybe (Maybe(..))


-- |  A suit. The `Ord` and `Bounded` instances use the bridge ordering: spades highest
-- | then hearts, clubs and diamonds (lowest)
data Suit = Diamonds | Clubs | Hearts | Spades

derive instance eqSuit :: Eq Suit
derive instance ordSuit :: Ord Suit

instance boundedSuit :: Bounded Suit where
  bottom = Diamonds
  top    = Spades

instance enumSuit :: Enum Suit where
  succ = toEnum <<< (_ + 1) <<< fromEnum
  pred = toEnum <<< (_ - 1) <<< fromEnum

instance boundedEnumSuit :: BoundedEnum Suit where
  cardinality = Cardinality 4
  toEnum = case _ of
	0 -> Just Diamonds
	1 -> Just Clubs
	2 -> Just Hearts
	3 -> Just Spades
	_ -> Nothing
  fromEnum = case _ of
	Diamonds -> 0
	Clubs    -> 1
	Hearts   -> 2
	Spades   -> 3

instance showSuit :: Show Suit where\
  show Hearts   = "♥"
  show Diamonds = "♦"
  show Clubs    = "♣"
  show Spades   = "♠"

-- | Card rank. The lowest is 2
data Rank
	= Two
	| Three
	| Four
	| Five
	| Six
	| Seven
	| Eight
	| Nine
	| Ten
	| Jack
	| Queen
	| King
	| Ace

derive instance eqRank :: Eq Rank
derive instance ordRank :: Ord Rank

instance boundedRank :: Bounded Rank where
	bottom = Two
	top    = Ace

instance enumRank :: Enum Rank where
  	succ = toEnum <<< (_ + 1) <<< fromEnum
  	pred = toEnum <<< (_ - 1) <<< fromEnum

instance boundedEnumRank :: BoundedEnum Rank where
  	cardinality = Cardinality 13
  	toEnum = case _ of
		0 -> Just Two
		1 -> Just Three
		2 -> Just Four
		3 -> Just Five
		4 -> Just Six
		5 -> Just Seven
		6 -> Just Eight
		7 -> Just Nine
		8 -> Just Ten
		9 -> Just Jack
		10 -> Just Queen
		11 -> Just King
		12 -> Just Ace
		_  -> Nothing
  	fromEnum = case _ of
	    Two   -> 0
	    Three -> 1
	    Four  -> 2
	    Five  -> 3
	    Six   -> 4
	    Seven -> 5
	    Eight -> 6
	    Nine  -> 7
	    Ten   -> 8
	    Jack  -> 9
	    Queen -> 10
	    King  -> 11
	    Ace   -> 12

instance showRank :: Show Rank where
	show Two	= "2"
	show Three  = "3"
	show Four   = "4"
	show Five   = "5"
	show Six	= "6"
	show Seven  = "7"
	show Eight  = "8"
	show Nine   = "9"
	show Ten    = "10"
	show Jack	= "J"
	show Queen	= "Q"
	show King	= "K"
	show Ace	= "A"

data Card = Card Rank Suit

derive instance eqCard :: Eq Card

instance ordCard :: Ord Card where
	compare = compare `on` rank

instance showCard :: Show Card where
	show (Card r s) = show r <> " " <> show s

data HandRank
	= HighCard
	| Pair
	| TwoPair
	| Triples
	| Straight
	| Flush
	| FullHouse
	| Quads
	| StraightFlush
	| RoyalStraightFlush

derive instance eqHandRank :: Eq HandRank
derive instance ordHandRank :: Ord HandRank

instance showHandRank :: Show HandRank where
	show HighCard      		= "HighCard"
	show Pair          		= "Pair"
	show TwoPair       		= "TwoPair"
	show Triples       		= "Triples"
	show Straight      		= "Straight"
	show Flush         		= "Flush"
	show FullHouse     		= "FullHouse"
	show Quads         		= "Quads"
	show StraightFlush 		= "StraightFlush"
	show RoyalStraightFlush = "RoyalStraightFlush"

initialCard :: List Card
initialCard = Card <$> ranks <*> suits
  where
  	ranks = enumFromTo Two Ace
  	suits = enumFromTo Diamonds Spades

rank :: Card -> Rank
rank (Card a _) = a

suit :: Card -> Suit
suit (Card _ b) = b