module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Data.List (List(Nil), (:))

import Capsa.Types (Card(..), Rank(..), Suit(..))
import Capsa.Hands (handRank, compareHandRank)

flush :: List Card
flush =
    ( Card Ace Clubs : Card Five Clubs : Card Six Clubs : Card Jack Clubs
	: Card Queen Clubs : Nil)

fullHouse :: List Card
fullHouse =
    ( Card King Clubs : Card Two Spades : Card King Hearts : Card Two Diamonds
	: Card King Spades : Nil)

straight :: List Card
straight =
    ( Card Two Spades : Card Six Clubs : Card Four Hearts : Card Three Clubs
	: Card Five Spades : Nil)

straightFlush :: List Card
straightFlush =
    ( Card Seven Hearts : Card Jack Hearts : Card Nine Hearts : Card Eight Hearts
	: Card Ten Hearts : Nil)

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
    log "Test flush card."
    logShow (handRank flush)

    log "Test full house"
    logShow (handRank fullHouse)

    log "Test straight"
    logShow (handRank straight)

    log "Test straightFlush"
    logShow (handRank straightFlush)

    log "Test compare handRank (fullHouse vs Flush)"
    logShow (compareHandRank fullHouse flush)

    log "Test compare handRank (Straight vs Flush)"
    logShow (compareHandRank straight flush)