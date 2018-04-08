-- Local Variables:
-- idris-load-packages: ("effects")
-- End:

module Main

import Effects
import Effect.StdIO
import Data.String

import DialogueParser

data Eliza : Type where
  Conversation : IO a -> (a -> Inf Eliza) -> Eliza

data KeepRunning = Stop | Continue (Lazy KeepRunning)

total
runConversation : KeepRunning -> Eliza -> IO ()
runConversation (Continue rest) (Conversation c f) = do
  res <- c
  runConversation rest (f res)
runConversation Stop comp = pure ()

runForever : KeepRunning
runForever = Continue runForever

total
conversation : Eliza
conversation = Conversation (getSeed >>= (\seed => run (listenAndReply seed))) (\_ => conversation)

main : IO ()
main = runConversation runForever conversation

