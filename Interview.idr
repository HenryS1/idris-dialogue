-- Local Variables:
-- idris-load-packages: ("effects")
-- End:

module Main

import Effects
import Effect.StdIO
import Data.String

import DialogueParser

data Eliza : Type where
  Conversation : IO a -> Inf Eliza -> Eliza

data KeepRunning = Stop | Continue (Lazy KeepRunning)

total
runConversation : KeepRunning -> Eliza -> IO ()
runConversation (Continue rest) (Conversation ioInteraction conv) = do
  ioInteraction
  runConversation rest conv
runConversation Stop comp = pure ()

runForever : KeepRunning
runForever = Continue runForever

total
conversation : Eliza
conversation = Conversation (getSeed >>= (\seed => run (listenAndReply seed))) conversation

main : IO ()
main = runConversation runForever conversation

