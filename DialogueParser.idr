-- Local Variables:
-- idris-load-packages: ("effects" "lightyear")
-- End:

module DialogueParser

import public Lightyear
import public Lightyear.Char
import public Lightyear.Strings
import public Effect.Random
import public Effect.StdIO
import public Effects
import public Effect.System

%access public export
%default total

Tokens : Type
Tokens = List String

Parts : Type
Parts = List Tokens

data Dialogue a = Hello
  | Want a a
  | Was a a
  | Feel a a
  | Felt a a
  | Elaborate

joinSpace : List String -> String
joinSpace = trim . concat . map (++" ")

Seed : Type
Seed = Integer

replyWant : Tokens -> Eff String [RND]
replyWant tokens = 
  let joined = joinSpace tokens
      replies : Vect 4 String = ["What would it mean to you if you got " ++ joined,
        "Have you considered why it is that you want " ++ joined,
        "If you got " ++ joined ++ " would it really make you feel better about yourself?",
        "How would it change things if you got " ++ joined]
  in rndSelect' replies 

replyHello : Eff String [RND]
replyHello = rndSelect' ["Hello, how are you doing?", "Please, lets begin.", 
  "Greetings, stay a while and listen."]

replyWas : Tokens -> Eff String [RND]
replyWas tokens = let joined = joinSpace tokens
  in rndSelect' ["Why is it no longer the case that " ++ joined,
      "Do you wish it was still the case that " ++ joined]

replyFeel : Tokens -> Eff String [RND]
replyFeel tokens = let joined = joinSpace tokens
  in rndSelect' ["How long have you felt this way?",
    "Do you think it's a good thing that you feel " ++ joined,
    "Would it be better if you didn't have these feelings?"]

replyFelt : Eff String [RND]
replyFelt  = rndSelect' ["Why do you think that you no longer feel this way?",
  "Do you want to feel like that again?"]
  
replyElaborate : Eff String [RND]
replyElaborate = rndSelect' ["Please continue, I'm finding our discussion most enlightening",
  "We are making excellent progress, please don't stop sharing your thoughts",
  "Could you provide me with more detail?",
  "Do you feel that this line of discussion is helpful to you?"]

reply : Dialogue Tokens -> Eff String [RND]
reply dlg = 
  case dlg of
    Hello => replyHello
    Want subj obj => replyWant obj
    Was subj obj => replyWas obj
    Feel subj obj => replyFeel obj
    Felt subj obj => replyFelt 
    Elaborate => replyElaborate 

Functor Dialogue where
  map f Hello = Hello 
  map f (Want subj obj) = Want (f subj) (f obj)
  map f (Was subj obj) = Was (f subj) (f obj)
  map f (Feel subj obj) = Feel (f subj) (f obj)
  map f (Felt subj obj) = Felt (f subj) (f obj)
  map f Elaborate = Elaborate

DlgParser : Type
DlgParser = Tokens -> Maybe (Dialogue Tokens)

tokenize : String -> Tokens
tokenize = words

parts : String -> Tokens -> Parts
parts sep tokens = List.split (== sep) tokens

dialogue : String -> (Tokens -> Tokens -> Dialogue Tokens) -> Tokens -> Maybe (Dialogue Tokens)
dialogue sep dlg tokens = if not (sep `elem` tokens)
  then Nothing
  else case parts sep tokens of
    (subject :: [object]) => Just (dlg subject object)
    _ => Nothing

hello : DlgParser
hello = dialogue "hello" (\a, b => Hello)

want : DlgParser
want = dialogue "want" Want

was : DlgParser
was = dialogue "was" Was

feel : DlgParser
feel = dialogue "feel" Feel

felt : DlgParser
felt = dialogue "felt" Felt

switchViewpoint : Dialogue Tokens -> Dialogue Tokens
switchViewpoint dlg = map (\tokens =>
  map (\token => if token `List.elem` ["I", "me"] then "you" else token) tokens) dlg

combine : DlgParser -> DlgParser -> DlgParser
combine one other = \tokens => case one tokens of
  d@(Just dlg) => d
  Nothing => other tokens

parsers : List DlgParser
parsers = [hello, want, was, feel, felt]

combined : DlgParser
combined = foldl combine (head parsers) (tail parsers)

parseDialogue : String -> Dialogue Tokens
parseDialogue s = 
  let tokens = tokenize s
  in case combined tokens of 
    Just dlg => dlg
    Nothing => Elaborate

getSeed : Eff Integer [SYSTEM]
getSeed = time  

initialize : IO Integer
initialize = assert_total (run getSeed)

listenAndReply : Integer -> Eff () [RND, STDIO]
listenAndReply seed = do
  srand seed
  print "Eliza> "
  listen <- getStr
  let dialogue = parseDialogue listen
  selectedReply <- reply dialogue
  print selectedReply

main : IO ()
main = do
  seed <- initialize
  run (listenAndReply seed)
