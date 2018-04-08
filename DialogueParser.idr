-- Local Variables:
-- idris-load-packages: ("lightyear" "effects")
-- End:

module DialogueParser

import Data.Vect
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
  | Want a
  | Was a
  | Feel a
  | Felt a
  | Elaborate

joinSpace : List String -> String
joinSpace = trim . concat . map (++" ")

Seed : Type
Seed = Integer

replyWant : Tokens -> Eff String [RND]
replyWant tokens = 
  let joined = joinSpace tokens
      replies : Vect 4 String = ["What would it mean to you if you got " ++ joined ++ "?",
        "Have you considered why it is that you want " ++ joined ++ "?",
        "If you got " ++ joined ++ " would it make you feel better about yourself?",
        "How would it change things if you got " ++ joined ++ "?"]
  in rndSelect' replies 

replyHello : Eff String [RND]
replyHello = rndSelect' ["Hello, how are you doing?", "Please, lets begin.", 
  "Greetings, stay a while and listen."]

replyWas : Tokens -> Eff String [RND]
replyWas tokens = let joined = joinSpace tokens
  in rndSelect' ["Is that no longer the case?",
      "Do you wish that was still the case?"]

replyFeel : Tokens -> Eff String [RND]
replyFeel tokens = let joined = joinSpace tokens
  in rndSelect' ["How long have you felt this way?",
    "Do you think it's a good thing that you feel " ++ joined ++ "?",
    "Would it be better if you didn't feel " ++ joined ++ "?"]

replyFelt : Eff String [RND]
replyFelt  = rndSelect' ["Why do you think that you no longer feel this way?",
  "Do you want to feel like that again?"]
  
replyElaborate : Eff String [RND]
replyElaborate = rndSelect' ["Please provide me with more detail.",
  "Please elaborate...",
  "Please share more."]

reply : Dialogue Tokens -> Eff String [RND]
reply dlg = 
  case dlg of
    Hello => replyHello
    Want obj => replyWant obj
    Was obj => replyWas obj
    Feel obj => replyFeel obj
    Felt obj => replyFelt 
    Elaborate => replyElaborate 

Functor Dialogue where
  map f Hello = Hello 
  map f (Want obj) = Want (f obj)
  map f (Was obj) = Was (f obj)
  map f (Feel obj) = Feel (f obj)
  map f (Felt obj) = Felt (f obj)
  map f Elaborate = Elaborate

DlgParser : Type
DlgParser = Tokens -> Maybe (Dialogue Tokens)

tokenize : String -> Tokens
tokenize = words

parts : String -> Tokens -> Parts
parts sep tokens = List.split (== sep) tokens

dialogue : String -> (Tokens -> Dialogue Tokens) -> Tokens -> Maybe (Dialogue Tokens)
dialogue sep dlg tokens = if not (sep `elem` tokens)
  then Nothing
  else case parts sep tokens of
    (subject :: [object]) => Just (dlg object)
    _ => Nothing

hello : DlgParser
hello = dialogue "hello" (\a => Hello)

want : DlgParser
want = dialogue "want" Want

was : DlgParser
was = dialogue "was" Was

feel : DlgParser
feel = dialogue "feel" Feel

felt : DlgParser
felt = dialogue "felt" Felt

replaceIMe : Dialogue Tokens -> Dialogue Tokens
replaceIMe = map (\tokens =>
  map (\token => if token `List.elem` ["i", "me"] then "you" else token) tokens)

replaceMy : Dialogue Tokens -> Dialogue Tokens
replaceMy = map (\tokens =>
  map (\token => if token == "my" then "your" else token) tokens)

switchViewpoint : Dialogue Tokens -> Dialogue Tokens
switchViewpoint = replaceIMe . replaceMy

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

getSeed : IO Integer
getSeed = assert_total (run time)

listenAndReply : Integer -> Eff () [RND, STDIO]
listenAndReply seed = do
  srand seed
  putStr "Eliza> "
  listen <- getStr
  let dialogue = switchViewpoint (parseDialogue (Strings.toLower listen))
  selectedReply <- reply dialogue
  putStrLn selectedReply
