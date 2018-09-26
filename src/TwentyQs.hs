-- CPSC 312 - 2018 - Games in Haskell
--  Copyright David Poole 2018, released on GNU General Public License

module TwentyQs where

-- To run it, try:
-- ghci
-- :load TwentyQs
-- go

import System.IO

data QATree = QLeaf String
            | QNode String QATree QATree
       deriving (Show)

initQATree = QNode "Is it living?"
                (QNode "Is it a person?"
                    (QLeaf "Justin Bieber")
                    (QLeaf "J-35, one of the southern resident killer whales"))
                (QNode "Is it a physical object?"
                    (QLeaf "Vancouver")
                    (QLeaf "CPSC 312"))

play :: QATree -> IO QATree
play tree =
   do
      putStrLn "Do you want to play 20 questions?"
      ans <- getLine
      if (ans `elem` ["y","yes","ye","oui"])
        then do
           putStrLn "Think of an entity"
           newtree <- askabout tree
           play newtree
        else return tree

askabout :: QATree -> IO QATree
askabout (QLeaf ans) =
  do
    putStrLn("Is it "++ans++"?")
    line <- getLine
    if (line `elem` ["y","yes","ye","oui"])
       then return (QLeaf ans)
       else do
          putStrLn("I'm sorry, please try again")
          return (QLeaf ans)
          
askabout (QNode q yes no) =
  do
    putStrLn(q)
    line <- getLine
    if (line `elem` ["y","yes","ye","oui"])
       then do
            newyes <- askabout yes
            return (QNode q newyes no)
       else do
            newno <- askabout no
            return (QNode q yes newno)
     
go :: IO QATree
go = play initQATree

