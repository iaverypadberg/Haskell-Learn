{-# LANGUAGE RecordWildCards, FlexibleContexts #-}
module Main where

import Control.Monad.IO.Class       (liftIO)
import Control.Monad.State.Strict   (StateT, evalStateT, gets, modify)
import Data.Default                 (def)
import System.Console.StructuredCLI
import Text.Read                    (readMaybe)

data AppState = AppState { bars :: Int,
                           bazs :: Int }

type StateM = StateT AppState IO

root :: CommandsT StateM ()
root = do
  basic
  foo
  grob

basic :: CommandsT StateM ()
basic = do
  command "top" "return to the top of the tree" top
  command "exit" "go back one level up" exit

foo :: CommandsT StateM ()
foo =
    command "foo" "pity the foo" (return NewLevel) >+ do
      basic
      bar
      baz

bar :: CommandsT StateM ()
bar = param "bar" "<number of bars>" parseBars setBars >+ do
        basic
        frob
            where setBars int = do
                    bars <- gets bars
                    modify $ \s -> s { bars = bars + int }
                    return NewLevel

baz :: CommandsT StateM ()
baz = command' "baz" "do the baz thing" checkBazs $ do
        n <- modify incBaz >> gets bazs
        liftIO . putStrLn $ "You have bazzed " ++ show n ++ " times"
        return NoAction
            where incBaz s@AppState{..} = s { bazs = bazs + 1 }
                  checkBazs = do
                    bazCount <- gets bazs
                    return $ bazCount < 3 -- after 3 bazs, disable baz command

frob :: CommandsT StateM ()
frob = command "frob" "frob this level" $ do
         n <- gets bars
         liftIO . putStrLn $ "frobbing " ++ show n ++ " bars"
         return NoAction

grob :: CommandsT StateM ()
grob = custom "grob" "grob something" (parseOneOf options "what to grob") always $
         const (return NoAction)
           where options = ["fee", "fa", "fo", "fum"]
                 always  = return True

parseBars :: Validator StateM Int
parseBars = return . readMaybe

main :: IO ()
main = do
  let state0 = AppState 0 0
  evalStateT run state0
      where run = do
              result <- runCLI "some CLI" settings root
              either (error.show) return result
            settings = def { getBanner = "Some CLI Application\nTab completion is your friend!",
                             getHistory = Just ".someCLI.history" }
