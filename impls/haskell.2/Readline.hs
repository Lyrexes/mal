module Readline
( addHistory, readline, loadHistory )
where

import qualified System.Console.Readline as RL

import Control.Monad (when)
import System.Directory (getHomeDirectory, doesFileExist)
import System.IO.Error (tryIOError)

historyFile :: IO String
historyFile = do
    home <- getHomeDirectory
    return $ home ++ "/.mal-history"

loadHistory :: IO ()
loadHistory = do
    hfile <- historyFile
    fileExists <- doesFileExist hfile
    when fileExists $ do
        content <- readFile hfile
        mapM_ RL.addHistory (lines content)

readline :: String -> IO (Maybe String)
readline = RL.readline

addHistory :: String -> IO ()
addHistory line = do
    hfile <- historyFile
    _ <- tryIOError (appendFile hfile (line ++ "\n"))
    RL.addHistory line
