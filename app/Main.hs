module Main where

import Control.Exception (bracket_)
import Data.Functor ((<&>))
import Data.Maybe (listToMaybe, mapMaybe)
import Options.Applicative (execParser)
import Palytte.Control.Generation
import Palytte.Data.Generation
import Palytte.Parse
import System.Console.ANSI
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.FilePath (FilePath, (</>))
import System.IO (hFlush, stdout)
import System.Process (createProcess, proc, waitForProcess)

readProcessExitWithPassthrough :: FilePath -> IO ExitCode
readProcessExitWithPassthrough path = do
  let create = proc path []
  (_, _, _, handle) <- createProcess create
  waitForProcess handle

activateFromPath :: FilePath -> IO (Either () ())
activateFromPath path = do
  exit <- readProcessExitWithPassthrough path
  pure $ case exit of
    ExitSuccess -> Left ()
    ExitFailure code -> Right ()

printInfo :: String -> IO ()
printInfo msg = do
  color <- hNowSupportsANSI stdout
  if color
    then
      bracket_
        ( setSGR
            [ SetColor Foreground Dull Magenta,
              SetConsoleIntensity BoldIntensity
            ]
        )
        (setSGR [Reset] >> hFlush stdout)
        (putStrLn msg)
    else putStrLn msg

activateGeneration :: Maybe Specialisation -> Generation -> IO (Either () ())
activateGeneration spec (Generation path) =
  let (message, script) = case spec of
        Nothing -> ("Activating generation " ++ path, path </> "activate")
        Just (Specialisation name) ->
          ( "Activating specialisation " ++ name ++ " of generation " ++ path,
            path </> "specialisation" </> name </> "activate"
          )
   in do
        printInfo message
        activateFromPath script

switchToSpecialisation :: Specialisation -> IO (Either () ())
switchToSpecialisation spec = do
  generations <- allGenerations
  target <- generationWith spec generations
  case target of
    Just gen -> activateGeneration (Just spec) gen
    Nothing -> putStrLn "Failed to find generation" <&> Right

switchToDefault :: IO (Either () ())
switchToDefault = do
  generations <- allGenerations
  target <- baseGeneration generations
  case target of
    Just gen -> activateGeneration Nothing gen
    Nothing -> putStrLn "Failed to find generation" <&> Right

main :: IO (Either () ())
main = do
  spec <- execParser options
  maybe switchToDefault switchToSpecialisation spec
