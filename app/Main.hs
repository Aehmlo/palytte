module Main where

import Control.Exception (bracket_)
import Control.Monad (filterM)
import Data.Functor ((<&>))
import Data.List (lines)
import Data.List.Split (splitOn)
import Data.Maybe (listToMaybe, mapMaybe)
import Options.Applicative
import System.Console.ANSI
import System.Directory (doesDirectoryExist, doesFileExist)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.FilePath (FilePath, (</>))
import System.IO (hFlush, stdout)
import System.Process (createProcess, proc, readProcess, waitForProcess)

newtype Generation = Generation FilePath

instance Show Generation where
  show (Generation path) = path

newtype Specialisation = Specialisation String

instance Show Specialisation where
  show (Specialisation spec) = spec

specialisation :: Parser (Maybe Specialisation)
specialisation =
  fmap Specialisation
    <$> optional
      ( strOption
          ( long "to"
              <> short 't'
              <> metavar "NAME"
              <> help "Target specialisation name"
          )
      )

parseGeneration :: String -> Maybe Generation
parseGeneration entry = case splitOn " -> " entry of
  (_ : path : _) -> Just $ Generation path
  _ -> Nothing

allGenerations :: IO [Generation]
allGenerations = readProcess "home-manager" ["generations"] [] <&> (mapMaybe parseGeneration . lines)

hasAnySpecialisation :: Generation -> IO Bool
hasAnySpecialisation (Generation gen) = doesDirectoryExist (gen </> "specialisation")

hasSpecialisation :: Specialisation -> Generation -> IO Bool
hasSpecialisation (Specialisation spec) (Generation gen) =
  doesFileExist (gen </> "specialisation" </> spec </> "activate")

-- Given a monadic filtering function and a list of values, return the first
-- value passing the filter (or None)
firstM :: (Monad m) => (a -> m Bool) -> [a] -> m (Maybe a)
firstM filter xs = filterM filter xs <&> listToMaybe

-- Find newest generation with the appropriate specialisation
generationWith :: Specialisation -> [Generation] -> IO (Maybe Generation)
generationWith spec = firstM (hasSpecialisation spec)

-- Find newest generation that has any specialisation (implying it is itself
-- not a specialisation)
baseGeneration :: [Generation] -> IO (Maybe Generation)
baseGeneration = firstM hasAnySpecialisation

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

options :: ParserInfo (Maybe Specialisation)
options =
  info
    specialisation
    ( fullDesc
        <> progDesc "Switch between home-manager specialisations"
        <> header "palytte - a home-manager helper"
    )

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
