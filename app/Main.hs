module Main where

import Control.Monad (filterM)
import Data.Functor ((<&>))
import Data.List (lines)
import Data.List.Split (splitOn)
import Data.Maybe (listToMaybe)
import System.Directory (doesDirectoryExist, doesFileExist)
import System.FilePath (FilePath, (</>))
import System.Process (readProcess)

newtype Generation = Generation FilePath

instance Show Generation where
  show (Generation path) = path

newtype Specialisation = Specialisation String

parseGeneration :: String -> Generation
parseGeneration entry =
  let parts = splitOn " -> " entry
      gen = parts !! 1
   in Generation gen

allGenerations :: IO [Generation]
allGenerations = readProcess "home-manager" ["generations"] [] <&> (map parseGeneration . lines)

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

printIfExists :: (Show a) => Maybe a -> IO ()
printIfExists (Just x) = print x
printIfExists Nothing = pure ()

main :: IO ()
main = do
  generations <- allGenerations
  base <- baseGeneration generations
  printIfExists base
