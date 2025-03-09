module Palytte.Control.Generation where

import Data.Functor ((<&>))
import Data.Maybe (mapMaybe)
import System.Directory (doesDirectoryExist, doesFileExist)
import System.FilePath ((</>))
import System.Process (readProcess)

import Palytte.Parse
import Palytte.Data.Generation
import Palytte.Control.Monad (firstM)

allGenerations :: IO [Generation]
allGenerations = readProcess "home-manager" ["generations"] [] <&> (mapMaybe parseGeneration . lines)

hasAnySpecialisation :: Generation -> IO Bool
hasAnySpecialisation (Generation gen) = doesDirectoryExist (gen </> "specialisation")

hasSpecialisation :: Specialisation -> Generation -> IO Bool
hasSpecialisation (Specialisation spec) (Generation gen) =
  doesFileExist (gen </> "specialisation" </> spec </> "activate")

-- Find newest generation with the appropriate specialisation
generationWith :: Specialisation -> [Generation] -> IO (Maybe Generation)
generationWith spec = firstM (hasSpecialisation spec)

-- Find newest generation that has any specialisation (implying it is itself
-- not a specialisation)
baseGeneration :: [Generation] -> IO (Maybe Generation)
baseGeneration = firstM hasAnySpecialisation
