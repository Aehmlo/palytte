module Palytte.Data.Generation where

newtype Generation = Generation FilePath

instance Show Generation where
  show (Generation path) = path

newtype Specialisation = Specialisation String

instance Show Specialisation where
  show (Specialisation spec) = spec
