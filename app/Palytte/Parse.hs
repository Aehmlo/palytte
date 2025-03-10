module Palytte.Parse where

import Data.List.Split (splitOn)
import Options.Applicative
import Palytte.Data.Generation (Generation (..), Specialisation (..))

specialisation :: Parser (Maybe Specialisation)
specialisation =
  fmap Specialisation
    <$> optional
      ( strOption
          ( long "specialisation"
              <> short 's'
              <> metavar "NAME"
              <> help "Name of desired specialisation"
          )
      )

parseGeneration :: String -> Maybe Generation
parseGeneration entry = case splitOn " -> " entry of
  (_ : path : _) -> Just $ Generation path
  _ -> Nothing

options :: ParserInfo (Maybe Specialisation)
options =
  info
    specialisation
    ( fullDesc
        <> progDesc "Switch between home-manager specialisations"
        <> header "palytte - a home-manager helper"
    )
