module Palytte.Parse where

import Data.List.Split (splitOn)
import Options.Applicative
import Palytte.Data.Generation (Generation (..), Specialisation (..))

parseGeneration :: String -> Maybe Generation
parseGeneration entry = case splitOn " -> " entry of
  (_ : path : _) -> Just $ Generation path
  _ -> Nothing

newtype SwitchOptions = SwitchOptions {switchTarget :: Maybe Specialisation} deriving (Show)

newtype ListOptions = ListOptions {listAll :: Bool} deriving (Show)

data Command
  = Switch SwitchOptions
  | List ListOptions
  deriving (Show)

specialisation :: Parser (Maybe Specialisation)
specialisation =
  fmap Specialisation
    <$> optional
      ( strOption
          ( long "specialisation"
              <> short 's'
              <> short 'c'
              <> metavar "NAME"
              <> help "Name of desired specialisation"
          )
      )

all :: Parser Bool
all = switch (long "all" <> short 'a' <> help "List all available generations")

listOptions :: ParserInfo ListOptions
listOptions =
  info
    ((ListOptions <$> Palytte.Parse.all) <**> helper)
    ( fullDesc
        <> progDesc "List home-manager generations"
    )

switchOptions :: ParserInfo SwitchOptions
switchOptions =
  SwitchOptions
    <$> info
      (specialisation <**> helper)
      ( fullDesc
          <> progDesc "Switch between home-manager specialisations"
      )

subcommand :: ParserInfo Command
subcommand =
  info
    ( subparser
        ( command "switch" (Switch <$> switchOptions)
            <> command "list-generations" (List <$> listOptions)
        )
        <**> helper
    )
    (header "palytte - a home-manager helper")
