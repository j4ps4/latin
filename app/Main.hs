module Main where

import           Lib
import           Options.Applicative
import           System.Console.ANSI (Color (..))
import           Data.Monoid

main :: IO ()
main = execParser opts >>= runApp
  where
    parser = CmdLine <$> switch (short 'v' <> long "verbose" <> help "output full declination tables")
                     <*> switch (short 't' <> long "translate" <> help "search for translation")
                     <*> switch (short 'i' <> long "invariant" <> help "case invariant")
                     <*> option auto (value Cyan <> long "color" <> help "color used in printing matches")
                     <*> switch (long "html" <> help "print out <br> linebreaks")
                     <*> switch (long "itoj" <> help "change i's to j's")
                     <*> strOption (value "" <> long "output" <> help "output file name")
                     <*> some   (argument str (metavar "QUERY..."))
    opts = info (helper <*> parser) (fullDesc <> header "latin - latin word search tool")
