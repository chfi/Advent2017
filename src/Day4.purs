module Day4 where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log)
import Data.Array as Array
import Data.Filterable (filter)
import Data.Foldable (all, length)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(Pattern), split)
import Data.String as String
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Node.Process (argv)
import Partial.Unsafe (unsafeCrashWith)

-- Part 1
isPassphrase :: String -> Boolean
isPassphrase l = all (\w -> length w == 1) $ Array.group' words
  where words = split (Pattern " ") l

-- Part 2
isPassphrase2 :: String -> Boolean
isPassphrase2 l = all (\w -> length w == 1) $ Array.group' words
  where words = Array.sort <<< String.toCharArray <$> split (Pattern " ") l

main :: forall e. Eff _ Unit
main = do
  args <- argv
  inputPath <- case args `Array.index` 2 of
    Nothing -> unsafeCrashWith "Provide path to input file as argument"
    Just p  -> pure p
  text <- readTextFile UTF8 inputPath

  let lines = split (Pattern "\n") (String.trim text)

  log "Results part 1:"
  let results1 :: Int
      results1 = length $ filter isPassphrase lines
  log $ show results1

  log "Results part 2:"
  let results2 :: Int
      results2 = length $ filter isPassphrase2 lines
  log $ show results2
