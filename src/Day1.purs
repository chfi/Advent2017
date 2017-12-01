module Day1 where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log)
import Data.Array as Array
import Data.Filterable (filter, filterMap)
import Data.Foldable (sum)
import Data.Int (fromString)
import Data.List.Lazy as LL
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (toCharArray)
import Data.String as String
import Data.Tuple (Tuple, fst, uncurry)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Node.Process (argv)
import Partial.Unsafe (unsafeCrashWith)


parseInput :: String -> Array Int
parseInput = filterMap (fromString <<< String.singleton) <<< toCharArray

shiftArray :: forall a. Array a -> Array a
shiftArray arr = case Array.uncons arr of
  Nothing -> []
  Just {head, tail} -> Array.snoc tail head

-- Slow but fun version
shiftArrayN :: forall a. Int -> Array a -> Maybe (Array a)
shiftArrayN n arr = flip LL.index n $ LL.iterate shiftArray arr

withNextN :: forall a. Int -> Array a -> Array (Tuple a a)
withNextN n arr = case shiftArrayN n arr of
  Nothing -> []
  Just arr2 -> Array.zip arr arr2


-- fast version
indexLoop :: forall a. Array a -> Int -> Maybe a
indexLoop arr n = arr `Array.index` (n `mod` Array.length arr)

shiftArrayN' :: forall a. Int -> Array a -> Array a
shiftArrayN' n arr = Array.mapWithIndex
                     (\i a -> fromMaybe a (indexLoop arr (i+n))) arr

withNextN' :: forall a. Int -> Array a -> Array (Tuple a a)
withNextN' n arr = Array.zip arr (shiftArrayN' n arr)

withNextHalf :: forall a. Array a -> Array (Tuple a a)
withNextHalf arr = withNextN' n arr
  where n = Array.length arr / 2

filterNext :: forall a. Eq a => Array (Tuple a a) -> Array (Tuple a a)
filterNext arr = filter (uncurry eq) arr

doSum :: Array (Tuple Int Int) -> Int
doSum = sum <<< map fst

main :: forall e. Eff _ Unit
main = do
  args <- argv
  inputPath <- case args `Array.index` 2 of
    Nothing -> unsafeCrashWith "Provide path to input file as argument"
    Just p  -> pure p
  text <- readTextFile UTF8 inputPath
  let parsed = parseInput text

  log "Results part 1:"
  let results1 = doSum $ filterNext $ withNextN 1 parsed
  log $ show results1

  log "Results part 2:"
  let results2 = doSum $ filterNext $ withNextHalf parsed
  log $ show results2
