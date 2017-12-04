module Day3 where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log)
import Control.MonadPlus (guard)
import Data.Array as Array
import Data.Enum (class Enum)
import Data.Filterable (filter, filterMap, maybeBool)
import Data.Foldable (sum)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Int (fromString)
import Data.Lens (class Wander, ALens', Fold', IndexedTraversal', Lens', _1, _2, addOver, cloneLens, lens, over, set, to, view, viewOn, (+~), (.~), (^.))
import Data.Lens.Indexed (iwander)
import Data.Lens.Internal.Indexed (Indexed(..))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.List.Lazy as LL
import Data.Map (Map)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (class Monoid, mempty)
import Data.Newtype (class Newtype)
import Data.Ord (abs, signum)
import Data.Profunctor (class Profunctor)
import Data.Profunctor.Choice (class Choice)
import Data.Profunctor.Strong (class Strong, fanout, first, second, (&&&))
import Data.String (toCharArray)
import Data.String as String
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..), fst, snd, uncurry)
import Data.Unfoldable (class Unfoldable, unfoldr)
import Data.Unfoldable as U
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Node.Process (argv)
import Partial.Unsafe (unsafeCrashWith)
import Unsafe.Coerce (unsafeCoerce)


newtype Pt = Pt { x :: Int, y :: Int }

derive instance eqPt :: Eq Pt
derive instance ordPt :: Ord Pt
derive instance newtypePt :: Newtype Pt _
derive instance genericPt :: Generic Pt _

instance showPt :: Show Pt where
  show (Pt {x,y}) = "(" <> show x <> ", " <> show y <> ")"


_neg :: Lens' Int Int
_neg = lens negate (const negate)

dirLens :: Int -> ALens' Pt Int
dirLens 0 = _y <<< _neg
dirLens 1 = _x
dirLens 2 = _y
dirLens 3 = _x <<< _neg
dirLens i = dirLens (i `mod` 4)

_x :: Lens' Pt Int
_x = _Newtype <<< prop (SProxy :: SProxy "x")

_y :: Lens' Pt Int
_y = _Newtype <<< prop (SProxy :: SProxy "y")


instance semigroupPt :: Semigroup Pt where
  append p1 p2 = Pt { x: p1 ^. _x + p2 ^. _x
                    , y: p1 ^. _y + p2 ^. _y }

instance monoidPt :: Monoid Pt where
  mempty = Pt { x: 0, y: 0 }


walk :: forall f.
        Unfoldable f
     => ALens' Pt Int
     -> Int
     -> Pt
     -> f Pt
walk l i pt = U.unfoldr (f <=< maybeBool (_ <= len)) 1
  where len = abs i
        s = signum i
        f :: Int -> Maybe (Tuple Pt Int)
        f n = Just $ Tuple (addOver (cloneLens l) (n*s) pt) (n+1)


-- TODO use a Beautiful Fold or similar (`Last` monoid!) to carry the last Pt thru the computation
walk' :: ALens' Pt Int
      -> Int -> Pt
      -> Tuple (Array Pt) Pt
walk' l i pt = fanout id (fromMaybe pt <<< Array.last) $ walk l i pt


cont :: forall a b c. (b -> c) -> Tuple a b -> c
cont = map snd <<< second

walkUnfold :: Tuple Int Pt -> Tuple (LL.List Pt) (Tuple Int Pt)
walkUnfold (Tuple i p) = Tuple (i+1) <$>
                         (over _1 LL.fromFoldable $ walk' (dirLens i) ((i+1)/2) p)

pathL :: LL.List Pt
pathL = LL.concat $ unfoldr (Just <<< walkUnfold) (Tuple 0 mempty)

tileAt :: Int -> Maybe Pt
tileAt i = pathL `LL.index` (i-2)


main :: forall e. Eff _ Unit
main = do
  args <- argv
  inputPath <- case args `Array.index` 2 of
    Nothing -> unsafeCrashWith "Provide path to input file as argument"
    Just p  -> pure p
  text <- readTextFile UTF8 inputPath
  pure unit
  -- let parsed = parseInput text

  -- log "Results part 1:"
  -- let results1 = doSum $ filterNext $ withNextN 1 parsed
  -- log $ show results1

  -- log "Results part 2:"
  -- let results2 = doSum $ filterNext $ withNextHalf parsed
  -- log $ show results2
