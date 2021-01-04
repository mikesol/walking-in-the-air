module Klank.Dev where

import Prelude
import Color (rgba)
import Control.Comonad.Cofree (Cofree, deferCofree)
import Control.Comonad.Cofree as Cf
import Control.Monad.Reader (Reader, ask, runReader)
import Control.Monad.Rec.Class (Step(..), tailRec)
import Data.DateTime.Instant (unInstant)
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Identity (Identity(..))
import Data.Int (toNumber)
import Data.List (List(..), (:))
import Data.List as L
import Data.Map (Map, insertWith)
import Data.Map as M
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Newtype (unwrap, wrap)
import Data.NonEmpty (NonEmpty(..), (:|))
import Data.Set (Set, member, union)
import Data.Set as S
import Data.String (indexOf, Pattern(..))
import Data.Tuple (Tuple(..), fst)
import Data.Typelevel.Num (D2, D8, d0, d1, d2, d3, d4, d5, d6, d7)
import Data.Vec (Vec, (+>), empty)
import Data.Vec as V
import Effect (Effect)
import Effect.Now (now)
import Effect.Ref as Ref
import FRP.Behavior (Behavior, behavior)
import FRP.Behavior.Audio (AV(..), AudioParameter, AudioUnit, CanvasInfo(..), defaultExporter, runInBrowser_)
import FRP.Event (Event, makeEvent, subscribe)
import Graphics.Canvas (Rectangle)
import Graphics.Drawing (Color, Point)
import Graphics.Painting (Painting)
import Math (pow, (%))
import Type.Klank.Dev (Klank', klank)
import Web.Event.EventTarget (EventListener, addEventListener, eventListener, removeEventListener)
import Web.HTML (window)
import Web.HTML.Navigator (userAgent)
import Web.HTML.Window (navigator, toEventTarget)
import Web.TouchEvent (TouchEvent, TouchList)
import Web.TouchEvent.Touch (identifier)
import Web.TouchEvent.Touch as T
import Web.TouchEvent.TouchEvent (changedTouches)
import Web.TouchEvent.TouchEvent as TE
import Web.TouchEvent.TouchList as TL
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.MouseEvent as ME

tempo = 60.0 :: Number

beat = 60.0 / tempo :: Number

measure = beat * 4.0 :: Number

sectionLen = measure * 9.0 :: Number

introLen = beat * 8.0 :: Number

firstVerseStarts = introLen :: Number

secondVerseStarts = firstVerseStarts + sectionLen :: Number

instrumentsFullyFadedIn = secondVerseStarts + (2.0 * measure) :: Number

instrumentsStartFadingOut = thirdVerseStarts - (2.0 * measure) :: Number

fluteFullyFadedIn = thirdVerseStarts + (2.0 * measure) :: Number

bridgeStarts = secondVerseStarts + sectionLen :: Number

bridgeStartPlus1 = bridgeStarts + measure :: Number

bridgeStartPlus2 = bridgeStarts + 2.0 * measure :: Number

bridgeStartPlus3 = bridgeStarts + 3.0 * measure :: Number

bridgeStartPlus4 = bridgeStarts + 4.0 * measure :: Number

bridgeStartPlus5 = bridgeStarts + 5.0 * measure :: Number

bridgeStartPlus6 = bridgeStarts + 6.0 * measure :: Number

bridgeStartPlus7 = bridgeStarts + 7.0 * measure :: Number

bridgeStartPlus8 = bridgeStarts + 8.0 * measure :: Number

thirdVerseStarts = bridgeStarts + sectionLen :: Number

calcSlope :: Number -> Number -> Number -> Number -> Number -> Number
calcSlope x0 y0 x1 y1 x =
  if x1 == x0 || y1 == y0 then
    y0
  else
    let
      m = (y1 - y0) / (x1 - x0)

      b = y0 - m * x0
    in
      m * x + b

type WAccumulator
  = { backgroundPlayhead :: BackgroundVoice -> Number
    , bells :: List BellAccumulatorInfo
    , bellsLoop :: CofreeList (Number -> Number)
    , prevClicks :: Set Int
    }

type RenderInfo
  = { audioEnv :: AudioEnv
    , visualEnv :: VideoEnv
    , accumulator :: WAccumulator
    }

type Env
  = { accumulator :: WAccumulator
    , time :: Number
    , interactions :: List (Tuple Int Interaction)
    , canvas :: { w :: Number, h :: Number }
    }

bellOutro = 0.8 :: Number

type BellListener
  = { touches :: List (Tuple Int Interaction)
    , bells :: List BellAccumulatorInfo
    }

hyp :: Number -> Number -> Number -> Number -> Number
hyp x0 x1 y0 y1 = (((x0 - x1) `pow` 2.0) + ((y0 - y1) `pow` 2.0)) `pow` 0.5

inCircle :: Point -> Number -> Number -> Number -> Boolean
inCircle pt x y r = hyp pt.x x pt.y y < r

inRect :: Point -> Number -> Number -> Number -> Number -> Boolean
inRect pt x y w h = pt.x >= x && pt.x <= x + w && pt.y >= y && pt.y <= y + h

consumeTouch :: List (Tuple Int Interaction) -> Number -> Number -> Number -> Maybe (List (Tuple Int Interaction))
consumeTouch l x y r = go false l Nil
  where
  go :: Boolean -> List (Tuple Int Interaction) -> List (Tuple Int Interaction) -> Maybe (List (Tuple Int Interaction))
  go true _ acc = Just acc

  go _ Nil _ = Nothing

  go tf (Tuple _ { pt: Right _ } : b) acc = go tf b acc

  go tf (a@(Tuple _ { pt: Left xy }) : b) acc = let q = inCircle xy x y r in go q b (if q then (acc <> b) else (a : acc))

type NewBellLoop
  = { i :: BellListener, o :: BellListener }

type NewBellStep
  = Step NewBellLoop BellListener

removeStaleBellsAndUpdateForTouches :: Number -> BellListener -> BellListener
removeStaleBellsAndUpdateForTouches time iter =
  tailRec
    go
    { i: iter, o: { touches: iter.touches, bells: Nil } }
  where
  go :: NewBellLoop -> NewBellStep
  go { i: { touches: Nil }, o } = Done o

  go { i: { bells: Nil }, o } = Done o

  go { i: { touches, bells: (a : b) }, o: acc } =
    let
      o
        | { activated: Just ac } <- a =
          let
            oo
              | ac + bellOutro > time = acc -- do not add a
              | { x, y, r } <- a
              , Just l <- consumeTouch touches x y r =
                -- if a bell has been activated,
                -- we still swallow the touch
                { touches: l
                , bells: (a : acc.bells)
                }
              | otherwise = { touches, bells: (a : acc.bells) }
          in
            oo
        | { x, y, r } <- a
        , Just l <- consumeTouch touches x y r =
          { touches: l
          , bells: (a { activated = Just time } : acc.bells)
          }
        | otherwise = { touches, bells: (a : acc.bells) }
    in
      Loop { i: { touches: o.touches, bells: b }, o }

maxBellLength = 3 :: Int

ik :: Number -> Number -> Number -> Number -> Number
ik v incr mn mx = (((v - mn) + incr) % (mx - mn)) + mn

type AddBellLoop
  = { x :: Number, y :: Number, r :: Number, b :: List BellAccumulatorInfo }

type NewBellRecord
  = { nbx :: Number
    , nby :: Number
    , nbr :: Number
    , xincr :: Number
    , yincr :: Number
    , xmin :: Number
    , ymin :: Number
    , xmax :: Number
    , ymax :: Number
    }

newBellAvoidingCollisions :: Number -> NewBellRecord -> Number -> List BellAccumulatorInfo -> BellAccumulatorInfo
newBellAvoidingCollisions time { nbx, nby, nbr, xincr, yincr, xmin, ymin, xmax, ymax } pitch l =
  tailRec
    go
    { x: nbx
    , y: nby
    , r: nbr
    , b: l
    }
  where
  go :: AddBellLoop -> Step AddBellLoop BellAccumulatorInfo
  go { x, y, r, b: Nil } = Done { activated: Nothing, onset: time, x, y, r, pitch }

  go { x, y, r, b: (h : t) }
    | hyp x y h.x h.y <= r + h.r = Loop { x: ik x xincr xmin xmax, y: ik y yincr ymin ymax, r, b: l }
    | otherwise = Loop { x, y, r, b: t }

type BellsIter
  = { bells :: List BellAccumulatorInfo, stream :: CofreeList (Number -> Number) }

introduceNewBells ::
  Int ->
  CofreeList (Number -> Number) ->
  Number ->
  NewBellRecord ->
  List BellAccumulatorInfo ->
  BellsIter
introduceNewBells nnb cfr time nbr l = go nnb { bells: l, stream: cfr }
  where
  go :: Int -> BellsIter -> BellsIter
  go 0 acc = acc

  go n acc =
    go (n - 1)
      { stream: unwrap $ Cf.tail acc.stream
      , bells:
          newBellAvoidingCollisions time nbr
            (Cf.head acc.stream time)
            acc.bells
            : acc.bells
      }

type CofreeList a
  = Cofree Identity a

nonEmptyListToLoopingCofree :: forall a. NonEmpty List a -> CofreeList a
nonEmptyListToLoopingCofree (NonEmpty a b) = deferCofree \_ -> Tuple a (Identity $ go b)
  where
  go :: List a -> CofreeList a
  go Nil = nonEmptyListToLoopingCofree (NonEmpty a b)

  go (h : t) = deferCofree \_ -> Tuple h (Identity $ go t)

bellsToFun :: Vec D8 Number -> Number -> Number
bellsToFun v t
  | t < bridgeStartPlus1 = V.index v d0
  | t < bridgeStartPlus2 = V.index v d1
  | t < bridgeStartPlus3 = V.index v d2
  | t < bridgeStartPlus4 = V.index v d3
  | t < bridgeStartPlus5 = V.index v d4
  | t < bridgeStartPlus6 = V.index v d5
  | t < bridgeStartPlus7 = V.index v d6
  | otherwise = V.index v d7

firstBellVec = 60.0 +> 60.0 +> 60.0 +> 60.0 +> 60.0 +> 60.0 +> 60.0 +> 60.0 +> empty :: Vec D8 Number

secondBellVec = 60.0 +> 60.0 +> 60.0 +> 60.0 +> 60.0 +> 60.0 +> 60.0 +> 60.0 +> empty :: Vec D8 Number

bellsCollection :: NonEmpty List (Number -> Number)
bellsCollection = bellsToFun firstBellVec :| bellsToFun secondBellVec : Nil

bellsAsCycle = nonEmptyListToLoopingCofree bellsCollection :: CofreeList (Number -> Number)

normalPositions :: Number -> Number -> BackgroundVoice -> Rectangle
normalPositions w h v =
  let
    width = w / 3.0

    height = h / 3.0
  in
    case v of
      Bv0 -> { x: 0.0, y: 0.0, width, height }
      Bv1 -> { x: width, y: 0.0, width, height }
      Bv2 -> { x: 2.0 * width, y: 0.0, width, height }
      Bv3 -> { x: 0.0, y: height, width, height }
      Bv4 -> { x: 2.0 * width, y: height, width, height }
      Bv5 -> { x: 0.0, y: 2.0 * height, width, height }
      Bv6 -> { x: width, y: 2.0 * height, width, height }
      Bv7 -> { x: 2.0 * width, y: 2.0 * height, width, height }

positionsWithRoomForInstruments :: Number -> Number -> BackgroundVoice -> Rectangle
positionsWithRoomForInstruments w h v =
  let
    instrw = w / 6.0

    instrh = h / 6.0

    width = w * 2.0 / 9.0

    height = h * 2.0 / 9.0

    instrwOver2 = w / 12.0

    instrhOver2 = h / 12.0
  in
    case v of
      Bv0 -> { x: 0.0, y: 0.0, width, height }
      Bv1 -> { x: width + instrwOver2, y: 0.0, width, height }
      Bv2 -> { x: 2.0 * width + instrwOver2, y: 0.0, width, height }
      Bv3 -> { x: 0.0, y: height + instrhOver2, width, height }
      Bv4 -> { x: 2.0 * width + instrwOver2, y: height + instrhOver2, width, height }
      Bv5 -> { x: 0.0, y: 2.0 * height + instrhOver2, width, height }
      Bv6 -> { x: width + instrwOver2, y: 2.0 * height + instrhOver2, width, height }
      Bv7 -> { x: 2.0 * width + instrwOver2, y: 2.0 * height, width, height }

positionsWithRoomForFlute :: Number -> Number -> BackgroundVoice -> Rectangle
positionsWithRoomForFlute w h v =
  let
    instrw = w / 6.0

    instrh = h / 6.0

    width = w * 2.0 / 9.0

    height = h * 2.0 / 9.0
  in
    case v of
      Bv0 -> { x: instrw, y: instrh, width, height }
      Bv1 -> { x: width + instrw, y: instrh, width, height }
      Bv2 -> { x: 2.0 * width + instrw, y: instrh, width, height }
      Bv3 -> { x: instrw, y: height + instrh, width, height }
      Bv4 -> { x: 2.0 * width + instrw, y: height + instrh, width, height }
      Bv5 -> { x: instrw, y: 2.0 * height + instrh, width, height }
      Bv6 -> { x: width + instrw, y: 2.0 * height + instrh, width, height }
      Bv7 -> { x: 2.0 * width + instrw, y: 2.0 * height + instrh, width, height }

interpolateRectangles :: Number -> Number -> Number -> Rectangle -> Rectangle -> Rectangle
interpolateRectangles n0 n1 t r0 r1 =
  { x: calcSlope n0 r0.x n1 r1.x t
  , y: calcSlope n0 r0.y n1 r1.y t
  , width: calcSlope n0 r0.width n1 r1.width t
  , height: calcSlope n0 r0.height n1 r1.height t
  }

backgroundVideoCoords :: Number -> Number -> Number -> BackgroundVoice -> Rectangle
backgroundVideoCoords w h n v
  | n < secondVerseStarts = normalPositions w h v
  | n < instrumentsFullyFadedIn =
    interpolateRectangles
      secondVerseStarts
      instrumentsFullyFadedIn
      n
      (normalPositions w h v)
      (positionsWithRoomForInstruments w h v)
  | n < instrumentsStartFadingOut = positionsWithRoomForInstruments w h v
  | n < thirdVerseStarts =
    interpolateRectangles
      instrumentsStartFadingOut
      thirdVerseStarts
      n
      (positionsWithRoomForInstruments w h v)
      (normalPositions w h v)
  | n < fluteFullyFadedIn =
    interpolateRectangles
      thirdVerseStarts
      fluteFullyFadedIn
      n
      (normalPositions w h v)
      (positionsWithRoomForFlute w h v)
  | otherwise = positionsWithRoomForFlute w h v

type SynthDims
  = { oneSixthW :: Number
    , oneSixthH :: Number
    , twoNinthsW :: Number
    , twoNinthsH :: Number
    , oneThirdW :: Number
    , oneThirdH :: Number
    , sevenEighteenthsW :: Number
    , sevenEighteenthsH :: Number
    , elevenEighteenthsW :: Number
    , elevenEighteenthsH :: Number
    , twoThirdsW :: Number
    , twoThirdsH :: Number
    , sevenNinthsW :: Number
    , sevenNinthsH :: Number
    }

synthDims :: Number -> Number -> SynthDims
synthDims w h =
  { oneSixthW: w / 6.0
  , oneSixthH: h / 6.0
  , twoNinthsW: 2.0 * w / 9.0
  , twoNinthsH: 2.0 * h / 9.0
  , oneThirdW: w / 3.0
  , oneThirdH: h / 3.0
  , sevenEighteenthsW: 7.0 * w / 18.0
  , sevenEighteenthsH: 7.0 * h / 18.0
  , elevenEighteenthsW: 11.0 * w / 18.0
  , elevenEighteenthsH: 11.0 * h / 18.0
  , twoThirdsW: 2.0 * w / 3.0
  , twoThirdsH: 2.0 * h / 3.0
  , sevenNinthsW: 7.0 * w / 9.0
  , sevenNinthsH: 7.0 * h / 9.0
  }

type FluteDims
  = { oneSixthW :: Number
    , oneFourthW :: Number
    , oneHalfW :: Number
    , threeFourthsW :: Number
    , oneSixthH :: Number
    , fiveEighteenthsH :: Number
    , sevenEighteenthsH :: Number
    , nineEighteenthsH :: Number
    , elevenEighteenthsH :: Number
    , thirteenEighteenthsH :: Number
    , fiveSixthsH :: Number
    , fiveSixthsW :: Number
    , h :: Number
    , oneHalfH :: Number
    , oneNinthH :: Number
    , oneThirdH :: Number
    , twoThirdsH :: Number
    , w :: Number
    }

fluteDims :: Number -> Number -> FluteDims
fluteDims w h =
  { oneSixthW: w / 6.0
  , oneFourthW: w / 4.0
  , oneHalfW: w / 2.0
  , threeFourthsW: 3.0 * w / 4.0
  , oneSixthH: h / 6.0
  , fiveEighteenthsH: 5.0 * h / 18.0
  , sevenEighteenthsH: 7.0 * h / 18.0
  , nineEighteenthsH: 9.0 * h / 18.0
  , elevenEighteenthsH: 11.0 * h / 18.0
  , thirteenEighteenthsH: 13.0 * h / 18.0
  , fiveSixthsH: 5.0 * h / 6.0
  , fiveSixthsW: 5.0 * w / 6.0
  , h
  , oneHalfH: h / 2.0
  , oneNinthH: h / 9.0
  , oneThirdH: h / 3.0
  , twoThirdsH: 2.0 * h / 3.0
  , w
  }

fluteDisappearPositions :: FluteNote -> Reader FluteDims Rectangle
fluteDisappearPositions v = do
  { oneSixthW
  , oneFourthW
  , oneHalfW
  , threeFourthsW
  , w
  , oneSixthH
  , fiveEighteenthsH
  , oneThirdH
  , sevenEighteenthsH
  , oneHalfH
  , nineEighteenthsH
  , elevenEighteenthsH
  , twoThirdsH
  , thirteenEighteenthsH
  , fiveSixthsH
  , h
  } <-
    ask
  pure
    $ case v of
        Fn0 -> mr 0.0 0.0 oneFourthW 0.0
        Fn1 -> mr oneFourthW 0.0 oneFourthW 0.0
        Fn2 -> mr oneHalfW 0.0 oneFourthW 0.0
        Fn3 -> mr threeFourthsW 0.0 oneFourthW 0.0
        Fn4 -> mr 0.0 0.0 0.0 oneSixthH
        Fn5 -> mr 0.0 oneSixthH 0.0 oneSixthH
        Fn6 -> mr 0.0 oneThirdH 0.0 oneSixthH
        Fn7 -> mr 0.0 oneHalfH 0.0 oneSixthH
        Fn8 -> mr 0.0 twoThirdsH 0.0 oneSixthH
        Fn9 -> mr 0.0 fiveSixthsH 0.0 oneSixthH
        Fn10 -> mr w 0.0 0.0 oneSixthH
        Fn11 -> mr w oneSixthH 0.0 oneSixthH
        Fn12 -> mr w oneThirdH 0.0 oneSixthH
        Fn13 -> mr w oneHalfH 0.0 oneSixthH
        Fn14 -> mr w twoThirdsH 0.0 oneSixthH
        Fn15 -> mr w fiveSixthsH 0.0 oneSixthH
        Fn16 -> mr 0.0 h oneFourthW 0.0
        Fn17 -> mr oneFourthW h oneFourthW 0.0
        Fn18 -> mr oneHalfW h oneFourthW 0.0
        Fn19 -> mr threeFourthsW h oneFourthW 0.0

flutePositions :: FluteNote -> Reader FluteDims Rectangle
flutePositions v = do
  { oneSixthW
  , oneFourthW
  , oneHalfW
  , threeFourthsW
  , fiveSixthsW
  , w
  , oneNinthH
  , oneSixthH
  , fiveEighteenthsH
  , oneThirdH
  , sevenEighteenthsH
  , oneHalfH
  , nineEighteenthsH
  , elevenEighteenthsH
  , twoThirdsH
  , thirteenEighteenthsH
  , fiveSixthsH
  , h
  } <-
    ask
  pure
    $ case v of
        Fn0 -> mr 0.0 0.0 oneFourthW oneSixthH
        Fn1 -> mr oneFourthW 0.0 oneFourthW oneSixthH
        Fn2 -> mr oneHalfW 0.0 oneFourthW oneSixthH
        Fn3 -> mr threeFourthsW 0.0 oneFourthW oneSixthH
        Fn4 -> mr 0.0 oneSixthH oneSixthW oneNinthH
        Fn5 -> mr 0.0 fiveEighteenthsH oneSixthW oneNinthH
        Fn6 -> mr 0.0 sevenEighteenthsH oneSixthW oneNinthH
        Fn7 -> mr 0.0 nineEighteenthsH oneSixthW oneNinthH
        Fn8 -> mr 0.0 elevenEighteenthsH oneSixthW oneNinthH
        Fn9 -> mr 0.0 thirteenEighteenthsH oneSixthW oneNinthH
        Fn10 -> mr fiveSixthsW oneSixthH oneSixthW oneNinthH
        Fn11 -> mr fiveSixthsW fiveEighteenthsH oneSixthW oneNinthH
        Fn12 -> mr fiveSixthsW sevenEighteenthsH oneSixthW oneNinthH
        Fn13 -> mr fiveSixthsW nineEighteenthsH oneSixthW oneNinthH
        Fn14 -> mr fiveSixthsW elevenEighteenthsH oneSixthW oneNinthH
        Fn15 -> mr fiveSixthsW thirteenEighteenthsH oneSixthW oneNinthH
        Fn16 -> mr 0.0 fiveSixthsH oneFourthW oneSixthH
        Fn17 -> mr oneFourthW fiveSixthsH oneFourthW oneSixthH
        Fn18 -> mr oneHalfW fiveSixthsH oneFourthW oneSixthH
        Fn19 -> mr threeFourthsW fiveSixthsH oneFourthW oneSixthH

synthPositions :: SynthVoice -> Reader SynthDims Rectangle
synthPositions v = do
  { oneSixthW
  , oneSixthH
  , twoNinthsW
  , twoNinthsH
  , oneThirdW
  , oneThirdH
  , sevenEighteenthsW
  , sevenEighteenthsH
  , elevenEighteenthsW
  , elevenEighteenthsH
  , twoThirdsW
  , twoThirdsH
  , sevenNinthsW
  , sevenNinthsH
  } <-
    ask
  pure
    $ case v of
        Sv0 -> mr twoNinthsW 0.0 oneSixthW oneThirdH
        Sv1 -> mr elevenEighteenthsW 0.0 oneSixthW oneThirdH
        Sv2 -> mr 0.0 twoNinthsH twoNinthsW oneSixthH
        Sv3 -> mr sevenEighteenthsW twoNinthsH twoNinthsW oneSixthH
        Sv4 -> mr sevenNinthsW twoNinthsH twoNinthsW oneSixthH
        Sv5 -> mr twoNinthsW oneThirdH oneSixthW oneThirdH
        Sv6 -> mr elevenEighteenthsW oneThirdH oneSixthW oneThirdH
        Sv7 -> mr 0.0 elevenEighteenthsH twoNinthsW oneSixthH
        Sv8 -> mr sevenEighteenthsW elevenEighteenthsH twoNinthsW oneSixthH
        Sv9 -> mr sevenNinthsW elevenEighteenthsH twoNinthsW oneSixthH
        Sv10 -> mr twoNinthsW twoThirdsH oneSixthW oneThirdH
        Sv11 -> mr elevenEighteenthsW twoThirdsH oneSixthW oneThirdH

mr :: Number -> Number -> Number -> Number -> Rectangle
mr x y width height = { x, y, width, height }

synthDisappearPositions :: SynthVoice -> Reader SynthDims Rectangle
synthDisappearPositions v = do
  { oneSixthW
  , oneSixthH
  , twoNinthsW
  , twoNinthsH
  , oneThirdW
  , oneThirdH
  , sevenEighteenthsW
  , sevenEighteenthsH
  , elevenEighteenthsW
  , elevenEighteenthsH
  , twoThirdsW
  , twoThirdsH
  , sevenNinthsW
  , sevenNinthsH
  } <-
    ask
  pure
    $ case v of
        Sv0 -> mr oneThirdW 0.0 0.0 oneThirdH
        Sv1 -> mr twoThirdsW 0.0 0.0 oneThirdH
        Sv2 -> mr 0.0 oneThirdH twoNinthsW 0.0
        Sv3 -> mr sevenEighteenthsW oneThirdH twoNinthsW 0.0
        Sv4 -> mr sevenNinthsW oneThirdH twoNinthsW 0.0
        Sv5 -> mr oneThirdW oneThirdH 0.0 oneThirdH
        Sv6 -> mr twoThirdsW oneThirdH 0.0 oneThirdH
        Sv7 -> mr 0.0 twoThirdsH twoNinthsW 0.0
        Sv8 -> mr sevenEighteenthsW twoThirdsH twoNinthsW 0.0
        Sv9 -> mr sevenNinthsW twoThirdsH twoNinthsW 0.0
        Sv10 -> mr oneThirdW twoThirdsH 0.0 oneThirdH
        Sv11 -> mr twoThirdsW twoThirdsH 0.0 oneThirdH

synthCoords :: Number -> Number -> Number -> SynthVoice -> Maybe Rectangle
synthCoords w h n v
  | n < secondVerseStarts = Nothing
  | n < instrumentsFullyFadedIn =
    Just
      $ runReader
          ( interpolateRectangles
              secondVerseStarts
              instrumentsFullyFadedIn
              n
              <$> synthDisappearPositions v
              <*> synthPositions v
          )
          (synthDims w h)
  | n < instrumentsStartFadingOut = Just $ runReader (synthPositions v) (synthDims w h)
  | n < thirdVerseStarts =
    Just
      $ runReader
          ( interpolateRectangles
              instrumentsStartFadingOut
              thirdVerseStarts
              n
              <$> synthPositions v
              <*> synthDisappearPositions v
          )
          (synthDims w h)
  | otherwise = Nothing

fluteCoords :: Number -> Number -> Number -> FluteNote -> Maybe Rectangle
fluteCoords w h n v
  | n < thirdVerseStarts = Nothing
  | n < fluteFullyFadedIn =
    Just
      $ runReader
          ( interpolateRectangles
              thirdVerseStarts
              fluteFullyFadedIn
              n
              <$> fluteDisappearPositions v
              <*> flutePositions v
          )
          (fluteDims w h)
  | otherwise = Just $ runReader (flutePositions v) (fluteDims w h)

env :: Env -> RenderInfo
env e =
  let
    bellRadius = (min e.canvas.w e.canvas.h) / 20.0

    bellPadding = 15.0

    { bells, touches } =
      removeStaleBellsAndUpdateForTouches e.time
        { bells: e.accumulator.bells
        , touches:
            L.filter
              (\(Tuple a _) -> not $ a `member` e.accumulator.prevClicks)
              e.interactions
        }

    { bells, stream: bellsLoop } =
      if false then
        { bells, stream: e.accumulator.bellsLoop }
      else
        introduceNewBells
          (max 0 $ 3 - (L.length e.accumulator.bells))
          e.accumulator.bellsLoop
          e.time
          { nbr: bellRadius
          , nbx: e.time % e.canvas.w
          , nby: e.time % e.canvas.h
          , xincr: e.canvas.w / 10.0
          , xmin: bellRadius + bellPadding
          , xmax: e.canvas.w - bellRadius - bellPadding
          , yincr: e.canvas.h / 10.0
          , ymin: bellRadius + bellPadding
          , ymax: e.canvas.h - bellRadius - bellPadding
          }
          bells

    bvCoords = backgroundVideoCoords e.canvas.w e.canvas.h e.time

    sCoords = synthCoords e.canvas.w e.canvas.h e.time

    fCoords = fluteCoords e.canvas.w e.canvas.h e.time
  in
    { audioEnv:
        { backgrondInfo: \_ -> { note: Nt0, onset: 0.0 }
        , synthInfo: \_ -> Nothing
        , bellInfo: Nil
        , fluteInfo: Nothing
        , soloistInfo: { soloistEffect: 0.0 }
        }
    , visualEnv:
        { backgroundInfo:
            \_ ->
              { currentTime: 0.0
              , h: 0.0
              , note: Nt0
              , w: 0.0
              , x: 0.0
              , y: 0.0
              }
        , synthInfo:
            \_ -> Nothing
        , bellInfo: Nil
        , fluteInfo:
            \_ ->
              { color: rgba 0 0 0 0.0
              , h: 0.0
              , w: 0.0
              , x: 0.0
              , y: 0.0
              }
        , soloistInfo: { color: rgba 0 0 0 0.0 }
        }
    , accumulator:
        { backgroundPlayhead: \_ -> 0.0
        , bells
        , bellsLoop
        , prevClicks:
            e.accumulator.prevClicks
              `union`
                (S.fromFoldable $ map fst e.interactions)
        }
    }

audioScene :: AudioEnv -> AudioUnit D2
audioScene _ = zero

visualScene :: VideoEnv -> Painting
visualScene _ = mempty

scene :: Interactions -> WAccumulator -> CanvasInfo -> Number -> Behavior (AV D2 WAccumulator)
scene inter acc (CanvasInfo { w, h }) time = go <$> interactionLog inter
  where
  go { interactions } =
    AV
      { audio: Just (audioScene audioEnv)
      , visual:
          Just
            { painting: \_ -> visualScene visualEnv
            , words: Nil
            }
      , accumulator
      }
    where
    { audioEnv, visualEnv, accumulator } =
      env
        { accumulator: acc
        , interactions: M.toUnfoldable interactions
        , time
        , canvas: { w, h }
        }

main :: Klank' WAccumulator
main =
  klank
    { run = runInBrowser_ (scene <$> getInteractivity)
    , accumulator =
      \res _ ->
        res
          { backgroundPlayhead: \_ -> 0.0
          , bells: Nil
          , bellsLoop: bellsAsCycle
          , prevClicks: S.empty
          }
    , exporter = defaultExporter
    , webcamCache = \_ _ -> identity
    }

data BackgroundVoice
  = Bv0
  | Bv1
  | Bv2
  | Bv3
  | Bv4
  | Bv5
  | Bv6
  | Bv7

data BackgroundNote
  = Nt0 -- We're walking in the air, We're floating in the moonlit
  | Nt1 -- sky. The people far below are
  | Nt2 -- sleeping as we fly.
  | Nt3 -- I'm holding very tight. I'm riding in the midnight 
  | Nt4 -- blue. I'm finding I can fly so 
  | Nt5 -- high above with you.
  | Nt6 -- Children gaze open mouth
  | Nt7 -- Taken by surprise
  | Nt8 -- Nobody down below
  | Nt9 -- believes their eyes.
  | Nt10 -- We're walking in the air. We're dancing in the midnight
  | Nt11 -- sky. And everyone who sees us
  | Nt12 -- greets us as we fly.
  | Nt13 -- [end]

data SynthVoice
  = Sv0
  | Sv1
  | Sv2
  | Sv3
  | Sv4
  | Sv5
  | Sv6
  | Sv7
  | Sv8
  | Sv9
  | Sv10
  | Sv11

data SynthNote
  = Sn0 -- I'm holding very tight.
  | Sn1 -- I'm riding in the midnight 
  | Sn2 -- blue. I'm
  | Sn3 -- finding I can fly so 
  | Sn4 -- high above with
  | Sn5 -- you.
  | Sn6 -- Children gaze
  | Sn7 -- open mouth
  | Sn8 -- Taken by
  | Sn9 -- surprise
  | Sn10 -- Nobody
  | Sn11 -- down below
  | Sn12 -- believes
  | Sn13 -- their
  | Sn14 -- eyes.

data FluteNote
  = Fn0
  | Fn1
  | Fn2
  | Fn3
  | Fn4
  | Fn5
  | Fn6
  | Fn7
  | Fn8
  | Fn9
  | Fn10
  | Fn11
  | Fn12
  | Fn13
  | Fn14
  | Fn15
  | Fn16
  | Fn17
  | Fn18
  | Fn19

type BackgroundNoteInfo
  = { onset :: Number
    , note :: BackgroundNote
    }

type SynthNoteInfo
  = { intensity :: Number
    , note :: SynthNote
    }

type BellNoteInfo
  = { onset :: Number, note :: Number }

type FluteNoteInfo
  = { gain :: AudioParameter
    , note :: FluteNote
    }

type SoloistNoteInfo
  = { soloistEffect :: Number
    }

type AudioEnv
  = { backgrondInfo :: BackgroundVoice -> BackgroundNoteInfo
    , synthInfo :: SynthVoice -> Maybe SynthNoteInfo
    , bellInfo :: List BellNoteInfo
    , fluteInfo :: Maybe FluteNoteInfo
    , soloistInfo :: SoloistNoteInfo
    }

type BackgroundVideoInfo
  = { currentTime :: Number
    , note :: BackgroundNote
    , x :: Number
    , y :: Number
    , w :: Number
    , h :: Number
    }

type SynthVideoInfo
  = { color :: Color
    , x :: Number
    , y :: Number
    , w :: Number
    , h :: Number
    }

type BellAccumulatorInfo
  = { onset :: Number
    , activated :: Maybe Number
    , pitch :: Number
    , x :: Number
    , y :: Number
    , r :: Number
    }

type BellVideoInfo
  = { color :: Color
    , x :: Number
    , y :: Number
    , r :: Number
    }

type FluteVideoInfo
  = { color :: Color
    , x :: Number
    , y :: Number
    , w :: Number
    , h :: Number
    }

type SoloistVideoInfo
  = { color :: Color
    }

type VideoEnv
  = { backgroundInfo :: BackgroundVoice -> BackgroundVideoInfo
    , synthInfo :: SynthVoice -> Maybe SynthVideoInfo
    , bellInfo :: List BellVideoInfo
    , fluteInfo :: FluteNote -> FluteVideoInfo
    , soloistInfo :: SoloistVideoInfo
    }

newtype Interactions
  = Interactions
  { interactions :: Ref.Ref InteractionMap
  , dispose :: Effect Unit
  }

type InteractionMap
  = Map Int Interaction

type Interaction
  = { pt :: Either Point Number -- either it has a place or has ended
    , onset :: Number
    }

purge :: Number -> InteractionMap -> InteractionMap
purge n = M.filter \{ onset } -> onset + 20.0 > n

handleTE :: Boolean -> Ref.Ref InteractionMap -> TouchEvent -> Effect Unit
handleTE isEnd il te = do
  tn <- map (unwrap <<< unInstant) now
  void $ Ref.modify (go tn l ts) il
  where
  go :: Number -> Int -> TouchList -> InteractionMap -> InteractionMap
  go tn 0 _ m = (if isEnd then purge tn else identity) m

  go tn n tl m = go tn (n - 1) tl (maybe m (\t -> insertWith (\e nw -> e { pt = nw.pt }) (identifier t) { onset: tn, pt: if isEnd then Right tn else Left { x: toNumber $ T.clientX t, y: toNumber $ T.clientY t } } m) (TL.item (l - n) tl))

  ts = changedTouches te

  l = TL.length ts

handleME :: Int -> Boolean -> Ref.Ref InteractionMap -> MouseEvent -> Effect Unit
handleME i isEnd ref me = do
  tn <- map (unwrap <<< unInstant) now
  void $ Ref.modify (if isEnd then purge tn else identity <<< insertWith (\e n -> e { pt = n.pt }) i { onset: tn, pt: if isEnd then Right tn else Left { x: toNumber $ ME.clientX me, y: toNumber $ ME.clientY me } }) ref

makeTouchListener :: Boolean -> Ref.Ref InteractionMap -> Effect EventListener
makeTouchListener isEnd interactions =
  eventListener \e -> do
    TE.fromEvent e
      # traverse_ \te -> do
          handleTE isEnd interactions te

makeMouseListener :: Boolean -> Ref.Ref Int -> Ref.Ref InteractionMap -> Effect EventListener
makeMouseListener isEnd ctr interactions =
  eventListener \e -> do
    ME.fromEvent e
      # traverse_ \me -> do
          nt <- if isEnd then Ref.modify (_ + 1) ctr else Ref.read ctr
          handleME nt isEnd interactions me

getInteractivity :: Effect Interactions
getInteractivity = do
  w <- window
  nav <- navigator w
  ua <- userAgent nav
  let
    mobile = isJust (indexOf (Pattern "iPhone") ua) || isJust (indexOf (Pattern "iPad") ua) || isJust (indexOf (Pattern "Android") ua)
  ctr <- Ref.new 0
  referencePosition <- Ref.new Nothing
  totalInteractions <- Ref.new 0
  interactions <- Ref.new M.empty
  target <- toEventTarget <$> window
  touchStartListener <- makeTouchListener false interactions
  touchMoveListener <- makeTouchListener false interactions
  touchEndListener <- makeTouchListener true interactions
  mouseDownListener <- makeMouseListener false ctr interactions
  mouseMoveListener <- makeMouseListener false ctr interactions
  mouseUpListener <- makeMouseListener true ctr interactions
  if mobile then do
    addEventListener (wrap "touchstart") touchStartListener false target
    addEventListener (wrap "touchmove") touchMoveListener false target
    addEventListener (wrap "touchend") touchEndListener false target
  else do
    addEventListener (wrap "mousedown") mouseDownListener false target
    addEventListener (wrap "mousemove") mouseMoveListener false target
    addEventListener (wrap "mouseup") mouseUpListener false target
  let
    dispose =
      if mobile then do
        removeEventListener (wrap "touchstart") touchStartListener false target
        removeEventListener (wrap "touchmove") touchMoveListener false target
        removeEventListener (wrap "touchend") touchEndListener false target
      else do
        removeEventListener (wrap "mousedown") mouseDownListener false target
        removeEventListener (wrap "mousemove") mouseMoveListener false target
        removeEventListener (wrap "mouseup") mouseUpListener false target
  pure (Interactions { interactions, dispose })

withInteractions ::
  forall a.
  Interactions ->
  Event a ->
  Event { value :: a, interactions :: InteractionMap }
withInteractions (Interactions { interactions }) e =
  makeEvent \k ->
    e
      `subscribe`
        \value -> do
          interactionsValue <- Ref.read interactions
          k { value, interactions: interactionsValue }

interactionLog :: Interactions -> Behavior { interactions :: InteractionMap }
interactionLog m = behavior \e -> map (\{ value, interactions } -> value { interactions }) (withInteractions m e)
