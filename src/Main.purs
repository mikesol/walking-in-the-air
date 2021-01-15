module Klank.Dev where

import Prelude
import Color (rgb, rgba)
import Control.Comonad.Cofree (Cofree, deferCofree)
import Control.Comonad.Cofree as Cf
import Control.Monad.Reader (Reader, ask, runReader)
import Control.Monad.Rec.Class (Step(..), tailRec)
import Data.Array as A
import Data.DateTime.Instant (unInstant)
import Data.Either (Either(..))
import Data.Foldable (traverse_, foldl, fold)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Identity (Identity(..))
import Data.Int (floor, toNumber)
import Data.Lens (over, traversed)
import Data.Lens.Record (prop)
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
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..), fst, snd)
import Data.Typelevel.Num (D2, D8, d0, d1, d2, d3, d4, d5, d6, d7)
import Data.Vec (Vec, (+>), empty)
import Data.Vec as V
import Effect (Effect)
import Effect.Now (now)
import Effect.Ref as Ref
import FRP.Behavior (Behavior, behavior)
import FRP.Behavior.Audio (AV(..), AudioUnit, CanvasInfo(..), EngineInfo, defaultExporter, defaultParam, gain_', pannerMono_, playBufT_, playBuf_, runInBrowser_, sinOsc_, speaker)
import FRP.Event (Event, makeEvent, subscribe)
import Graphics.Canvas (Rectangle)
import Graphics.Drawing (Color, Point)
import Graphics.Painting (Gradient(..), ImageSource(..), Painting, circle, drawImage, drawImageFull, fillColor, fillGradient, filled, rectangle)
import Klank.Dev.Util (makeBuffersKeepingCache, makeCanvasesKeepingCache, makeImagesKeepingCache, makePooledCanvasesKeepingCache, makeVideosKeepingCache)
import Klank.Dev.Util as KU
import Math (pi, pow, sin, (%))
import Type.Klank.Dev (Klank', defaultEngineInfo, klank)
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

fluteNoteToRGB :: FluteNote -> RGB
fluteNoteToRGB fn = case fn of
  Fn0 -> RGB 255 231 190
  Fn1 -> RGB 246 219 198
  Fn2 -> RGB 255 190 190
  Fn3 -> RGB 255 179 191
  Fn4 -> RGB 255 194 255
  Fn5 -> RGB 225 201 248
  Fn6 -> RGB 194 194 255
  Fn7 -> RGB 255 219 153
  Fn8 -> RGB 243 198 165
  Fn9 -> RGB 255 153 153
  Fn10 -> RGB 255 179 191
  Fn11 -> RGB 255 153 255
  Fn12 -> RGB 205 165 243
  Fn13 -> RGB 153 153 255
  Fn14 -> RGB 255 193 77
  Fn15 -> RGB 234 154 98
  Fn16 -> RGB 255 77 77
  Fn17 -> RGB 255 102 128
  Fn18 -> RGB 255 77 255
  Fn19 -> RGB 168 98 234

fluteNoteToPeriod :: FluteNote -> Number
fluteNoteToPeriod fn = case fn of
  Fn0 -> 0.00
  Fn1 -> 0.05
  Fn2 -> 0.10
  Fn3 -> 0.15
  Fn4 -> 0.20
  Fn5 -> 0.25
  Fn6 -> 0.30
  Fn7 -> 0.35
  Fn8 -> 0.40
  Fn9 -> 0.45
  Fn10 -> 0.50
  Fn11 -> 0.55
  Fn12 -> 0.60
  Fn13 -> 0.65
  Fn14 -> 0.70
  Fn15 -> 0.75
  Fn16 -> 0.80
  Fn17 -> 0.85
  Fn18 -> 0.90
  Fn19 -> 0.95

wereWalkingOnTheAirEngineInfo =
  defaultEngineInfo
    { msBetweenSamples = 50
    , msBetweenPings = 45
    } ::
    EngineInfo

kr = (toNumber wereWalkingOnTheAirEngineInfo.msBetweenSamples) / 1000.0 :: Number

nFrames = 1560 :: Int -- 165 :: Int

fullVideoWidth = 840.0 :: Number

fullVideoHeight = 480.0 :: Number

videoWidth = fullVideoWidth / 3.0 :: Number

videoHeight = fullVideoHeight / 3.0 :: Number

backgroundFps = 20.0 :: Number

--videoWidth = 768.0 :: Number
--videoHeight = 512.0 :: Number
snowWidth = 1000.0 :: Number

snowHeight = 1000.0 :: Number

snowVideoLen = 30.0 :: Number

tempo = 60.0 :: Number

beat = 60.0 / tempo :: Number

halfBeat = beat / 2.0 :: Number

measure = beat * 4.0 :: Number

backgroundOverhang = beat :: Number

sectionLen = measure * 9.0 :: Number

introLen = measure * 6.0 :: Number

touchableStarts = measure * 4.0 :: Number

singingStarts = measure * 2.0 :: Number

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

thirdVerseEnds = thirdVerseStarts + sectionLen :: Number

pieceEnds = thirdVerseStarts + 2.0 * measure :: Number

type AudioUnitD2
  = AudioUnit D2

type ResizeInfo
  = { x :: Number
    , y :: Number
    , sWidth :: Number
    , sHeight :: Number
    }

resizeVideo :: Number -> Number -> Number -> Number -> ResizeInfo
resizeVideo sourceWidth sourceHeight targetWidth targetHeight =
  let
    sourceRatio = sourceWidth / sourceHeight

    targetRatio = targetWidth / targetHeight

    scale =
      if sourceRatio < targetRatio then
        sourceWidth / targetWidth
      else
        sourceHeight / targetHeight

    resizeWidth = (sourceWidth / scale)

    resizeHeight = (sourceHeight / scale)

    cropLeft = ((resizeWidth - targetWidth) / 2.0)

    cropTop = ((resizeHeight - targetHeight) / 2.0)
  in
    { x: cropLeft * scale
    , y: cropTop * scale
    , sWidth: sourceWidth - cropLeft * scale * 2.0
    , sHeight: sourceHeight - cropTop * scale * 2.0
    }

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

type BackgroundEventInfo
  = { onset :: Number
    , interruptedAt :: Maybe Number
    , note :: BackgroundNote
    }

type SynthEventInfo
  = { energy :: Number
    , note :: SynthNote
    }

type WAccumulator
  = { backgroundEvents :: BackgroundVoice' (List BackgroundEventInfo)
    , activeSynthEvents :: SynthVoice' (Maybe SynthEventInfo)
    , bells :: List BellAccumulatorInfo
    , bellsLoop :: CofreeList (Number -> Number)
    , fluteHistory :: List FluteAccumulatorInfo
    , prevClicks :: Set Int
    }

type RenderInfo
  = { audio :: List AudioUnitD2
    , visual :: Painting
    , accumulator :: WAccumulator
    }

type Env
  = { accumulator :: WAccumulator
    , time :: Number
    , interactions :: List (Tuple Int Interaction)
    , canvas :: { w :: Number, h :: Number }
    }

bellOutro = 1.6 :: Number

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
backgroundVideoCoords w h n v = f'
  where
  f' = normalPositions w h v

  f
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

isRectangleTouched :: List Interaction -> Rectangle -> Boolean
isRectangleTouched l r = go l
  where
  go Nil = false

  go ({ pt: Left pt } : b) = inRect pt r.x r.y r.width r.height || go b

  go ({ pt: Right _ } : b) = go b

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
  | n < instrumentsStartFadingOut =
    Just
      $ runReader (synthPositions v) (synthDims w h)
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

listAsNel :: forall a b. b -> (NonEmpty List a -> b) -> List a -> b
listAsNel b f Nil = b

listAsNel _ f (a : b) = f (a :| b)

toNel :: forall a. Monoid a => List a -> NonEmpty List a
toNel Nil = mempty :| Nil

toNel (a : b) = a :| b

timeToBackgroundFrame :: BackgroundNote -> Number -> Int
timeToBackgroundFrame _ n = max 0 (min (nFrames - 1) (floor (n * backgroundFps)))

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

bindBetween :: Number -> Number -> Number -> Number
bindBetween mn mx n = max mn (min mx n)

backgroundEventsToVideo :: BackgroundVoice -> (BackgroundVoice -> Rectangle) -> List BackgroundEventInfo -> Number -> Painting
backgroundEventsToVideo v bvCoords Nil time = mempty

backgroundEventsToVideo v bvCoords (a : _) time =
  let
    currentEvent = a

    videoCoords = bvCoords v

    whRatio = videoCoords.width / videoCoords.height

    wcrop = if whRatio > 1.0 then videoWidth else whRatio * videoHeight

    resizeInfo = resizeVideo videoWidth videoHeight videoCoords.width videoCoords.height

    -- img = FromVideo { name: show v <> show currentEvent.note, currentTime: Just $ time - currentEvent.onset }
    img = FromImage { name: show (timeToBackgroundFrame currentEvent.note (time - currentEvent.onset)) }

    -- out = drawImage img' 0.0 0.0
    out = drawImageFull img (resizeInfo.x + (voiceToWidth v)) (resizeInfo.y + (voiceToHeight v)) resizeInfo.sWidth resizeInfo.sHeight videoCoords.x videoCoords.y videoCoords.width videoCoords.height
  in
    out

synthEventToVideo :: SynthVoice -> Number -> Rectangle -> Painting
synthEventToVideo v energy a =
  filled
    ( fillGradient
        (energyToGradient a (synthVoiceToBaseRGB v) energy)
    )
    (rectangle a.x a.y a.width a.height)

synthEventToAudio :: SynthVoice -> Number -> SynthEventInfo -> AudioUnitD2
synthEventToAudio v time evt =
  let
    maxG = synthVoiceOtMaxGain v

    mult = synthVoiceOtMult v

    freq = synthNoteBase evt.note

    dist = synthNoteOvertoneDistortion evt.note
  in
    pannerMono_ (show v <> "pan") 0.0 (gain_' (show v <> "gain") (bindBetween 0.0 maxG (calcSlope 0.0 0.0 3.0 maxG evt.energy)) (sinOsc_ (show v <> "osc") (freq * mult * (dist `pow` mult))))

midiToMult :: Number -> Number
midiToMult n = 2.0 `pow` ((60.0 - n) / 12.0)

bellsToVisual :: List BellAccumulatorInfo -> Number -> Painting
bellsToVisual l time = fold $ map go l
  where
  go bell =
    let
      opacity =
        max 0.0
          ( ( min 1.0
                ( ( case bell.activated of
                      Just x -> x
                      Nothing -> time
                  )
                    - bell.onset
                )
            )
              - ( case bell.activated of
                    Just x -> time - x
                    Nothing -> 0.0
                )
          )
    in
      filled (fillColor $ rgba 0 0 0 opacity) (circle bell.x bell.y bell.r)

bellsToAudio :: List BellAccumulatorInfo -> Number -> List AudioUnitD2
bellsToAudio l time = go l Nil
  where
  go Nil acc = acc

  go (a : b) acc =
    go b
      ( case a.activated of
          Nothing -> acc
          Just x -> playBuf_ ("bell" <> show x <> show a.pitch) "bell" (midiToMult a.pitch) : acc
      )

backgroundEventsToAudio :: BackgroundVoice -> Number -> List BackgroundEventInfo -> List AudioUnitD2
backgroundEventsToAudio v time l = go l
  where
  go ({ onset, interruptedAt, note } : b)
    | onset + 2.0 * measure < time = Nil
    | otherwise =
      let
        gmult
          | time < singingStarts = 0.0
          | time < firstVerseStarts = calcSlope singingStarts 0.0 firstVerseStarts 1.0 time
          | otherwise = 1.0
      in
        ( gain_' (show onset <> show v <> show note <> "gain") (gmult * (maybe 1.0 (\x -> bindBetween 0.0 1.0 $ calcSlope x 1.0 (x + 0.4) 0.0 time) interruptedAt))
            ( playBufT_ (show onset <> show v <> show note <> "buf") (show v <> show note)
                defaultParam
                  { param = 1.0
                  , timeOffset = if time < onset then onset - time else 0.0
                  }
            )
        )
          : go b

  go Nil = mempty

fluteNotes = Fn0 : Fn1 : Fn2 : Fn3 : Fn4 : Fn5 : Fn6 : Fn7 : Fn8 : Fn9 : Fn10 : Fn11 : Fn12 : Fn13 : Fn14 : Fn15 : Fn16 : Fn17 : Fn18 : Fn19 : Nil :: List FluteNote

type FluteHistoryIntermediaryCalc
  = { fac :: Number, note :: FluteNote, noteAsNumber :: Number }

type FluteHistoryIntermediaryCalcHolder
  = { sum :: Number, res :: List FluteHistoryIntermediaryCalc, minTime :: Number }

fluteGain = 0.2 :: Number

fluteHistoryToAudioUnit :: List FluteAccumulatorInfo -> Number -> Maybe AudioUnitD2
fluteHistoryToAudioUnit Nil time = Nothing

fluteHistoryToAudioUnit l@(a : b) time =
  let
    { sum, res, minTime } = go l { sum: 0.0, res: Nil, minTime: a.onset }

    pitchAsFloat = pitchInfo sum res 0.0
  in
    Just
      $ pannerMono_ ("flutePannerMono") 0.0
          ( gain_' ("fluteGain") (bindBetween 0.0 fluteGain (fluteGain * (time - minTime) / 3.0))
              (sinOsc_ ("fluteGainPhlder") (calcSlope 0.0 200.0 1.0 1500.0 pitchAsFloat))
          )
  where
  pitchInfo sum Nil n = n

  pitchInfo sum (a' : b') n = pitchInfo sum b' ((a'.noteAsNumber * a'.fac / sum) + n)

  go Nil acc = acc

  go (x : y) { sum, res } =
    let
      fac = 1.0 / (1.0 + (a.onset - x.onset))
    in
      go y
        { sum: sum + fac
        , res:
            ( { fac
              , note: x.note
              , noteAsNumber: fluteNoteToPeriod x.note
              }
                : res
            )
        , minTime: x.onset
        }

unselectedFluteColor :: Number -> Rectangle -> FluteNote -> Gradient
unselectedFluteColor time rect v =
  let
    (RGB r g b) = fluteNoteToRGB v

    halfX = (rect.x + rect.width) / 2.0
  in
    LinearGradient { x0: halfX, y0: rect.y, x1: halfX, y1: rect.height }
      ( { color: rgb r g b
        , position: 0.5 + 0.5 * sin (0.3 * pi * (time + fluteNoteToPeriod v))
        }
          : { color: rgb r g (b - 15)
            , position: 0.5 + 0.5 * sin (0.3 * pi * (time + 0.66 + fluteNoteToPeriod v))
            }
          : { color: rgb (r + 15) g b
            , position: 0.5 + 0.5 * sin (0.3 * pi * (time + 1.33 + fluteNoteToPeriod v))
            }
          : Nil
      )

selectedFluteColor :: Number -> Rectangle -> FluteNote -> Gradient
selectedFluteColor time' rect v =
  let
    (RGB r g b) = fluteNoteToRGB v

    halfX = (rect.x + rect.width) / 2.0

    time = time' `pow` 2.0
  in
    LinearGradient { x0: halfX, y0: rect.y, x1: halfX, y1: rect.height }
      ( { color: rgb r g b
        , position: 0.5 + 0.5 * sin (2.3 * pi * (time + fluteNoteToPeriod v))
        }
          : { color: rgb 255 255 255
            , position: 0.5 + 0.5 * sin (2.3 * pi * (time + 0.4 + fluteNoteToPeriod v))
            }
          : { color: rgb r g (b - 15)
            , position: 0.5 + 0.5 * sin (2.3 * pi * (time + 0.8 + fluteNoteToPeriod v))
            }
          : { color: rgb 255 255 255
            , position: 0.5 + 0.5 * sin (2.3 * pi * (time + 1.2 + fluteNoteToPeriod v))
            }
          : { color: rgb (r + 15) g b
            , position: 0.5 + 0.5 * sin (2.3 * pi * (time + 1.6 + fluteNoteToPeriod v))
            }
          : Nil
      )

fluteHistoryToVideo :: (FluteNote -> Maybe Rectangle) -> List FluteAccumulatorInfo -> Number -> List Painting
fluteHistoryToVideo f l time =
  if time < thirdVerseStarts then
    Nil
  else
    go
      ( case l of
          Nil -> Nothing
          (a : b) -> Just a.note
      )
      (map (\x -> Tuple x (f x)) fluteNotes)
      Nil
  where
  go _ Nil acc = acc

  go sel (Tuple fn (Just a) : b) acc =
    go sel b
      ( filled
          ( fillGradient
              $ ( case sel of
                    Nothing -> unselectedFluteColor
                    Just x -> if x == fn then selectedFluteColor else unselectedFluteColor
                )
                  time
                  a
                  fn
          )
          (rectangle a.x a.y a.width a.height)
          : acc
      )

  go sel (_ : b) acc = go sel b acc

modFluteHistory :: (FluteNote -> Boolean) -> List FluteAccumulatorInfo -> Number -> List FluteNote -> List FluteAccumulatorInfo
modFluteHistory f acc' time notes = go notes acc'
  where
  go Nil acc = acc

  go (a : b) acc = go b (if f a then { onset: time, note: a } : acc else acc)

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
      if e.time >= bridgeStarts && e.time < thirdVerseStarts then
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
      else
        { bells, stream: e.accumulator.bellsLoop }

    bvCoords = backgroundVideoCoords e.canvas.w e.canvas.h e.time

    isBackgroundVideoTocuhed = if e.time < touchableStarts then const false else isRectangleTouched (map snd touches) <<< bvCoords

    backgroundEvents =
      memoize \v ->
        let
          prevEvents = functionize e.accumulator.backgroundEvents v

          isTouched = isBackgroundVideoTocuhed v

          maxT = maybe 0.0 _.onset (L.head prevEvents)

          rv
            | isTouched =
              { onset: e.time
              , interruptedAt: Nothing
              , note: backgroundNoteAt e.time
              }
                : case prevEvents of
                    Nil -> Nil
                    (a : b) -> case a.interruptedAt of
                      Just x -> (a : b)
                      Nothing -> (a { interruptedAt = Just e.time } : b)
            | maxT + 2.0 * measure < e.time - kr =
              { onset: maxT + 2.0 * measure
              , interruptedAt: Nothing
              , note: backgroundNoteAt e.time
              }
                : prevEvents
            | otherwise = prevEvents
        in
          rv

    backgroundRenderingInfo =
      map
        ( \v ->
            let
              evts = functionize backgroundEvents v
            in
              { a: backgroundEventsToAudio v e.time evts
              , v: backgroundEventsToVideo v bvCoords evts e.time
              }
        )
        backgroundVoices

    sCoords = synthCoords e.canvas.w e.canvas.h e.time

    -- for the synth, we also count ongoing touches
    isSynthTouched =
      if e.time < touchableStarts then
        const false
      else
        ( case _ of
            Nothing -> false
            Just x ->
              isRectangleTouched
                ( map snd
                    ( touches
                        <> L.filter
                            (\(Tuple a _) -> a `member` e.accumulator.prevClicks)
                            e.interactions
                    )
                )
                x
        )
          <<< sCoords

    activeSynthEvents =
      memoize \v ->
        let
          prevEvent = functionize e.accumulator.activeSynthEvents v

          isTouched = isSynthTouched v
        in
          case prevEvent of
            Nothing -> if not isTouched then Nothing else Just { energy: kr, note: synthNoteAt e.time }
            Just { energy, note } -> if not isTouched && energy - kr <= 0.0 then Nothing else Just { energy: energy - kr, note }

    synthRenderingInfo =
      map
        ( \(Tuple v evt) ->
            { a: synthEventToAudio v e.time <$> evt
            , v:
                synthEventToVideo v
                  ( case evt of
                      Nothing -> 0.0
                      Just x -> x.energy
                  )
                  <$> sCoords v
            }
        )
        (map (\v -> Tuple v (functionize activeSynthEvents v)) synthVoices)

    fCoords = fluteCoords e.canvas.w e.canvas.h e.time

    isFluteTouched =
      if e.time < touchableStarts then
        const false
      else
        ( case _ of
            Nothing -> false
            Just x -> isRectangleTouched (map snd touches) x
        )
          <<< fCoords

    fluteHistory = modFluteHistory isFluteTouched e.accumulator.fluteHistory e.time fluteNotes

    fadeIn
      | e.time < singingStarts = filled (fillColor (rgb 0 0 0)) (rectangle 0.0 0.0 e.canvas.w e.canvas.h)
      | e.time < firstVerseStarts = filled (fillColor (rgba 0 0 0 (calcSlope singingStarts 1.0 firstVerseStarts 0.0 e.time))) (rectangle 0.0 0.0 e.canvas.w e.canvas.h)
      | otherwise = mempty

    fadeOut
      | e.time < thirdVerseEnds = mempty
      | e.time < pieceEnds = filled (fillColor (rgba 0 0 0 (calcSlope thirdVerseEnds 0.0 pieceEnds 1.0 e.time))) (rectangle 0.0 0.0 e.canvas.w e.canvas.h)
      | otherwise = filled (fillColor (rgb 0 0 0)) (rectangle 0.0 0.0 e.canvas.w e.canvas.h)

    snowResizeInfo = resizeVideo snowWidth snowHeight e.canvas.w e.canvas.h

    snow = mempty
  -- snow = drawImageFull (FromImage { name: "snow1" }) snowResizeInfo.x snowResizeInfo.y snowResizeInfo.sWidth snowResizeInfo.sHeight 0.0 0.0 e.canvas.w e.canvas.h
  --snow = drawImageFull (FromVideo { name: "snow", currentTime: Just $ e.time % snowVideoLen }) snowResizeInfo.x snowResizeInfo.y snowResizeInfo.sWidth snowResizeInfo.sHeight 0.0 0.0 e.canvas.w e.canvas.h
  in
    { audio:
        (fold $ map _.a backgroundRenderingInfo)
          <> L.catMaybes (map _.a synthRenderingInfo)
          <> bellsToAudio bells e.time
          <> ( case fluteHistoryToAudioUnit fluteHistory e.time of
                Just x -> pure x
                Nothing -> Nil
            )
    , visual:
        fold
          ( fold (map _.v backgroundRenderingInfo)
              -- : fold (L.catMaybes (map _.v synthRenderingInfo))
              
              -- : bellsToVisual bells e.time
              
              -- : fold (fluteHistoryToVideo fCoords fluteHistory e.time)
              
              -- : fadeIn
              
              -- : snow
              
              -- : fadeOut
              
              : Nil
          )
    , accumulator:
        { backgroundEvents
        , activeSynthEvents
        , bells
        , bellsLoop
        , fluteHistory
        , prevClicks:
            e.accumulator.prevClicks
              `union`
                (S.fromFoldable $ map fst e.interactions)
        }
    }

scene :: Interactions -> WAccumulator -> CanvasInfo -> Number -> Behavior (AV D2 WAccumulator)
scene inter acc (CanvasInfo { w, h }) time = go <$> interactionLog inter
  where
  go { interactions } =
    AV
      { audio: Just (speaker (playBuf_ "backgroundWind" "backgroundWind" 1.0 :| audio))
      , visual:
          Just
            { painting: \_ -> visual
            , words: Nil
            }
      , accumulator
      }
    where
    { audio, visual, accumulator } =
      env
        { accumulator: acc
        , interactions: M.toUnfoldable interactions
        , time
        , canvas: { w, h }
        }

voiceToWidth :: BackgroundVoice -> Number
voiceToWidth bv = case bv of
  Bv0 -> 0.0
  Bv1 -> videoWidth
  Bv2 -> videoWidth * 2.0
  Bv3 -> 0.0
  Bv4 -> videoWidth * 2.0
  Bv5 -> 0.0
  Bv6 -> videoWidth
  Bv7 -> videoWidth * 2.0

voiceToHeight :: BackgroundVoice -> Number
voiceToHeight bv = case bv of
  Bv0 -> 0.0
  Bv1 -> 0.0
  Bv2 -> 0.0
  Bv3 -> videoHeight
  Bv4 -> videoHeight
  Bv5 -> videoHeight * 2.0
  Bv6 -> videoHeight * 2.0
  Bv7 -> videoHeight * 2.0

toTime :: BackgroundNote -> Int -> Number
toTime bn i = x + (toNumber i / 30.0)
  where
  x = case bn of
    Nt0 -> 0.0
    Nt1 -> 2.0
    Nt2 -> 4.0
    Nt3 -> 6.0
    Nt4 -> 8.0
    Nt5 -> 10.0
    Nt6 -> 12.0
    Nt7 -> 14.0
    Nt8 -> 16.0
    Nt9 -> 18.0
    Nt10 -> 20.0
    Nt11 -> 22.0
    Nt12 -> 24.0
    Nt13 -> 26.0

canvasesUsingVideo :: Array (Tuple String KU.CanvasRenderInfo)
canvasesUsingVideo =
  A.fromFoldable
    ( map
        ( \o ->
            let
              name = show o
            in
              Tuple name
                { painting:
                    \_ ->
                      drawImage
                        ( FromVideo
                            { name: "vid"
                            , currentTime: Just $ toNumber o * kr
                            }
                        )
                        0.0
                        0.0
                , words: Nil
                , width: floor fullVideoWidth
                , height: floor fullVideoHeight
                }
        )
        (L.range 0 nFrames)
    )

canvasesUsingImages :: Array (Tuple String KU.CanvasInfo)
canvasesUsingImages =
  (A.fromFoldable <<< join <<< join)
    ( map
        ( \v ->
            map
              ( \n ->
                  map
                    ( \o ->
                        let
                          subname = show v <> show n

                          name = subname <> "_" <> show o
                        in
                          Tuple name
                            { images: [ Tuple "img" ("https://klank-share.s3-eu-west-1.amazonaws.com/wwia/fake/" <> name <> "/" <> show (o + 1) <> ".jpg") ]
                            , videos: []
                            , painting: \_ -> drawImage (FromImage { name: "img" }) 0.0 0.0
                            , words: Nil
                            , width: 560
                            , height: 320
                            }
                    )
                    (L.range 0 nFrames)
              )
              backgroundNotes
        )
        backgroundVoices
    )

main :: Klank' WAccumulator
main =
  klank
    { run = runInBrowser_ (scene <$> getInteractivity)
    , engineInfo = \res rej -> res wereWalkingOnTheAirEngineInfo
    , accumulator =
      \res _ ->
        res
          { backgroundEvents:
              memoize
                $ const
                    ( pure
                        { onset: 0.0
                        , note: Nt0
                        , interruptedAt: Nothing
                        }
                    )
          , activeSynthEvents: memoize (const Nothing)
          , bells: Nil
          , fluteHistory: Nil
          , bellsLoop: bellsAsCycle
          , prevClicks: S.empty
          }
    , exporter = defaultExporter
    , webcamCache = \_ _ -> identity
    , buffers =
      makeBuffersKeepingCache 20
        ( [ Tuple "bell" "https://freesound.org/data/previews/439/439616_737466-hq.mp3", Tuple "backgroundWind" "https://freesound.org/data/previews/244/244942_263745-lq.mp3" ]
            <> (A.fromFoldable <<< join) (map (\v -> map (\n -> let name = show v <> show n in Tuple name ("https://klank-share.s3-eu-west-1.amazonaws.com/wwia/fake/" <> name <> ".ogg")) backgroundNotes) backgroundVoices)
        )
    -- courtesy of <a href="https://www.freestock.com/free-videos/loop-animation-falling-snowflakes-alpha-matte-3102526">Image used under license from Freestock.com</a>
    , canvases =
      makePooledCanvasesKeepingCache 20
        { videos: [ Tuple "vid" "https://klank-share.s3-eu-west-1.amazonaws.com/wwia/fake/brady.webm" ]
        , images: []
        }
        canvasesUsingVideo
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

newtype BackgroundVoice' a
  = BackgroundVoice'
  { bv0 :: a
  , bv1 :: a
  , bv2 :: a
  , bv3 :: a
  , bv4 :: a
  , bv5 :: a
  , bv6 :: a
  , bv7 :: a
  }

class Memoizable f g | f -> g, g -> f where
  memoize :: Function f ~> g
  functionize :: g ~> Function f

instance memoizableBackgroundVoice :: Memoizable BackgroundVoice BackgroundVoice' where
  memoize f =
    BackgroundVoice'
      { bv0: f Bv0
      , bv1: f Bv1
      , bv2: f Bv2
      , bv3: f Bv3
      , bv4: f Bv4
      , bv5: f Bv5
      , bv6: f Bv6
      , bv7: f Bv7
      }
  functionize (BackgroundVoice' { bv0 }) Bv0 = bv0
  functionize (BackgroundVoice' { bv1 }) Bv1 = bv1
  functionize (BackgroundVoice' { bv2 }) Bv2 = bv2
  functionize (BackgroundVoice' { bv3 }) Bv3 = bv3
  functionize (BackgroundVoice' { bv4 }) Bv4 = bv4
  functionize (BackgroundVoice' { bv5 }) Bv5 = bv5
  functionize (BackgroundVoice' { bv6 }) Bv6 = bv6
  functionize (BackgroundVoice' { bv7 }) Bv7 = bv7

derive instance backgroundVoiceGeneric :: Generic BackgroundVoice _

instance backgroundVoiceShow :: Show BackgroundVoice where
  show = genericShow

backgroundVoices :: List BackgroundVoice
backgroundVoices = Bv0 : Bv1 : Bv2 : Bv3 : Bv4 : Bv5 : Bv6 : Bv7 : Nil

backgroundNotes :: List BackgroundNote
backgroundNotes = Nt0 : Nt1 : Nt2 : Nt3 : Nt4 : Nt5 : Nt6 : Nt7 : Nt8 : Nt9 : Nt10 : Nt11 : Nt12 : Nt13 : Nil

synthVoices :: List SynthVoice
synthVoices = Sv0 : Sv1 : Sv2 : Sv3 : Sv4 : Sv5 : Sv6 : Sv7 : Sv8 : Sv9 : Sv10 : Sv11 : Nil

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

derive instance backgroundNoteGeneric :: Generic BackgroundNote _

instance backgroundNoteShow :: Show BackgroundNote where
  show = genericShow

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

derive instance synthVoiceGeneric :: Generic SynthVoice _

instance synthVoiceShow :: Show SynthVoice where
  show = genericShow

data RGB
  = RGB Int Int Int

synthVoiceToBaseRGB :: SynthVoice -> RGB
synthVoiceToBaseRGB Sv0 = RGB 85 239 196

synthVoiceToBaseRGB Sv1 = RGB 0 206 201

synthVoiceToBaseRGB Sv2 = RGB 108 92 231

synthVoiceToBaseRGB Sv3 = RGB 162 155 254

synthVoiceToBaseRGB Sv4 = RGB 255 234 167

synthVoiceToBaseRGB Sv5 = RGB 129 236 236

synthVoiceToBaseRGB Sv6 = RGB 250 177 160

synthVoiceToBaseRGB Sv7 = RGB 214 48 49

synthVoiceToBaseRGB Sv8 = RGB 0 184 148

synthVoiceToBaseRGB Sv9 = RGB 9 132 227

synthVoiceToBaseRGB Sv10 = RGB 0 206 201

synthVoiceToBaseRGB Sv11 = RGB 116 185 255

rectanglePeriod :: Number -> Number -> Number -> Number -> Number -> { x0 :: Number, y0 :: Number, x1 :: Number, y1 :: Number }
rectanglePeriod x0 y0 x1 y1 energy = go (energy % 1.0)
  where
  pt0 = (x1 - x0) / (2.0 * ((x1 - x0) + (y1 - y0)))

  go t
    | t < pt0 = let xp = (x1 - x0) * t / pt0 in { x0: x0 + xp, y0, x1: x1 - xp, y1 }
    | t < 0.5 = let yp = (y1 - y0) * (t - pt0) / (0.5 - pt0) in { x0: x1, y0: y0 + yp, x1: x0, y1: y1 - yp }
    | t < 0.5 + pt0 = let xp = (x1 - x0) * (t - 0.5) / pt0 in { x0: x1 - xp, y0: y1, x1: x0 + xp, y1: y0 }
    | otherwise = let yp = (y1 - y0) * (t - 0.5 - pt0) / (0.5 - pt0) in { x0, y0: y1 - yp, x1, y1: y0 + yp }

energyToGradient :: Rectangle -> RGB -> Number -> Gradient
energyToGradient { x, y, width, height } (RGB r g b) energy =
  let
    { x0, y0, x1, y1 } = rectanglePeriod x y (x + width) (y + height) energy
  in
    LinearGradient
      { x0, y0, x1, y1
      }
      ( map
          (\i' -> let i = toNumber i' in { color: rgb ((r + floor (i * energy)) `mod` 256) ((g - floor (i * energy)) `mod` 256) ((b + floor (i * energy)) `mod` 256), position: 0.5 + 0.5 * sin (pi * (energy + i / 5.0)) })
          (L.range 0 9)
      )

newtype SynthVoice' a
  = SynthVoice'
  { sv0 :: a
  , sv1 :: a
  , sv2 :: a
  , sv3 :: a
  , sv4 :: a
  , sv5 :: a
  , sv6 :: a
  , sv7 :: a
  , sv8 :: a
  , sv9 :: a
  , sv10 :: a
  , sv11 :: a
  }

instance memoizableSynthVoice :: Memoizable SynthVoice SynthVoice' where
  memoize f =
    SynthVoice'
      { sv0: f Sv0
      , sv1: f Sv1
      , sv2: f Sv2
      , sv3: f Sv3
      , sv4: f Sv4
      , sv5: f Sv5
      , sv6: f Sv6
      , sv7: f Sv7
      , sv8: f Sv8
      , sv9: f Sv9
      , sv10: f Sv10
      , sv11: f Sv11
      }
  functionize (SynthVoice' { sv0 }) Sv0 = sv0
  functionize (SynthVoice' { sv1 }) Sv1 = sv1
  functionize (SynthVoice' { sv2 }) Sv2 = sv2
  functionize (SynthVoice' { sv3 }) Sv3 = sv3
  functionize (SynthVoice' { sv4 }) Sv4 = sv4
  functionize (SynthVoice' { sv5 }) Sv5 = sv5
  functionize (SynthVoice' { sv6 }) Sv6 = sv6
  functionize (SynthVoice' { sv7 }) Sv7 = sv7
  functionize (SynthVoice' { sv8 }) Sv8 = sv8
  functionize (SynthVoice' { sv9 }) Sv9 = sv9
  functionize (SynthVoice' { sv10 }) Sv10 = sv10
  functionize (SynthVoice' { sv11 }) Sv11 = sv11

synthVoiceOtMult :: SynthVoice -> Number
synthVoiceOtMult v = case v of
  Sv0 -> 1.0
  Sv1 -> 2.0
  Sv2 -> 3.0
  Sv3 -> 4.0
  Sv4 -> 5.0
  Sv5 -> 6.0
  Sv6 -> 7.0
  Sv7 -> 8.0
  Sv8 -> 9.0
  Sv9 -> 10.0
  Sv10 -> 11.0
  Sv11 -> 12.0

synthVoiceOtMaxGain :: SynthVoice -> Number
synthVoiceOtMaxGain v = case v of
  Sv0 -> 0.3
  Sv1 -> 0.22
  Sv2 -> 0.2
  Sv3 -> 0.16
  Sv4 -> 0.13
  Sv5 -> 0.1
  Sv6 -> 0.08
  Sv7 -> 0.07
  Sv8 -> 0.05
  Sv9 -> 0.05
  Sv10 -> 0.04
  Sv11 -> 0.03

midi2cps :: Number -> Number
midi2cps n = 440.0 * (2.0 `pow` ((68.0 - n) / 12.0))

synthNoteBase :: SynthNote -> Number
synthNoteBase v = case v of
  Sn0 -> midi2cps 20.0
  Sn1 -> midi2cps 20.0
  Sn2 -> midi2cps 20.0
  Sn3 -> midi2cps 20.0
  Sn4 -> midi2cps 20.0
  Sn5 -> midi2cps 20.0
  Sn6 -> midi2cps 20.0
  Sn7 -> midi2cps 20.0
  Sn8 -> midi2cps 20.0
  Sn9 -> midi2cps 20.0
  Sn10 -> midi2cps 20.0
  Sn11 -> midi2cps 20.0
  Sn12 -> midi2cps 20.0
  Sn13 -> midi2cps 20.0
  Sn14 -> midi2cps 20.0

synthNoteOvertoneDistortion :: SynthNote -> Number
synthNoteOvertoneDistortion v = case v of
  Sn0 -> 1.0
  Sn1 -> 1.0
  Sn2 -> 1.0
  Sn3 -> 1.0
  Sn4 -> 1.0
  Sn5 -> 1.0
  Sn6 -> 1.0
  Sn7 -> 1.03
  Sn8 -> 0.98
  Sn9 -> 1.05
  Sn10 -> 0.96
  Sn11 -> 1.08
  Sn12 -> 1.1
  Sn13 -> 1.11
  Sn14 -> 1.12

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

synthNoteAt :: Number -> SynthNote
synthNoteAt t
  | t < secondVerseStarts + 2.0 * measure - halfBeat = Sn0
  | t < secondVerseStarts + 3.0 * measure - halfBeat = Sn1
  | t < secondVerseStarts + 5.0 * measure - halfBeat = Sn2
  | t < secondVerseStarts + 6.0 * measure - halfBeat = Sn3
  | t < secondVerseStarts + 7.0 * measure - halfBeat = Sn4
  | t < bridgeStarts - halfBeat = Sn5
  | t < bridgeStarts + 1.0 * measure - halfBeat = Sn6
  | t < bridgeStarts + 2.0 * measure - halfBeat = Sn7
  | t < bridgeStarts + 3.0 * measure - halfBeat = Sn8
  | t < bridgeStarts + 4.0 * measure - halfBeat = Sn9
  | t < bridgeStarts + 5.0 * measure - halfBeat = Sn10
  | t < bridgeStarts + 6.0 * measure - halfBeat = Sn11
  | t < bridgeStarts + 7.0 * measure - halfBeat = Sn12
  | t < bridgeStarts + 8.0 * measure - halfBeat = Sn13
  | otherwise = Sn14

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

fluteNote01 :: FluteNote -> Number
fluteNote01 = case _ of
  Fn0 -> 0.00
  Fn1 -> 0.05
  Fn2 -> 0.10
  Fn3 -> 0.15
  Fn4 -> 0.20
  Fn5 -> 0.25
  Fn6 -> 0.30
  Fn7 -> 0.35
  Fn8 -> 0.40
  Fn9 -> 0.45
  Fn10 -> 0.50
  Fn11 -> 0.55
  Fn12 -> 0.60
  Fn13 -> 0.65
  Fn14 -> 0.70
  Fn15 -> 0.75
  Fn16 -> 0.80
  Fn17 -> 0.85
  Fn18 -> 0.90
  Fn19 -> 0.95

derive instance fluteNoteEq :: Eq FluteNote

backgroundNoteAt :: Number -> BackgroundNote
backgroundNoteAt t
  | t < firstVerseStarts + 3.0 * measure - beat = Nt0
  | t < firstVerseStarts + 6.0 * measure - beat = Nt1
  | t < firstVerseStarts + 7.0 * measure - beat = Nt2
  | t < secondVerseStarts + 3.0 * measure - beat = Nt3
  | t < secondVerseStarts + 6.0 * measure - beat = Nt4
  | t < secondVerseStarts + 7.0 * measure - beat = Nt5
  | t < bridgeStarts + 2.0 * measure - beat = Nt6
  | t < bridgeStarts + 4.0 * measure - beat = Nt7
  | t < bridgeStarts + 6.0 * measure - beat = Nt8
  | t < bridgeStarts + 8.0 * measure - beat = Nt9
  | t < thirdVerseStarts + 3.0 * measure - beat = Nt10
  | t < thirdVerseStarts + 6.0 * measure - beat = Nt11
  | t < thirdVerseStarts + 7.0 * measure - beat = Nt12
  | otherwise = Nt13

type BellNoteInfo
  = { onset :: Number, note :: Number }

type SoloistNoteInfo
  = { soloistEffect :: Number
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

type FluteAccumulatorInfo
  = { onset :: Number
    , note :: FluteNote
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
