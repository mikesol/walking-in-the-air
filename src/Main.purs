module Klank.Dev where

import Prelude
import Color (rgba)
import Control.Comonad.Cofree (Cofree, deferCofree)
import Control.Comonad.Cofree as Cf
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

instrumentsFullyFadedOut = thirdVerseStarts + (2.0 * measure) :: Number

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

type WAccumulator
  = { backgroundBeat :: BackgroundVoice -> Number
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

backgroundVideoCoords :: Number -> Number -> BackgroundVoice -> Number -> Rectangle
backgroundVideoCoords w h v n
  | n < secondVerseStarts = normalPositions w h v
  | n < instrumentsFullyFadedIn = normalPositions w h v
  | n < thirdVerseStarts = normalPositions w h v
  | n < instrumentsFullyFadedOut = normalPositions w h v
  | otherwise = normalPositions w h v

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
        { backgroundBeat: \_ -> 0.0
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
          { backgroundBeat: \_ -> 0.0
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
  | Fn20
  | Fn21
  | Fn22
  | Fn23

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
