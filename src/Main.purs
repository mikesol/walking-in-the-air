module Klank.Dev where

import Prelude
import Color (rgba)
import Control.Monad.Reader (Reader, runReader)
import Data.DateTime.Instant (unInstant)
import Data.Foldable (traverse_)
import Data.Int (toNumber)
import Data.List (List(..))
import Data.Map (Map, insertWith)
import Data.Map as M
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Newtype (unwrap, wrap)
import Data.String (indexOf, Pattern(..))
import Data.Typelevel.Num (D2)
import Effect (Effect)
import Effect.Now (now)
import Effect.Ref as Ref
import FRP.Behavior (Behavior, behavior)
import FRP.Behavior.Audio (AV(..), AudioParameter, AudioUnit, CanvasInfo, defaultExporter, runInBrowser_)
import FRP.Event (Event, makeEvent, subscribe)
import Graphics.Drawing (Color, Point)
import Graphics.Painting (Painting)
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

type WAccumulator
  = {}

type RenderInfo
  = { audioEnv :: AudioEnv
    , visualEnv :: VideoEnv
    , accumulator :: WAccumulator
    }

type Env
  = { accumulator :: WAccumulator
    , time :: Number
    , interactions :: InteractionMap
    }

env :: Reader Env RenderInfo
env =
  pure
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
    , accumulator: {}
    }

audioScene :: AudioEnv -> AudioUnit D2
audioScene _ = zero

visualScene :: VideoEnv -> Painting
visualScene _ = mempty

scene :: Interactions -> WAccumulator -> CanvasInfo -> Number -> Behavior (AV D2 WAccumulator)
scene inter acc ci time = go <$> interactionLog inter
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
      runReader env
        { accumulator: acc
        , interactions
        , time
        }

main :: Klank' WAccumulator
main =
  klank
    { run = runInBrowser_ (scene <$> getInteractivity)
    , accumulator = \res _ -> res {}
    , exporter = defaultExporter
    , webcamCache = \_ _ -> identity
    }

data BackgroundVoice
  = V0
  | V1
  | V2
  | V3
  | V4
  | V5
  | V6
  | V7

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

data BellNote
  = Ln0
  | Ln1
  | Ln2
  | Ln3
  | Ln4
  | Ln5
  | Ln6
  | Ln7
  | Ln8
  | Ln9

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
  = { onset :: Number, note :: BellNote }

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
  = { pt :: Maybe Point
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

  go tn n tl m = go tn (n - 1) tl (maybe m (\t -> insertWith (\e nw -> e { pt = nw.pt }) (identifier t) { onset: tn, pt: if isEnd then Nothing else Just { x: toNumber $ T.clientX t, y: toNumber $ T.clientY t } } m) (TL.item (l - n) tl))

  ts = changedTouches te

  l = TL.length ts

handleME :: Int -> Boolean -> Ref.Ref InteractionMap -> MouseEvent -> Effect Unit
handleME i isEnd ref me = do
  tn <- map (unwrap <<< unInstant) now
  void $ Ref.modify (if isEnd then purge tn else identity <<< insertWith (\e n -> e { pt = n.pt }) i { onset: tn, pt: if isEnd then Nothing else Just { x: toNumber $ ME.clientX me, y: toNumber $ ME.clientY me } }) ref

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
