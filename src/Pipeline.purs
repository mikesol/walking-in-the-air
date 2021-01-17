module Pipeline where

import Prelude
import Data.Foldable (for_)
import Data.Int (floor, toNumber)
import Data.Lens (over)
import Data.Lens.Record (prop)
import Data.List as L
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Typelevel.Num (D8, D14)
import Data.Vec ((+>))
import Data.Vec as V
import Effect (Effect)
import Effect.Aff (error, launchAff_, throwError)
import Effect.Class (liftEffect)
import Node.ChildProcess (Exit(..))
import Node.ChildProcess as CP
import Node.FS.Sync as FS
import Sunde as S

outdir = "z" :: String

tempo = 60.0 :: Number

frameRate = 10.0 :: Number

framesInSection = floor $ frameRate * tempo * 8.0 / 60.0 :: Int

videoHeight = 1920.0 :: Number

videoWidth = 1080.0 :: Number

type VideoMarker
  = { start :: Number }

type VideoMarkers
  = V.Vec D14 VideoMarker

type Voice
  = { yOffset :: Number
    , file :: String
    , markers :: VideoMarkers
    }

type Voices
  = V.Vec D8 Voice

defaultMarkers :: VideoMarkers
defaultMarkers = V.fill ({ start: _ } <<< (_ * 12.0 * tempo) <<< toNumber)

offsetDefaultMarkers :: Number -> VideoMarkers
offsetDefaultMarkers =
  flip map defaultMarkers
    <<< over (prop (SProxy :: SProxy "start"))
    <<< (+)

outputFrame :: Number -> String -> String -> Array String
outputFrame seconds inputFile outputFile = [ "-ss", show seconds, "-i", inputFile, "-vframes", "1", "-q:v", "0", outputFile ]

voices :: Voices
voices =
  { yOffset: 0.0
  , file: "voice0.mp4"
  , markers: defaultMarkers
  }
    +> { yOffset: 0.0
      , file: "voice1.mp4"
      , markers: defaultMarkers
      }
    +> { yOffset: 0.0
      , file: "voice2.mp4"
      , markers: defaultMarkers
      }
    +> { yOffset: 0.0
      , file: "voice3.mp4"
      , markers: defaultMarkers
      }
    +> { yOffset: 0.0
      , file: "voice4.mp4"
      , markers: defaultMarkers
      }
    +> { yOffset: 0.0
      , file: "voice5.mp4"
      , markers: defaultMarkers
      }
    +> { yOffset: 0.0
      , file: "voice6.mp4"
      , markers: defaultMarkers
      }
    +> { yOffset: 0.0
      , file: "voice7.mp4"
      , markers: defaultMarkers
      }
    +> V.empty

outJpg :: forall a. Show a => String -> String -> a -> String
outJpg odr file i = odr <> "/" <> file <> "." <> show i <> ".jpg"

main :: Effect Unit
main =
  launchAff_ do
    liftEffect $ FS.mkdir outdir
    for_ voices \{ yOffset, file, markers } ->
      for_ markers \{ start } ->
        for_ (L.range 0 (framesInSection - 1)) \i -> do
          { exit } <-
            S.spawn
              { cmd: "ffmpeg"
              , args:
                  outputFrame
                    (start + (toNumber i / frameRate))
                    file
                    (outJpg outdir file i)
              , stdin: Nothing
              }
              CP.defaultSpawnOptions
          case exit of
            Normally _ -> pure unit
            _ -> throwError (error "Non-zero exit of ffmpeg")
