module Main where

import Prelude
import Color (Color, rgb, toHexString)
import Data.Foldable (for_, foldl)
import Data.Int (floor, toNumber)
import Data.Lens (over)
import Data.Lens.Record (prop)
import Data.List as L
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..), fst)
import Data.Typelevel.Num (D8, D14)
import Data.Vec ((+>))
import Data.Vec as V
import Effect (Effect)
import Effect.Aff (Aff, error, launchAff_, throwError)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Math ((%))
import Node.ChildProcess (Exit(..))
import Node.ChildProcess as CP
import Node.FS.Sync as FS
import Sunde as S

dryRun = true :: Boolean

outdir = "z" :: String

tempo = 60.0 :: Number

frameRate = 10.0 :: Number

framesInSection = floor $ frameRate * tempo * 8.0 / 60.0 :: Int

videoHeight = 1920 :: Int

videoWidth = 1080 :: Int

type VideoMarker
  = { start :: Number, v :: Int }

type VideoMarkers
  = V.Vec D14 VideoMarker

type Voice
  = { yOffset :: Int
    , file :: String
    , markers :: VideoMarkers
    }

type Voices
  = V.Vec D8 Voice

defaultMarkers :: VideoMarkers
defaultMarkers = V.fill (\v -> { v, start: (toNumber v * 12.0 * tempo / 60.0) })

offsetDefaultMarkersFPS :: Int -> Int -> VideoMarkers
offsetDefaultMarkersFPS i f = offsetDefaultMarkers (toNumber i + toNumber f / 30.0)

offsetDefaultMarkers :: Number -> VideoMarkers
offsetDefaultMarkers =
  flip map defaultMarkers
    <<< over (prop (SProxy :: SProxy "start"))
    <<< (+)

outputFrame :: Int -> Number -> String -> String -> Array String
outputFrame yOffset seconds inputFile outputFile =
  [ "-ss"
  , show seconds
  , "-i"
  , inputFile
  , "-vframes"
  , "1"
  , "-q:v"
  , "0"
  , "-filter:v"
  , "crop=" <> show videoWidth <> ":" <> show videoWidth <> ":0:" <> show yOffset
  , outputFile
  ]

snowGradientAt :: (Number -> Number) -> (Number -> Number) -> Number -> String -> Array String
snowGradientAt posF rotF time outf = makeGradient end stops (floor rotC) videoWidth outf
  where
  w = toNumber videoWidth

  posC = posF time

  rotC = (rotF time) % 360.0

  posCMod = posC % w

  stop0 = floor $ posCMod + w

  stop1 = floor $ posCMod + (2.0 * w)

  stop2 = floor $ posCMod + (3.0 * w)

  end = floor $ 5.0 * w

  white = rgb 255 255 255

  blue = rgb 255 255 0

  stops = (foldl (\{ acc, n } (Tuple a b) -> { acc: acc <> [ Tuple (a - n) b ], n: a }) { acc: [], n: 0 } [ Tuple stop0 (Tuple white blue), Tuple (stop0 + ((stop1 - stop0) / 2)) (Tuple blue white), Tuple stop1 (Tuple white blue), Tuple (stop1 + ((stop2 - stop1) / 2)) (Tuple blue white), Tuple stop2 (Tuple white blue), Tuple end (Tuple blue white) ]).acc

makeGradient :: Int -> Array (Tuple Int (Tuple Color Color)) -> Int -> Int -> String -> Array String
makeGradient w colors rot realDim outf =
  [ "magick", "convert"
  ]
    <> join
        ( map
            ( \(Tuple l (Tuple c0 c1)) ->
                [ "("
                , "-size"
                , "1x" <> show l
                , "gradient:\"" <> toHexString c0 <> "-" <> toHexString c1 <> "\""
                , ")"
                ]
            )
            colors
        )
    <> [ "-append"
      , "-scale"
      , show w <> "x" <> show (foldl (+) 0 (map fst colors)) <> "!"
      , "-rotate"
      , show rot
      , "-gravity"
      , "Center"
      , "-extent"
      , show realDim <> "x" <> show realDim
      , outf
      ]

ffmpegSingleFrame :: String -> String -> Int -> Int -> Number -> Int -> Aff Unit
ffmpegSingleFrame odr file v i start yOffset = do
  let
    args =
      outputFrame
        yOffset
        (start + (toNumber i / frameRate))
        file
        (outJpg odr file v i)
  (log <<< show) args
  when (not dryRun) do
    { exit } <-
      S.spawn
        { cmd: "ffmpeg"
        , args
        , stdin: Nothing
        }
        CP.defaultSpawnOptions
    case exit of
      Normally _ -> pure unit
      _ -> throwError (error "Non-zero exit of ffmpeg")

imageMagickNoop :: String -> String -> Int -> Int -> Aff Unit
imageMagickNoop odr file v i = do
  let
    args =
      [ outJpg odr file v i
      , finalJpg odr file v i
      ]
  (log <<< show) args
  when (not dryRun) do
    { exit } <-
      S.spawn
        { cmd: "magick"
        -- offset the time by 2 seconds in the position function
        , args
        , stdin: Nothing
        }
        CP.defaultSpawnOptions
    case exit of
      Normally _ -> pure unit
      _ -> throwError (error "Non-zero exit of ffmpeg")

imageMagickDissolve :: String -> String -> Int -> Int -> Int -> Aff Unit
imageMagickDissolve odr file v i pct = do
  let
    args =
      [ "composite"
      , "-dissolve"
      , show pct
      , (outGrad odr file v i)
      , (outJpg odr file v i)
      , (finalJpg odr file v i)
      ]
  (log <<< show) args
  when (not dryRun) do
    { exit } <-
      S.spawn
        { cmd: "magick"
        -- offset the time by 2 seconds in the position function
        , args
        , stdin: Nothing
        }
        CP.defaultSpawnOptions
    case exit of
      Normally _ -> pure unit
      _ -> throwError (error "Non-zero exit of ffmpeg")

imageMagickGradient :: String -> String -> Int -> Int -> Aff Unit
imageMagickGradient odr file v i = do
  let
    args =
      snowGradientAt
        (toNumber videoWidth * _)
        (360.0 * _)
        ((toNumber i / 10.0 + 2.0) % (toNumber framesInSection))
        (outGrad odr file v i)
  (log <<< show) args
  when (not dryRun) do
    { exit } <-
      S.spawn
        { cmd: "magick"
        -- offset the time by 2 seconds in the position function
        , args
        , stdin: Nothing
        }
        CP.defaultSpawnOptions
    case exit of
      Normally _ -> pure unit
      _ -> throwError (error "Non-zero exit of ffmpeg")

voices :: Voices
voices =
  { yOffset: 226
  , file: "media/0.mp4"
  , markers: offsetDefaultMarkersFPS 10 25
  }
    +> { yOffset: 120
      , file: "media/1.mp4"
      , markers: offsetDefaultMarkersFPS 6 15
      }
    +> { yOffset: 320
      , file: "media/2.mp4"
      , markers: offsetDefaultMarkersFPS 7 20
      }
    +> { yOffset: 720
      , file: "media/3.mp4"
      , markers: offsetDefaultMarkersFPS 13 5
      }
    +> { yOffset: 192
      , file: "media/4.mp4"
      , markers: offsetDefaultMarkersFPS 11 9
      }
    +> { yOffset: 180
      , file: "media/5.mp4"
      , markers: offsetDefaultMarkersFPS 132 22
      }
    +> { yOffset: 70
      , file: "media/6.mp4"
      , markers: offsetDefaultMarkersFPS 134 29
      }
    +> { yOffset: 736
      , file: "media/7.mp4"
      , markers: offsetDefaultMarkersFPS 6 14
      }
    +> V.empty

outJpg :: forall v i. Show v => Show i => String -> String -> v -> i -> String
outJpg odr file v i = odr <> "/" <> file <> "." <> show v <> "." <> show i <> ".jpg"

finalJpg :: forall v i. Show v => Show i => String -> String -> v -> i -> String
finalJpg odr file v i = odr <> "/" <> file <> "." <> show v <> "." <> show i <> ".final.jpg"

outGrad :: forall v i. Show v => Show i => String -> String -> v -> i -> String
outGrad odr file v i = odr <> "/" <> file <> "." <> show v <> "." <> show i <> ".gradient.jpg"

main :: Effect Unit
main =
  launchAff_ do
    (log <<< show) voices
    when (not dryRun)
      ( liftEffect
          $ do
              FS.mkdir outdir
              FS.mkdir $ outdir <> "/media"
      )
    for_ voices \{ yOffset, file, markers } ->
      for_ markers \{ start, v } ->
        for_ (L.range 0 (framesInSection - 1)) \i -> do
          -- create the frame
          ffmpegSingleFrame outdir file v i start yOffset
          let
            next
              | i < 20 || i > framesInSection - 20 = do
                let
                  pct
                    | i < 20 = max 0 (min 100 (floor $ 100.0 * (toNumber (20 - i)) / 20.0))
                    | otherwise = max 0 (min 100 (floor $ 100.0 * (toNumber ((framesInSection - 1) - i + 20)) / 20.0))
                imageMagickGradient outdir file v i
                imageMagickDissolve outdir file v i pct
                when (not dryRun)
                  $ liftEffect do
                      FS.unlink (outJpg outdir file v i)
                      FS.unlink (outGrad outdir file v i)
              | otherwise = do
                imageMagickNoop outdir file v i
                when (not dryRun) $ liftEffect (FS.unlink (outJpg outdir file v i))
          next

-- overlay the gradient on the frame
-- delete the gradient
-- delete the original
