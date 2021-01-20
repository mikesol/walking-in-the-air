module Main where

import Prelude
import Data.Array as A
import Color (Color, rgb, toHexString)
import Data.Foldable (for_, foldl)
import Data.Int (floor, toNumber)
import Data.Lens (over)
import Data.Lens.Record (prop)
import Data.List as L
import Data.Maybe (Maybe(..))
import Data.String (toUpper)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..), fst)
import Effect (Effect)
import Effect.Aff (Aff, error, launchAff_, throwError)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Klank.Dev (BackgroundNote(..), BackgroundVoice(..), backgroundNotes, backgroundVoices)
import Math ((%))
import Node.ChildProcess (Exit(..))
import Node.ChildProcess as CP
import Node.FS.Sync as FS
import Sunde as S

dryRun = false :: Boolean

outdir = "z" :: String

tempo = 60.0 :: Number

frameRate = 10.0 :: Number

framesInSection = floor $ frameRate * tempo * 8.0 / 60.0 :: Int

videoHeight = 1920 :: Int

videoWidth = 1080 :: Int

type VideoMarker
  = { start :: Number, v :: Int }

type Voice
  = { yOffset :: Int
    , file :: String
    , markers :: BackgroundNote -> VideoMarker
    }

type Voices
  = BackgroundVoice -> Voice

defaultMarkers :: BackgroundNote -> VideoMarker
defaultMarkers m = let v = markerToIdx m in { v, start: (toNumber v * 12.0 * tempo / 60.0) }

offsetDefaultMarkersFPS :: Int -> Int -> BackgroundNote -> VideoMarker
offsetDefaultMarkersFPS i f = offsetDefaultMarkers (toNumber i + toNumber f / 30.0)

offsetDefaultMarkers :: Number -> BackgroundNote -> VideoMarker
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

  blue = rgb 64 163 255

  stops = (foldl (\{ acc, n } (Tuple a b) -> { acc: acc <> [ Tuple (a - n) b ], n: a }) { acc: [], n: 0 } [ Tuple stop0 (Tuple white blue), Tuple (stop0 + ((stop1 - stop0) / 2)) (Tuple blue white), Tuple stop1 (Tuple white blue), Tuple (stop1 + ((stop2 - stop1) / 2)) (Tuple blue white), Tuple stop2 (Tuple white blue), Tuple end (Tuple blue white) ]).acc

makeGradient :: Int -> Array (Tuple Int (Tuple Color Color)) -> Int -> Int -> String -> Array String
makeGradient w colors rot realDim outf =
  [ "convert", "-colorspace", "rgb"
  ]
    <> join
        ( map
            ( \(Tuple l (Tuple c0 c1)) ->
                [ "("
                , "-size"
                , "1x" <> show l
                , "gradient:" <> (toUpper <<< toHexString) c0 <> "-" <> (toUpper <<< toHexString) c1 <> ""
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
    { exit, stderr, stdout } <-
      S.spawn
        { cmd: "ffmpeg"
        , args
        , stdin: Nothing
        }
        CP.defaultSpawnOptions
    case exit of
      Normally 0 -> pure unit
      Normally n -> throwError (error $ "Non-zero exit of ffmpeg " <> stderr <> " " <> stdout)
      _ -> throwError (error $ "Non-zero exit of ffmpeg " <> stderr <> " " <> stdout)

imageMagickNoop :: String -> String -> Int -> Int -> Aff Unit
imageMagickNoop odr file v i = do
  let
    args =
      [ outJpg odr file v i
      , finalJpg odr file v i
      ]
  (log <<< show) args
  when (not dryRun) do
    { exit, stdout, stderr } <-
      S.spawn
        { cmd: "magick"
        , args
        , stdin: Nothing
        }
        CP.defaultSpawnOptions
    case exit of
      Normally 0 -> pure unit
      Normally n -> do
        log (stdout <> " " <> stderr)
        throwError (error $ "Non-zero exit of magick: " <> show n)
      _ -> throwError (error "Non-zero exit of magick")

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
    { exit, stderr, stdout } <-
      S.spawn
        { cmd: "magick"
        -- offset the time by 2 seconds in the position function
        , args
        , stdin: Nothing
        }
        CP.defaultSpawnOptions
    case exit of
      Normally 0 -> pure unit
      Normally n -> do
        log (stdout <> " " <> stderr)
        throwError (error $ "Non-zero exit of magick: " <> show n)
      _ -> throwError (error "Non-zero exit of magick")

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
    { exit, stderr, stdout } <-
      S.spawn
        { cmd: "magick"
        -- offset the time by 2 seconds in the position function
        , args
        , stdin: Nothing
        }
        CP.defaultSpawnOptions
    case exit of
      Normally 0 -> pure unit
      Normally n -> do
        log (stdout <> " " <> stderr)
        throwError (error $ "Non-zero exit of magick: " <> show n)
      _ -> throwError (error "Non-zero exit of magick")

markerToIdx :: BackgroundNote -> Int
markerToIdx n = case n of
  Nt0 -> 0
  Nt1 -> 0
  Nt2 -> 0
  Nt3 -> 0
  Nt4 -> 0
  Nt5 -> 0
  Nt6 -> 0
  Nt7 -> 0
  Nt8 -> 0
  Nt9 -> 0
  Nt10 -> 0
  Nt11 -> 0
  Nt12 -> 0
  Nt13 -> 0

voices :: Voices
voices v = case v of
  Bv0 ->
    { yOffset: 280
    , file: "media/0.mp4"
    , markers: offsetDefaultMarkersFPS 10 25
    }
  Bv1 ->
    { yOffset: 120
    , file: "media/1.mp4"
    , markers: offsetDefaultMarkersFPS 6 15
    }
  Bv2 ->
    { yOffset: 320
    , file: "media/2.mp4"
    , markers: offsetDefaultMarkersFPS 7 20
    }
  Bv3 ->
    { yOffset: 720
    , file: "media/3.mp4"
    , markers: offsetDefaultMarkersFPS 13 5
    }
  Bv4 ->
    { yOffset: 192
    , file: "media/4.mp4"
    , markers: offsetDefaultMarkersFPS 11 9
    }
  Bv5 ->
    { yOffset: 180
    , file: "media/5.mp4"
    , markers: offsetDefaultMarkersFPS 12 21
    }
  Bv6 ->
    { yOffset: 70
    , file: "media/6.mp4"
    , markers: offsetDefaultMarkersFPS 134 29
    }
  Bv7 ->
    { yOffset: 736
    , file: "media/7.mp4"
    , markers: offsetDefaultMarkersFPS 6 14
    }

outJpg :: forall v i. Show v => Show i => String -> String -> v -> i -> String
outJpg odr file v i = odr <> "/" <> file <> "." <> show v <> "." <> show i <> ".jpg"

finalJpg :: forall v i. Show v => Show i => String -> String -> v -> i -> String
finalJpg odr file v i = odr <> "/" <> file <> "." <> show v <> "." <> show i <> ".final.jpg"

outGrad :: forall v i. Show v => Show i => String -> String -> v -> i -> String
outGrad odr file v i = odr <> "/" <> file <> "." <> show v <> "." <> show i <> ".gradient.jpg"

endpt :: Int
endpt = framesInSection - 1

mergeVoicesAndCleanSingleFrame :: BackgroundNote -> Int -> Aff Unit
mergeVoicesAndCleanSingleFrame b i =
  when (not dryRun) do
    { exit, stderr, stdout } <-
      S.spawn
        { cmd: "ffmpeg"
        , args
        , stdin: Nothing
        }
        CP.defaultSpawnOptions
    case exit of
      Normally 0 -> do
        for_ finalJpgs \f -> liftEffect $ FS.unlink f
        pure unit
      Normally n -> throwError (error $ "Non-zero exit of ffmpeg " <> stderr <> " " <> stdout)
      _ -> throwError (error $ "Non-zero exit of ffmpeg " <> stderr <> " " <> stdout)
  where
  finalJpgs = A.fromFoldable $ map (\a -> let { file } = voices a in finalJpg outdir file (markerToIdx b) i) backgroundVoices

  args =
    (join (map (\a -> [ "-i", a ]) finalJpgs))
      <> [ "-i"
        , "media/hole.png"
        , "-filter_complex"
        , "[0:v][1:v][2:v]hstack=inputs=3[top];[3:v][8:v][4:v]hstack=inputs=3[middle];[5:v][6:v][7:v]hstack=inputs=3[bottom];[top][middle][bottom]vstack=inputs=3[bigimg];[bigimg]scale=600:600[v]"
        , "-map"
        , "[v]"
        , "-q:v"
        , "23"
        , (show $ markerToIdx b) <> "." <> (show i) <> ".mosaic.jpg"
        ]

makeSingleFrame :: BackgroundVoice -> BackgroundNote -> Int -> Aff Unit
makeSingleFrame a b i =
  let
    { yOffset, file, markers } = voices a

    { start, v } = markers b
  in
    do
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
                  pure unit
          | otherwise = do
            imageMagickNoop outdir file v i
            when (not dryRun) $ liftEffect (FS.unlink (outJpg outdir file v i))
      next

main :: Effect Unit
main =
  launchAff_ do
    when (not dryRun)
      ( liftEffect
          $ do
              FS.mkdir outdir
              FS.mkdir $ outdir <> "/media"
      )
    for_ (L.range 0 endpt) \i ->
      for_ backgroundNotes \marker -> do
        for_ backgroundVoices \voice ->
          makeSingleFrame voice marker i
        mergeVoicesAndCleanSingleFrame marker i
