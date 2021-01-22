module Main where

import Prelude
import Color (Color, rgb, toHexString)
import Data.Array as A
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
import Klank.Dev (BackgroundNote(..), BackgroundVoice(..), backgroundNotes, backgroundVoices, framesInSection, markerToIdx)
import Math ((%))
import Node.ChildProcess (Exit(..))
import Node.ChildProcess as CP
import Node.FS.Sync as FS
import Sunde as S

dryRun = false :: Boolean

outdir = "z" :: String

tempo = 60.0 :: Number

frameRate = 10.0 :: Number

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

outputFrame :: Int -> Number -> String -> String -> Boolean -> Array String
outputFrame yOffset seconds inputFile outputFile scale =
  [ "-ss"
  , show seconds
  , "-i"
  , inputFile
  , "-vframes"
  , "1"
  , "-filter:v"
  , "crop=" <> show videoWidth <> ":" <> show videoWidth <> ":0:" <> show yOffset <> (if scale then ",scale=" <> show vScale <> ":" <> show vScale else mempty)
  , outputFile
  ]

vScale = 200 :: Int

-- on4false
backgroundNoteToStartColor :: BackgroundNote -> Color
backgroundNoteToStartColor Nt0 = rgb 194 233 251

backgroundNoteToStartColor Nt1 = rgb 250 208 196

backgroundNoteToStartColor Nt2 = rgb 255 236 210

backgroundNoteToStartColor Nt3 = rgb 254 207 239

backgroundNoteToStartColor Nt4 = rgb 226 235 240

backgroundNoteToStartColor Nt5 = rgb 102 126 234

backgroundNoteToStartColor Nt6 = rgb 253 252 251

backgroundNoteToStartColor Nt7 = rgb 137 247 254

backgroundNoteToStartColor Nt8 = rgb 245 247 250

backgroundNoteToStartColor Nt9 = rgb 245 239 239

backgroundNoteToStartColor Nt10 = rgb 163 189 237

backgroundNoteToStartColor Nt11 = rgb 67 67 67

backgroundNoteToStartColor Nt12 = rgb 150 222 218

backgroundNoteToStartColor Nt13 = rgb 255 195 160

backgroundNoteToEndColor :: BackgroundNote -> Color
backgroundNoteToEndColor Nt0 = rgb 161 196 253

backgroundNoteToEndColor Nt1 = rgb 255 154 158

backgroundNoteToEndColor Nt2 = rgb 252 182 159

backgroundNoteToEndColor Nt3 = rgb 255 154 158

backgroundNoteToEndColor Nt4 = rgb 207 217 223

backgroundNoteToEndColor Nt5 = rgb 118 75 162

backgroundNoteToEndColor Nt6 = rgb 226 209 195

backgroundNoteToEndColor Nt7 = rgb 102 166 255

backgroundNoteToEndColor Nt8 = rgb 195 207 226

backgroundNoteToEndColor Nt9 = rgb 254 173 166

backgroundNoteToEndColor Nt10 = rgb 105 145 199

backgroundNoteToEndColor Nt11 = rgb 0 0 0

backgroundNoteToEndColor Nt12 = rgb 80 201 195

backgroundNoteToEndColor Nt13 = rgb 255 175 189

snowGradientAt :: Color -> Color -> (Number -> Number) -> (Number -> Number) -> Number -> String -> Array String
snowGradientAt startColor endColor posF rotF time outf = makeGradient end stops (floor rotC) videoWidth outf
  where
  w = toNumber videoWidth

  posC = posF time

  rotC = (rotF time) % 360.0

  posCMod = posC % w

  stop0 = floor $ posCMod + w

  stop1 = floor $ posCMod + (2.0 * w)

  stop2 = floor $ posCMod + (3.0 * w)

  end = floor $ 5.0 * w

  stops = (foldl (\{ acc, n } (Tuple a b) -> { acc: acc <> [ Tuple (a - n) b ], n: a }) { acc: [], n: 0 } [ Tuple stop0 (Tuple startColor endColor), Tuple (stop0 + ((stop1 - stop0) / 2)) (Tuple endColor startColor), Tuple stop1 (Tuple startColor endColor), Tuple (stop1 + ((stop2 - stop1) / 2)) (Tuple endColor startColor), Tuple stop2 (Tuple startColor endColor), Tuple end (Tuple endColor startColor) ]).acc

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

runFFMpeg :: Array String -> Aff Unit
runFFMpeg args = do
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

ffmpegSingleFrame :: String -> String -> Int -> Int -> Number -> Int -> Aff Unit
ffmpegSingleFrame odr file v i start yOffset = do
  let
    args =
      outputFrame
        yOffset
        (start + (toNumber i / frameRate))
        file
        (outJpg odr file v i)
        false
  runFFMpeg args

ffmpegSoloFrame :: Int -> Aff Unit
ffmpegSoloFrame i = do
  let
    args =
      outputFrame
        390
        (toNumber i / frameRate + 3.8)
        "media/wwia.mp4"
        ("z/media/wwia." <> show i <> ".png")
        true
  runFFMpeg args

imageMagickScale :: Boolean -> String -> String -> Int -> Int -> Aff Unit
imageMagickScale fromFinal odr file v i = do
  let
    args =
      [ (if fromFinal then finalJpg else outJpg) odr file v i
      , "-scale"
      , show vScale <> "x" <> show vScale
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

imageMagickGradient :: Color -> Color -> String -> String -> Int -> Int -> Aff Unit
imageMagickGradient startColor endColor odr file v i = do
  let
    args =
      snowGradientAt
        startColor
        endColor
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
outJpg odr file v i = odr <> "/" <> file <> "." <> show v <> "." <> show i <> ".png"

finalJpg :: forall v i. Show v => Show i => String -> String -> v -> i -> String
finalJpg odr file v i = odr <> "/" <> file <> "." <> show v <> "." <> show i <> ".final.png"

outGrad :: forall v i. Show v => Show i => String -> String -> v -> i -> String
outGrad odr file v i = odr <> "/" <> file <> "." <> show v <> "." <> show i <> ".gradient.png"

endpt :: Int
endpt = framesInSection - 1

mergeVoicesAndCleanSingleFrame :: BackgroundNote -> Int -> Aff Unit
mergeVoicesAndCleanSingleFrame b i = do
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
      Normally 0 -> do
        for_ finalJpgs \f -> liftEffect $ FS.unlink f
        liftEffect $ FS.unlink wwiaFile
        pure unit
      Normally n -> throwError (error $ "Non-zero exit of ffmpeg " <> stderr <> " " <> stdout)
      _ -> throwError (error $ "Non-zero exit of ffmpeg " <> stderr <> " " <> stdout)
  where
  finalJpgs = A.fromFoldable $ map (\a -> let { file } = voices a in finalJpg outdir file (markerToIdx b) i) backgroundVoices

  wwiaFile = "z/media/wwia." <> show (markerToIdx b * framesInSection + i) <> ".png"

  args =
    (join (map (\a -> [ "-i", a ]) finalJpgs))
      <> [ "-i"
        , wwiaFile
        , "-filter_complex"
        , "[0:v][1:v][2:v]hstack=inputs=3[top];[3:v][8:v][4:v]hstack=inputs=3[middle];[5:v][6:v][7:v]hstack=inputs=3[bottom];[top][middle][bottom]vstack=inputs=3[v]"
        , "-map"
        , "[v]"
        , "-q:v"
        , "10"
        , "z/" <> (show $ markerToIdx b) <> "." <> (show i) <> ".mosaic.jpg"
        ]

fadeInFrames = 20 :: Int

fadeInFramesN = toNumber fadeInFrames :: Number

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
          | i < fadeInFrames || i > framesInSection - fadeInFrames = do
            let
              vars
                | i < fadeInFrames = { pct: max 0 (min 100 (floor $ 100.0 * (toNumber (fadeInFrames - i)) / fadeInFramesN)), startColor: (backgroundNoteToStartColor b), endColor: (backgroundNoteToEndColor b) }
                | otherwise = { pct: max 0 (min 100 (floor $ 100.0 * (fadeInFramesN - toNumber (framesInSection - i)) / fadeInFramesN)), startColor: (backgroundNoteToStartColor b), endColor: (backgroundNoteToEndColor b) }
            imageMagickGradient vars.startColor vars.endColor outdir file v i
            imageMagickDissolve outdir file v i vars.pct
            imageMagickScale true outdir file v i
            when (not dryRun)
              $ liftEffect do
                  FS.unlink (outJpg outdir file v i)
                  FS.unlink (outGrad outdir file v i)
                  pure unit
          | otherwise = do
            imageMagickScale false outdir file v i
            when (not dryRun) $ liftEffect (FS.unlink (outJpg outdir file v i))
      next

-- 3 24 390px
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
        ffmpegSoloFrame (markerToIdx marker * framesInSection + i)
        mergeVoicesAndCleanSingleFrame marker i
