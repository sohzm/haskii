{-# LANGUAGE OverloadedStrings #-}

module Main where

import Codec.Picture
import qualified Data.ByteString as BS
import Data.Char (chr)
import Options.Applicative
import System.IO (hPutStrLn, stderr)

data Options = Options
  { inputFile :: FilePath, scale :: Double }

options :: Parser Options
options = Options
    <$> strOption ( long "input"
          <> metavar "FILE"
          <> help "Input image file"
      )
    <*> option auto ( long "scale"
          <> metavar "SCALE"
          <> help "Scale factor for the output (e.g., 0.5 for half size)"
          <> value 1.0
      )

asciiChars :: String
asciiChars = " .:-=+*#%@"

pixelToAscii :: Pixel8 -> Char
pixelToAscii p = asciiChars !! (fromIntegral p * length asciiChars `div` 256)

imageToAscii :: Image Pixel8 -> String
imageToAscii img = unlines [ [pixelToAscii (pixelAt img x y) | x <- [0 .. imageWidth img - 1]] | y <- [0 .. imageHeight img - 1] ]

convertToGrayscale :: DynamicImage -> Image Pixel8
convertToGrayscale img =
  case img of
    ImageY8 i -> i
    ImageYA8 i -> pixelMap (\(PixelYA8 y _) -> y) i
    ImageRGB8 i -> pixelMap (\(PixelRGB8 r g b) -> floor (0.299 * fromIntegral r + 0.587 * fromIntegral g + 0.114 * fromIntegral b)) i
    ImageRGBA8 i -> pixelMap (\(PixelRGBA8 r g b _) -> floor (0.299 * fromIntegral r + 0.587 * fromIntegral g + 0.114 * fromIntegral b)) i
    ImageYCbCr8 i -> pixelMap (\(PixelYCbCr8 y _ _) -> y) i
    ImageCMYK8 i -> pixelMap (\(PixelCMYK8 c m y k) -> floor (255 * (1 - fromIntegral c / 255) * (1 - fromIntegral k / 255))) i
    _ -> error "Unsupported image format"

scaleBilinear :: Int -> Int -> Image Pixel8 -> Image Pixel8
scaleBilinear newWidth newHeight img = generateImage pixelFunc newWidth newHeight
  where
    oldWidth = imageWidth img
    oldHeight = imageHeight img
    pixelFunc x y =
      let xf = fromIntegral x / fromIntegral (newWidth - 1) * fromIntegral (oldWidth - 1)
          yf = fromIntegral y / fromIntegral (newHeight - 1) * fromIntegral (oldHeight - 1)
          x1 = floor xf
          y1 = floor yf
          x2 = min (x1 + 1) (oldWidth - 1)
          y2 = min (y1 + 1) (oldHeight - 1)
          xr = xf - fromIntegral x1
          yr = yf - fromIntegral y1
          tl = fromIntegral $ pixelAt img x1 y1
          tr = fromIntegral $ pixelAt img x2 y1
          bl = fromIntegral $ pixelAt img x1 y2
          br = fromIntegral $ pixelAt img x2 y2
          top = tl * (1 - xr) + tr * xr
          bottom = bl * (1 - xr) + br * xr
       in round $ top * (1 - yr) + bottom * yr

main :: IO ()
main = do
  opts <- execParser $ info (options <**> helper) ( fullDesc
            <> progDesc "Convert an image to ASCII art"
            <> header "haskii - an image to ASCII converter"
        )

  eimg <- readImage (inputFile opts)
  case eimg of
    Left err -> hPutStrLn stderr $ "Error: " ++ err
    Right img -> do
      let grayImg = convertToGrayscale img
          (w, h) = (imageWidth grayImg, imageHeight grayImg)
          newWidth = round (fromIntegral w * scale opts)
          newHeight = round (fromIntegral h * scale opts * 0.4) -- since ascii characters are taller than they are wide
          scaledImg = scaleBilinear newWidth newHeight grayImg
      putStrLn $ imageToAscii scaledImg
