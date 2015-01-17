#!/usr/bin/env runhaskell

-- depends on /usr/bin/convert

import Data.List
import System.Directory
import System.Process
import System.Exit
import System.FilePath

axial ::
  String
  -> String
axial n =
  "\"image/axial/MPR Bone 3mm Axial.00" ++ n ++ ".jpg\""

axial' ::
  (Show a, Ord a, Num a) =>
  a
  -> String
axial' =
  axial . digit

coronal ::
  String
  -> String
coronal n =
  "\"image/coronal/MPR CORONAL.00" ++ n ++ ".jpg\""

coronal' ::
  (Show a, Ord a, Num a) =>
  a
  -> String
coronal' =
  coronal . digit

sagittal ::
  String
  -> String
sagittal n =
  "\"image/sagittal/MPR MPR Bone Sagitt.00" ++ n ++ ".jpg\""

sagittal' ::
  (Show a, Ord a, Num a) =>
  a
  -> String
sagittal' =
  sagittal . digit

digit ::
  (Show a, Ord a, Num a) =>
  a
  -> String
digit =
  digits ""

digits ::
  (Show a, Ord a, Num a) =>
  String
  -> a
  -> String
digits suff n =
  let p = if n < 10 then ('0':) else id
  in p (shows n suff)

(~>) ::
  (Show a, Ord a, Num a) =>
  (String -> String)
  -> [a]
  -> String
f ~> n =
  unwords (map (f . digit) n)

(!~>) ::
  (Show a, Ord a, Num a) =>
  (String -> String)
  -> [a]
  -> String
f !~> n =
  unwords (map (f . digits "-annotated") n)

(!.~>) ::
  (Show a, Ord a, Num a) =>
  (String -> String)
  -> a
  -> String
f !.~> n =
  unwords
    [
      f (digit n)
    , f !~> [n]
    ]

(!!~>) ::
  (Show a, Ord a, Num a) =>
  (String -> String)
  -> [a]
  -> String
(!!~>) f r =
  unwords (map (f !.~>) r)

main ::
  IO ()
main =
  let dist =
        "dist"
      out =
        "\"" ++ dist </> "20140818-CAT.gif\""
      r =
        unwords
          [
            "convert"
          , "-delay 300"
          , "image" </> "title.png"
          , "image" </> "subtitle.png"
          , "-delay 90"
          , "image" </> "blank.jpg"
          , "-delay 300"
          , "image" </> "axial.png"          
          , "-delay 50"
          , axial ~> [39..52]
          , axial !!~> [53..60]
          , axial ~> [61..65]
          , "-delay 90"
          , "image" </> "blank.jpg"
          , "-delay 300"
          , "image" </> "sagittal.png"          
          , "-delay 50"
          , sagittal ~> [1..7]
          , sagittal !!~> [7..13]
          , sagittal ~> [14..18]
          , "image" </> "blank.jpg"
          , out
          ]
  in do createDirectoryIfMissing True dist
        putStrLn r
        e <- system r
        exitWith e
