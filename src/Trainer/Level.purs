module Trainer.Level where

import Prelude
import Baduk (Game, loadBaduk)
import Data.Foldable (for_)
import Data.Int (fromString)
import Data.List (List(..), catMaybes, drop, elem, filter, foldl, fromFoldable, take, toUnfoldable, (:))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), joinWith, split)
import Data.Traversable (for)
import Effect (Effect)
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (Storage, getItem, removeItem, setItem)

data Size
  = Short
  | Medium
  | Long

data Level
  = Intro
  | Size Size

derive instance eqSize :: Eq Size

derive instance ordSize :: Ord Size

derive instance eqLevel :: Eq Level

derive instance ordLevel :: Ord Level

instance showLevel :: Show Level where
  show Intro = "bt-intro"
  show (Size Short) = "bt-short"
  show (Size Medium) = "bt-medium"
  show (Size Long) = "bt-long"

allLevels :: List Level
allLevels = Intro : Size Short : Size Medium : Size Long : Nil

type TrainingGame
  = { game :: Game
    , completed :: Boolean
    }

data LevelGames
  = LevelGames Level Int (List TrainingGame)

new :: Boolean -> List String -> List TrainingGame
new firstClear = go <<< map (\game -> { game, completed: false }) <<< catMaybes <<< map loadBaduk
  where
  go (x : xs) = (x { completed = firstClear }) : xs

  go Nil = Nil

-- | TODO: create level based on rank level
-- Check https://senseis.xmp.net/?HandicapForSmallerBoardSizes
-- 9   4.4568
-- 13  2.1361
-- 19  1
createTrainingGames :: Int -> Size -> List TrainingGame
createTrainingGames rank size =
  new (rank /= baseRank) $ take 3 $ drop (baseRank - rank - 1)
    $ case size of
        Short ->
          "(;GN[K30]SZ[9]KM[0.5]PL[B];AB[cc][gc][gg][cg][ee])"
            : "(;GN[K28]SZ[9]KM[0.5]PL[B];AB[cc][gc][gg][cg])"
            : "(;GN[K26]SZ[9]KM[0.5]PL[B];AB[cc][gg][ee])"
            : "(;GN[K22]SZ[9]KM[0.5]PL[B];AB[cc][gg])"
            : "(;GN[K15]SZ[9]KM[0.5]PL[B];AB[ee])"
            : Nil
        Medium ->
          "(;GN[K28]SZ[13]KM[0.5]PL[B];AB[dd][jd][gd][dg][dj][gj][jj][jg][gg])"
            : "(;GN[K26]SZ[13]KM[0.5]PL[B];AB[dd][jd][gd][dg][dj][gj][jj][jg])"
            : "(;GN[K24]SZ[13]KM[0.5]PL[B];AB[dd][jd][gd][dj][gj][jj][gg])"
            : "(;GN[K21]SZ[13]KM[0.5]PL[B];AB[dd][jd][gd][dj][gj][jj])"
            : "(;GN[K19]SZ[13]KM[0.5]PL[B];AB[dd][jd][dj][jj][gg])"
            : "(;GN[K17]SZ[13]KM[0.5]PL[B];AB[dd][jd][dj][jj])"
            : "(;GN[K17]SZ[13]KM[0.5]PL[B];AB[dd][jd][dj][jj])"
            : "(;GN[K15]SZ[13]KM[0.5]PL[B];AB[dd][jj][gg])"
            : Nil
        Long ->
          "(;GN[K30]SZ[19]PL[B]KM[0.5]AB[dd][pd][dp][pp][jp][jj][pj][dj][jd][gd][md][mp][gp][dg][dm][pm][pg][jg][jm][gj][mj])"
            : "(;GN[K29]SZ[19]PL[B]KM[0.5]AB[dd][pd][dp][pp][jp][pj][dj][jd][gd][md][mp][gp][dg][dm][pm][pg][jg][jm][gj][mj])"
            : "(;GN[K28]SZ[19]PL[B]KM[0.5]AB[dd][pd][dp][pp][jp][pj][dj][jd][gd][md][mp][gp][dg][dm][pm][pg][jg][jm][jj])"
            : "(;GN[K27]SZ[19]PL[B]KM[0.5]AB[dd][pd][dp][pp][jp][pj][dj][jd][gd][md][mp][gp][dg][dm][pm][pg][jg][jm])"
            : "(;GN[K26]SZ[19]PL[B]KM[0.5]AB[dd][pd][dp][pp][jp][pj][dj][jd][gd][md][mp][gp][dg][dm][pm][pg][jj])"
            : "(;GN[K25]SZ[19]PL[B]KM[0.5]AB[dd][pd][dp][pp][jp][pj][dj][jd][gd][md][mp][gp][dg][dm][pm][pg])"
            : "(;GN[K24]SZ[19]PL[B]KM[0.5]AB[dd][pd][dp][pp][jp][pj][dj][jd][md][mp][gp][dg][dm][pm][pg])"
            : Nil

defaultIntroGames :: List TrainingGame
defaultIntroGames =
  new false
    $ "(;GN[Capture]SZ[5]PL[B];AB[bb][db][bd][ac][dd][ec];AW[bc][dc];)"
    : "(;GN[Pass]SZ[6]PL[B]KM[2.5];AB[bb][eb][be][bd][bc][bf][cb][db][fb][aa];AW[cd][dc][ed][de][ce][cf][ec][fc][cc];)"
    : "(;GN[Ko ]SZ[5]KM[2]PL[B];AB[bc][ac][cb][dc][ec][bb][db];AW[cc][bd][ad][dd][ed][ce];)"
    : "(;GN[Intro]SZ[6]PL[B];AB[bb][db][bd][cc];AW[bc][ee];)"
    : "(;GN[Territory]SZ[5]KM[6.5]PL[B];)"
    : "(;GN[Handicap2]SZ[7]KM[6.5]PL[B];AB[ce][ec];)"
    : "(;GN[Handicap1]SZ[7]KM[6.5]PL[B];AB[dd];)"
    : "(;GN[Seven]SZ[7]KM[6.5]PL[B];)"
    : Nil

introGames :: List String -> List TrainingGame
introGames xs = map setComplete defaultIntroGames
  where
  setComplete tg = tg { completed = elem tg.game.name xs }

isNew :: List LevelGames -> Boolean
isNew = foldl isLevelClean true
  where
  isLevelClean :: Boolean -> LevelGames -> Boolean
  isLevelClean r = case _ of
    LevelGames Intro _ tgs -> tgs == defaultIntroGames
    _ -> r

loadLevels :: Effect (List LevelGames)
loadLevels = do
  s <- localStorage =<< window
  for allLevels (loadLevel s)
  where
  loadLevel :: Storage -> Level -> Effect LevelGames
  loadLevel storage level = case level of
    Intro -> do
      intro <- fromFoldable <<< split (Pattern ",") <<< fromMaybe "" <$> getItem (show level) storage
      pure $ LevelGames level 0
        $ case intro of
            ("" : Nil) -> defaultIntroGames
            Nil -> defaultIntroGames
            xs -> introGames xs
    Size size -> do
      rank <- readLevel <$> getItem (show level) storage
      pure $ LevelGames level rank $ createTrainingGames rank size

  readLevel :: Maybe String -> Int
  readLevel = case _ of
    Nothing -> baseRank
    Just s -> fromMaybe baseRank $ fromString s

resetLevels :: Effect (List LevelGames)
resetLevels = do
  s <- localStorage =<< window
  for_ allLevels (\l -> removeItem (show l) s)
  loadLevels

baseRank :: Int
baseRank = 30

updateIntroList :: String -> List LevelGames -> String
updateIntroList name = joinWith "," <<< toUnfoldable <<< go <<< intro
  where
  intro :: List LevelGames -> List TrainingGame
  intro = case _ of
    Nil -> Nil
    LevelGames Intro _rank games : xs -> games
    x : xs -> intro xs

  go :: List TrainingGame -> List String
  go = case _ of
    Nil -> Nil
    x : xs ->
      if x.completed || x.game.name == name then
        x.game.name : go xs
      else
        go xs

updateIntroGames :: String -> List LevelGames -> List LevelGames
updateIntroGames name = case _ of
  Nil -> Nil
  (LevelGames Intro rank games : xs) -> (LevelGames Intro rank (newGames games)) : updateIntroGames name xs
  x : xs -> x : updateIntroGames name xs
  where
  newGames :: List TrainingGame -> List TrainingGame
  newGames = case _ of
    Nil -> Nil
    (g : gs) -> (g { completed = name == g.game.name || g.completed }) : newGames gs

completeIntro :: String -> List LevelGames -> Effect (List LevelGames)
completeIntro name levels = do
  s <- localStorage =<< window
  setItem (show Intro) (updateIntroList name levels) s
  pure (updateIntroGames name levels)

updateSizeRank :: Size -> List LevelGames -> Int
updateSizeRank size = case _ of
  Nil -> baseRank
  (LevelGames (Size sz) rank _) : xs -> case sz == size of
    true -> rank - 1
    false -> updateSizeRank size xs
  x : xs -> updateSizeRank size xs

updateSizeGames :: Size -> Int -> List LevelGames -> List LevelGames
updateSizeGames size rank = case _ of
  Nil -> Nil
  (LevelGames (Size sz) _rank _) : xs -> case sz == size of
    true -> (LevelGames (Size sz) rank (createTrainingGames rank size)) : updateSizeGames size rank xs
    false -> updateSizeGames size rank xs
  x : xs -> x : updateSizeGames size rank xs

completeSize :: Size -> List LevelGames -> Effect (List LevelGames)
completeSize size levels = do
  s <- localStorage =<< window
  setItem (show (Size size)) (show newRank) s
  pure (updateSizeGames size newRank levels)
  where
  newRank = (updateSizeRank size levels)

completeLevel :: Level -> Game -> List LevelGames -> Effect (List LevelGames)
completeLevel level game levels = case level of
  Intro -> completeIntro game.name levels
  Size sz -> completeSize sz levels
