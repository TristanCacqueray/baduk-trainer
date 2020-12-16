-- | A module to manage training level
-- This module is quite verbose and could be simplified, but it gets the job done
module Trainer.Level
  ( Level(..)
  , LevelGames(..)
  , Size(..)
  , TrainingGame
  , addCustomLevel
  , completeLevel
  , isNew
  , loadLevels
  , removeCustomLevel
  , resetLevels
  ) where

import Prelude
import Baduk (Game, loadBaduk, save)
import Data.Foldable (for_)
import Data.Int (fromString)
import Data.List (List(..), catMaybes, drop, elem, foldl, fromFoldable, mapMaybe, take, toUnfoldable, (:))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
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
  = Custom
  | Intro
  | Size Size

derive instance eqSize :: Eq Size

derive instance ordSize :: Ord Size

derive instance eqLevel :: Eq Level

derive instance ordLevel :: Ord Level

instance showLevel :: Show Level where
  show Custom = "bt-custom"
  show Intro = "bt-intro"
  show (Size Short) = "bt-short"
  show (Size Medium) = "bt-medium"
  show (Size Long) = "bt-long"

allLevels :: List Level
allLevels = Custom : Intro : Size Short : Size Medium : Size Long : Nil

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
    : "(;GN[Ko]SZ[5]KM[2]PL[B];AB[bc][ac][cb][dc][ec][bb][db];AW[cc][bd][ad][dd][ed][ce];)"
    : "(;GN[Intro]SZ[6]PL[B];AB[bb][db][bd][cc];AW[bc][ee];)"
    : "(;GN[Territory]SZ[5]KM[6.5]PL[B];)"
    : "(;GN[Atari]SZ[7]KM[6.5]PL[B];AB[dd][be][cf][df][ef][fe][bd][fd];AW[cd][de][ed];)"
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
      intro <- readList level "," storage
      pure $ LevelGames level 0
        $ case intro of
            Nil -> defaultIntroGames
            xs -> introGames xs
    Custom -> do
      games <- readList level "##" storage
      pure $ LevelGames Custom 0
        $ case games of
            Nil -> Nil
            xs -> new false xs
    Size size -> do
      rank <- readRank <$> getItem (show level) storage
      pure $ LevelGames level rank $ createTrainingGames rank size

  readList :: Level -> String -> Storage -> Effect (List String)
  readList level sep storage =
    strip
      <<< fromFoldable
      <<< split (Pattern sep)
      <<< fromMaybe ""
      <$> getItem (show level) storage

  strip = case _ of
    ("" : Nil) -> Nil
    xs -> xs

  readRank :: Maybe String -> Int
  readRank = case _ of
    Nothing -> baseRank
    Just s -> fromMaybe baseRank $ fromString s

resetLevels :: Effect (List LevelGames)
resetLevels = do
  s <- localStorage =<< window
  for_ allLevels (\l -> removeItem (show l) s)
  loadLevels

baseRank :: Int
baseRank = 30

-- | Updating functions
getLevelGames :: Level -> List LevelGames -> LevelGames
getLevelGames level = go
  where
  go = case _ of
    Nil -> LevelGames level baseRank Nil
    x@(LevelGames level' _ _) : xs -> case level == level' of
      true -> x
      false -> go xs

setLevelGames :: LevelGames -> List LevelGames -> List LevelGames
setLevelGames lg@(LevelGames level _ _) = go
  where
  go = case _ of
    Nil -> lg : Nil
    x@(LevelGames level' _ _) : xs -> case level == level' of
      true -> lg : xs
      false -> x : go xs

updateLevelGames :: Level -> (LevelGames -> LevelGames) -> List LevelGames -> List LevelGames
updateLevelGames level f lgs = setLevelGames (f $ getLevelGames level lgs) lgs

updateTrainingGames :: (TrainingGame -> TrainingGame) -> LevelGames -> LevelGames
updateTrainingGames f (LevelGames rank level tgs) = (LevelGames rank level (map f tgs))

-- | Add a new completed intro game name to existing list
updateIntroList :: String -> List LevelGames -> String
updateIntroList name lgs = joinWith "," $ toUnfoldable $ mapMaybe tgName $ trainingGames
  where
  LevelGames _ _ trainingGames = getLevelGames Intro lgs

  tgName tg = case tg.completed || tg.game.name == name of
    true -> Just tg.game.name
    false -> Nothing

-- | Mark a new completed intro game to be completed
updateIntroGames :: String -> List LevelGames -> List LevelGames
updateIntroGames name = updateLevelGames Intro (updateTrainingGames setCompleted)
  where
  setCompleted tg = tg { completed = name == tg.game.name || tg.completed }

-- | Update the list of LevelGames when an intro game is completed
completeIntro :: String -> List LevelGames -> Effect (List LevelGames)
completeIntro name levels = do
  s <- localStorage =<< window
  setItem (show Intro) (updateIntroList name levels) s
  pure (updateIntroGames name levels)

-- | Update the list of LevelGames when a non intro game is completed
completeSize :: Size -> List LevelGames -> Effect (List LevelGames)
completeSize size levels = do
  s <- localStorage =<< window
  setItem (show (Size size)) (show newRank) s
  pure (updateSizeGames newRank levels)
  where
  updateSizeGames rank =
    updateLevelGames (Size size)
      (\(LevelGames sz _ _) -> LevelGames sz rank (createTrainingGames rank size))

  newRank = let (LevelGames _ rank _) = getLevelGames (Size size) levels in rank - 1

-- | Update the list of LevelGames when a game is completed
completeLevel :: Level -> Game -> List LevelGames -> Effect (List LevelGames)
completeLevel level game levels = case level of
  -- Completed intro games are recorded by adding the game name to a list
  Intro -> completeIntro game.name levels
  -- Custom game completion are not recorded
  Custom -> pure levels
  -- Other game completion are recorded by increasing the rank
  Size sz -> completeSize sz levels

-- | Update the list of LevelGames when a new custom game is created or updated
addCustomLevel :: Game -> List LevelGames -> Effect (List LevelGames)
addCustomLevel game levels = do
  s <- localStorage =<< window
  setItem (show Custom) (joinWith "##" $ toUnfoldable $ map (\tg -> save tg.game) $ newGames) s
  pure newLevels
  where
  LevelGames _ _ customGames = getLevelGames Custom levels

  newGames = addGame customGames

  newLevels = setLevelGames (LevelGames Custom 0 newGames) levels

  addGame :: List TrainingGame -> List TrainingGame
  addGame = case _ of
    Nil -> { completed: false, game } : Nil
    g : gs ->
      if g.game.name == game.name then
        { completed: false, game } : gs
      else
        g : addGame gs

-- | Update the list of LevelGames when a new custom game is completed
removeCustomLevel :: Game -> List LevelGames -> Effect (List LevelGames)
removeCustomLevel game levels = do
  s <- localStorage =<< window
  setItem (show Custom) (joinWith "##" $ toUnfoldable $ map (\tg -> save tg.game) $ newGames) s
  pure newLevels
  where
  LevelGames _ _ customGames = getLevelGames Custom levels

  newGames = removeGame customGames

  newLevels = setLevelGames (LevelGames Custom 0 newGames) levels

  removeGame :: List TrainingGame -> List TrainingGame
  removeGame = case _ of
    Nil -> Nil
    g : gs ->
      if g.game.name == game.name then
        gs
      else
        g : removeGame gs
