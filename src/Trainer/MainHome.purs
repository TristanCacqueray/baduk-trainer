module Trainer.MainHome (component) where

import Prelude
import Baduk (Game, Result(..), initGame, loadBaduk)
import Bootstrap as Bootstrap
import Data.Array (concat, fromFoldable, mapWithIndex)
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.List (List(..), concatMap, length, range, zipWith)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Traversable (sequence, traverse)
import Data.Tuple (Tuple(..), uncurry)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Console (log)
import GnuGO as GnuGO
import Graphics.Canvas (CanvasElement, getCanvasElementById)
import Graphics.Canvas as Canvas
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Trainer.Board (boardSize, renderMiniBoard)
import Trainer.Editor as Editor
import Trainer.Level (Level(..), LevelGames(..), Size(..), TrainingGame, addCustomLevel, completeLevel, isNew, loadLevels, removeCustomLevel, resetLevels)
import Trainer.Player as Player

type Slots
  = ( editor :: Editor.Slot Unit
    , player :: Player.Slot Unit
    )

editor = SProxy :: SProxy "editor"

player = SProxy :: SProxy "player"

data Mode
  = ShowGames
  | ShowAbout
  | EditGame Level Game
  | PlayGame Level Game

data Action
  = SwitchMode Mode
  | Edited Level String (Maybe String)
  | Played Level Game (Maybe Result)
  | AddNewGame
  | RemoveGame Game
  | ResetSave
  | Initialize

gnuGoURL :: String
gnuGoURL = "/wasm-gnugo/gnugo.wasm"

navClass :: (Mode -> Boolean) -> Mode -> String
navClass s m = case s m of
  true -> " active"
  false -> ""

homeNavClass :: Mode -> String
homeNavClass = navClass isHome
  where
  isHome = case _ of
    ShowGames -> true
    _ -> false

aboutNavClass :: Mode -> String
aboutNavClass = navClass isAbout
  where
  isAbout = case _ of
    ShowAbout -> true
    _ -> false

type State
  = { levels :: List LevelGames
    , gnugo :: Maybe (Either String GnuGO.WASM)
    , mode :: Mode
    }

component :: forall query input output m. MonadAff m => MonadEffect m => H.Component HH.HTML query input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction, initialize = Just Initialize }
    }

initialState :: forall input. input -> State
initialState = const { levels: Nil, gnugo: Nothing, mode: ShowGames }

handleAction :: forall output m. MonadAff m => MonadEffect m => Action -> H.HalogenM State Action Slots output m Unit
handleAction = case _ of
  SwitchMode mode -> (H.modify \s -> s { mode = mode }) >>= redraw
  AddNewGame -> do
    -- state <- H.get
    H.modify_ \s -> s { mode = EditGame Custom (initGame { size = 9 }) }
  RemoveGame game -> do
    state <- H.get
    levels <- H.liftEffect $ removeCustomLevel game state.levels
    (H.modify \s -> s { levels = levels, mode = ShowGames }) >>= redraw
  Edited level id maybeGame -> case maybeGame of
    Just gameSgf -> case loadBaduk gameSgf of
      Just game -> do
        state <- H.get
        levels <- H.liftEffect $ addCustomLevel game state.levels
        (H.modify \s -> s { levels = levels, mode = ShowGames }) >>= redraw
      Nothing -> showGames
    Nothing -> showGames
  Played level game maybeResult -> case maybeResult of
    Just Win -> do
      state <- H.get
      levels <- H.liftEffect $ completeLevel level game state.levels
      (H.modify \s -> s { levels = levels, mode = ShowGames }) >>= redraw
    _ -> showGames
  Initialize -> do
    gnugo <- H.liftAff $ GnuGO.get gnuGoURL
    levels <- H.liftEffect loadLevels
    (H.modify \s -> s { levels = levels, gnugo = Just gnugo }) >>= redraw
  ResetSave -> do
    levels <- H.liftEffect $ resetLevels
    H.modify_ \s -> s { levels = levels }
  where
  showGames = (H.modify \s -> s { mode = ShowGames }) >>= redraw

  redraw state =
    H.liftEffect do
      -- Get all the board canvases using their id (level "-" index)
      canvases' <- getCanvases state
      case canvases' of
        Just canvases -> do
          -- Get all the canvas contexts
          ctxs <- traverse Canvas.getContext2D canvases
          traverse_
            (uncurry renderMiniBoard)
            -- Combine canvas contexts and board game, assuming the order is identical
            (zipWith (\ctx tg -> Tuple ctx tg.game) ctxs (concatMap getGames state.levels))
        Nothing -> log "Where are the canvases?!"

  getCanvases :: State -> Effect (Maybe (List CanvasElement))
  getCanvases state =
    sequence
      <$> traverse getCanvasElementById (concatMap getGameId state.levels)

  getGameId :: LevelGames -> List String
  getGameId (LevelGames level _ games) = case games of
    Nil -> Nil
    _ -> map (\idx -> show level <> "-" <> show idx) (range 0 (length games - 1))

  getGames :: LevelGames -> List TrainingGame
  getGames (LevelGames _ _ games) = games

render :: forall m. MonadAff m => MonadEffect m => State -> H.ComponentHTML Action Slots m
render state = HH.div [ HP.class_ (ClassName "container") ] (nav <> body)
  where
  nav =
    [ HH.nav
        [ HP.class_ (ClassName "navbar navbar-expand-lg navbar-light bg-light") ]
        [ HH.a [ HP.class_ (ClassName "navbar-brand") ] [ HH.text "Baduk Trainer" ]
        , HH.div
            [ HP.class_ (ClassName "navbar-nav") ]
            [ HH.a
                [ HP.class_ (ClassName ("nav-link" <> homeNavClass state.mode))
                , HE.onClick $ clk ShowGames
                , HP.href "#"
                ]
                [ HH.text "Home" ]
            , HH.a
                [ HP.class_ (ClassName "nav-link")
                , HE.onClick $ \e -> Just AddNewGame
                , HP.href "#"
                ]
                [ HH.text "Create" ]
            , HH.a
                [ HP.class_ (ClassName ("nav-link" <> aboutNavClass state.mode))
                , HE.onClick $ clk ShowAbout
                , HP.href "#"
                ]
                [ HH.text "About" ]
            ]
        ]
    ]

  info extra =
    Bootstrap.card "Welcome to Baduk Trainer"
      $ [ HH.text "Learn the game of baduk by playing training games against the gnugo AI. "
        , HH.text "Checkout the "
        , Bootstrap.a "https://senseis.xmp.net/?RulesOfGoIntroductory" "rules of baduk"
        , HH.text " to learn the basics first."
        ]
      <> extra

  newPlayer = isNew state.levels

  mainInfo = case newPlayer of
    true -> [ info [] ]
    false -> []

  aboutInfo =
    info
      ( [ HH.hr_
        , HH.p_
            [ HH.text "Baduk Trainer is a free software, checkout the "
            , Bootstrap.a "https://github.com/TristanCacqueray/baduk-trainer" "source"
            , HH.text ". It is written in PureScript and it integrates a WebAssembly build of "
            , Bootstrap.a "https://www.gnu.org/software/gnugo" "gnugo"
            , HH.text " so that it can be used in a web browser offline."
            ]
        , case newPlayer of
            true -> Bootstrap.button "primary" "Pick a game" $ clk ShowGames
            false -> Bootstrap.button "warning" "Reset state" $ \e -> Just ResetSave
        ]
      )

  body = case state.gnugo of
    -- Loading
    Nothing -> [ Bootstrap.center [ Bootstrap.spinner ] ]
    -- Failed to load
    Just (Left error) ->
      [ Bootstrap.alertDanger
          [ HH.h4
              [ HP.class_ (ClassName "alert-heading") ]
              [ HH.text $ "Failed to load " <> gnuGoURL ]
          , HH.text error
          , HH.hr_
          , HH.text "Try to refresh or use another browser."
          ]
      ]
    -- WASM loaded, display body
    Just (Right gnugo) -> case state.mode of
      ShowAbout -> [ aboutInfo ]
      ShowGames ->
        mainInfo
          <> ( [ HH.h1_
                  [ HH.text "Select a training game" ]
              ]
                <> (concat $ fromFoldable $ map renderGamesPicker state.levels)
            )
      EditGame level game -> [ HH.slot editor unit Editor.component { game, gnugo } (Just <<< Edited level game.name) ]
      PlayGame level game -> [ HH.slot player unit Player.component { game, gnugo } (Just <<< Played level game) ]

  clk mode = \e -> Just (SwitchMode mode)

  showLevel = case _ of
    Custom -> "My"
    Intro -> "Introduction"
    (Size Short) -> "Short"
    (Size Medium) -> "Medium"
    (Size Long) -> "Long"

  renderGamesPicker = case _ of
    LevelGames level _ Nil -> []
    LevelGames level rank games ->
      [ HH.h4_ [ HH.text (showLevel level <> " games") ]
      , Bootstrap.row $ mapWithIndex (renderGamePicker level) (fromFoldable games)
      , HH.hr_
      ]

  renderGamePicker level idx tg =
    Bootstrap.card tg.game.name
      [ Bootstrap.row [ Bootstrap.canvas (show level <> "-" <> show idx) $ boardSize tg.game.size `div` 2 ]
      , Bootstrap.row
          ( [ Bootstrap.button
                (if tg.completed then "success" else "primary")
                (if tg.completed then "replay" else "play")
                $ clk (PlayGame level tg.game)
            , Bootstrap.button "secondary" "edit" $ clk (EditGame level tg.game)
            ]
              <> case level of
                  Custom -> [ Bootstrap.button "danger" "X" $ \e -> Just $ RemoveGame tg.game ]
                  _ -> []
          )
      ]
