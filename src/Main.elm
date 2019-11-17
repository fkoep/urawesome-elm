module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Tafl.Game as Game
import Tafl.Variants as Variants
import Tafl.Web as Web

initGame =
    { turn = Game.Black
    , board = Variants.fetlarBoard
    }

main = Browser.sandbox
    { init = Web.init initGame
    , update = Web.update
    , view = Web.view
    }

