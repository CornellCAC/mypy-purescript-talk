module Main where

import Prelude hiding (div)

import Colors (blue, red, stormy, white, darkBg)
import Components (link)
import Concur.Core (Widget)
import Concur.Core.Types (display)
import Concur.React (HTML, renderComponent)
import Concur.React.DOM (div)
import Concur.React.DOM as D
import Concur.React.Props (ReactProps)
import Concur.React.Props as P
import Concur.React.Run (runWidgetInDom)
import Concur.Spectacle (codePane, deck, heading, slide)
import Concur.Spectacle.Props (Progress(..), Transition(..), bgColor, lang, preload, progress, textColor, theme, transition, transitionDuration)
import Data.Array (concat, (:))
import Data.Time.Duration (Milliseconds(Milliseconds))
import Effect (Effect)

main :: Effect Unit
main = runWidgetInDom "root" do
  deck [ slideTheme
       , preload assets
       , progress Pacman
       , transition [ Zoom, Slide ]
       , transitionDuration (Milliseconds 1000.0)
       ] slides
{-   where
    rootStyle = P.style{
        "position" : "relative"
      , "top" : "0px"
      , "left" : "0px"
      , "width" : "100%"
      , "height" : "100%"
      } -}

assets :: {}
assets = {}

h :: forall a. Int -> String -> Widget HTML a
h size txt = heading [P.style {"fontWeight": "lighter"}, P.size size, textColor grey] [D.text txt]

codeHeading :: forall a. String -> Widget HTML a
codeHeading title = heading [ P.style { "position": "relative"
                                      , "left": "50%"
                                      , "top": "0px"
                                      , "transform": "translate(-50%)"
                                      , "padding": "20px 40px"
                                      , "border": "10px solid hotpink"
                                      , "font-size": "2.5em"
                                      , "color": "white"
                                      , "white-space": "nowrap"
                                      }
                            , P.size 1
                            ] [D.text title]

cornellLogo :: forall a. Widget HTML a
cornellLogo = D.img [
    P.src "images/cornell_logo_simple_white.svg"
  , P.alt "Cornell University Logo"
  , P.height "100px"
  , P.style {"padding": "10px"}
  ]

cacHeader :: forall a. Widget HTML a
cacHeader = div [P.style {
    "background-color": cornellRed
  , "color": "white"
  , "text-align": "left"
  , "position": "fixed"
  , "top": "0px"
  , "left": "0px"
  , "width": "100%"
  , "display": "block"
  }] [
    cornellLogo
  , D.div [P.style {
        "position"  : "relative"
      , "left"      : "107px"
      , "top"       : "-40px"
      , "font-size" : "large"
      }]
      [D.text "Center for Advanced Computing"]
  ]

cacSlide :: forall a. Array (Widget HTML a) -> Widget HTML a
cacSlide es = slide [bgColor white] (cacHeader : es)


-- Spectacle has issues with wrapping everything inside one big component
-- So we have to create separate components for each Concur Widget
disassociate :: forall a b. Widget HTML a -> Widget HTML b
disassociate w = display [renderComponent w]

slides :: forall a. Array (Widget HTML a)
slides =
  [ cacSlide [ h 1 "Concur", h 5 "scalable functional UIs" ]
  , cacSlide [ h 3 "Scalable?" ]
  ] <>
  [ cacSlide [ h 3 "Functional?" ]
  , cacSlide [ h 3 "DSL for Functional Views" ]
  ] <>
  [ cacSlide [ h 3 "Monadic Event Handling" ]
  ] <>
  [ cacSlide [link "http://github.com/ajnsit/purescript-concur" "Go to Concur Purescript repository page"]
  ]

-- Theming
slideTheme :: forall a. ReactProps a
slideTheme = theme
  { colors:
    { primary: grey
    , secondary: grey
    , tertiary: blue
    , quartenary: grey
    }
  , fonts:
    { primary: "-apple-system,BlinkMacSystemFont,Segoe UI,Roboto,Oxygen,Ubuntu,Cantarell,Fira Sans,Droid Sans,Helvetica Neue,sans-serif"
    , secondary: "-apple-system,BlinkMacSystemFont,Segoe UI,Roboto,Oxygen,Ubuntu,Cantarell,Fira Sans,Droid Sans,Helvetica Neue,sans-serif"
    , tertiary: "Ahamono,Menlo,monospace"
    }
  }


grey :: String
grey = "#808080"

cornellRed :: String
cornellRed = "#c90022"
