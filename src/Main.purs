module Main where

import Prelude hiding (div)

import Colors (black, blue, white)
import Components (link)
import Concur.Core (Widget)
import Concur.Core.Types (display)
import Concur.React (HTML, renderComponent)
import Concur.React.DOM (div)
import Concur.React.DOM as D
import Concur.React.Props (ReactProps)
import Concur.React.Props as P
import Concur.React.Run (runWidgetInDom)
import Concur.Spectacle (appear, codePane, deck, heading, slide)
import Concur.Spectacle.Props (Progress(..), Transition(..), bgColor, lang, preload, progress, textColor, source, theme, transition, transitionDuration)
import Data.Array ((:))
import Data.Maybe (Maybe, maybe)
import Data.Time.Duration (Milliseconds(Milliseconds))
import Effect.Console (log)
import Web.DOM (Element) as DOM
import Web.DOM.NonElementParentNode (getElementById) as DOM
import Web.HTML (window) as DOM
import Web.HTML.HTMLDocument (toNonElementParentNode) as DOM
import Web.HTML.HTMLTextAreaElement as TAE
import Web.HTML.Window (document) as DOM
import Effect (Effect)

main :: Effect Unit
main = runWidgetInDom "root" do
  deck [ slideTheme
       , preload assets
       , progress Pacman
       , transition [ Zoom, Slide ]
       , transitionDuration (Milliseconds 1000.0)
       ] slides

assets :: {}
assets = {}

h :: forall a. Int -> String -> Widget HTML a
h size txt = heading [P.style {"fontWeight": "lighter"}, P.size size, textColor grey] [D.text txt]

codeHeading :: forall a. String -> Widget HTML a
codeHeading title = heading [
  P.style {
      "position": "relative"
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

codePaneHs :: forall a. String -> Widget HTML a
codePaneHs src =
  codePane
    [ P.style { "fontSize": "1.4rem" }
    , lang "haskell"
    , source src
    ] []

codePanePy :: forall a. String -> Widget HTML a
codePanePy src =
  codePane
    [ P.style { "fontSize": "1.4rem" }
    , lang "python"
    , source src
    ] []

{- codePanePyRun :: forall a. String -> String -> Widget HTML a
codePanePyRun src =
  codePane
    [ P.style { "fontSize": "1.4rem" }
    , lang "python"
    , source src
    , P.id shaId
    ] []
  where
    shaId = src -}

headerHeight :: String
headerHeight = "100px"

cornellLogo :: forall a. Widget HTML a
cornellLogo = D.img [
    P.src "images/cornell_logo_simple_white.svg"
  , P.alt "Cornell University Logo"
  , P.height headerHeight
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
  , "height" : headerHeight
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
  [ cacSlide [
      h 2 "Programming with Types and Functions"
    , h 4 "using Python and PureScript"
    , D.br', D.br', D.text "Brandon Barker"
    , D.br', D.text "brandon.barker@cornell.edu" ]
  , followAlongSlide
  , cacSlide [ h 4 "Safer Software for Science"]
  , cacSlide [ h 4 "What is Functional Programming?", fpList, pureVsImpurePy]
  , cacSlide [ h 4 "Don't lie with null", codePanePy pyOptionStr]
  , cacSlide [ closingSlideTable ]
  ]

followAlongSlide :: forall a. Widget HTML a
followAlongSlide = cacSlide [
    h 2 "Follow Along"
  , listAppear [
      D.span' [D.text "Follow along at ", D.br', selfHref slidesUrl]
    , D.text "Edit and run live examples from the browser "]
    , D.text "Or, try it later"
  ]

fpList :: forall a. Widget HTML a
fpList = listAppearTxt [
    "Side effects are modeled by types"
  , "Functions do not side effect by default"
  ]

pyPure :: String
pyPure = """yy = 1
zz = 2
def foo(xx: int) -> int:
    return xx ++ yy ++ (zz + 1)

foo(0)
print(zz)
"""


pyImpure :: String
pyImpure = """yy = 1
zz = 2
def foo(xx: int) -> int:
    zz = zz + 1
    return xx ++ yy ++ zz

foo(0)
print(zz)
"""

pureVsImpurePy :: forall a. Widget HTML a
pureVsImpurePy = D.div [] [
    appear_' $ D.div [] [h 6 "Pure", codePanePy pyPure]
  , appear_' $ D.div [] [h 6 "Not Pure", codePanePy pyImpure]
]

closingSlideTable :: forall a. Widget HTML a
closingSlideTable= D.table [P.style {
    "fontSize" : "1.4rem"
  , "border"   : "1px double " <> grey
  }] [
    D.tr [] [
      td $ D.text "CAC services"
    , td $ selfHref $ "https://www.cac.cornell.edu"
    ]
  , D.tr [] [
      td $ D.text "These slides (with runnable examples)"
    , td $ selfHref $ slidesUrl
    ]
  , D.tr [] [
      td $ D.text "Slide source"
    , td $ selfHref $ "https://github.com/CornellCAC/mypy-purescript-talk"
    ]
  ]
  where
    tdThProps = [P.style{
        "text-align" : "left"
      , "border"     : "1px solid " <> grey
      }]
    td = D.td_ tdThProps


selfHref :: forall a. String -> Widget HTML a
selfHref url = link url url

listAppear :: forall a. Array (Widget HTML a) -> Widget HTML a
listAppear items = D.ul [P.style{"text-align" : "left"}] $
   appear_' <<< D.li_ [] <$> items

listAppearTxt :: forall a. Array String -> Widget HTML a
listAppearTxt = listAppear <<< (map D.text)

-- Theming
slideTheme :: forall a. ReactProps a
slideTheme = theme
  { colors:
    { primary: grey
    , secondary: black
    , tertiary: blue
    , quartenary: grey
    }
  , fonts:
    { primary: "-apple-system,BlinkMacSystemFont,Segoe UI,Roboto,Oxygen,Ubuntu,Cantarell,Fira Sans,Droid Sans,Helvetica Neue,sans-serif"
    , secondary: "-apple-system,BlinkMacSystemFont,Segoe UI,Roboto,Oxygen,Ubuntu,Cantarell,Fira Sans,Droid Sans,Helvetica Neue,sans-serif"
    , tertiary: "Ahamono,Menlo,monospace"
    }
  }


appear_' :: forall a. Widget HTML a -> Widget HTML a
appear_' = (appear []) <<< pure

grey :: String
grey = "#808080"

cornellRed :: String
cornellRed = "#c90022"


pyOptionStr :: String
pyOptionStr = """if listing_path is not None:
    bibtex_path = get_dblp_bibtex_path(listing_path)
else:
    return None
"""

slidesUrl :: String
slidesUrl = "https://www.cac.cornell.edu/barker/mypy-purs-talk"


docElemById :: String -> Effect (Maybe DOM.Element)
docElemById id = do
  win <- DOM.window
  doc <- DOM.document win
  let dpNode = DOM.toNonElementParentNode doc
  DOM.getElementById id dpNode

-- TODO: consider showing these as exampels in the presentation:
textOfElem :: DOM.Element -> Effect String
textOfElem ele = do
  let textAreaMay = TAE.fromElement ele
  maybe emptyElem TAE.value textAreaMay
  where
    emptyElem = do
      log "in textOfElem: coudldn't convert to text area"
      pure ""

textAtId :: String -> Effect String
textAtId id = do
  eleMay <- docElemById id
  maybe emptyElem textOfElem eleMay
  where
    emptyElem = do
      log "in textAtId: coudldn't find element by id"
      pure ""

{- getShaPfx :: Int -> String -> String
getShaPfx len inStr = take len $ Crypto.toString digest
  where
    digest = Crypto.hash Crypto.SHA256 inStr
 -}
