module Main where

--TODO: Pursuit
--TODO: fill in combinators
--TODO: show do syntax, maybe with Effect and Maybe before writer


import Prelude hiding (div)

import Colors (black, blue, white)
import Components (link)
import Concur.Core (Widget)
import Concur.Core.FRP (Signal, dyn, step)
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
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect (Effect)
import Web.DOM (Element) as DOM
import Web.DOM.Element as ELE
import Web.DOM.NonElementParentNode (getElementById) as DOM
import Web.HTML (window) as DOM
import Web.DOM.HTMLCollection as HTMLCollection
import Web.HTML.HTMLDocument (toNonElementParentNode) as DOM
import Web.HTML.HTMLTextAreaElement as TAE
import Web.HTML.Window (document) as DOM

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

codePanePyRun :: forall a. String -> String -> Widget HTML a
codePanePyRun codeId src = D.div_ [P._id codeId] $
  codePane
    [ P.style { "fontSize": "1.4rem" }
    , lang "python"
    , source src
    ] []

runCodePane :: String -> Signal HTML String
runCodePane codeId = go "" where
  go :: String -> Signal HTML String
  go output = step output do
    resultEf <- (textAtId codeId) <$ D.button [P.onClick] [D.text "Run"]
    result <- liftEffect resultEf
    -- liftEffect $ log result
    pure (go result)

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
  ]
  <> pureScripIntroSlides
  <> staticTypeSlides
  <> fpSlides
  <> endSlides
  <> [ cacSlide [ closingSlideTable ] ]

pureScripIntroSlides :: forall a. Array (Widget HTML a)
pureScripIntroSlides = [
    cacSlide [ h 4 "PureScript's Language Family"]
  ]

staticTypeSlides :: forall a. Array (Widget HTML a)
staticTypeSlides = [
    cacSlide [ h 4 "Don't lie with null", codePanePy pyOptionStr]
  , cacSlide [
      h 4 "Don't lie with null (continued)"
    , D.text "list offenders: C/C++, Java, Python (without mypy)"]  
  ]

fpSlides :: forall a. Array (Widget HTML a)
fpSlides = [
    cacSlide [ h 4 "Safer Software for Science and FP", scienceFpList]
  , cacSlide [ h 4 "What's FP? Well, what's an effect?", effList]
  ]
  <> workingWithFunctions
  <> [ whatIsFP
  , cacSlide [ h 4 "The Synergy of FP and Types", staticFpSynergy ]
  ]
  where
    scienceFpList = listAppearTxt [
        "Scientists try to understand complex processes - not create them!"
      , "FP mitigates complexity by removing side effects"
      , "Simple does not always mean easy"
      ]
    effList = listAppearTxt [
        "(Any IO)"
      , "Writing or reading files"
      , "Working with Network data"
      , "Accessing any device, e.g., a printer"
      , "GUI updates, etc."
      , "But also: writing to a variable"
      ]
    staticFpSynergy = listAppearTxt [
        "FP keeps types honest: recall Effect"
      , "Effect is a type that enforces purity"
      , "Both FP and Static types work"
      ]

workingWithFunctions :: forall a. Array (Widget HTML a)
workingWithFunctions = [
    cacSlide [ h 4 $ getWorkDone <> ": combinators"]
  , cacSlide [ 
        h 4 $ getWorkDone <> ": loops"
      , listAppearTxt [
          "Use map combinators to apply a function to a collection of data"
        ]
    ]
  , cacSlide [ h 4 $ getWorkDone <> ": Writer Monad"]
  ] 
  where
    getWorkDone = "Getting Work Done with Functions"
    writerList = listAppearTxt [
        "Wait, what's a monad? The reason FP sometimes gets ignored"
      , "But we've already seen a few: Maybe and Effect"
      , "For now, a Monad type wraps a computation and let's us use `do` syntax"
    ]

-- TOOD: Other Safe languages (e.g. Rust)
endSlides :: forall a. Array (Widget HTML a)
endSlides = [
    cacSlide [ h 4 "Another Safe Language: Rust", rustList]  
  ]
  where
    rustList = listAppearTxt [
        "Uses a borrow checker to enforce data ownership rules"
      , "Generally a highly typed language, but no purity checks"
      , "Pro for HPC: same level of performance as C or C++"
      , "Con for HPC: Can't call C++ directly in FFI"
      ]

followAlongSlide :: forall a. Widget HTML a
followAlongSlide = cacSlide [
    h 2 "Follow Along"
  , listAppear [
      D.span' [D.text "Follow along at ", D.br', selfHref slidesUrl]
    , D.text "Edit and run live examples from the browser "
    , D.text "To reset examples: reload page"
    ]
  , appear [] $ pure $ D.div' [D.br', D.text "Or, try it later (from a Cornell IP)"]
  ]

whatIsFP :: forall a. Widget HTML a
whatIsFP = cacSlide [ h 4 "Now: What is Functional Programming?", fpList, pureVsImpurePy]
  where
    fpList = listAppearTxt [
        "Pure FP and FP should mean the same"
      , "Without purity, there is not much benefit other than combinator convenience"
      , "Side effects are modeled by types"
      , "We need something like the Effect type to guarantee purity"
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

pyPureId :: String
pyPureId = "pyPure"

pyImpure :: String
pyImpure = """yy = 1
zz = 2
def foo(xx: int) -> int:
    zz = zz + 1
    return xx ++ yy ++ zz

foo(0)
print(zz)
"""

pyImpureId :: String
pyImpureId = "pyImpure"

pureVsImpurePy :: forall a. Widget HTML a
pureVsImpurePy = D.div [P.style{
      "display": "flex"
    , "flex-direction": "row"
  }] [
    appear_' $ D.div_ [flexGrow 1] $ D.div [pad 10] [
        h 6 "Pure"
      , codePanePyRun pyPureId pyPure
      , dyn $ runCodePane pyPureId
      ]
  , appear_' $ D.div_ [flexGrow 1] $ D.div [pad 10] [
      h 6 "Not Pure"
    , codePanePyRun pyImpureId pyImpure
    , dyn $ runCodePane pyImpureId
    ]
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

pad :: forall a. Int -> ReactProps a
pad px = P.style {"padding" : (show px) <> "px"}

flexGrow :: forall a. Int -> ReactProps a
flexGrow fg = P.style {"flex-grow" : show fg}

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

-- | Retrieves the first text area child of the given element,
-- | if it exists
textAreaChild :: DOM.Element -> Effect (Maybe (DOM.Element))
textAreaChild ele = do
  taColl <- ELE.getElementsByTagName "textarea" ele
  HTMLCollection.item 0 taColl

-- TODO: consider showing these as exampels in the presentation:
textOfElem :: DOM.Element -> Effect String
textOfElem ele = do
  taEleMay <- textAreaChild ele
  let textAreaMay = join $ TAE.fromElement <$> taEleMay
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
      log $ "in textAtId: coudldn't find element by id: " <> id
      pure ""

{- getShaPfx :: Int -> String -> String
getShaPfx len inStr = take len $ Crypto.toString digest
  where
    digest = Crypto.hash Crypto.SHA256 inStr
 -}
