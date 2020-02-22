module Main where

--TODO: add something to CCRS like oneshot but without the need for view components.
--TODO: PureScript: Basic Syntax: Functions, function calls, Records, 
--TODO: Pursuit
--TODO: fill in HOFs
--TODO: show do syntax, maybe with Effect and Maybe before writer
--TODO: show examples: metajelo, matlab
--TODO: add PureScript logo in intro
--TODO: quicksort in purescript, shout out to Hoare?
--TODO: Coconut, futhark

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
import Data.Array ((:), head)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Time.Duration (Milliseconds(Milliseconds))
import Data.Tuple (Tuple(..), fst, snd)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect (Effect)
import Talk.CCRS as CCRS
import Talk.Exec as Exec
import Web.DOM (Element) as DOM
import Web.DOM.Element as ELE
import Web.DOM.NonElementParentNode (getElementById) as DOM
import Web.HTML (window) as DOM
import Web.DOM.HTMLCollection as HTMLCollection
import Web.HTML.HTMLDocument (toNonElementParentNode) as DOM
import Web.HTML.HTMLTextAreaElement as TAE
import Web.HTML.Window (document) as DOM

type CtrlSignal v a = a -> Signal v a

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

codePanePs :: forall a. String -> Widget HTML a
codePanePs src =
  codePane
    [ P.style { "fontSize": "1.4rem" }
    , lang "haskell"
    , source src
    ] []

codePanePsRun :: forall a. String -> String -> Widget HTML a
codePanePsRun codeId src = D.div [P._id codeId] [
    codePane
      [ P.style { "fontSize": "1.4rem" }
      , lang "haskell"
      , source src
      ] []
  ]

codePanePy :: forall a. String -> Widget HTML a
codePanePy src =
  codePane
    [ P.style { "fontSize": "1.4rem" }
    , lang "python"
    , source src
    ] []

codePanePyRun :: forall a. String -> String -> Widget HTML a
codePanePyRun codeId src = D.div [P._id codeId] [
    codePane
      [ P.style { "fontSize": "1.4rem" }
      , lang "python"
      , source src
      ] []
  ]

runCodePane ::
     String
  -> Array String
  -> (Array String -> CCRS.ExecFileCmd)
  -> Signal HTML (Tuple (Maybe CCRS.JobId) String)
runCodePane codeId initCmds mkFileCmd = step (Tuple Nothing "") do
  jobId <- liftEffect CCRS.mkJobId
  pure $ go $ Tuple (Just jobId) ""
  where
    go :: CtrlSignal HTML (Tuple (Maybe CCRS.JobId) String)
    go ctrl = step ctrl $ D.div' [
        do
          let jobIdEf = maybe CCRS.mkJobId pure jobIdMay
          jobId <- liftEffect jobIdEf
          codeEf <- (textAtId codeId) <$ D.button [P.onClick] [D.text "Run"]
          codeTxt <- liftEffect codeEf
          let fileCmd = mkFileCmd [codeTxt]
          let fileContents = CCRS.fileContentsFromArray fileCmd.files
          let cmd = fileCmd.command (fst <$> fileCmd.files)
          viewEleMay <- liftEffect $ docElemById $ viewIdOf codeId
          let viewNodeMay = ELE.toNode <$> viewEleMay
          liftEffect $ case viewNodeMay of
            Just viewNode -> do
              viewWidg <- CCRS.makeExecFileCommandWidgClear viewNode
              _ <- CCRS.updateOptFileCmd
                viewWidg fileCmd.meta jobId fileContents cmd
              pure unit
            Nothing -> pure unit
          -- liftEffect $ log result
          pure $ go $ Tuple (Just jobId) codeTxt
        , D.div [P._id $ viewIdOf codeId] []
        ]
      where
        jobIdMay = fst ctrl
        output = snd ctrl

viewIdOf :: String -> String
viewIdOf id = id <> "_view"

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
  , cacSlide [
      h 4 "Safer Software for Science"
    , listAppearTxt [
          "Safety tools traditionally used for mission critical projects"
        , "Increasingly used across industries to mitigate refactor cost"
        , "This is useful for any project, including Science"
        , "Also: increase reusability by other scientists"
        ]
    ]
  ]
  <> pureScripIntroSlides
  <> staticTypeSlides
  <> fpSlides
  <> endSlides
  <> [ cacSlide [ closingSlideTable ] ]

pureScripIntroSlides :: forall a. Array (Widget HTML a)
pureScripIntroSlides = [
    cacSlide [ h 4 "Why PureScript?",
      listAppearTxt [
          "We'll have to cover some ground first..."
        , "In short: it allows us to enforce purity checks"
        , "Compiles to somewhat readable (non-idiomatic) code"
        , "Not as quirky as mypy - wasn't done as an afer-thought"
        , "Still, for mid-sized or larger projects, mypy is a huge benefit!"
        ]
    ]
  , cacSlide [ h 4 "PureScript's Language Family",
      D.text "Much like C++, Java, and Python are similar, PureScript is similar to:"
    , listAppear [ D.div' [ D.text "Haskell - Common ancestor of this family"
        , listTxt [
            "still very good for backend"
          , "lazily evaluated"
          , "many language extensions"
          , "but requires a hefty runtime (not ideal for browsers)"
          ]
        ]
      , D.div' [D.text "Idris - More academic"
        , listTxt ["stronger typing features than even Haskell" ]
        ]
      , D.text "The common syntax of this family is very concise"
      , D.text "(even compared to Python!)"
      , D.text "PureScript aims for simplicity and primarily targets JavaScript"
      ]
    ]
  , cacSlide [ h 4 "What can I do with PureScript?",
      listAppear [
          D.text "The PureScript ecosystem primarily targets JavaScript"
        , D.text "Generates efficient JavaScript code for the Browser"
        , D.span' [
            D.text "These slides are written in PureScript/"
          , link "https://github.com/purescript-concur" "Concur"
          , listTxt ["Concur is a React-based library for PureScript" ]
          ]
        , D.text "Use Node.js for server or desktop coding"
        , D.div' [ D.text "Use another backend such as "
          , list [
              link "https://github.com/andyarvanitis/purescript-native/tree/cpp" "C++"
            , link "https://github.com/andyarvanitis/purescript-native/tree/golang" "Go"
            , D.span' [link "https://github.com/csicar/pskt" "Kotlin"
              , D.text " — useful for interacting with Java or Android"]
            , link "https://github.com/purescript/documentation/blob/master/ecosystem/Alternate-backends.md" "others"
            ]
          ]
        ]
    ]
  , cacSlide [h 4 "PureScript Functions", funsInPs]
  ]

staticTypeSlides :: forall a. Array (Widget HTML a)
staticTypeSlides = [
    cacSlide [
        h 4 "How do types help us?"
      , listTxt [
          "Type annotations are (good) documentation"
        , "Static typing make its safer (and quicker!) to change code"
        , "Static typing make us think about design"
        ]
      ]
  , cacSlide [
      h 4 "Types as Documentation"
    , listAppearTxt [
        "Types are docs that don't get stale or lie"
      , "Reality: code changes, sometimes docs don't"
      , "Types become very recognizeable once familiar"
      , "(Usually much faster than reading docs)"
      , "TODO: add an example here"
      ]
    ]
  , cacSlide [
      h 4 "Types Enable Confident Refactoring"
    , listAppearTxt [
        "When you change something ..."
      , "The compiler will tell you what broke as a result"
      , "No need to run long, hand-written tests in most cases"
      , "Or worse, find out from another user or reviewer"
      , "The stronger the type system, the fewer tests needed"
      , "(Still need some)"
      ]
    ]
  , cacSlide [
        h 4 "Types Guide Design (TODO)"
      ]
  , cacSlide [
        h 4 "Some type systems are more honest than others"
      , list [ D.div' [D.text "Incomplete list of offenders: "
        , listTxt $ pure "C/C++, Java, Python (without mypy)" ]]
      , D.div_ [] $ D.text "Tony Hoare on Null: the billion dollar mistake"
      , D.iframe [
          P.width "728"
        , P.height "410"
        -- TODO: fix start point
        , P.src "https://www.youtube.com/embed/YYkOWzrO3xg?start=128?rel=0"
        , P.frameBorder "0"
        -- , P.allow "accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture"
        -- , P.allowFullscreen
        ] []
      ]
  , cacSlide [
      h 4 "Don't lie with null"
    , h 6 "Optional to the Rescue!"
    , codePanePy pyOptionStr
    ]
  , cacSlide [
      h 4 "Don't lie with null (continued)"
      -- TODO: Maybe example in PureScript
    ]
  , cacSlide [
      h 4 "From Maybe to Either"
    , listAppear [
        D.text "What if we want more information than just None for a failure?"
      , D.span' [
            code "Either a b", D.text " is a ", code "Left", D.text " of ", code "a"
          , D.text " or a ", code "Right", D.text  " of ",  code "b"]
      , code "Maybe b ≅ Either Unit b"
      , D.text "Usually Left holds an error value, but can be used in other ways"
      , D.text "Either is an sum type, not supported in Python"
      , D.text "Though you could make a class that behaves similarly"
      -- TODO: add either
      ]
    ]
  ]

fpSlides :: forall a. Array (Widget HTML a)
fpSlides = [
    cacSlide [ h 4 "Safer Software for Science and FP", scienceFpList]
  , cacSlide [ h 4 "What's FP? Well, what's an effect?", effList]
  ]
  <> workingWithFunctions
  <> [ whatIsFP, pureVsImpure1
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
      , "(this last one can be achieved with writer monad ..."
      , "... but modifying a variable outside of scope is prohibited)"
      ]
    staticFpSynergy = listAppearTxt [
        "FP keeps types honest: recall Effect"
      , "Effect is a type that enforces purity"
      , "Both FP and Static types work"
      ]

workingWithFunctions :: forall a. Array (Widget HTML a)
workingWithFunctions = [
    cacSlide [ h 5 $ getWorkDone <> ": HOFs", h 6 "(Higher Order Functions)"
      , listAppear [
          D.text "HOFs take other functions as arguments"
        , D.text "When functions are \"first class\", HOF is just a function"
        , D.span' [ D.text "Most common example: ", code "map" ]
        ]
      ]
  , cacSlide [
      h 5 $ getWorkDone <> ": loops"
    , listAppearTxt [
        "Use map HOF to apply a function to a collection of data"
      ]
    ]
  , cacSlide [ h 5 $ getWorkDone <> ": Writer Monad"]
  ]
  where
    getWorkDone = "Getting Work Done with Functions"
    writerList = listAppearTxt [
        "Wait, what's a monad? The reason FP sometimes gets ignored"
      , "But we've already seen a few: Maybe and Effect"
      , "For now, a Monad type wraps a computation and let's us use `do` syntax"
    ]

endSlides :: forall a. Array (Widget HTML a)
endSlides = [
    cacSlide [ h 4 "Another Safe Language: Rust", rustList]
  , cacSlide [ h 4 "Another Safe Language: Coconut", coconutList] --TODO add monty python scene
  ]
  where
    rustList = listAppearTxt [
        "Uses a borrow checker to enforce data ownership rules"
      , "Generally a highly typed language, but no purity checks"
      , "Pro for HPC: same level of performance as C or C++"
      , "Con for HPC: Can't call C++ directly in FFI"
      ]
    coconutList = listAppearTxt [] -- TODO

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
whatIsFP = cacSlide [ h 4 "Now: What is Functional Programming?", fpList]
  where
    fpList = listAppearTxt [
        "Pure FP and FP should mean the same"
      , "Without purity, there is not much benefit other than HOF convenience"
      , "Side effects are modeled by types"
      , "We need something like the Effect type to guarantee purity"
      , "Functions do not side effect by default"
      , "FP limits our capabilities to increase our effectiveness"
      ]

pureVsImpure1 :: forall a. Widget HTML a
pureVsImpure1 = cacSlide [h 4 "Pure vs Impure", pureVsImpurePy]



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
      , dyn $ runCodePane pyPureId [] mkCmd
      ]
  , appear_' $ D.div_ [flexGrow 1] $ D.div [pad 10] [
      h 6 "Not Pure"
    , codePanePyRun pyImpureId pyImpure
    , dyn $ runCodePane pyImpureId [] mkCmd
    ]
  ]
  where
    mkCmd :: Array String -> CCRS.ExecFileCmd
    mkCmd fContents = {
        files: [Tuple "foo.py" fc0]
      , command: Exec.runPyFile
      , meta: CCRS.mypyPursMeta
      }
      where
        fc0 = fromMaybe "" (head fContents)

funsInPs :: forall a. Widget HTML a
funsInPs = D.div [P.style{
      "display": "flex"
    , "flex-direction": "column"
  }] [
    appear_' $ D.div_ [flexGrow 1] $ D.div [pad 10] [
        h 6 "Function syntax"
      , codePanePs psFunction
      ]
  , appear_' $ D.div_ [flexGrow 1] $ D.div [pad 10] [
      h 6 "Simple example: double"
    , codePanePsRun psDoubleId psDouble
    , dyn $ runCodePane psDoubleId ["spago init"] mkCmd
    , italic "Note: need 2.0 (float) and not 2 (Int)"
    ]
  ]
  where
    mkCmd :: Array String -> CCRS.ExecFileCmd
    mkCmd fContents = {
        files: [Tuple "double.purs" fc0]
      , command: Exec.compilePsFile
      , meta: CCRS.mypyPursMeta
      }
      where
        fc0 = fromMaybe "" (head fContents)


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

list :: forall a. Array (Widget HTML a) -> Widget HTML a
list items = D.ul [P.style{"text-align" : "left"}] $
  D.li_ [] <$> items

listTxt :: forall a. Array String -> Widget HTML a
listTxt = list <<< (map D.text)

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

code :: forall a. String -> Widget HTML a
code = D.code' <<< pure <<< D.text

bold :: forall a. String -> Widget HTML a
bold s = D.span_ [P.style {"fontWeight" : "bold"} ] $ D.text s

italic :: forall a. String -> Widget HTML a
italic s = D.span_ [P.style {"fontWeight" : "italic"} ] $ D.text s

pyOptionStr :: String
pyOptionStr = """if listing_path is not None:
    bibtex_path = get_dblp_bibtex_path(listing_path)
else:
    return None
"""

psFunction :: String
psFunction = """functionName :: InputType1 -> InputType2 -> ... -> OutputType
functionName input1 input2 = ...
"""

psDouble :: String
psDouble = """module Main where

import Prelude

double :: Number -> Number
double x = 2.0 * x
"""

psDoubleId :: String
psDoubleId = "psDouble"


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
