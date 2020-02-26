module Main where

--TODO: protocols compared to typeclasses?
--TODO: TCO as a benefit of statically typed fp languages
--TODO: add something to CCRS like oneshot but without the need for view components.
--TODO: mypy literal type and comparison to Newtype in PureScript
--TODO: Pursuit
--TODO: show examples: metajelo, matlab
--TODO: add PureScript logo in intro
--TODO: quicksort in purescript, shout out to Hoare?
--TODO: Coconut, futhark
--TODO: add a clear button to runCodeExample
--TODO: add a linked page for non-spectacle mypy and codeworld sandbox
--TODO: all design languages (e.g. UML, like docs, eventually lie)
--    : is it possible to extract design diagrams from the types?

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
import Control.Alt ((<|>))
import Control.MultiAlternative (orr)
import Data.Array ((:), head)
import Data.Foldable (intercalate)
import Data.Int (floor)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Time.Duration (Milliseconds(Milliseconds))
import Data.Tuple (Tuple(..), fst)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect (Effect)
import Talk.CCRS as CCRS
import Talk.Exec as Exec
import Talk.Exec (spago, spagoInit)
import Web.DOM (Element) as DOM
import Web.DOM.Document (getElementsByClassName) as DOM
import Web.DOM.Element as ELE
import Web.DOM.NonElementParentNode (getElementById) as DOM
import Web.DOM.HTMLCollection as HTMLCollection
import Web.HTML (window) as DOM
import Web.HTML.HTMLDocument (toDocument, toNonElementParentNode) as DOM
import Web.HTML.HTMLElement as HTMLEle
import Web.HTML.HTMLTextAreaElement as TAE
import Web.HTML.Window (document) as DOM

type CtrlSignal v a = a -> Signal v a
type ArrayWidgMerge = forall a. Array (Widget HTML a) -> Widget HTML a

-- | This seems to be hardcoded (or at least a default) of Spectacle
spectacleMaxHeight :: Number
spectacleMaxHeight = 700.0

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
    [ P.style { "fontSize": "1.2rem" }
    , lang "haskell"
    , source src
    ] []

codePanePsRun :: forall a. String -> String -> Widget HTML a
codePanePsRun codeId src = D.div [P._id codeId] [
    codePane
      [ P.style { "fontSize": "1.2rem" }
      , lang "haskell"
      , source src
      ] []
  ]

codePanePy :: forall a. String -> Widget HTML a
codePanePy src =
  codePane
    [ P.style { "fontSize": "1.2rem" }
    , lang "python"
    , source src
    -- , theme "light"
    ] []

codePanePyRun :: forall a. String -> String -> Widget HTML a
codePanePyRun codeId src = D.div [P._id codeId] [
    codePane
      [ P.style { "fontSize": "1.2rem" }
      , lang "python"
      , source src
      ] []
  ]

type CodePaneCtrl = {jobIdMay :: Maybe CCRS.JobId, output :: String, maxHeight :: Int}
-- TODO: could allow it to take an array of codeIds to support multiple
--     : panes, possibly even on different slides: a good example would be
--     | the newtype smart constructor example, and a separate main file to use it
runCodePane ::
     String
  -> Array String
  -> (Array String -> CCRS.ExecFileCmd)
  -> Signal HTML CodePaneCtrl
runCodePane codeId initCmds mkFileCmd =
  step {jobIdMay: Nothing, output: "", maxHeight: 0} do
    jobId <- liftEffect $ Exec.mkSysJobIdWithInits initCmds
    pure $ go {jobIdMay: Just jobId, output: "", maxHeight: 0}
    where
      resultDivStyle maxHeight = P.style{
          "fontSize": "1.2rem"
        , "overflow": "auto"
        , "text-align": "left"
        , "max-height": (show maxHeight) <> "px"
        }
      go :: CtrlSignal HTML CodePaneCtrl
      go ctrl = step ctrl $ D.div' [
        do
          currentSpecHeight <- liftEffect elemHeightByClass "spectacle-content"
          let maxHeight = floor $ max 0.0 $ spectacleMaxHeight - currentSpecHeight
          liftEffect $ log $ show maxHeight
          -- FIXME: two problems, 1: there's more than one spectacle-content
          -- FIXME: , 2: need to subtract (viewIdOf codeId) height
          let jobIdEf = maybe (Exec.mkSysJobIdWithInits initCmds) pure ctrl.jobIdMay
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
          pure $ go {jobIdMay: Just jobId, output: codeTxt, maxHeight: ctrl.maxHeight}
        , D.div [P._id $ viewIdOf codeId] []
        ]

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

cacSlide :: ArrayWidgMerge
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
        , "This is useful for many projects, including in Science"
        , "Also: increase reusability by other scientists"
        ]
    ]
  , howTypesHelpSlide listAppear
  ]
  <> mypySlides
  <> pureScripIntroSlides
  <> staticTypeSlides
  <> fpSlides
  <> endSlides
  <> [ cacSlide [ closingSlideTable ] ]

howTypesHelpSlide :: ArrayWidgMerge -> forall a. Widget HTML a
howTypesHelpSlide listWidg = cacSlide [
    h 4 "How do types help us?"
  , listWidg [
      D.div' [
        D.text "Type annotations are (good) documentation"
      , listTxt ["We almost never write enough documentation!"]
      ]
    , D.div' [
        D.text "Static typing make its safer (and quicker!) to change code"
      , listTxt ["Some up-front cost", "Save time in the long run"]
      , D.text "Static typing make us think about design"
      ]
    ]
  ]

mypySlides :: forall a. Array (Widget HTML a)
mypySlides = [
  cacSlide [ h 4 "What is mypy?",
    listAppear [
          D.text "A static type checker for Python " <|> link "" "type annotations"
        , link "https://github.com/facebook/pyre-check" "pyre-check"
          <|> D.text " from Facebook is another, focused on performance"
        , D.text $ "Typings were introduced in Python 3.5 and have been"
          <> "expanded in each subsequent release (now at 3.8)"
        , D.text "Typings do not add any runtime overhead (usually)"
        , D.text "Spearheaded by Dropbox as their Python codebase grew"
        , link "https://blogs.dropbox.com/tech/2019/09/our-journey-to-type-checking-4-million-lines-of-python/" $
          "\"At our scale—millions of lines of Python—the dynamic "
            <> "typing in Python made code needlessly hard to understand "
            <> "and started to seriously impact productivity. \""
        ]
    ]
  , mypySimpleSlide
  , cacSlide [
      h 4 "Some common types"
    , mypyTypesTable
    , appear_' $ D.span' [D.text "Much more at ", pythonTypingLink ]]
  , mypyTwoStylesSlide
  , cacSlide [
      h 4 "Ignoring type errors"
    , listAppearTxt [
        "Some corners of Python still problematic for mypy"
      , "Maybe you don't have time to think about a type"
      , "Common when using highly dynamically-typed libraries"
      , "Introducing # type: ignore"
      ]
    ]
  , cacSlide [
      h 4 "Ignoring type errors (continued)"
    , appear_' $ D.div_ [] $ codePanePy pyFlask1
    , listAppear [
        D.span' [
            D.text flaskIssue1
          , link "https://github.com/python/mypy/issues/7218" " #7218"
        ]
      , D.span' [
            D.text flaskIssue2
          , link "https://github.com/python/mypy/issues/7187" " #7187"
          , D.text " (still open)"
        ]
      ]
    , appear_' $ D.div_ [] $ codePanePy pyFlask2
    ]
    -- , cacSlide ["Configuring mypy"] -- TODO: mypy.ini from arXiv
    , mypyLiteralSlide
  ]
  where
    flaskIssue1 = "Cannot type Flask view functions return "
      <> "types in the natural way"
    flaskIssue2 = "Union subtyping doesn't work with"
      <> "covariant type constructors Tuple and Callable"

mypySimpleSlide :: forall a. Widget HTML a
mypySimpleSlide =
  cacSlide [
      h 4 "A simple example of using mypy"
    , codePanePyRun pyBadTypeSimpleId pyBadTypeSimple
    , dyn $ runCodePane pyBadTypeSimpleId [] mkCmd
    ]
  where
    mkCmd :: Array String -> CCRS.ExecFileCmd
    mkCmd fContents = {
        files: [Tuple "foo.py" fc0]
      , command: Exec.runMypyFile
      , meta: CCRS.mypyPursMeta
      }
      where
        fc0 = fromMaybe "" (head fContents)

mypyTypesTable :: forall a. Widget HTML a
mypyTypesTable = appear_' $ D.table [P.style {
    "fontSize" : "1.4rem"
  , "border"   : "1px double " <> grey
  }] [
    appear_' $ D.tr [] [
      td $ D.text "(Semi-)primitive types"
    , td $ codeList $ ["str", "int", "float", "complex"]
    ]
  , appear_' $ D.tr [] [
      td $ D.text "Classes"
    , td $ D.text "The type is just the class name"
    ]
  , appear_' $ D.tr [] [
      td $ D.text "Container types"
    , td $ codeList ["Dict", "List", "Optional", "Tuple", "NamedTuple"]
      <|> D.text " etc."
    ]
  , appear_' $ D.tr [] [
      td $ D.text "Union types"
    , td $ code "Union[str, Foo]"
    ]
  , appear_' $ D.tr [] [
      td $ D.text "Type Alias"
    , td $ code "Response = Tuple[Dict[str, Any], int, Dict[str, Any]]"
    ]
  , appear_' $ D.tr [] [
      td $ D.text "Any: not much on it's own"
    , td $ code "Dict[str, Any]"
    ]
  , appear_' $ D.tr [] [
      td $ codeList ["Newtype", "Literal"]
    , td $ D.text "Logical types wrapping classes or values"
    ]
  , appear_' $ D.tr [] [
      td $ D.text "function (λ) type"
    , td $ code "Callable[[int], str]"
        <|> D.text " is a function of " <|> code "(int) -> str"
    ]
  ]
  where
    tdThProps = [P.style{
        "text-align" : "left"
      , "border"     : "1px solid " <> grey
      }]
    td = D.td_ tdThProps

mypyTwoStylesSlide :: forall a. Widget HTML a
mypyTwoStylesSlide = cacSlide [
    h 4 "Two ways to ascribe types"
  , D.div [P.style{
        "display": "flex"
      , "flex-direction": "row"
    }] [
      appear_' $ D.div_ [flexGrow 1] $ D.div [pad 10] [
          codePanePy pyTypeStyle1
        ]
    , appear_' $ D.div_ [flexGrow 1] $ D.div [pad 10] [
          codePanePy pyTypeStyle2
        ]
    ]
  ]

mypyLiteralSlide :: forall a. Widget HTML a
mypyLiteralSlide =
  cacSlide [
      h 4 "Literal types in Python"
    , listAppear [
        D.text "Sometimes called " <|> italic "singleton types"
      , D.text "Types constructed from a single value"
      , D.text "No runtime overhead"
      ]
    , appear_' $ D.div' [
        codePanePyRun pyLiteralId pyLiteral
      , dyn $ runCodePane pyLiteralId [] mkCmd
      ]
    , D.div [P.style{
        "display": "flex"
      , "flex-direction": "row"
      }] [
        D.div_ [flexGrow 1] $ listAppear [ D.div' [
          D.text "Not all values can be used, e.g.: "
        , listAppear [
            codeList ["float", "complex"]
          , D.text "anything not a value"
          , D.text "object instances"
          , D.text "some other caveats"
          ]
        ]]
      , D.div_ [flexGrow 1] $ appear_' $ list [ D.div' [
            D.text "Not available in PureScript directly, though planned"
          , listTxt ["Can use Symbols for Strings, or a custom typeclass"]
          ]]
      ]
    ]
  where
    mkCmd :: Array String -> CCRS.ExecFileCmd
    mkCmd fContents = {
        files: [Tuple "literal.py" fc0]
      , command: Exec.runMypyFile
      , meta: CCRS.mypyPursMeta
      }
      where
        fc0 = fromMaybe "" (head fContents)

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
      , D.text "Also, very different, so there is a learning curve"
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
              link "https://github.com/purescript-python/purescript-python" "Python"
            , link "https://github.com/andyarvanitis/purescript-native/tree/cpp" "C++"
            , link "https://github.com/andyarvanitis/purescript-native/tree/golang" "Go"
            , D.span' [link "https://github.com/csicar/pskt" "Kotlin"
              , D.text " — useful for interacting with Java or Android"]
            , link "https://github.com/purescript/documentation/blob/master/ecosystem/Alternate-backends.md" "others"
            ]
          ]
        ]
    ]
  , cacSlide [h 4 "PureScript functions", funsInPs]
  , cacSlide [h 4 "PureScript function calls", funCallsInPs]
  , cacSlide [h 4 "Partially supplied parameters (Currying)", curryInPs]
  , cacSlide [h 4 "Record types and type aliases", recInPs]
  , cacSlide [h 4 "Newtypes: why we need them", noNewtypeInPs]
  , cacSlide [h 4 "Newtypes", newtypeInPs]
  , cacSlide [h 4 "NewTypes in mypy", newtypeInPy]
  , cacSlide [h 4 "Algebraic Data Types", adtsInPs]
  , cacSlide [h 4 "Extract data safely", matchInPs]
  , cacSlide [h 4 "Type Classes", classesInPs]
  ]

staticTypeSlides :: forall a. Array (Widget HTML a)
staticTypeSlides = [
    howTypesHelpSlide list
  , cacSlide [
      h 4 "Types as Documentation"
    , listAppear [
        D.text "Types are docs that don't get stale or lie"
      , D.text "Reality: code changes, sometimes docs don't"
      , D.text "Types become very recognizeable once familiar"
      , D.text "(Usually much faster than reading docs)"
      , D.text "Allow for semantic documentation search: "
        <|> link "Pursuit" "https://pursuit.purescript.org/"
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
        h 4 "Types Guide Design"
      , listAppearTxt [
          "ADTs and allow us to combine and shape our data models"
        , "Type Classes allow us to organize and re-use them"
        , "In pure languages, no difficult-to-track global mutations"
        ]
      , appear_' $ D.div' [
          codePanePs wierdType
        , listTxt ["Even variable names aren't needed"]
        ]
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
      , D.div' [
          codePanePs psUnit
        , code "Maybe b ≅ Either Unit b"
        ]
      , code "data Either a b = Left a | Right b"
      , D.text "Usually Left holds an error value, but can be used in other ways"
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
      , "Object simulation, as possible in OO, reflects reality but is complex"
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
        "FP adds a very important dimension to static typing: Effect"
      , "Effect is a type that enforces purity"
      , "Both FP and Static types work to keep code and code "
        <> "changes less surprising"
      , "Types compensate and supplement testing, purity makes testing"
        <> "pure (non-effectful) functions simpler"
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
    , listAppear [
        D.text "Use map HOF to apply a function to a collection of data"
      , D.span' [ D.text "map :: "
        , link
          "https://pursuit.purescript.org/packages/purescript-prelude/docs/Data.Functor#v:map"
          "(a -> b) -> f a -> f b"
        ]
      ]
    , curryHOFInPs
    ]
  , cacSlide [h 4 "Newtypes: Smart Constructors", smartConsInPs]
  , cacSlide [ h 5 $ getWorkDone <> ": Monads", monadList, doNotation ]
  ]
  where
    getWorkDone = "Getting Work Done with Functions"
    monadList = listAppear [
        D.text "Wait, what's a monad? The reason FP sometimes gets ignored"
      , D.text "But we've already seen a few: " <|> code "Maybe" <|> D.text " and "
        <|> code "Effect"
      , D.text "For now, a " <|> code "Monad"
        <|> D.text " type wraps a computation and let's us use"
        <|> code "do" <|> D.text " syntax"
      , D.text "Can use " <|> code "State" <|> D.text " monad to allow isolated mutability"
      , D.text "Left side of " <|> code " <- "
        <|> D.text "is extracted value of monadic right side"
    ]
    doNotation = D.div [P.style{
        "display": "flex"
      , "flex-direction": "row"
      }] [
        appear_' $ D.div_ [flexGrow 1] $ D.div [pad 10] [
            h 6 "Do for Effect"
          , codePanePs psEffectDo
          ]
      , appear_' $ D.div_ [flexGrow 1] $ D.div [pad 10] [
          h 6 "Do for Maybe"
        , codePanePs psEffectMay
        ]
      ]

endSlides :: forall a. Array (Widget HTML a)
endSlides = [
    cacSlide [ h 4 "Another Safe Language: Rust", rustList]
  , cacSlide [ h 4 "Another Safe Language: Coconut", coconutList] --TODO add monty python scene
  ]
  where
    rustList = listTxt [
        "Uses a borrow checker to enforce data ownership rules"
      , "Generally a highly typed language, but no purity checks"
      , "Pro for HPC: same level of performance as C or C++"
      , "Con for HPC: Can't call C++ directly in FFI"
      ]
    coconutList = list [
        D.text "Has ADTs built in"
      , D.text "Supports Tail-Call-Optimization (like PureScript)"
      , D.text "All Python code is valid Coconut code"
      , selfHref "http://coconut-lang.org/"
    ] -- TODO

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
    , dyn $ runCodePane psDoubleId [spagoInit] mkCmd
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

funCallsInPs :: forall a. Widget HTML a
funCallsInPs = D.div [P.style{
      "display": "flex"
    , "flex-direction": "column"
  }] [
    listAppear [
        D.text "Traditional function call syntax is like: " <|> code "f(x,y)"
      , D.text "In PureScript, Haskell, etc: " <|> code "f x y"
        <|> D.text " or " <|> code "f (x) (y)"
      , D.text "Nested calls can use () or "
        <|> link "https://pursuit.purescript.org/packages/purescript-prelude/docs/Data.Function#v:($)" "$"
        <|> D.text " to specify argument grouping"
      , code "f x y" <|> D.text " and we want " <|> code "y = g x" <|> D.text " then: "
        <|> code "f x (g x)" <|> D.text " or " <|> code "f x $ g x" 
      ]
  , appear_' $ D.div_ [flexGrow 1] $ D.div [pad 10] [
      codePanePsRun psMulCallId psMulCall
    , dyn $ runCodePane psMulCallId initCmds mkCmd
    ]
  ]
  where
    initCmds = [spagoInit, spago "install console"]
    mkCmd :: Array String -> CCRS.ExecFileCmd
    mkCmd fContents = {
        files: [Tuple "mulCall.purs" $
          Exec.preludeEffectImports <> mulFunPs <> "\n" <> fc0]
      , command: Exec.runPsFile
      , meta: CCRS.mypyPursMeta
      }
      where
        fc0 = fromMaybe "" (head fContents)


curryInPs :: forall a. Widget HTML a
curryInPs = D.div [P.style{
      "display": "flex"
    , "flex-direction": "column"
  }] [
    appear_' $ D.div_ [flexGrow 1] $ D.div [pad 10] [
        codePanePsRun psCurryId psCurry
      , dyn $ runCodePane psCurryId initCmds mkCmd
      , italic $ "\"Currying is a process "
        <> "in which we can transform a function with multiple "
        <> "arguments into a sequence of nested functions. \""
      ]
  ]
  where
    initCmds = [spagoInit, spago "install console newtype"]
    mkCmd :: Array String -> CCRS.ExecFileCmd
    mkCmd fContents = {
        files: [Tuple "curry.purs" fc0]
      , command: Exec.runPsFile
      , meta: CCRS.mypyPursMeta
      }
      where
        fc0 = fromMaybe "" (head fContents)

recInPs :: forall a. Widget HTML a
recInPs = D.div [P.style{
      "display": "flex"
    , "flex-direction": "column"
  }] [
    appear_' $ D.div_ [flexGrow 1] $ D.div [pad 10] [
        codePanePsRun psRecordTypeId psRecordType
      , dyn $ runCodePane psRecordTypeId initCmds mkCmd
      ]
  ]
  where
    initCmds = [spagoInit, spago "install console"]
    mkCmd :: Array String -> CCRS.ExecFileCmd
    mkCmd fContents = {
        files: [Tuple "recType.purs" fc0]
      , command: Exec.runPsFile
      , meta: CCRS.mypyPursMeta
      }
      where
        fc0 = fromMaybe "" (head fContents)

noNewtypeInPs:: forall a. Widget HTML a
noNewtypeInPs = listAppear [
    D.text "Values of the same machine type may have different "
      <|> italic "logical" <|> D.text " meanings"
  , D.text "Can be used for improved safety in argument passing"
  , D.div [P.style{
          "display": "flex"
        , "flex-direction": "row"
      }] [
        D.div_ [flexGrow 1] $ D.div [pad 10] [
          codePanePsRun psNoNewtypeId psNoNewtype
        ,   dyn $ runCodePane psNoNewtypeId initCmds mkCmd
        ]
      ]
  ]
  where
    initCmds = [spagoInit, spago "install console"]
    mkCmd :: Array String -> CCRS.ExecFileCmd
    mkCmd fContents = {
        files: [Tuple "newtype.purs" $ Exec.preludeEffectImports <> fc0]
      , command: Exec.runPsFile
      , meta: CCRS.mypyPursMeta
      }
      where
        fc0 = fromMaybe "" (head fContents)


newtypeInPs :: forall a. Widget HTML a
newtypeInPs = listAppear [
    D.text "Not an alias, but a static wrapper (no runtime overhead)"
  , D.div [P.style{
          "display": "flex"
        , "flex-direction": "row"
      }] [
        D.div_ [flexGrow 1] $ D.div [pad 10] [
          codePanePsRun psNewtypeId psNewtype
        , dyn $ runCodePane psNewtypeId initCmds mkCmd
        ]
      ]
  , D.text "Newtype has other uses as well: see instances"
  ]
  where
    initCmds = [spagoInit, spago "install console"]
    mkCmd :: Array String -> CCRS.ExecFileCmd
    mkCmd fContents = {
        files: [Tuple "newtype.purs" $ Exec.preludeEffectImports <> fc0]
      , command: Exec.runPsFile
      , meta: CCRS.mypyPursMeta
      }
      where
        fc0 = fromMaybe "" (head fContents)

newtypeInPy :: forall a. Widget HTML a
newtypeInPy = listAppear [
    D.div [P.style{
          "display": "flex"
        , "flex-direction": "row"
      }] [
        D.div_ [flexGrow 1] $ D.div [pad 10] [
          codePanePyRun pyNewtypeId pyNewtype
        , dyn $ runCodePane pyNewtypeId initCmds mkCmd
        ]
      ]
  , listTxt ["Some runtime overhead in python"]
  ]
  where
    initCmds = [spagoInit, spago "install console"]
    mkCmd :: Array String -> CCRS.ExecFileCmd
    mkCmd fContents = {
        files: [Tuple "newtype.py" fc0]
      , command: Exec.runPyAndMypyFile
      , meta: CCRS.mypyPursMeta
      }
      where
        fc0 = fromMaybe "" (head fContents)

adtsInPs :: forall a. Widget HTML a
adtsInPs = listAppear [
    D.text "ADTs are Sum Types + Product Types"
  , D.text "Sums are like tagged unions of types"
  , D.text "Product refers to the constructor arguments"
  , code "data Maybe a = Nothing | Just a"
  , code "data Tuple a b = Tuple a b"
  , D.text "can build up types like " <|> code "type MayTup = Maybe (Tuple a b)"
  , D.text "This is roughly the \"algebra\""
  , D.text "Python does not have ADTs builtin, but theres a "
    <|> link "https://pypi.org/project/algebraic-data-types/" "library"
  , D.text "Unlike newtypes, data types do have runtime overhead"
  ]

matchInPs :: forall a. Widget HTML a
matchInPs = listAppear [
    code "case" <|> D.text " allows us to extract data from ADT values"
  , D.div [P.style{
          "display": "flex"
        , "flex-direction": "row"
      }] [
        D.div_ [flexGrow 1] $ D.div [pad 10] [
          codePanePsRun psMatchId psMatch
        ,   dyn $ runCodePane psMatchId initCmds mkCmd
        ]
      ]
  , D.text "If not all constructors are covered, get:" <|> D.br'
    <|> D.text "case expression could not be determined to cover all inputs"
  ]
  where
    initCmds = [spagoInit, spago "install console maybe"]
    mkCmd :: Array String -> CCRS.ExecFileCmd
    mkCmd fContents = {
        files: [Tuple "match.purs" $ Exec.preludeEffectImports <> fc0]
      , command: Exec.runPsFile
      , meta: CCRS.mypyPursMeta
      }
      where
        fc0 = fromMaybe "" (head fContents)


classesInPs :: forall a. Widget HTML a
classesInPs = listAppear [
    D.text "Classes define interfaces (allow ad-hoc polymorphism)"
  , D.text "An " <|> italic "instance"
    <|> D.text " of a class can be given for a data type or newtype"
  , codePanePs psShowMaybe
  , D.text $ "A newtype can be used to circumvent that instances can only be "
    <> "defined in the same module of either the data type or class definition"
  ]

smartConsInPs:: forall a. Widget HTML a
smartConsInPs = listAppear [
    D.text "Avoid erroneous value construction by only exporting a "
    <|> italic "smart constructor and not "
    <|> code "User" <|> D.text " constructor"
  , D.text "Explicit exports: Only " <|> code "parseUser" <|> D.text " and "
      <|> code "User" <|> D.text " type visible outside"
  , D.div [P.style{
          "display": "flex"
        , "flex-direction": "row"
      }] [
        D.div_ [flexGrow 1] $ D.div [pad 10] [
          codePanePsRun psSmartConsId psSmartCons
        ,   dyn $ runCodePane psSmartConsId initCmds mkCmd
        ]
      ]
  , D.text "Smart constructor " <|> code "parseUser"
    <|> D.text " is just a simple function"
  , D.text "Note: there is runtime overhead in this smart constructor"
  ]
  where
    initCmds = [spagoInit, spago "install strings unicode"]
    mkCmd :: Array String -> CCRS.ExecFileCmd
    mkCmd fContents = {
        files: [Tuple "smartCons.purs" fc0]
      , command: Exec.compilePsFile
      , meta: CCRS.mypyPursMeta
      }
      where
        fc0 = fromMaybe "" (head fContents)

curryHOFInPs :: forall a. Widget HTML a
curryHOFInPs = D.div [P.style{
      "display": "flex"
    , "flex-direction": "column"
  }] [
    appear_' $ D.div_ [flexGrow 1] $ D.div [pad 10] [
        codePanePsRun psCurryHOFId psCurryHOF
      , dyn $ runCodePane psCurryHOFId initCmds mkCmd
      ]
  ]
  where
    initCmds = [spagoInit, spago "install console"]
    mkCmd :: Array String -> CCRS.ExecFileCmd
    mkCmd fContents = {
        files: [Tuple "curryHOF.purs" fc0]
      , command: Exec.runPsFile
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
  , D.tr [] [
      td $ D.text "My page (provides a stable link to slides)"
    , td $ selfHref $ myPageUrl
    ]
  , D.tr [] [
      td $ D.text "# purescript channel on "
        <|> link "https://functionalprogramming.slack.com" "FP Slack"
    , td $ link "https://fpchat-invite.herokuapp.com/" "Get an Invite"
    ]
  , D.tr [] [
      td $ D.text "Try more PureScript examples online"
    , td $ selfHref $ "http://try.purescript.org"
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

list :: ArrayWidgMerge
list items = D.ul [P.style{"text-align" : "left"}] $
  D.li_ [] <$> items

listTxt :: forall a. Array String -> Widget HTML a
listTxt = list <<< (map D.text)

listAppear :: ArrayWidgMerge
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

codeList :: forall a. Array String -> Widget HTML a
codeList cs = orr $
  intercalate (pure $ D.text ", ") (pure <<< code <$> cs)

bold :: forall a. String -> Widget HTML a
bold s = D.span_ [P.style {"fontWeight" : "bold"} ] $ D.text s

italic :: forall a. String -> Widget HTML a
italic s = D.span_ [P.style {"fontWeight" : "italic"} ] $ D.text s


-- -- -- -- Python code snippets -- -- -- --

pyOptionStr :: String
pyOptionStr = """if listing_path is not None:
    bibtex_path = get_dblp_bibtex_path(listing_path)
else:
    return None"""

pyBadTypeSimple :: String
pyBadTypeSimple = """def foo(xx: int) -> str:
    return xx ++ 1"""

pyBadTypeSimpleId :: String
pyBadTypeSimpleId = "badTypeSimple"

pyTypeStyle1 :: String
pyTypeStyle1 = """def foo(xx: int) -> int:
    return xx ++ 1
yy: int = foo(3)
print(yy)"""

pyTypeStyle1Id :: String
pyTypeStyle1Id = "pyTypeStyle1"

pyTypeStyle2 :: String
pyTypeStyle2 = """def foo(xx):        # type: (int) -> int
    return xx ++ 1

yy = foo(3)         # type: int
print(yy)"""

pyNewtype :: String
pyNewtype = """from typing import NewType

UserId = NewType('UserId', int)

def name_by_id(user_id: UserId) -> str:
    return 'Foo Bar'

UserId('user')          # Fails type check
name_by_id(42)          # Fails type check
name_by_id(UserId(42))  # OK
print(UserId(5) + 1)    # OK"""

pyNewtypeId :: String
pyNewtypeId = "pyNewtype"

pyLiteral :: String
pyLiteral = """from typing_extensions import Literal

def accepts_only_four(x: Literal[4]) -> None:
    pass

accepts_only_four(4)   # OK
accepts_only_four(19)  # Rejected"""

pyLiteralId :: String
pyLiteralId = "pyLiteral"

pyFlask1 :: String
pyFlask1 = """from typing import Union
from flask import Flask, redirect, render_template
from werkzeug import Response

app = Flask(__name__)

@app.route("/")
def index() -> Union[Response, str]:
    if True:
        return redirect("/foo")           # is a Response
    return render_template("index.html")  # is a str"""

pyFlask2 :: String
pyFlask2 = """        return redirect("/foo")           # type: ignore
    return render_template("index.html")  # type: ignore"""

pyPure :: String
pyPure = """yy = 1
zz = 2
def foo(xx: int) -> int:
    return xx ++ yy ++ (zz + 1)

print([foo(0), zz])
"""

pyPureId :: String
pyPureId = "pyPure"

pyImpure :: String
pyImpure = """yy = 1
zz = 2
def foo(xx: int) -> int:
    global zz
    zz = zz + 1
    return xx ++ yy ++ zz

print([foo(0), zz])
"""

pyImpureId :: String
pyImpureId = "pyImpure"

-- -- -- -- PureScript code snippets -- -- -- --

wierdType :: String
wierdType = "rectangleArea :: Number -> Number -> String -> Number"

psFunction :: String
psFunction = """functionName :: InputType1 -> InputType2 -> ... -> OutputType
functionName input1 input2 = ..."""

psDouble :: String
psDouble = """module Main where

import Prelude

double :: Number -> Number
double x = 2.0 * x"""

psDoubleId :: String
psDoubleId = "psDouble"

psMulCall :: String
psMulCall = """main :: Effect Unit
main = do
  logShow (mul 3.0 4.0)
  logShow $ mul 3.0 4.0
  logShow $ mul 3.0 (2.0 * 2.0)
  logShow $ mul 3.0 $ 2.0 * 2.0
  logShow (mul 3.0 (2.0 * 2.0))"""

psMulCallId :: String
psMulCallId = "psMul"


mulFunPs :: String
mulFunPs = """mul :: Number -> Number -> Number
mul x y = x * y"""


psCurry :: String
psCurry = """module Main where

import Prelude
import Effect (Effect)
import Effect.Class.Console (logShow)

""" <> mulFunPs
    <> """
mul5 :: Number -> Number
mul5 = mul 5

main :: Effect Unit
main = do
  logShow (mul 3.0 4.0)
  logShow (mul5 4.0)"""

psCurryId :: String
psCurryId = "psCurry"


psCurryHOF :: String
psCurryHOF = """module Main where

import Prelude
import Effect (Effect)
import Effect.Class.Console (logShow)

main :: Effect
main = do
  logShow $ map (* 2) [0, 1, 2, 3]"""

psCurryHOFId :: String
psCurryHOFId = "psCurryHOF"

psEffectDo :: String
psEffectDo = """-- readString :: Effect String
-- parseInt   :: String -> Maybe Int
-- pure       :: a -> m a

myEff :: Effect (Maybe Int)
myEff = do
  myStr <- readString
  let intMay = parseInt myStr
  pure intMay"""

psEffectMay :: String
psEffectMay = """-- f1 :: a -> Maybe b
-- f2 :: b -> Maybe c
-- f3 :: c -> Maybe d

maybeDfromA :: Maybe d
maybeDfromA a = do
  bMay <- f1 a
  cMay <- f2 bMay
  f3 cMay"""


psRecordType :: String
psRecordType = """module Main where

import Prelude
import Effect (Effect)
import Effect.Class.Console (logShow)

type Point2D = {x :: Number, y :: Number}
myPoint :: Point2D
myPoint = {x: 0.0, y: 1.5}

main :: Effect
main = do
  logShow myPoint"""

psRecordTypeId :: String
psRecordTypeId = "psRecordType"

psNoNewtype :: String
psNoNewtype = """setContact :: String -> String -> Effect Unit
setContact user email = do
  logShow $ "Hello " <> user <> ", your email is set to " <> email

main :: Effect Unit
main = do
  setContact "bob@foo.com" "Bob" """

psNoNewtypeId :: String
psNoNewtypeId = "psNoNewtype"

psNewtype :: String
psNewtype = """import Data.Newtype (class Newtype, unwrap)

newtype User = User String
derive instance newtypeUser :: Newtype User _
newtype Email = Email String
derive instance newtypeEmail :: Newtype Email _

setContact :: User -> Email -> Effect Unit
setContact user email = do
  logShow $ "Hello " <> unwrap user
    <> ", your email is set to " <> unwrap email

main :: Effect Unit
main = do
  setContact (Email "bob@foo.com") (User "Bob")"""

psNewtypeId :: String
psNewtypeId = "psNewtype"

psSmartCons :: String
psSmartCons = """module User (User, parseUser) where

import Data.Char.Unicode (isAlphaNum)
import Data.Foldable (all)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (toCharArray)

newtype User = User String

parseUser :: String -> Maybe User
parseUser s = case all isAlphaNum (toCharArray s) of
    true -> Just (User s)
    false -> Nothing"""

psMatch :: String
psMatch = """import Data.Maybe (Maybe(..))

yepString :: Maybe String -> String
yepString sm = case sm of
  Just s -> "yep, " <> s
  Nothing -> "nope"

main :: Effect Unit
main = do
  log $ yepString (Just "foo")
  log $ yepString Nothing"""

psMatchId :: String
psMatchId = "psMatch"



psUnit :: String
psUnit = """foreign import data Unit :: Type
-- | `unit` is the sole inhabitant of the `Unit` type.
foreign import unit :: Unit"""

psShowMaybe :: String
psShowMaybe = """class Show a where
  show :: a -> String

instance showMaybe :: Show a => Show (Maybe a) where
  show (Just x) = "(Just " <> show x <> ")"
  show Nothing = "Nothing" """

psSmartConsId :: String
psSmartConsId = "psSmartCons"

myPageUrl :: String
myPageUrl = "https://www.cac.cornell.edu/barker/"

slidesUrl :: String
slidesUrl = "http://ccrs.cac.cornell.edu:8080/mypy-purescript/index.html"
-- slidesUrl = "https://www.cac.cornell.edu/barker/mypy-purs-talk"

pythonTypingUrl :: String
pythonTypingUrl = "https://docs.python.org/3/library/typing.html"

pythonTypingLink :: forall a. Widget HTML a
pythonTypingLink =  selfHref pythonTypingUrl

elemHeightByClass :: String -> Effect Number
elemHeightByClass eleClass = do
  win <- DOM.window
  doc <- DOM.document win
  specElems <- DOM.getElementsByClassName eleClass $
    DOM.toDocument doc
  specEleMay <- HTMLCollection.item 0 specElems
  case join $ HTMLEle.fromElement <$> specEleMay of
    Nothing -> pure 0.0
    Just hEle -> HTMLEle.offsetHeight hEle

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

