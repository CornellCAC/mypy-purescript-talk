{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "concur-spectacle"
, dependencies =
    [ "aff"
    , "affjax"
    , "argonaut-codecs"
    , "argonaut-core"
    , "arrays"
    , "avar"
    , "console"
    , "datetime"
    , "effect"
    , "foldable-traversable"
    , "free"
    , "nonempty"
    , "now"
    , "psci-support"
    , "react"
    , "react-dom"
    , "tailrec"
    , "web-dom"
    , "web-html"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
