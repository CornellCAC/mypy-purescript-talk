{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "mypy-purescript-talk"
, dependencies = [
    "concur-spectacle"
  , "console"
  , "effect"
  , "foreign-object"
  , "psci-support"
  , "web-dom"
  , "web-events"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
