{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "affjax"
  , "audio-behaviors"
  , "canvas"
  , "console"
  , "drawing"
  , "effect"
  , "free"
  , "klank-dev-util"
  , "psci-support"
  , "typelevel-klank-dev"
  , "web-touchevents"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
