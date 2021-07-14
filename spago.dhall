{ name = "halogen-project"
, dependencies =
  [ "console"
  , "debug"
  , "effect"
  , "halogen"
  , "prelude"
  , "psci-support"
  , "random"
  , "stringutils"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
