{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "my-project"
, dependencies =
    [ "web-events"
    , "console"
    , "effect"
    , "halogen"
    , "psci-support"
    , "profunctor-lenses"
    , "spec"
    , "spec-discovery"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
