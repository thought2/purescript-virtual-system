{ name = "virtual-system"
, dependencies =
  [ "checked-exceptions"
  , "console"
  , "effect"
  , "either"
  , "exceptions"
  , "maybe"
  , "newtype"
  , "node-buffer"
  , "node-fs"
  , "node-process"
  , "ordered-collections"
  , "partial"
  , "pathy"
  , "prelude"
  , "psci-support"
  , "transformers"
  , "undefined"
  , "variant"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
