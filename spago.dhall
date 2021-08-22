{-
Welcome to a Spago project!
You can edit this file as you like.

Need help? See the following resources:
- Spago documentation: https://github.com/purescript/spago
- Dhall language tour: https://docs.dhall-lang.org/tutorials/Language-Tour.html

When creating a new Spago project, you can use
`spago init --no-comments` or `spago init -C`
to generate this file without the comments in this block.
-}
{ name = "my-project"
, dependencies =
  [ "aff"
  , "aff-promise"
  , "affjax"
  , "argonaut"
  , "argonaut-codecs"
  , "arrays"
  , "bifunctors"
  , "console"
  , "datetime"
  , "dotenv"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "foreign-generic"
  , "http-methods"
  , "integers"
  , "lists"
  , "math"
  , "maybe"
  , "media-types"
  , "node-buffer"
  , "node-process"
  , "node-streams"
  , "now"
  , "nullable"
  , "ordered-collections"
  , "owoify"
  , "prelude"
  , "psci-support"
  , "random"
  , "strings"
  , "tailrec"
  , "transformers"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
