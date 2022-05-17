let conf = ./spago.dhall

in    conf
    ⫽ { sources =
          conf.sources # [ "test/**/*.purs" ]
      , dependencies =
            conf.dependencies #
            [ "assert"
            , "datetime"
            , "debug"
            , "erl-kernel"
            , "erl-test-eunit"
            , "partial"
            , "unsafe-coerce"
            ]
      }
