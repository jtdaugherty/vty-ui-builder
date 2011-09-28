module Graphics.Vty.Widgets.Builder.Config
    ( BuilderConfig(..)
    )
where

data BuilderConfig =
    BuilderConfig { moduleName :: String
                  , generateImports :: Bool
                  , generateModulePreamble :: Bool
                  }