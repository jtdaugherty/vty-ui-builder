module Graphics.Vty.Widgets.Builder.Config
    ( BuilderConfig(..)
    , defaultConfig
    )
where

data BuilderConfig =
    BuilderConfig { moduleName :: String
                  , generateImports :: Bool
                  , generateModulePreamble :: Bool
                  , generateMain :: Bool
                  }
    deriving (Eq, Show)

defaultConfig :: BuilderConfig
defaultConfig = BuilderConfig { moduleName = "Interface"
                              , generateImports = True
                              , generateModulePreamble = True
                              , generateMain = False
                              }