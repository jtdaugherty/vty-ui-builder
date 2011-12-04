module Graphics.Vty.Widgets.Builder.Config
    ( BuilderConfig(..)
    , defaultConfig
    )
where

data BuilderConfig =
    BuilderConfig { moduleName :: String
                  , generateMain :: Bool
                  , generateInterfaceType :: Bool
                  , generateInterfaceBuilder :: Bool
                  }
    deriving (Eq, Show)

defaultConfig :: BuilderConfig
defaultConfig = BuilderConfig { moduleName = "Interface"
                              , generateMain = False
                              , generateInterfaceType = True
                              , generateInterfaceBuilder = True
                              }