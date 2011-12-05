{-# OPTIONS_GHC -Werror  #-}
module Graphics.Vty.Widgets.Builder.Handlers
    ( handleDoc
    , coreSpecHandlers
    )
where

import Control.Monad

import Graphics.Vty.Widgets.Builder.Types
import Graphics.Vty.Widgets.Builder.GenLib
import qualified Language.Haskell.Exts as Hs
import qualified Graphics.Vty.Widgets.Builder.AST as A
import qualified Graphics.Vty.Widgets.Builder.Names as Names

import qualified Graphics.Vty.Widgets.Builder.Handlers.Box as Box
import qualified Graphics.Vty.Widgets.Builder.Handlers.Wrap as Wrap
import qualified Graphics.Vty.Widgets.Builder.Handlers.Borders as Borders
import qualified Graphics.Vty.Widgets.Builder.Handlers.Fills as Fills
import qualified Graphics.Vty.Widgets.Builder.Handlers.Centered as Centered
import qualified Graphics.Vty.Widgets.Builder.Handlers.Limits as Limits
import qualified Graphics.Vty.Widgets.Builder.Handlers.Fixed as Fixed
import qualified Graphics.Vty.Widgets.Builder.Handlers.Pad as Pad
import qualified Graphics.Vty.Widgets.Builder.Handlers.Table as Table
import qualified Graphics.Vty.Widgets.Builder.Handlers.List as List
import qualified Graphics.Vty.Widgets.Builder.Handlers.Text as Text
import qualified Graphics.Vty.Widgets.Builder.Handlers.Edit as Edit
import qualified Graphics.Vty.Widgets.Builder.Handlers.Button as Button
import qualified Graphics.Vty.Widgets.Builder.Handlers.ProgressBar as ProgressBar
import qualified Graphics.Vty.Widgets.Builder.Handlers.Dialog as Dialog
import qualified Graphics.Vty.Widgets.Builder.Handlers.DirBrowser as DirBrowser
import qualified Graphics.Vty.Widgets.Builder.Handlers.CheckBox as CheckBox

coreSpecHandlers :: [WidgetElementHandler]
coreSpecHandlers =
    concat [ Box.handlers
           , Wrap.handlers
           , Borders.handlers
           , Fills.handlers
           , Centered.handlers
           , Limits.handlers
           , Fixed.handlers
           , Pad.handlers
           , Table.handlers
           , List.handlers
           , Text.handlers
           , Edit.handlers
           , Button.handlers
           , ProgressBar.handlers
           , Dialog.handlers
           , DirBrowser.handlers
           , CheckBox.handlers
           ]

handleDoc :: A.Doc -> GenM ()
handleDoc doc = do
  mapM_ handleParam $ A.documentParams doc
  forM_ (A.documentSharedWidgets doc) $ \(_, spec) ->
      do
        nam <- newEntry $ A.widgetElementName spec
        gen (A.Widget spec) nam

  append $ bind Names.collectionName "newCollection" []

  forM_ (A.documentInterfaces doc) $ \iface ->
      do
        setCurrentInterface $ Just iface
        nam <- newEntry "interface"
        handleInterface iface nam

handleInterface :: A.Interface -> Hs.Name -> GenM ()
handleInterface iface nam = do
  gen (A.interfaceContent iface) nam

  actName <- newEntry "act"
  fgName <- newEntry "focusGroup"

  append $ bind fgName "newFocusGroup" []

  forM_ (A.interfaceFocusEntries iface) $ \info ->
      handleFocusEntry iface info fgName

  append $ bind actName "addToCollection" [ expr Names.collectionName
                                          , expr nam
                                          , expr fgName
                                          ]

  let vals = InterfaceValues { topLevelWidgetName = nam
                             , switchActionName = actName
                             , focusGroupName = fgName
                             }
  registerInterface (A.interfaceName iface) vals

handleParam :: A.Param -> GenM ()
handleParam p = do
  let paramName = mkName $ A.paramName p
      typ = parseType $ A.paramType p
      refType = A.ParameterRef
  registerReferenceTarget paramName refType paramName typ
  setFocusValue paramName $ WidgetName { widgetName = paramName
                                       , widgetType = typ
                                       }

handleFocusEntry :: A.Interface -> A.FocusReference
                 -> Hs.Name -> GenM ()
handleFocusEntry iface (A.FocusReference entryName loc) fgName =
    case entryName `elem` A.validFocusNames iface of
        False -> error $ "BUG: " ++ show loc ++ ": Validation did not prevent widget name "
                 ++ show entryName
                 ++ " from being used in focus group entry in interface "
                 ++ (show $ A.interfaceName iface)
        True -> do
          -- Since we know the name is valid for this interface, this
          -- lookup should always succeed.
          result <- lookupFocusValue (mkName entryName)
          case result of
            Nothing -> error $ "BUG: no focus value found for widget ID " ++ (show entryName)
                       ++ "; did we fail to register one?"
            Just wName -> do
              m <- lookupFocusMethod $ widgetName wName
              case m of
                Just (Merge fgName') ->
                    append $ act $ call "appendFocusGroup" [ expr fgName
                                                           , expr fgName'
                                                           ]
                -- Covers the Just Direct and Nothing cases (default
                -- is Direct so handlers don't have to register focus
                -- method unless it's Merge)
                _ -> append $ act $ call "addToFocusGroup" [ expr fgName
                                                           , expr $ widgetName wName
                                                           ]

