{-# LANGUAGE OverloadedStrings #-}
module Main where

import Graphics.Vty
import Graphics.Vty.Widgets.All
import System.Exit
import System.FilePath.Posix
import Control.Concurrent.STM.TVar
import Control.Monad.STM

data Orientation = Vertical | Horizontal deriving (Eq)

main :: IO ()
main = do

  currentOrientation <- newTVarIO Vertical

  (browser1, fg1) <- newDirBrowser defaultBrowserSkin
  (browser2, fg2) <- newDirBrowser defaultBrowserSkin

  fg <- mergeFocusGroups fg1 fg2

  horizontalBox <- hBox (dirBrowserWidget browser1) (dirBrowserWidget browser2)
  verticalBox <- vBox (dirBrowserWidget browser1) (dirBrowserWidget browser2)

  c <- newCollection
  horizontalLayout <- addToCollection c horizontalBox fg
  verticalLayout <- addToCollection c verticalBox fg

  (dirBrowserWidget browser1) `onKeyPressed` (handleBsKey browser1)
  (dirBrowserWidget browser2) `onKeyPressed` (handleBsKey browser2)

  fg `onKeyPressed` \_ key _ -> if key == KChar 'q' || key == KChar 'Q'
                                then do fgT <- newFocusGroup
                                        t <- plainText "Close KungCommander"
                                        _ <- addToFocusGroup fgT t
                                        (dlg, fgDialog) <- newDialog t "Close"
                                        fg' <- mergeFocusGroups fgT fgDialog

                                        dlg `onDialogAccept` (const exitSuccess)
                                        dlg `onDialogCancel` (const horizontalLayout)

                                        switchToDialog <- addToCollection c (dialogWidget dlg) fg'
                                        switchToDialog
                                        return True
                                else return False

  fg `onKeyPressed` \_ key _ -> if key == KChar '|'
                                then do
                                  orient <- atomically $ readTVar currentOrientation

                                  case orient of
                                       Horizontal -> do verticalLayout
                                                        atomically $ modifyTVar currentOrientation swapOrientation
                                                        return True
                                       Vertical   -> do horizontalLayout
                                                        atomically $ modifyTVar currentOrientation swapOrientation
                                                        return True
                                else return False

  runUi c $ defaultContext { focusAttr = white `on` blue }

swapOrientation :: Orientation -> Orientation
swapOrientation Vertical = Horizontal
swapOrientation Horizontal = Vertical

handleBsKey :: DirBrowser -> Widget DirBrowserWidgetType -> Key -> [Modifier] -> IO Bool
handleBsKey browser _ key _ =  if key == KBS
                               then do path <- getDirBrowserPath browser
                                       setDirBrowserPath
                                         browser
                                         (joinPath (init (splitPath path)))
                                       return True
                               else return False
