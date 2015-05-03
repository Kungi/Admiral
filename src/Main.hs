{-# LANGUAGE OverloadedStrings #-}
module Main where

import Graphics.Vty
import Graphics.Vty.Widgets.All
import System.Exit
import System.FilePath.Posix


main :: IO ()
main = do
  (browser1, fg1) <- newDirBrowser defaultBrowserSkin
  (browser2, fg2) <- newDirBrowser defaultBrowserSkin

  fg <- mergeFocusGroups fg1 fg2

  mainBox <- hBox (dirBrowserWidget browser1) (dirBrowserWidget browser2)

  c <- newCollection
  switchToMain <- addToCollection c mainBox fg

  (dirBrowserWidget browser1) `onKeyPressed` (handleBsKey browser1)
  (dirBrowserWidget browser2) `onKeyPressed` (handleBsKey browser2)

  fg `onKeyPressed` \_ key _ -> if key == KChar 'q' || key == KChar 'Q'
                                then do fgT <- newFocusGroup
                                        t <- plainText "Close KungCommander"
                                        _ <- addToFocusGroup fgT t
                                        (dlg, fgDialog) <- newDialog t "Close"
                                        fg' <- mergeFocusGroups fgT fgDialog

                                        dlg `onDialogAccept` (const exitSuccess)
                                        dlg `onDialogCancel` (const switchToMain)

                                        switchToDialog <- addToCollection c (dialogWidget dlg) fg'
                                        switchToDialog
                                        return True
                                else return False

  runUi c $ defaultContext { focusAttr = white `on` blue }

handleBsKey :: DirBrowser -> Widget DirBrowserWidgetType -> Key -> [Modifier] -> IO Bool
handleBsKey browser _ key _ =  if key ==KBS
                               then do path <- getDirBrowserPath browser
                                       setDirBrowserPath browser (joinPath (init (splitPath path)))
                                       return True
                               else return False
