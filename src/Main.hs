{-# LANGUAGE OverloadedStrings #-}
module Main where

import Graphics.Vty
import Graphics.Vty.Widgets.All
import System.Exit
import System.FilePath.Posix
import Control.Concurrent.STM.TVar
import Control.Monad.STM

data Orientation = Vertical | Horizontal deriving (Eq, Show)

data ProgramState = ProgramState { currentOrientation :: Orientation
                                 } deriving Show

newQuitDialog collection horizontalLayout =
  do fgT <- newFocusGroup
     t <- plainText "Close KungCommander"
     _ <- addToFocusGroup fgT t
     (dlg, fgDialog) <- newDialog t "Close"
     fg' <- mergeFocusGroups fgT fgDialog

     dlg `onDialogAccept` (const exitSuccess)
     dlg `onDialogCancel` (const horizontalLayout)

     switchToDialog <- addToCollection collection (dialogWidget dlg) fg'
     switchToDialog
     return True

main :: IO ()
main = do
  currentState <- newTVarIO $ ProgramState { currentOrientation = Horizontal }

  (browser1, fg1) <- newDirBrowser defaultBrowserSkin
  (browser2, fg2) <- newDirBrowser defaultBrowserSkin

  fg <- mergeFocusGroups fg1 fg2

  horizontalBox <- hBox (dirBrowserWidget browser1) (dirBrowserWidget browser2)
  verticalBox <- vBox (dirBrowserWidget browser1) (dirBrowserWidget browser2)

  c <- newCollection
  horizontalLayout <- addToCollection c horizontalBox fg
  verticalLayout  <- addToCollection c verticalBox fg

  (dirBrowserWidget browser1) `onKeyPressed` (handleBrowserInput browser1)
  (dirBrowserWidget browser2) `onKeyPressed` (handleBrowserInput browser2)

  fg `onKeyPressed` \_ key _ ->
    if key == KChar 'q' || key == KChar 'Q'
    then newQuitDialog c horizontalLayout
    else return False

  fg `onKeyPressed` handleOnBSKeyPressed currentState horizontalLayout verticalLayout
  runUi c $ defaultContext { focusAttr = white `on` blue }

swapOrientation :: ProgramState -> ProgramState
swapOrientation (ProgramState Vertical) = ProgramState Horizontal
swapOrientation (ProgramState Horizontal) = ProgramState Vertical

handleBrowserInput :: DirBrowser -> Widget DirBrowserWidgetType -> Key -> [Modifier] -> IO Bool
handleBrowserInput browser _ key modifier =
  case key of
   KBS -> do path <- getDirBrowserPath browser
             setDirBrowserPath browser (joinPath (init (splitPath path)))
             return True
   otherwise -> return False

handleOnBSKeyPressed currentState horizontalLayout verticalLayout _ key _ =
  if key == KChar '|'
  then do
    state <- atomically $ readTVar currentState

    case state of
     (ProgramState Horizontal) -> do verticalLayout
                                     atomically $ modifyTVar currentState swapOrientation
                                     return True
     (ProgramState Vertical)  -> do horizontalLayout
                                    atomically $ modifyTVar currentState swapOrientation
                                    return True
  else return False
