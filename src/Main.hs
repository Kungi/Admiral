{-# LANGUAGE OverloadedStrings #-}
module Main where

import Graphics.Vty
import qualified Graphics.Vty.Widgets.All as W
import KungBrowserWidget
import System.Exit
import qualified System.Cmd as Cmd
import System.FilePath.Posix
import Control.Concurrent.STM.TVar
import Control.Monad.STM

data Orientation = Vertical | Horizontal deriving (Eq, Show)

data ProgramState = ProgramState { currentOrientation :: Orientation
                                 } deriving Show

newQuitDialog collection horizontalLayout =
  do fgT <- W.newFocusGroup
     t <- W.plainText "Close KungCommander"
     _ <- W.addToFocusGroup fgT t
     (dlg, fgDialog) <- W.newDialog t "Close"
     fg' <- W.mergeFocusGroups fgT fgDialog

     dlg `W.onDialogAccept` (const exitSuccess)
     dlg `W.onDialogCancel` (const horizontalLayout)

     switchToDialog <- W.addToCollection collection (W.dialogWidget dlg) fg'
     switchToDialog
     return True

main :: IO ()
main = do
  currentState <- newTVarIO $ ProgramState { currentOrientation = Horizontal }

  (browser1, fg1) <- newDirBrowser defaultBrowserSkin
  (browser2, fg2) <- newDirBrowser defaultBrowserSkin

  fg <- W.mergeFocusGroups fg1 fg2

  horizontalBox <- W.hBox (dirBrowserWidget browser1) (dirBrowserWidget browser2)
  verticalBox <- W.vBox (dirBrowserWidget browser1) (dirBrowserWidget browser2)

  c <- W.newCollection
  horizontalLayout <- W.addToCollection c horizontalBox fg
  verticalLayout  <- W.addToCollection c verticalBox fg

  (dirBrowserWidget browser1) `W.onKeyPressed` (handleBrowserInput browser1)
  (dirBrowserWidget browser2) `W.onKeyPressed` (handleBrowserInput browser2)
  browser1 `onBrowseAccept` openFile browser1
  browser2 `onBrowseAccept` openFile browser2
  (dirBrowserWidget browser1) `W.onGainFocus` handleGainFocus browser1
  (dirBrowserWidget browser2) `W.onGainFocus` handleGainFocus browser2

  fg `W.onKeyPressed` \_ key _ ->
    if key == KChar 'q' || key == KChar 'Q'
    then newQuitDialog c horizontalLayout
    else return False

  fg `W.onKeyPressed` handleOnBSKeyPressed currentState horizontalLayout verticalLayout
  W.runUi c $ W.defaultContext { W.focusAttr = white `W.on` blue }

swapOrientation :: ProgramState -> ProgramState
swapOrientation (ProgramState Vertical) = ProgramState Horizontal
swapOrientation (ProgramState Horizontal) = ProgramState Vertical

handleGainFocus :: DirBrowser -> W.Widget DirBrowserWidgetType -> IO ()
handleGainFocus browser widget = do W.setFocusAttribute widget (W.bgColor white)
                                    return ()

handleBrowserInput :: DirBrowser -> W.Widget DirBrowserWidgetType -> Key -> [Modifier] -> IO Bool
handleBrowserInput browser _ key modifier =
  case modifier of
   [MMeta] -> case key of KChar 'x' -> do reportBrowserError browser "M-x pressed"
                                          return True
                          otherwise -> return False

   otherwise -> case key of
                 KBS -> do path <- getDirBrowserPath browser
                           setDirBrowserPath browser (joinPath (init (splitPath path)))
                           return True
                 otherwise -> return False

openFile :: DirBrowser -> FilePath -> IO ()
openFile browser filePath = do errorCode <- Cmd.system ("open " ++ show filePath)
                               if errorCode /= ExitSuccess then
                                 do reportBrowserError browser "An error occured"
                                    return ()
                               else return ()

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
