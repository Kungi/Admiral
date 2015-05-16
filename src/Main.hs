{-# LANGUAGE OverloadedStrings #-}
module Main where

import Graphics.Vty
import qualified Graphics.Vty.Widgets.All as W
import KungBrowserWidget
import System.Exit
import qualified Data.Text as T
import qualified System.Cmd as Cmd
import System.FilePath.Posix
import Control.Concurrent.STM.TVar
import Control.Monad.STM

data Orientation = Vertical | Horizontal deriving (Eq, Show)

data ProgramState = ProgramState { currentOrientation :: Orientation
                                 , currentLayout :: IO ()
                                 }

newQuitDialog collection state =
  do fgT <- W.newFocusGroup
     t <- W.plainText "Close KungCommander"
     _ <- W.addToFocusGroup fgT t
     (dlg, fgDialog) <- W.newDialog t "Close"
     fg' <- W.mergeFocusGroups fgT fgDialog

     dlg `W.onDialogAccept` (const exitSuccess)

     s <- atomically $ readTVar state
     dlg `W.onDialogCancel` (const $ currentLayout s)

     switchToDialog <- W.addToCollection collection (W.dialogWidget dlg) fg'
     switchToDialog
     return True

main :: IO ()
main = do
  (browser1, fg1) <- newDirBrowser defaultBrowserSkin
  (browser2, fg2) <- newDirBrowser defaultBrowserSkin

  fg <- W.mergeFocusGroups fg1 fg2

  horizontalBox <- W.hBox (dirBrowserWidget browser1) (dirBrowserWidget browser2)
  verticalBox <- W.vBox (dirBrowserWidget browser1) (dirBrowserWidget browser2)

  c <- W.newCollection
  horizontalLayout <- W.addToCollection c horizontalBox fg
  verticalLayout  <- W.addToCollection c verticalBox fg

  state <- newTVarIO $ ProgramState { currentOrientation = Horizontal
                                    , currentLayout = horizontalLayout }

  (dirBrowserWidget browser1) `W.onKeyPressed` (handleBrowserInput c state browser1)
  (dirBrowserWidget browser2) `W.onKeyPressed` (handleBrowserInput c state browser2)
  browser1 `onBrowseAccept` openFile browser1
  browser2 `onBrowseAccept` openFile browser2
  (dirBrowserWidget browser1) `W.onGainFocus` handleGainFocus browser1
  (dirBrowserWidget browser2) `W.onGainFocus` handleGainFocus browser2

  (dirBrowserWidget browser1) `W.onLoseFocus` handleLoseFocus browser1
  (dirBrowserWidget browser2) `W.onLoseFocus` handleLoseFocus browser2

  fg `W.onKeyPressed` \_ key _ ->
    if key == KChar 'q' || key == KChar 'Q'
    then newQuitDialog c state
    else return False

  fg `W.onKeyPressed` handleOnPipeKeyPressed state horizontalLayout verticalLayout
  W.runUi c $ W.defaultContext

swapOrientation :: IO () -> ProgramState -> ProgramState
swapOrientation l p = case p of
                       ProgramState Horizontal _ -> ProgramState Vertical l
                       ProgramState Vertical _ -> ProgramState Horizontal l

setHeaderFooterColor browser color = do W.setFocusAttribute (dirBrowserHeader browser) color
                                        W.setNormalAttribute (dirBrowserHeader browser) color
                                        W.setNormalAttribute (dirBrowserFooter browser) color
                                        W.setFocusAttribute (dirBrowserFooter browser) color
                                        return ()

handleGainFocus :: DirBrowser -> W.Widget DirBrowserWidgetType -> IO ()
handleGainFocus browser widget = setHeaderFooterColor browser (white `W.on` blue)

handleLoseFocus :: DirBrowser -> W.Widget DirBrowserWidgetType -> IO ()
handleLoseFocus browser widget = setHeaderFooterColor browser (white `W.on` black)

-- handleBrowserInput :: DirBrowser -> W.Widget DirBrowserWidgetType -> Key -> [Modifier] -> IO Bool
handleBrowserInput collection state browser _ key modifier =
  case modifier of
   [MMeta] -> case key of KChar 'x' -> do reportBrowserError browser "M-x pressed"
                                          return True
                          otherwise -> return False

   [MCtrl] -> case key of KChar 's' -> do newFilterDialog collection state browser
                                          return True
                          otherwise -> return False

   otherwise -> case key of
                 KBS -> do path <- getDirBrowserPath browser
                           setDirBrowserPath browser (joinPath (init (splitPath path)))
                           return True
                 KChar 'e' -> do f <- currentSelection browser

                                 case f of
                                  Just f -> editFile browser f >> return True
                                  Nothing -> return True
                 otherwise -> return False

newFilterDialog collection state browser =
  do fgT <- W.newFocusGroup
     e <- W.editWidget
     t <- W.plainText "Filter for: " W.<++> (return e)
     _ <- W.addToFocusGroup fgT t

     (dlg, fgDialog) <- W.newDialog t "Filter"
     fg' <- W.mergeFocusGroups fgT fgDialog
     s <- atomically $ readTVar state

     e `W.onActivate` (const $ W.acceptDialog dlg)
     dlg `W.onDialogAccept` (const $ do currentLayout s
                                        text <- W.getEditText e
                                        filterBrowser browser $ T.unpack text
                                        return ())
     dlg `W.onDialogCancel` (const $ currentLayout s)

     switchToDialog <- W.addToCollection collection (W.dialogWidget dlg) fg'
     switchToDialog
     return True

runCommandOnFile :: String -> DirBrowser -> FilePath -> IO ()
runCommandOnFile c b f = do errorCode <- Cmd.system (c ++ " " ++ show f)
                            if errorCode /= ExitSuccess then
                              do reportBrowserError b "An error occured"
                                 return ()
                              else return ()

openFile :: DirBrowser -> FilePath -> IO ()
openFile = runCommandOnFile "open"

editFile :: DirBrowser -> FilePath -> IO ()
editFile = runCommandOnFile "open -e"

handleOnPipeKeyPressed state horizontalLayout verticalLayout _ key _ =
  if key == KChar '|'
  then do
    s <- atomically $ readTVar state

    let orientation = currentOrientation s
      in case orientation of
          Horizontal -> do atomically $ modifyTVar state (swapOrientation verticalLayout)
                           l <- atomically $ readTVar state
                           currentLayout l
                           return True
          Vertical  -> do atomically $ modifyTVar state (swapOrientation horizontalLayout)
                          l <- atomically $ readTVar state
                          currentLayout l
                          return True
  else return False
