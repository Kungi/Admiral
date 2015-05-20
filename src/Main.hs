{-# LANGUAGE OverloadedStrings #-}
module Main where

import Graphics.Vty
import qualified Graphics.Vty.Widgets.All as W
import KungBrowserWidget
import System.Exit
import qualified Data.Text as T
import System.Process
import System.FilePath.Posix
import Control.Monad
import Control.Concurrent.STM.TVar
import Control.Monad.STM

data Orientation = Vertical | Horizontal deriving (Eq, Show)

data ProgramState = ProgramState { currentOrientation :: Orientation
                                 , currentLayout :: IO ()
                                 }

newHelpDialog :: W.Collection -> TVar ProgramState -> IO ()
newHelpDialog c state = do
  fg <- W.newFocusGroup
  t <- W.plainText $ T.pack (unlines
                             ["Help:"
                             ,"====="
                             ,""
                             ,"Commands:"
                             ,"---------"
                             ,""
                             ,"?     - display this help message"
                             ,"|     - toggle orientation between horizontal and vertical"
                             ,"C-q   - quit Admiral and quit this message"
                             ,"Tab   - Switch panel"
                             ,"Enter - Open selected file"
                             ,"e     - Edit selected file in TextEdit.app"
                             ,"C-f   - Filter directory browser"
                             ,"C-s   - Search directory browser"])
  _ <- W.addToFocusGroup fg t

  s <- atomically $ readTVar state
  t `W.onKeyPressed` \_ key _ -> if key == KChar 'q' then
                                   do currentLayout s
                                      return True
                                 else return False

  join (W.addToCollection c t fg)

newQuitDialog :: W.Collection -> TVar ProgramState -> IO ()
newQuitDialog collection state =
  do fgT <- W.newFocusGroup
     t <- W.plainText "Close KungCommander"
     _ <- W.addToFocusGroup fgT t
     (dlg, fgDialog) <- W.newDialog t "Close"
     fg' <- W.mergeFocusGroups fgT fgDialog

     dlg `W.onDialogAccept` const exitSuccess

     s <- atomically $ readTVar state
     dlg `W.onDialogCancel` const (currentLayout s)

     join (W.addToCollection collection (W.dialogWidget dlg) fg')

main :: IO ()
main = do
  (filter1, search1, browser1, fg1) <- newDirBrowser defaultBrowserSkin
  (filter2, search2, browser2, fg2) <- newDirBrowser defaultBrowserSkin

  fg <- W.mergeFocusGroups fg1 fg2

  W.setFocusGroupNextKey fg (KChar 'u') [MCtrl]

  b1 <- (return (dirBrowserWidget browser1) W.<--> (return filter1)) W.<--> (return search1)
  b2 <- (return (dirBrowserWidget browser2) W.<--> (return filter2)) W.<--> (return search2)

  horizontalBox <- W.hBox b1 b2
  verticalBox <- W.vBox b1 b2

  c <- W.newCollection
  horizontalLayout <- W.addToCollection c horizontalBox fg
  verticalLayout  <- W.addToCollection c verticalBox fg

  state <- newTVarIO ProgramState { currentOrientation = Horizontal
                                  , currentLayout = horizontalLayout }

  dirBrowserWidget browser1 `W.onKeyPressed` handleBrowserInput c state browser1 browser2
  dirBrowserWidget browser2 `W.onKeyPressed` handleBrowserInput c state browser2 browser1

  browser1 `onBrowseAccept` openFile browser1
  browser2 `onBrowseAccept` openFile browser2

  dirBrowserWidget browser1 `W.onGainFocus` handleGainFocus browser1
  dirBrowserWidget browser2 `W.onGainFocus` handleGainFocus browser2

  dirBrowserWidget browser1 `W.onLoseFocus` handleLoseFocus browser1
  dirBrowserWidget browser2 `W.onLoseFocus` handleLoseFocus browser2

  dirBrowserFilterEdit browser1 `W.onChange` filterBrowser browser1
  dirBrowserFilterEdit browser2 `W.onChange` filterBrowser browser2

  dirBrowserSearchEdit browser1 `W.onChange` searchBrowser browser1
  dirBrowserSearchEdit browser2 `W.onChange` searchBrowser browser2

  dirBrowserSearchEdit browser1 `W.onActivate` acceptSearch browser1
  dirBrowserSearchEdit browser2 `W.onActivate` acceptSearch browser2

  filter1 `W.onKeyPressed` handleFilterKey browser1
  filter2 `W.onKeyPressed` handleFilterKey browser2

  search1 `W.onKeyPressed` handleSearchKey browser1
  search2 `W.onKeyPressed` handleSearchKey browser2

  fg `W.onKeyPressed` handleFocusGroupKeys state c

  fg `W.onKeyPressed` handleOnPipeKeyPressed state horizontalLayout verticalLayout
  W.runUi c W.defaultContext

acceptSearch :: DirBrowser -> W.Widget W.Edit -> IO ()
acceptSearch b _ = do toggleWidgetVisible (dirBrowserSearch b)
                      W.focus (dirBrowserWidget b)

handleFilterKey :: DirBrowser -> t -> Key -> [Modifier] -> IO Bool
handleFilterKey browser _ (KChar 'f') [MCtrl] = do W.focus (dirBrowserWidget browser)
                                                   toggleWidgetVisible (dirBrowserFilter browser)
                                                   return True
handleFilterKey browser _ (KChar '\t') [] = do reportBrowserError browser "Tab pressed in filter"
                                               return True
handleFilterKey _ _ _ _ = do return False

handleSearchKey :: DirBrowser -> t -> Key -> [Modifier] -> IO Bool
handleSearchKey browser _ (KChar 's') [MCtrl] = do W.focus (dirBrowserWidget browser)
                                                   toggleWidgetVisible (dirBrowserSearch browser)
                                                   return True
handleSearchKey browser _ (KChar '\t') [] = do reportBrowserError browser "Tab pressed in search"
                                               return True
handleSearchKey _ _ _ _ = do return False

handleFocusGroupKeys :: TVar ProgramState -> W.Collection -> t -> Key -> [Modifier] -> IO Bool
handleFocusGroupKeys state c _ (KChar 'q') [MCtrl] = newQuitDialog c state >> return True
handleFocusGroupKeys _ _ _ _ _ = return False

swapOrientation :: IO () -> ProgramState -> ProgramState
swapOrientation l p = case p of
                       ProgramState Horizontal _ -> ProgramState Vertical l
                       ProgramState Vertical _ -> ProgramState Horizontal l

setHeaderFooterColor :: DirBrowser -> Attr -> IO ()
setHeaderFooterColor browser color = do W.setFocusAttribute (dirBrowserHeader browser) color
                                        W.setNormalAttribute (dirBrowserHeader browser) color
                                        W.setNormalAttribute (dirBrowserFooter browser) color
                                        W.setFocusAttribute (dirBrowserFooter browser) color
                                        return ()

handleGainFocus :: DirBrowser -> W.Widget DirBrowserWidgetType -> IO ()
handleGainFocus browser _ = setHeaderFooterColor browser (white `W.on` blue)

handleLoseFocus :: DirBrowser -> W.Widget DirBrowserWidgetType -> IO ()
handleLoseFocus browser _ = setHeaderFooterColor browser (white `W.on` black)

handleBrowserInput :: W.Collection -> TVar ProgramState -> DirBrowser -> DirBrowser -> t
                   -> Key -> [Modifier] -> IO Bool
handleBrowserInput collection state browser otherBrowser _ key modifier =
  case modifier of
   [MMeta] -> case key of KChar 'x' -> do reportBrowserError browser "M-x pressed"
                                          return True
                          _ -> return False

   [MCtrl] -> case key of KChar 'f' -> do toggleWidgetVisible (dirBrowserFilter browser)
                                          W.focus (dirBrowserFilter browser)
                                          return True
                          KChar 's' -> do toggleWidgetVisible (dirBrowserSearch browser)
                                          W.focus (dirBrowserSearch browser)
                                          return True

                          _ -> return False

   _ -> case key of
         KBS -> do path <- getDirBrowserPath browser
                   setDirBrowserPath browser (joinPath (init (splitPath path)))
                   return True
         KChar 'e' -> do f <- currentSelection browser
                         case f of
                          Just x -> editFile browser x >> return True
                          Nothing -> return True
         KChar '?' -> do newHelpDialog collection state
                         return True
         KChar '\t' -> do W.focus (dirBrowserWidget otherBrowser)
                          return True
         _ -> return False

runCommandOnFile :: String -> Maybe String -> FilePath -> IO (ExitCode, String, String)
runCommandOnFile c p f = case p of Nothing -> readProcessWithExitCode c [f] []
                                   Just p' -> readProcessWithExitCode c [p', f] []

runCommandOnFileInBrowser :: String -> Maybe String -> DirBrowser -> FilePath -> IO ()
runCommandOnFileInBrowser c p b f = do (_, _, errorMsg) <- runCommandOnFile c p f
                                       reportBrowserError b $ T.pack errorMsg
                                       return ()

openFile :: DirBrowser -> FilePath -> IO ()
openFile = runCommandOnFileInBrowser "open" Nothing

editFile :: DirBrowser -> FilePath -> IO ()
editFile = runCommandOnFileInBrowser "open" (Just "-e")

renderCurrentLayout :: TVar ProgramState -> IO Bool
renderCurrentLayout state = do l <- atomically $ readTVar state
                               currentLayout l
                               return True

handleOnPipeKeyPressed :: TVar ProgramState -> IO () -> IO () -> t -> Key -> t1 -> IO Bool
handleOnPipeKeyPressed state horizontalLayout verticalLayout _ key _ =
  if key == KChar '|'
  then do
    s <- atomically $ readTVar state
    let orientation = currentOrientation s
      in case orientation of
          Horizontal -> do atomically $ modifyTVar state (swapOrientation verticalLayout)
                           renderCurrentLayout state
          Vertical  -> do atomically $ modifyTVar state (swapOrientation horizontalLayout)
                          renderCurrentLayout state
  else return False
