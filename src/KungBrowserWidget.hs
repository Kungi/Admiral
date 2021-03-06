{-# LANGUAGE OverloadedStrings #-}
-- |This module provides a directory browser interface widget.
module KungBrowserWidget
    (DirBrowser(..)
    , BrowserSkin(..)
    , DirBrowserWidgetType
    , newDirBrowser
    , withAnnotations
    , currentSelection
    , setDirBrowserPath
    , getDirBrowserPath
    , defaultBrowserSkin
    , onBrowseAccept
    , onBrowseCancel
    , onBrowserPathChange
    , reportBrowserError
    , refreshBrowser
    , filterBrowser
    , searchBrowser
    , noStyle
    , toggleWidgetVisible
    , setSearchPosition
    , resetSearchPosition
    , searchCurrentPosition
    , newSearch
    , incSearchPosition
    , decSearchPosition
    , copyFromBrowserToBrowser
    )
where

import Control.Concurrent.STM.TVar
import Control.Monad.STM
import Data.IORef
import Data.Foldable
import qualified Data.Map as Map
import qualified Control.Exception as E
import Control.Monad
import qualified Data.Text as T
import qualified Data.List as L
import Graphics.Vty
import Graphics.Vty.Widgets.Core
import Graphics.Vty.Widgets.List
import Graphics.Vty.Widgets.Text
import Graphics.Vty.Widgets.Box
import Graphics.Vty.Widgets.Fills
import Graphics.Vty.Widgets.Util
import Graphics.Vty.Widgets.Events
import Graphics.Vty.Widgets.Edit
import System.Directory
import System.FilePath
import System.Posix.Files
import System.IO.Error
import CopyFiles

type Header = Box (Box FormattedText FormattedText) HFill
type Footer = Box
              (Box (Box FormattedText FormattedText) HFill)
              FormattedText
type BrowserList = List String (Box FormattedText FormattedText)
type Filter = Box FormattedText Edit
type Search = Box FormattedText Edit

type DirBrowserWidgetType = Box
                             Header
                             (Box BrowserList Footer)

data DirBrowser = DirBrowser { dirBrowserWidget :: Widget DirBrowserWidgetType
                             , dirBrowserList :: Widget BrowserList
                             , dirBrowserPath :: IORef FilePath
                             , dirBrowserPathDisplay :: Widget FormattedText
                             , dirBrowserHeader :: Widget Header
                             , dirBrowserSelectionMap :: IORef (Map.Map FilePath Int)
                             , dirBrowserFileInfo :: Widget FormattedText
                             , dirBrowserFilter :: Widget Filter
                             , dirBrowserFilterEdit :: Widget Edit
                             , dirBrowserSearch :: Widget Search
                             , dirBrowserSearchEdit :: Widget Edit
                             , dirBrowserSearchCurrentPosition :: TVar Int
                             , dirBrowserFooter :: Widget Footer
                             , dirBrowserSkin :: BrowserSkin
                             , dirBrowserErrorWidget :: Widget FormattedText
                             , dirBrowserChooseHandlers :: Handlers FilePath
                             , dirBrowserCancelHandlers :: Handlers FilePath
                             , dirBrowserPathChangeHandlers :: Handlers FilePath
                             }

-- |The collection of attributes and annotations used to determine the
-- browser's visual appearance.
data BrowserSkin = BrowserSkin { browserHeaderAttr :: Attr
                               -- ^Used for the header and footer
                               -- areas of the interface.
                               , browserUnfocusedSelAttr :: Attr
                               -- ^Used for the selected entry when
                               -- the browser does not have focus.
                               , browserErrorAttr :: Attr
                               -- ^Used for the browser's
                               -- error-reporting area.
                               , browserDirAttr :: Attr
                               -- ^Used for directory entries.
                               , browserLinkAttr :: Attr
                               -- ^Used for symbolic link entries.
                               , browserBlockDevAttr :: Attr
                               -- ^Used for block device entries.
                               , browserNamedPipeAttr :: Attr
                               -- ^Used for named pipe entries.
                               , browserCharDevAttr :: Attr
                               -- ^Used for device entries.
                               , browserSockAttr :: Attr
                               -- ^Used for socket entries.
                               , browserShowHeader :: Bool
                               -- ^Whether the browser header should
                               -- be shown.
                               , browserShowFooter :: Bool
                               -- ^Whether the browser footer should
                               -- be shown.
                               , browserCustomAnnotations :: [ (FilePath -> FileStatus -> Bool
                                                               , FilePath -> FileStatus -> IO T.Text
                                                               , Attr)
                                                             ]
                               -- ^File annotations.
                               , browserIncludeEntry :: FilePath -> FileStatus -> Bool
                               -- ^The predicate which determines
                               -- which entries get listed in the
                               -- browser.
                               }

-- |The default browser skin with (hopefully) sane attribute defaults.
defaultBrowserSkin :: BrowserSkin
defaultBrowserSkin = BrowserSkin { browserHeaderAttr = white `on` blue
                                 , browserUnfocusedSelAttr = bgColor black
                                 , browserErrorAttr = white `on` red
                                 , browserDirAttr = fgColor green
                                 , browserLinkAttr = fgColor cyan
                                 , browserBlockDevAttr = fgColor red
                                 , browserNamedPipeAttr = fgColor yellow
                                 , browserCharDevAttr = fgColor red
                                 , browserSockAttr = fgColor magenta
                                 , browserShowHeader = True
                                 , browserShowFooter = True
                                 , browserCustomAnnotations = []
                                 , browserIncludeEntry = (const . const) True
                                 }

-- |Apply annotations to a browser skin.
withAnnotations :: BrowserSkin
                -> [(FilePath -> FileStatus -> Bool, FilePath -> FileStatus -> IO T.Text, Attr)]
                -> BrowserSkin
withAnnotations sk as = sk { browserCustomAnnotations = browserCustomAnnotations sk ++ as }

newHeader :: BrowserSkin -> IO (Widget Header, Widget FormattedText)
newHeader bSkin = do pathWidget <- plainText T.empty
                     header <- (plainText " Path: "
                                <++> return pathWidget <++> hFill ' ' 1)
                               >>= withNormalAttribute (browserHeaderAttr bSkin)
                     return (header, pathWidget)

newFooter :: BrowserSkin -> IO (Widget Footer, Widget FormattedText, Widget FormattedText)
newFooter bSkin = do errorText <- plainText T.empty
                                  >>= withNormalAttribute (browserErrorAttr bSkin)
                     fileInfo <- plainText T.empty
                     footer <- (plainText " "
                                <++> return fileInfo <++> hFill ' ' 1 <++> return errorText)
                               >>= withNormalAttribute (browserHeaderAttr bSkin)
                     return (footer, fileInfo, errorText)

noStyle :: Style
noStyle = 0x00

newFilter :: IO (Widget (Box FormattedText Edit), Widget Edit)
newFilter = do e <- editWidget
               setNormalAttribute e $ style noStyle
               setFocusAttribute e $ style noStyle
               setNormalAttribute e $ white `on` black
               setFocusAttribute e $ white `on` black
               filterWidget <- (plainText "Filter: ") <++> return e
               setVisible filterWidget False
               return (filterWidget, e)

newSearchWidget :: IO (Widget (Box FormattedText Edit), Widget Edit, TVar Int)
newSearchWidget = do e <- editWidget
                     setNormalAttribute e $ style noStyle
                     setFocusAttribute e $ style noStyle
                     setNormalAttribute e $ white `on` black
                     setFocusAttribute e $ white `on` black
                     searchWidget <- (plainText "Search: ") <++> return e
                     setVisible searchWidget False
                     c <- newTVarIO 0
                     return (searchWidget, e, c)

-- |Create a directory browser widget with the specified skin.
-- Returns the browser itself along with its focus group.
newDirBrowser :: BrowserSkin -> IO (Widget Filter, Widget Search, DirBrowser, Widget FocusGroup)
newDirBrowser bSkin = do
  path <- getCurrentDirectory

  (header, pathWidget) <- newHeader bSkin
  (footer, fileInfo, errorText) <- newFooter bSkin
  (filterWidget, filterEditWidget) <- newFilter
  (searchWidget, searchEditWidget, currentSearchPosition) <- newSearchWidget

  l <- newList 1
  setSelectedUnfocusedAttr l $ Just (browserUnfocusedSelAttr bSkin)

  dirBrowser <- ((return header) <--> (return l <--> return footer))

  r <- newIORef ""
  r2 <- newIORef Map.empty

  hs <- newHandlers
  chs <- newHandlers
  pchs <- newHandlers

  let b = DirBrowser { dirBrowserWidget = dirBrowser
                     , dirBrowserList = l
                     , dirBrowserPath = r
                     , dirBrowserPathDisplay = pathWidget
                     , dirBrowserHeader = header
                     , dirBrowserFilter = filterWidget
                     , dirBrowserFilterEdit = filterEditWidget
                     , dirBrowserSearch = searchWidget
                     , dirBrowserSearchEdit = searchEditWidget
                     , dirBrowserSearchCurrentPosition = currentSearchPosition
                     , dirBrowserSelectionMap = r2
                     , dirBrowserFileInfo = fileInfo
                     , dirBrowserFooter = footer
                     , dirBrowserSkin = bSkin
                     , dirBrowserChooseHandlers = hs
                     , dirBrowserCancelHandlers = chs
                     , dirBrowserPathChangeHandlers = pchs
                     , dirBrowserErrorWidget = errorText
                     }

  l `onKeyPressed` handleBrowserKey b
  l `onSelectionChange` (\e -> clearError b >> handleSelectionChange b e)
  b `onBrowserPathChange` (setText (dirBrowserPathDisplay b) . T.pack)

  setVisible header $ browserShowHeader bSkin
  setVisible footer $ browserShowFooter bSkin

  fg <- newFocusGroup
  _ <- addToFocusGroup fg dirBrowser
  _ <- addToFocusGroup fg filterWidget
  _ <- addToFocusGroup fg searchWidget

  setDirBrowserPath b path
  return (filterWidget, searchWidget, b, fg)

-- |Report an error in the browser's error-reporting area.  Useful for
-- reporting application-specific errors with the user's file
-- selection.
reportBrowserError :: DirBrowser -> T.Text -> IO ()
reportBrowserError b msg = setText (dirBrowserErrorWidget b) $
                           T.concat [ "Error: "
                                    , msg
                                    ]

clearError :: DirBrowser -> IO ()
clearError b = setText (dirBrowserErrorWidget b) T.empty

-- |Register handlers to be invoked when the user makes a selection.
onBrowseAccept :: DirBrowser -> (FilePath -> IO ()) -> IO ()
onBrowseAccept = addHandler (return . dirBrowserChooseHandlers)

-- |Register handlers to be invoked when the user cancels browsing.
onBrowseCancel :: DirBrowser -> (FilePath -> IO ()) -> IO ()
onBrowseCancel = addHandler (return . dirBrowserCancelHandlers)

-- |Register handlers to be invoked when the browser's path changes.
onBrowserPathChange :: DirBrowser -> (FilePath -> IO ()) -> IO ()
onBrowserPathChange = addHandler (return . dirBrowserPathChangeHandlers)

cancelBrowse :: DirBrowser -> IO ()
cancelBrowse b = fireEvent b (return . dirBrowserCancelHandlers) =<< getDirBrowserPath b

currentSelection :: DirBrowser -> IO (Maybe FilePath)
currentSelection b = do p <- getDirBrowserPath b
                        mCur <- getSelected (dirBrowserList b)
                        case mCur of
                         Nothing -> return Nothing
                         Just (_, (e, _)) -> return $ Just (p </> e)

chooseCurrentEntry :: DirBrowser -> IO ()
chooseCurrentEntry b = do
  p <- getDirBrowserPath b
  mCur <- getSelected (dirBrowserList b)
  case mCur of
    Nothing -> return ()
    Just (_, (e, _)) -> fireEvent b (return . dirBrowserChooseHandlers) (p </> e)

handleSelectionChange :: DirBrowser -> SelectionEvent String b -> IO ()
handleSelectionChange b ev = case ev of
                              SelectionOff -> setText (dirBrowserFileInfo b) "-"
                              SelectionOn _ path _ -> setText (dirBrowserFileInfo b) =<<
                                                      getFileInfo b path

getFileInfo :: DirBrowser -> FilePath -> IO T.Text
getFileInfo b path = do
  cur <- getDirBrowserPath b
  let newPath = cur </> path
  st <- getSymbolicLinkStatus newPath
  (_, mkAnn) <- fileAnnotation (dirBrowserSkin b) st cur path
  ann <- mkAnn
  return $ T.concat [ T.pack (path ++ ": ")
                    , ann
                    ]

builtInAnnotations :: FilePath -> BrowserSkin -> [(FilePath -> FileStatus -> Bool, FilePath -> FileStatus -> IO T.Text, Attr)]
builtInAnnotations cur sk =
    [ (\_ s -> isRegularFile s
      , \_ s -> return $ T.pack $ "regular file, " ++
                show (fileSize s) ++ " bytes"
      , defAttr)
    , (\_ s -> isSymbolicLink s,
       \p stat -> do
          linkDest <- if not $ isSymbolicLink stat
                      then return ""
                      else do
                        linkPath <- readSymbolicLink p
                        canonicalizePath $ cur </> linkPath
          return $ T.pack $ "symbolic link to " ++ linkDest
      , browserLinkAttr sk)
    , (\_ s -> isDirectory s, \_ _ -> return "directory", browserDirAttr sk)
    , (\_ s -> isBlockDevice s, \_ _ -> return "block device", browserBlockDevAttr sk)
    , (\_ s -> isNamedPipe s, \_ _ -> return "named pipe", browserNamedPipeAttr sk)
    , (\_ s -> isCharacterDevice s, \_ _ -> return "character device", browserCharDevAttr sk)
    , (\_ s -> isSocket s, \_ _ -> return "socket", browserSockAttr sk)
    ]

fileAnnotation :: BrowserSkin -> FileStatus -> FilePath -> FilePath -> IO (Attr, IO T.Text)
fileAnnotation sk st cur shortPath = do
  let fullPath = cur </> shortPath

      annotation = getAnnotation' fullPath st $ browserCustomAnnotations sk ++
                   builtInAnnotations cur sk

      getAnnotation' _ _ [] = (defAttr, return T.empty)
      getAnnotation' pth stat ((f,mkAnn,a):rest) =
          if f pth stat
          then (a, mkAnn pth stat)
          else getAnnotation' pth stat rest

  return annotation

handleBrowserKey :: DirBrowser -> Widget (List a b) -> Key -> [Modifier] -> IO Bool
handleBrowserKey b _ KEnter [] = descend b True >> return True
handleBrowserKey b _ (KChar ' ') [] = toggleHeaderAndFooter b >> return True
handleBrowserKey b _ KRight [] = descend b False >> return True
handleBrowserKey b _ KLeft [] = ascend b >> return True
handleBrowserKey b _ KEsc [] = cancelBrowse b >> return True
handleBrowserKey b _ (KChar 'q') [] = cancelBrowse b >> return True
handleBrowserKey b _ (KChar 'r') [] = refreshBrowser b >> return True
handleBrowserKey _ _ _ _ = return False

-- |Refresh the browser by reloading and displaying the contents of
-- the browser's current path.
refreshBrowser :: DirBrowser -> IO ()
refreshBrowser b = setDirBrowserPath b =<< getDirBrowserPath b

-- |Set the browser's current path.
setDirBrowserPath :: DirBrowser -> FilePath -> IO ()
setDirBrowserPath b path = do
  cPath <- canonicalizePath path

  -- If for some reason we can't load the directory, report an error
  -- and don't change the browser state.
  (res, entries) <- (do
                      entries <- getDirectoryContents cPath
                      return (True, entries))
                     `E.catch` \e -> do
                             reportBrowserError b (T.pack $ ioeGetErrorString e)
                             return (False, [])

  when res $ do
    -- If something is currently selected, store that in the selection
    -- map before changing the path.
    cur <- getDirBrowserPath b
    mCur <- getSelected (dirBrowserList b)
    case mCur of
      Nothing -> return ()
      Just (i, _) -> storeSelection b cur i

    clearList (dirBrowserList b)
    modifyIORef (dirBrowserPath b) $ const cPath

    load b cPath entries

    sel <- getSelection b path
    Data.Foldable.forM_ sel (scrollBy (dirBrowserList b))

    fireEvent b (return . dirBrowserPathChangeHandlers) cPath

-- |Get the browser's current path.
getDirBrowserPath :: DirBrowser -> IO FilePath
getDirBrowserPath = readIORef . dirBrowserPath

storeSelection :: DirBrowser -> FilePath -> Int -> IO ()
storeSelection b path i =
    modifyIORef (dirBrowserSelectionMap b) $ \m -> Map.insert path i m

getSelection :: DirBrowser -> FilePath -> IO (Maybe Int)
getSelection b path = do
  st <- readIORef (dirBrowserSelectionMap b)
  return $ Map.lookup path st

load :: DirBrowser -> FilePath -> [FilePath] -> IO ()
load b cur entries =
    forM_ entries $ \entry -> do
      let fullPath = cur </> entry
      f <- getSymbolicLinkStatus fullPath
      when (browserIncludeEntry (dirBrowserSkin b) fullPath f) $
           do
             (attr, _) <- fileAnnotation (dirBrowserSkin b) f cur entry
             w <- plainText " " <++> plainText (T.pack entry)
             addToList (dirBrowserList b) entry w
             ch <- getSecondChild w
             setNormalAttribute ch attr

descend :: DirBrowser -> Bool -> IO ()
descend b shouldSelect = do
  base <- getDirBrowserPath b
  mCur <- getSelected (dirBrowserList b)
  case mCur of
    Nothing -> return ()
    Just (_, (p, _)) -> do
              let newPath = base </> p
              e <- doesDirectoryExist newPath
              if e then
                do
                  cPath <- canonicalizePath newPath
                  cur <- getDirBrowserPath b
                  when (cur /= cPath) $
                    if takeDirectory cur == cPath then
                      ascend b
                    else setDirBrowserPath b cPath

                else when shouldSelect $ chooseCurrentEntry b

ascend :: DirBrowser -> IO ()
ascend b = do
  cur <- getDirBrowserPath b
  let newPath = takeDirectory cur
  when (newPath /= cur) $
       setDirBrowserPath b newPath

getEntries :: DirBrowser -> FilePath -> IO (Bool, [FilePath])
getEntries b curPath = do entries <- getDirectoryContents curPath
                          return (True, entries)
                       `E.catch` \e -> do
                            reportBrowserError b (T.pack $ ioeGetErrorString e)
                            return (False, [])

filterBrowser :: DirBrowser -> T.Text -> IO ()
filterBrowser b s = do clearList (dirBrowserList b)
                       curPath <- getDirBrowserPath b

                       (res, entries) <- getEntries b curPath
                       when res $
                         load b curPath $ filter ((T.unpack s) `L.isInfixOf`) entries

resetSearchPosition :: DirBrowser -> IO ()
resetSearchPosition b = do setSearchPosition b 0

setSearchPosition :: DirBrowser -> Int -> IO ()
setSearchPosition b p = do atomically $ modifyTVar (dirBrowserSearchCurrentPosition b) (const p)
                           return ()

getSearchPosition :: DirBrowser -> IO Int
getSearchPosition b = do atomically $ readTVar (dirBrowserSearchCurrentPosition b)

incSearchPosition :: DirBrowser -> IO ()
incSearchPosition b = do a <- getSearchPosition b
                         setSearchPosition  b (a + 1)
                         return ()

decSearchPosition :: DirBrowser -> IO ()
decSearchPosition b = do a <- getSearchPosition b
                         setSearchPosition  b (a - 1)
                         return ()

newSearch :: DirBrowser -> T.Text -> IO ()
newSearch b s = do resetSearchPosition b
                   searchCurrentPosition b s

searchCurrentPosition :: DirBrowser -> T.Text -> IO ()
searchCurrentPosition b s = do currentSearchPosition <- getSearchPosition b
                               searchBrowser b s currentSearchPosition

searchBrowser :: DirBrowser -> T.Text -> Int -> IO ()
searchBrowser b s index = do pos <- listFindAllBy (L.isPrefixOf (T.unpack s)) (dirBrowserList b)
                             case pos of
                              (_:_) -> do setSelected (dirBrowserList b) (pos !! (index `mod` (length pos)))
                              [] -> do return ()

toggleWidgetVisible :: Widget a -> IO ()
toggleWidgetVisible w = do v <- getVisible w
                           setVisible w $ not v

toggleHeaderAndFooter :: DirBrowser -> IO ()
toggleHeaderAndFooter b = do toggleWidgetVisible $ dirBrowserHeader b
                             toggleWidgetVisible $ dirBrowserFooter b


-- Only copies directories at the moment
copyFromBrowserToBrowser :: DirBrowser -> DirBrowser -> IO ()
copyFromBrowserToBrowser from to = do c <- currentSelection from
                                      case c of
                                       Just src -> do
                                         isDirectory <- doesDirectoryExist src
                                         if isDirectory
                                            then do dest <- (getDirBrowserPath to)
                                                    copyTree' src $ dest </> last (splitDirectories src)
                                                    -- putStrLn "Directory"
                                                    -- putStrLn $ last (splitDirectories src)
                                                    -- putStrLn $ "Copying " ++ show src ++ " to "
                                                    --   ++ show (dest </> last (splitDirectories src))
                                                    return ()
                                            else do dest <- (getDirBrowserPath to)
                                                    -- putStrLn $ "Copying " ++ show src ++ " to " ++ show dest
                                                    copyFile src $ dest </> last (splitPath src)
                                                    return ()
                                       Nothing -> return ()
