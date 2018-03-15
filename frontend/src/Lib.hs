{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( startApp ) where

import qualified LibReflex (makeStyle)
import LibReflex hiding (makeStyle)
import qualified Nested as N

import Control.Monad.Trans ( liftIO )
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Tree (unfoldTree)
import Data.Text.Encoding (decodeUtf8)
import Reflex.Dom
import Data.Maybe (isNothing, fromJust)
import Safe (atMay)
import System.Random (getStdRandom, random)
import Data.FileEmbed (embedFile)

makeStyle :: MonadWidget t m => m ()
makeStyle = el "style" $ text $ decodeUtf8 $(embedFile "assets/css/Lib.css")

selectModule :: MonadWidget t m => m ()
selectModule = elId "div" "selectModule" $ do
    elAttr "h1" (M.singleton ("style", "margin-top: 0;")) $ text "select module #1"

    item <- selectListValue ["aaaaa", "bbbbb", "aiueo", "kakikukeko"]

    dynText $ T.pack <$> show <$> item

    br

    txt <- fmap (\(x:y:_) -> if x == y then "" else "=== VALUE CHANGED ===") <$> foldDyn (:) [Nothing, Nothing] (updated item)

    dynText txt

    return ()

selectModuleTogglable :: MonadWidget t m => m ()
selectModuleTogglable = elId "div" "selectModuleTogglable" $ do
    elAttr "h1" (M.singleton ("style", "margin-top: 0;")) $ text "select module #2"

    item <- selectListTogglableValue ["aaaaa", "bbbbb", "aiueo", "kakikukeko"]

    dynText $ T.pack <$> show <$> item

    return ()

treeModule :: MonadWidget t m => m ()
treeModule = elId "div" "treeModule" $ do

    seed <- liftIO (getStdRandom $ random :: IO Int)

    elAttr "h1" (M.singleton ("style", "margin-top: 0;")) $ text "tree module"

    let init_ext = N.Universe seed (N.E init_ext)

    drawTree $ unfoldTree (\(N.E x) -> (T.pack $ N.name x, N.childs x)) $ N.E init_ext

    return ()

-- probabilistic cellular automata
pcaModule :: MonadWidget t m => m ()
pcaModule = elId "div" "pcaModule" $ do

    seed <- liftIO (getStdRandom $ random :: IO Int)

    elAttr "h1" (M.singleton ("style", "margin-top: 0;")) $ text "PCA module"

    pca seed

startApp :: IO ()
startApp = mainWidget $ do
    LibReflex.makeStyle
    makeStyle
    selectModule
    selectModuleTogglable
    treeModule
    pcaModule
    return ()
