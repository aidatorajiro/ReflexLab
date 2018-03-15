{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module LibReflex where

import qualified Data.Text as T
import qualified Data.Map as M
import Data.Text.Encoding (decodeUtf8)
import Data.Tree
import LibTree (treeAt)
import Reflex.Dom
import Control.Monad (replicateM)
import Data.Maybe (isNothing, fromJust)
import Safe (atMay)
import Data.FileEmbed (embedFile)

makeStyle :: MonadWidget t m => m ()
makeStyle = el "style" $ text $ decodeUtf8 $(embedFile "assets/css/LibReflex.css")

elId :: MonadWidget t m => T.Text -> T.Text -> m a -> m a
elId tagName id func = elAttr tagName (M.fromList [("id", id)]) func

-- elClass' :: MonadWidget t m => T.Text -> T.Text -> m a -> m (El t, a)
-- elClass' tagName className func = elAttr' tagName (M.fromList [("class", className)]) func

div_attrsDyn :: MonadWidget t m => Dynamic t (M.Map T.Text T.Text) -> m (El t, Event t ())
div_attrsDyn attrsDyn = do
    (elm, _) <- elDynAttr' "div" attrsDyn (return ())
    return (elm, domEvent Click elm)

generate_button_style :: Bool -> M.Map T.Text T.Text
generate_button_style bool = M.fromList [("class", (if bool == True then "button enabled" else "button"))]

generate_button :: MonadWidget t m => Dynamic t Bool -> m ( El t, Event t () )
generate_button = div_attrsDyn . fmap (\b -> generate_button_style b)

btn :: MonadWidget t m => m (Dynamic t Bool)
btn = mdo
    bool <- toggle False events
    (btn_elem, events) <- generate_button bool
    return bool

-- | generate selective list from a function that transforms user's click event to a Dynamic Int value. returns index.
generateSelectList :: MonadWidget t m => ([Event t Int] -> m (Dynamic t Int)) -> [T.Text] -> m ( Dynamic t Int )
generateSelectList events_arr_func arr =
    return =<< elClass "div" "selectList" $ mdo
        index <- events_arr_func events_arr
        events_arr <- mapM (\x -> do
            (elm, _) <- elClass' "div" "item" $ do
                generate_button $ fmap (==x) index
                text $ arr !! x
            return $ tag (constant x) $ domEvent Click elm) $ take (length arr) [0..]
        return index

-- | generate selective list from a function that transforms user's click event to a Dynamic Int value. returns value.
generateSelectListValue :: MonadWidget t m => ([Event t Int] -> m (Dynamic t Int)) -> [T.Text] -> m ( Dynamic t (Maybe T.Text) )
generateSelectListValue events_arr_func arr = fmap (atMay arr) <$> generateSelectList events_arr_func arr

-- | generate selective list from a list of text. returns index.
selectList :: MonadWidget t m => [T.Text] -> m ( Dynamic t Int )
selectList      = generateSelectList       (\x -> foldDyn (\e p -> e) (-1) $ leftmost x)

-- | generate selective list from a list of text. returns value.
selectListValue :: MonadWidget t m => [T.Text] -> m ( Dynamic t (Maybe T.Text) )
selectListValue = generateSelectListValue (\x -> foldDyn (\e p -> e) (-1) $ leftmost x)

-- | generate selective list from a list of text. returns index.
selectListTogglable :: MonadWidget t m => [T.Text] -> m ( Dynamic t Int )
selectListTogglable      = generateSelectList       (\x -> foldDyn (\e p -> if e == p then -1 else e) (-1) $ leftmost x)

-- | generate selective list from a list of text. returns value.
selectListTogglableValue :: MonadWidget t m => [T.Text] -> m ( Dynamic t (Maybe T.Text) )
selectListTogglableValue = generateSelectListValue (\x -> foldDyn (\e p -> if e == p then -1 else e) (-1) $ leftmost x)

-- | draw a tree
drawTree :: MonadWidget t m => Tree T.Text -> m ()
drawTree tree = elClass "div" "tree" $ drawTreeMain tree []

-- # of clicks                     : 0 1 2 3 4 5 6 7 8 9 ...
-- whether the button is enabled   : F T F T F T F T F T ...
-- whether child items exists      : F T T T T T T T T T ...
-- whether child items are visible : F T F T F T F T F T ...
drawTreeMain :: MonadWidget t m => Tree T.Text -> [Int] -> m ()
drawTreeMain tree pos = mdo
    let this        = treeAt tree pos
        this_label  = rootLabel this
        this_forest = subForest this

    (elm, _) <- elClass' "div" "label" $ do
        generate_button bool_button
        text this_label

    bool_button <- toggle False $ domEvent Click elm                 -- whether the button is enabled / whether child items are visible
    bool_childs <- foldDyn (\x y -> True) False $ domEvent Click elm -- whether child items exists

    let child_empty_list = M.fromList $ map (,()) $ take (length this_forest) [0..]
        childs  = fmap (\b ->                       if b then child_empty_list else M.empty      ) bool_childs
        attrDyn = fmap (\b -> M.singleton "class" $ if b then "childs"         else "childs none") bool_button

    elDynAttr "div" attrDyn $ do
        listWithKey childs $ \k v ->
            let child        = treeAt tree $ pos ++ [k]
                child_label  = rootLabel child
                child_forest = subForest child
             in if child_forest == [] then elClass "div" "label" $ text child_label else drawTreeMain tree $ pos ++ [k]

    return ()

-- | Returns a random value of given range and given init gen as a Dunamic
randomRDyn :: (Random a, RandomGen g, MonadWidget t m) => (a, a) -> g -> Event t b -> m (Dynamic t a, Dynamic t g)
randomRDyn range initGen ev = splitDynPure <$> foldDyn (\_ (_, g) -> randomR range g) (randomR range initGen) ev

type Rule = (Double, Double, Double, Double, Double, Double, Double, Double)

-- | Probabilistic Cellular Automata
pca :: MonadWidget t m => Int -> Rule -> m ()
pca seed (a, b, c, d, e, f, g, h) = do
    inputs <- replicateM 5 btn
    now <- liftIO getCurrentTime
    tickev <- tickLossy 0.01 now
    random <- randomRDyn (0, 1)

-- | <br> tag
br :: MonadWidget t m => m ()
br = el "br" $ do
    return ()