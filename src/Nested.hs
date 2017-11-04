{-# LANGUAGE ExistentialQuantification #-}

-- TODO: splitを使おう
-- TODO: this@
-- TODO: treeを返すようにしよう
-- TODO: treeにmapはできないけどfmapはできる！
-- TODO: ツリー生成部分とdataは分ける

module Nested where

import NameDB (german_all)
import System.Random

data AllExistence = forall a. (Existence a) => E a

class Existence a where
    seed   :: a -> Int
    parent :: a -> AllExistence
    childs :: a -> [AllExistence]
    childs x = []
    name   :: a -> String

randomRs' :: (Random a, RandomGen g) => (a, a) -> Int -> g -> ([a], g)
randomRs' range len gen = foldl
                (\(arr, g) _ ->
                    let (rnd, ng) = randomR range g
                     in (arr ++ [rnd], ng))
                ([], gen)
                (take len [0..])

randoms' :: (Random a, RandomGen g) => Int -> g -> ([a], g)
randoms' len gen = foldl
                (\(arr, g) _ ->
                    let (rnd, ng) = random g
                     in (arr ++ [rnd], ng))
                ([], gen)
                (take len [0..])



data Universe = Universe Int AllExistence

instance Existence Universe where
    seed   (Universe s _) = s
    parent (Universe _ p) = p
    childs (Universe seed parent) =
        let this = Universe seed parent
            (n_childs, gen) = randomR (7, 22) $ mkStdGen seed
            (seedInts, _  ) = randoms' n_childs gen
         in map ( \s -> E $ Supercluster s (E this) ) seedInts
    name   (Universe _ _) = "宇宙"



data Supercluster = Supercluster Int AllExistence

instance Existence Supercluster where
    seed   (Supercluster s _) = s
    parent (Supercluster _ p) = p
    childs (Supercluster seed parent) =
        let this = Supercluster seed parent
            (n_childs, gen1) = randomR (3, 14) $ mkStdGen seed
            (seedInts, gen2) = randoms' n_childs gen1
            (gTypes  , _   ) = randoms' n_childs gen2
         in map ( \(s, gt) -> E $ Galaxy s (E this) gt ) $ zip seedInts gTypes
    name   (Supercluster _ _) = "超銀河団"



data Galaxy = Galaxy Int AllExistence GalaxyType

instance Existence Galaxy where
    seed   (Galaxy s _ _) = s
    parent (Galaxy _ p _) = p
    childs (Galaxy seed parent gt) =
        let this = Galaxy seed parent gt
            (n_childs, gen1) = randomR (8, 19) $ mkStdGen seed
            (d100Ints, gen2) = randomRs' (0, 100) n_childs gen1
            (seedInts, _   ) = randoms' n_childs gen2
         in map ( \(d, s) -> case d of
            d | d <= (20 :: Int) -> E $ Blackhole   s (E this)
            d | d <= (80 :: Int) -> E $ SolarSystem s (E this)
            d | otherwise        -> E $ Nebula      s (E this) ) $ zip d100Ints seedInts
    name   (Galaxy _ _ Elliptical   ) = "楕円型銀河"
    name   (Galaxy _ _ Lenticular   ) = "レンズ型銀河"
    name   (Galaxy _ _ Spirals      ) = "渦巻型銀河"
    name   (Galaxy _ _ BarredSpirals) = "棒渦巻型銀河"

data GalaxyType =
      Elliptical
    | Lenticular
    | Spirals
    | BarredSpirals deriving (Enum, Bounded)

instance Random GalaxyType where
    randomR range gen =
        let t = randomR (fromEnum $ fst $ range, fromEnum $ snd $ range) gen
         in (toEnum $ fst t, snd t)
    random gen = 
        let t = randomR (fromEnum (minBound :: GalaxyType), fromEnum (maxBound :: GalaxyType)) gen
         in (toEnum $ fst t, snd t)



data SolarSystem = SolarSystem Int AllExistence

instance Existence SolarSystem where
    seed   (SolarSystem s _) = s
    parent (SolarSystem _ p) = p
    childs (SolarSystem seed parent) = 
        let this = SolarSystem seed parent
            (n_planets  , gen1) = randomR (2, 8) $ mkStdGen seed
            (seedstar   , gen2) = random gen1
            (seedplanets, _   ) = randoms' n_planets gen2
         in ( E $ Star seedstar (E this) ) : map ( \s -> E $ Planet s (E this) ) seedplanets
    name   (SolarSystem _ _) = "星系"



data Nebula = Nebula Int AllExistence

instance Existence Nebula where
    seed   (Nebula s _) = s
    parent (Nebula _ p) = p
    childs (Nebula seed parent) = []
    name   (Nebula _ _) = "星雲"



data Star = Star Int AllExistence

instance Existence Star where
    seed   (Star s _) = s
    parent (Star _ p) = p
    childs (Star seed parent) =
        let this = Star seed parent
            randInts = randoms $ mkStdGen seed
         in [E $ Hydrogen (randInts !! 0) (E this), E $ Helium (randInts !! 1) (E this)]
    name   (Star _ _) = "恒星"



data Planet = Planet Int AllExistence

instance Existence Planet where
    seed   (Planet s _) = s
    parent (Planet _ p) = p
    childs (Planet seed parent) =
        let this = Planet seed parent
            (n_moons , gen1) = randomR (0, 3) $ mkStdGen seed
            (n_conts , gen2) = randomR (1, 6) gen1
            (n_ocean , gen3) = randomR (1, 9) gen2
            (s_moons , gen4) = randoms' n_moons gen3
            (s_conts , gen5) = randoms' n_conts gen4
            (d100Ints, gen6) = randomRs' (0, 100) n_conts gen5
            (s_ocean , gen7) = randoms' n_ocean gen6
            (s_sky   , gen8) = random gen7
            (s_core  , _   ) = random gen8
         in    ( E $ Sky  s_sky  (E this) )
            :  ( E $ Core s_core (E this) )
            :  map ( \s      -> E $ Moon      s (E this)                   ) s_moons
            ++ map ( \(s, d) -> E $ Continent s (E this) (d < (20 :: Int)) ) ( zip s_conts d100Ints )
            ++ map ( \s      -> E $ Ocean     s (E this)                   ) s_ocean
    name   (Planet _ _) = "惑星"



data Continent = Continent Int AllExistence Bool

instance Existence Continent where
    seed   (Continent s _ _) = s
    parent (Continent _ p _) = p
    childs (Continent seed parent isExplored) =
        let this = Continent seed parent
            (nameindex, gen1) = randomR (0, length german_all - 1) $ mkStdGen seed
         in []
    name   (Continent seed parent isExplored) =
        let (nameindex, gen1) = randomR (0, length german_all - 1) $ mkStdGen seed
         in if isExplored then (german_all !! nameindex) ++ "大陸" else "大陸"



data Ocean = Ocean Int AllExistence

instance Existence Ocean where
    seed   (Ocean s _) = s
    parent (Ocean _ p) = p
    childs (Ocean seed parent) =
        let this = Ocean seed parent
         in []
    name   (Ocean _ _) = "海"



data Sky = Sky Int AllExistence

instance Existence Sky where
    seed   (Sky s _) = s
    parent (Sky _ p) = p
    childs (Sky seed parent) =
        let this = Sky seed parent
         in []
    name   (Sky _ _) = "空"



data Core = Core Int AllExistence

instance Existence Core where
    seed   (Core s _) = s
    parent (Core _ p) = p
    childs (Core seed parent) =
        let this = Core seed parent
         in []
    name   (Core _ _) = "核"



data Moon = Moon Int AllExistence

instance Existence Moon where
    seed   (Moon s _) = s
    parent (Moon _ p) = p
    childs (Moon seed parent) =
        let this = Moon seed parent
            (n_rocks, gen1) = randomR (0 :: Int, 11) $ mkStdGen seed
            (n_water, gen2) = randomR (0 :: Int, 4) gen1
            (n_soils, gen3) = randomR (0 :: Int, 4) gen2
            (n_oxygs, gen4) = randomR (0 :: Int, 8) gen3
            (s_rocks, gen5) = randoms' n_rocks gen4
            (s_water, gen6) = randoms' n_rocks gen5
            (s_soils, gen7) = randoms' n_rocks gen6
            (s_oxygs, _   ) = randoms' n_rocks gen7
         in    map ( \s -> E $ Rock   s (E this) ) s_rocks
            ++ map ( \s -> E $ Water  s (E this) ) s_water
            ++ map ( \s -> E $ Soil   s (E this) ) s_soils
            ++ map ( \s -> E $ Oxygen s (E this) ) s_oxygs
    name   (Moon _ _) = "月"



data Rock = Rock Int AllExistence

instance Existence Rock where
    seed   (Rock s _) = s
    parent (Rock _ p) = p
    childs (Rock seed parent) =
        let this = Rock seed parent
         in []
    name   (Rock _ _) = "岩石"



data Soil = Soil Int AllExistence

instance Existence Soil where
    seed   (Soil s _) = s
    parent (Soil _ p) = p
    childs (Soil seed parent) =
        let this = Soil seed parent
         in []
    name   (Soil _ _) = "土"



data Water = Water Int AllExistence

instance Existence Water where
    seed   (Water s _) = s
    parent (Water _ p) = p
    childs (Water seed parent) =
        let this = Water seed parent
            randInts = randoms $ mkStdGen seed
         in [E $ Hydrogen (randInts !! 0) (E this), E $ Oxygen (randInts !! 1) (E this)]
    name   (Water _ _) = "水"



data Hydrogen = Hydrogen Int AllExistence

instance Existence Hydrogen where
    seed   (Hydrogen s _) = s
    parent (Hydrogen _ p) = p
    childs (Hydrogen seed parent) =
        let this = Hydrogen seed parent
            randInts = randoms $ mkStdGen seed
         in [E $ Proton (randInts !! 0) (E this), E $ Electron (randInts !! 1) (E this)]
    name   (Hydrogen _ _) = "水素"



data Oxygen = Oxygen Int AllExistence

instance Existence Oxygen where
    seed   (Oxygen s _) = s
    parent (Oxygen _ p) = p
    childs (Oxygen seed parent) =
        let this = Oxygen seed parent
            randInts = randoms $ mkStdGen seed
         in [E $ Proton (randInts !! 0) (E this), E $ Electron (randInts !! 1) (E this)]
    name   (Oxygen _ _) = "酸素"



data Helium = Helium Int AllExistence

instance Existence Helium where
    seed   (Helium s _) = s
    parent (Helium _ p) = p
    childs (Helium seed parent) =
        let this = Helium seed parent
            randInts = randoms $ mkStdGen seed
         in [E $ Proton (randInts !! 0) (E this), E $ Neutron (randInts !! 1) (E this), E $ Electron (randInts !! 2) (E this)]
    name   (Helium _ _) = "ヘリウム"



data Proton = Proton Int AllExistence

instance Existence Proton where
    seed   (Proton s _) = s
    parent (Proton _ p) = p
    childs (Proton seed parent) =
        let this = Proton seed parent
            randInts = randoms $ mkStdGen seed
         in [E $ UpQuark (randInts !! 0) (E this), E $ UpQuark (randInts !! 1) (E this), E $ DownQuark (randInts !! 2) (E this)]
    name   (Proton _ _) = "陽子"



data Neutron = Neutron Int AllExistence

instance Existence Neutron where
    seed   (Neutron s _) = s
    parent (Neutron _ p) = p
    childs (Neutron seed parent) =
        let this = Neutron seed parent
            randInts = randoms $ mkStdGen seed
         in [E $ DownQuark (randInts !! 0) (E this), E $ DownQuark (randInts !! 1) (E this), E $ UpQuark (randInts !! 2) (E this)]
    name   (Neutron _ _) = "中性子"



data Electron = Electron Int AllExistence

instance Existence Electron where
    seed   (Electron s _) = s
    parent (Electron _ p) = p
    childs (Electron seed parent) =
        let this = Neutron seed parent
            (n_childs, gen) = randomR (4, 16) $ mkStdGen seed
            (seedInts, _  ) = randoms' n_childs gen
         in map ( \s -> E $ Universe s (E this) ) seedInts
    name   (Electron _ _) = "電子"


data UpQuark = UpQuark Int AllExistence

instance Existence UpQuark where
    seed   (UpQuark s _) = s
    parent (UpQuark _ p) = p
    childs (UpQuark seed parent) =
        let this = Neutron seed parent
            (n_childs, gen) = randomR (3, 9) $ mkStdGen seed
            (seedInts, _  ) = randoms' n_childs gen
         in map ( \s -> E $ Universe s (E this) ) seedInts
    name   (UpQuark _ _) = "アップクオーク"



data DownQuark = DownQuark Int AllExistence

instance Existence DownQuark where
    seed   (DownQuark s _) = s
    parent (DownQuark _ p) = p
    childs (DownQuark seed parent) =
        let this = Neutron seed parent
            (n_childs, gen) = randomR (8, 13) $ mkStdGen seed
            (seedInts, _  ) = randoms' n_childs gen
         in map ( \s -> E $ Universe s (E this) ) seedInts
    name   (DownQuark _ _) = "ダウンクオーク"



data Blackhole = Blackhole Int AllExistence

instance Existence Blackhole where
    seed   (Blackhole s _) = s
    parent (Blackhole _ p) = p
    childs (Blackhole seed parent) = 
        let this = Blackhole seed parent
            randInts = randoms $ mkStdGen seed
         in [E $ Whitehole (randInts !! 0) (E this) ]
    name   (Blackhole _ _) = "ブラックホール"



data Whitehole = Whitehole Int AllExistence

instance Existence Whitehole where
    seed   (Whitehole s _) = s
    parent (Whitehole _ p) = p
    childs (Whitehole seed parent) =
        let this = Whitehole seed parent
         in [getParentUniverse $ E this]
    name   (Whitehole _ _) = "ホワイトホール"

getParentUniverse :: AllExistence -> AllExistence
getParentUniverse (E a) = (\(E b) -> case name b of
    "宇宙" -> E b -- TODO: あとで直す Unsafeつかわないとむりかも
    otherwise -> getParentUniverse $ E b) (parent a)