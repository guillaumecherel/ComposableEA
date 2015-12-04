{-# LANGUAGE RankNTypes #-}

module ComposableEA where

import Data.Ord
import Data.List
import qualified Data.Map.Strict as Map
import System.Random
import Control.Monad
import qualified Data.Vector as V
import Control.Lens

---- Types correspondant aux différentes étapes de l'EA ----

type Breeding i m g = [i] -> m [g]

type Expression g i = g -> i 

type Objective m i = [i] -> m [i]


---- Composition functions: Breeding ----

bindB :: (Monad m) => Breeding i m g1 -> ([g1] -> Breeding i m g2) -> Breeding i m g2
bindB b1 b2 = \individuals -> do
    g1s <- b1 individuals
    b2 g1s individuals 

zipB :: (Monad m) => Breeding i m g1 -> Breeding i m g2 -> Breeding i m (g1,g2)
zipB = zipWithB (,)

zipWithB :: (Monad m) => (g1 -> g2 -> g3) -> Breeding i m g1 -> Breeding i m g2 -> Breeding i m g3
zipWithB f b1 b2 individuals = do
    g1s <- b1 individuals
    g2s <- b2 individuals
    return $ zipWith f g1s g2s

productB :: (Monad m) => Breeding i m g1 -> (g1 -> Breeding i m g2) -> Breeding i m g2
productB = productWithB (\_ b -> b)

productWithB :: (Monad m) => (g1 -> g2 -> g3) -> Breeding i m g1 -> (g1 -> Breeding i m g2) -> Breeding i m g3
productWithB f b1 b2 individuals = do
    g1s <- b1 individuals
    nested <- mapM (\g1 -> (fmap (fmap (f g1))) (b2 g1 individuals)) g1s
    return $ join nested


---- Composition functions: Expression ----

bindE :: Expression g p1 -> (p1 -> Expression g p2) -> Expression g p2
bindE e1 e2 = \genome -> e2 (e1 genome) genome

zipE :: Expression g p1 -> Expression g p2 -> Expression g (p1,p2)
zipE = zipWithE (,)

zipWithE :: (p1 -> p2 -> p3) -> Expression g p1 -> Expression g p2 -> Expression g p3
zipWithE f e1 e2 genome = f (e1 genome) (e2 genome)

---- Composition functions: Objective ----

bindO :: (Monad m) => Objective m i -> ([i] -> Objective m i) -> Objective m i
bindO o1 o2 phenotypes = do
    i1s <- o1 phenotypes
    o2 i1s phenotypes

andO :: (Monad m, Eq i) => Objective m i -> Objective m i -> Objective m i
andO o1 o2 phenotypes = do
    selected1s <- o1 phenotypes
    selected2s <- o2 phenotypes
    return $ intersect selected1s selected2s

orO :: (Monad m, Eq i) => Objective m i -> Objective m i -> Objective m i
orO o1 o2 phenotypes = do
    selected1s <- o1 phenotypes
    selected2s <- o2 phenotypes
    return $ union selected1s selected2s

thenO :: (Monad m) => Objective m i -> Objective m i -> Objective m i
thenO o1 o2 phenotypes = do
    selected1s <- o1 phenotypes
    o2 selected1s


---- Functions for running the EA ----

stepEA :: (Monad m) => ([i] -> m ()) -> Breeding i m g -> Expression g i -> Objective m i -> ReplacementStrategy i -> [i] -> m [i]
stepEA preStep breeding expression objective replacementStrategy pop = do
    preStep pop
    breeded <- breeding pop
    let expressed = map expression breeded 
    objective (replacementStrategy pop expressed)

runEAUntil :: ( Monad m ) => ([i] -> m Bool) -> ( [i] -> m [i] ) -> [i] -> m [i]
runEAUntil stopCondition stepFunction pop = do
    stop <- stopCondition pop
    if stop
    then return pop
    else do
        newpop <- stepFunction pop
        runEAUntil stopCondition stepFunction newpop

runEA :: (Monad m) => ( [i] -> m [i] ) -> [i] -> m [i] 
runEA = runEAUntil (\_ -> return False)


---- Common stop conditions ----

anyReaches :: (Monad m) => (i -> Bool) -> [i] -> m Bool
anyReaches goalReached pop = return (any goalReached pop)

---- Common pre-step functions ----

writepop :: (Monad m, Show i) => [i] -> m (IO ())
writepop pop = return $ putStrLn $ "Pop " ++ show pop

---- Replacement strategies ----

type ReplacementStrategy i = [i] -> [i] -> [i]

muPlusLambda :: ReplacementStrategy i
muPlusLambda parents offsprings = parents ++ offsprings

muCommaLambda :: ReplacementStrategy i
muCommaLambda parents offsprings = offsprings


---- Breedings ----

-- Generic breeding functions

breedAs :: (i -> i1) -> Breeding i1 m g -> Breeding i m g
breedAs itoi1 b = b . fmap itoi1

mapB :: (Monad m) => (i -> m g) -> Breeding i m g
mapB mutation individuals = mapM mutation individuals

byNicheB :: (Monad m, Ord n) => (i -> n) -> Breeding i m g -> Breeding i m g
byNicheB niche b individuals = 
    let indivsByNiche = Map.elems $ Map.fromListWith (++) [(niche i, [i]) | i <- individuals]
    in fmap join (mapM b indivsByNiche)

-- Breeding building blocks

-- Mating

--tournament :: (Monad m, Ord o) => (i -> o) -> [i] -> m [(o, i)]

--mqlskdjf  :: [(o, i)] -> m i

randomGroup :: (Monad m, RandomGen g) => m g -> Int -> Int -> [i] -> m [[i]]
randomGroup useRandomGen groupcount groupsize [] = return []
randomGroup useRandomGen groupcount groupsize individuals = do
    gs <- replicateM groupcount useRandomGen
    let individualsV = V.fromList individuals
    let indivscount = length individualsV
    return $ map (\g -> map (individualsV V.!) (take groupsize (randomRs (0,indivscount - 1) g))) gs

randomPair :: (Monad m, RandomGen g) => m g -> Int -> [i] -> m [(i,i)]
randomPair useRandomGen paircount [] = return []
randomPair useRandomGen paircount individuals = do
    gs <- replicateM paircount useRandomGen
    let individualsV = V.fromList individuals
    let indivscount = length individualsV
    return $ map (\g -> let (g1,g2) = split g 
                        in (individualsV V.! fst (randomR (0, indivscount - 1) g1), 
                            individualsV V.! fst (randomR (0, indivscount - 1) g2))) gs


-- Crossover 

oneOfCrossover :: (Monad m, RandomGen g) => m g -> [[i]] -> m [i]
oneOfCrossover useRandomGen parents = do
    gs <- mapM (\_ -> useRandomGen) parents
    return $ map (\(individuals,g) -> individuals !! (fst (randomR (0, (length individuals) - 1) g))) (zip parents gs)


-- Mutation

stepMutation :: (Monad m, RandomGen g, Random i, Num i) => m g -> i -> i -> m i
stepMutation useRandomGen maxStepSize individual = do
    g <- useRandomGen
    let step = fst (randomR (-maxStepSize,maxStepSize) g)
    return (individual + step)

probabilisticMutation :: (Monad m, RandomGen g) => m g -> Double -> (i -> m i) -> i -> m i
probabilisticMutation useRandomGen mutateProba mutation individual = do
    g <- useRandomGen
    let (r, _) = randomR (0,1) g
    if r < mutateProba
    then mutation individual
    else return individual

-- Stochasticity

withRandomGen :: (Monad m, RandomGen r) => m r -> Breeding i m g -> Breeding i m (r,g)
withRandomGen useRandomGen b individuals = do
    breeded <- b individuals
    rgs <- replicateM (length breeded) useRandomGen
    return $ zip rgs breeded


---- Expressions ----

-- Generic functions

expressAs :: (g -> g1) -> Expression g1 p1 -> Expression g p1
expressAs gtog1 e = e . gtog1

withGenomeE :: Expression g p -> Expression g (p,g)
withGenomeE express = \g -> (express g, g)

expressWith :: (g -> p) -> Expression g p
expressWith f genome = f genome

---- Objectives ----

-- Objective building blocks

minimise :: (Monad m, Ord p1) => (p -> p1) -> Int -> Objective m p
minimise on keep = return . take keep . sortBy (comparing on)

maximise :: (Monad m, Ord p1) => (p -> p1) -> Int -> Objective m p
maximise on keep = return . take keep . sortBy (flip $ comparing on)

pareto :: (Monad m) => Objective m p
pareto = undefined

randomSelect :: (Monad m, RandomGen g) => m g -> Int -> Objective m i
randomSelect useRandomGen n = \individuals -> do
    g <- useRandomGen 
    return $ pickElems n g individuals

pickElems :: (RandomGen g) => Int -> g -> [a] -> [a]
pickElems 0 _ _ = []
pickElems n g l = 
    let (i, g1) = randomR (0, length l - 1) g
        (prefix, elem:suffix) = splitAt i l
    in elem : pickElems (n - 1) g1 (prefix ++ suffix)

byNicheO :: (Monad m, Ord n) => (i -> n) -> Objective m i -> Objective m i
byNicheO niche o = \individuals -> 
    let indivsByNiche = Map.elems $ Map.fromListWith (++) [(niche i, [i]) | i <- individuals]
    in fmap join (mapM o indivsByNiche)


