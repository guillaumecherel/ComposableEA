{-# LANGUAGE RankNTypes #-}

module ComposableEA where

import Data.Ord
import Data.List
import qualified Data.Map.Strict as Map
import System.Random
import Control.Monad
import qualified Data.Vector as V
import Control.Lens

-- TODO: enlever les duplicats dans la composition de breedings

---- Types correspondant aux différentes étapes de l'EA ----

type Breeding i m g = [i] -> m [g]

-- Expression: C'est l'étape distribuée en parallèle (équivalent à évaluation dans mgo).
-- Elle ne fait pas intervenir la monad m qui contient l'état de l'évolution
-- car cet état peut avoir besoin d'être synchronisé entre les différents
-- éléments évalués. La mise à jour de l'état devra donc être traité à une
-- étape suivante (objective) si besoin Ceci impose la restriction que les
-- expressions des individus ne peut pas dépendre les unes des autres, mais
-- c'est une restriction nécessaire pour le parallelisme.

type Expression g m i = g -> m i 

type Objective m i = [i] -> m [i]


---- Composition functions: Breeding ----

bindB :: (Monad m) => Breeding i m g1 -> ([g1] -> Breeding i m g2) -> Breeding i m g2
bindB b1 b2 = \individuals -> do
    g1s <- b1 individuals
    b2 g1s individuals 

productB :: (Monad m) => Breeding i m g1 -> (g1 -> Breeding i m g2) -> Breeding i m g2
productB = productWithB (\_ b -> b)

productWithB :: (Monad m) => (g1 -> g2 -> g3) -> Breeding i m g1 -> (g1 -> Breeding i m g2) -> Breeding i m g3
productWithB f b1 b2 individuals = do
    g1s <- b1 individuals
    nested <- mapM (\g1 -> (fmap (fmap (f g1))) (b2 g1 individuals)) g1s
    return $ join nested

zipB :: (Monad m) => Breeding i m g1 -> Breeding i m g2 -> Breeding i m (g1,g2)
zipB = zipWithB (,)

zipWithB :: (Monad m) => (g1 -> g2 -> g3) -> Breeding i m g1 -> Breeding i m g2 -> Breeding i m g3
zipWithB f b1 b2 individuals = do
    g1s <- b1 individuals
    g2s <- b2 individuals
    return $ zipWith f g1s g2s

---- Composition functions: Expression ----

bindE :: (Monad m) => Expression g m p1 -> (p1 -> Expression g m p2) -> Expression g m p2
bindE e1 e2 = \genome -> do
    p1 <- e1 genome
    e2 p1 genome

zipE :: (Monad m) => Expression g m p1 -> Expression g m p2 -> Expression g m (p1,p2)
zipE = zipWithE (,)

zipWithE :: (Monad m) => (p1 -> p2 -> p3) -> Expression g m p1 -> Expression g m p2 -> Expression g m p3
zipWithE f e1 e2 genome = do
    expressed1 <- e1 genome
    expressed2 <- e2 genome
    return (f expressed1 expressed2)

expressWith :: (Monad m) => (g -> p) -> Expression g m p
expressWith f genome = return $ f genome



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

stepEA :: (Monad m) => ([i] -> m ()) -> Breeding i m g -> Expression g m i -> Objective m i -> [i] -> m [i]
stepEA preStep breeding expression objective pop = do
    preStep pop
    breeded <- breeding pop
    expressed <- mapM expression breeded --mapM est l'étape parallelisable
    objective expressed

runEAUntil :: ( Monad m ) => ([i] -> m Bool) -> ( [i] -> m () ) -> Breeding i m g -> Expression g m i -> Objective m i -> [i] -> m [i]
runEAUntil stopCondition preStep b e o pop = do
    stop <- stopCondition pop
    if stop
    then return pop
    else do
        newpop <- stepEA preStep b e o pop
        runEAUntil stopCondition preStep b e o newpop

runEA :: (Monad m) => ([i] -> m ()) -> Breeding i m g -> Expression g m i -> Objective m i -> [i] -> m [i]
runEA = runEAUntil (\_ -> return False)



---- Functions for displaying things at each step ----

writepop :: (Monad m, Show i) => [i] -> m (IO ())
writepop pop = return $ putStrLn $ "Pop " ++ show pop



---- Breedings ----

-- Generic breeding functions

breedAs :: (i -> i1) -> Breeding i1 m g1 -> Breeding i m g1
breedAs itoi1 b = b . fmap itoi1

mapB :: (Monad m) => (i -> m j) -> Breeding i m j
mapB mutation individuals = mapM mutation individuals

class Neighbourhood a where
    neighbours :: Int -> a -> [a]

instance Neighbourhood Int where
    neighbours size a = [a - size .. a + size]

neighbourGenomes :: (Monad m, Neighbourhood g, Eq g) => Int -> (i -> g) -> Breeding i m g
neighbourGenomes size gini = return . nub . join . map ((neighbours size) . gini)


-- Mating

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

probabilisticMutation :: (Monad m, RandomGen g) => m g -> Double -> (i -> m i) -> i -> m i
probabilisticMutation useRandomGen mutateProba mutation individual = do
    g <- useRandomGen
    let (r, _) = randomR (0,1) g
    if r < mutateProba
    then mutation individual
    else return individual

stepMutation :: (Monad m, RandomGen g, Random i, Num i) => m g -> i -> i -> m i
stepMutation useRandomGen maxStepSize individual = do
    g <- useRandomGen
    let step = fst (randomR (-maxStepSize,maxStepSize) g)
    return (individual + step)
    -- return $ map (\(indiv,g) -> let step = fst (randomR (-maxStepSize,maxStepSize) g) in indiv + step ) (zip individuals gs)

---- Expressions ----

-- Generic functions

expressAs :: (g -> g1) -> Expression g1 m p1 -> Expression g m p1
expressAs gtog1 e = e . gtog1

withGenomeE :: (Monad m) => Expression g m p -> Expression g m (p,g)
withGenomeE express = \g -> (express g) >>= \p -> return (p, g)

---- Objectives ----

-- Generic functions

objectiveAs :: (Monad m) => (p -> p1) -> (p -> i1 -> i) -> Objective m i1 -> Objective m i
objectiveAs getp1 seti1 o = undefined -- (fmap (fmap (\(p,i1) -> set p i1))) . ( otup ) . (fmap (\p -> (p,getp1)))

-- Objective building blocks

anyReaches :: (Eq a, Monad m) => (i -> a) -> a -> [i] -> m Bool
anyReaches f goal pop = return (any goalReached pop)
    where goalReached individual = (f individual) == goal

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

-- diversity :: (Monad m) => 

byNiche :: (Monad m, Ord n) => (i -> n) -> Objective m i -> Objective m i
byNiche niche o = \individuals -> 
    let indivsByNiche = Map.elems $ Map.fromListWith (++) [(niche i, [i]) | i <- individuals]
    in fmap join (mapM o indivsByNiche)


