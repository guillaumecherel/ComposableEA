module ComposableEA where

{-# LANGUAGE RankNTypes #-}

import Data.Ord
import Data.List
import qualified Data.Map.Strict as Map
import System.Random
import Control.Monad

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

breedAs :: (i -> i1) -> Breeding i1 m g1 -> Breeding i m g1
breedAs itoi1 b = b . fmap itoi1

---- Composition functions: Expression ----

bindE :: (Monad m) => Expression g m p1 -> (p1 -> Expression g m p2) -> Expression g m p2
bindE e1 e2 = \genome -> do
    p1 <- e1 genome
    e2 p1 genome

expressAs :: (g -> g1) -> Expression g1 m p1 -> Expression g m p1
expressAs gtog1 e = e . gtog1

withGenomeE :: (Monad m) => Expression g m p -> Expression g m (p,g)
withGenomeE express = \g -> (express g) >>= \p -> return (p, g)
-- withGenomeE express = bindE (return . id) (\g -> (fmap (\p -> (p,g))) . express )



---- Composition functions: Objective ----

bindO :: (Monad m) => Objective m i -> ([i] -> Objective m i) -> Objective m i
bindO o1 o2 = \phenotypes -> do
    i1s <- o1 phenotypes
    o2 i1s phenotypes

objectiveAs :: (Monad m) => (p -> p1) -> (p -> i1 -> i) -> Objective m i1 -> Objective m i
objectiveAs getp1 seti1 o = undefined -- (fmap (fmap (\(p,i1) -> set p i1))) . ( otup ) . (fmap (\p -> (p,getp1)))

-- select the i that also respect the second objective 

andO :: (Monad m, Eq i) => Objective m i -> [i] -> Objective m i
andO o =  \selected1s -> fmap (intersect selected1s) . o

orO :: (Monad m, Eq i) => Objective m i -> [i] -> Objective m i
orO o =  \selected1s -> fmap (union selected1s) . o

-- Not associative

thenO :: (Monad m) => Objective m i -> [i] -> Objective m i
thenO o selected1s _ = o selected1s



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

class Neighbourhood a where
    neighbours :: Int -> a -> [a]

instance Neighbourhood Int where
    neighbours size a = [a - size .. a + size]

neighbourGenomes :: (Monad m, Neighbourhood g, Eq g) => Int -> (i -> g) -> Breeding i m g
neighbourGenomes size gini = return . nub . join . map ((neighbours size) . gini)



---- Expressions ----



---- Objectives ----

minimise :: (Monad m, Ord p1) => (p -> p1) -> Int -> Objective m p
minimise on keep = return . take keep . sortBy (comparing on)

maximise :: (Monad m, Ord p1) => (p -> p1) -> Int -> Objective m p
maximise on keep = return . take keep . sortBy (flip $ comparing on)

pareto :: (Monad m) => Objective m p
pareto = undefined

randomSelect :: (Monad m, RandomGen g) => m g -> (g -> m ()) -> Int -> Objective m i
randomSelect getRandomGen putRandomGen n = \individuals -> do
    g <- getRandomGen 
    let (g1,g2) = split g
    putRandomGen g1
    return $ pickElems n g1 individuals

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


