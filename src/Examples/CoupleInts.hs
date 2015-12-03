{-# LANGUAGE RankNTypes #-}

module Examples.CoupleInts where


import Data.Ord
import Data.List
import System.Random
import Control.Monad.State
import Control.Lens

import ComposableEA


-- Breedings 

breedInt :: Int -> Breeding Int (StateT EAState IO) Int
breedInt n individuals = do
    parents <- randomGroup useRG n 3 individuals
    offsprings <- oneOfCrossover useRG parents
    mutated <- mapB (probabilisticMutation useRG 0.5 (stepMutation useRG 3)) offsprings
    return mutated

breedIntWithin :: Int -> Int -> Int -> Breeding Int (StateT EAState IO) Int
breedIntWithin minval maxval n individuals = do
    breeded <- breedInt n individuals
    return $ map (\x -> min maxval (max x minval)) breeded
    
-- Pre-step

showStateAndPop :: (Show s, Show i) => [i] -> StateT s IO ()
showStateAndPop pop = do
    s <- get
    lift $ putStrLn $ "state: " ++ (show s) ++ " pop " ++ (show $ length pop) ++ ": " ++ (show pop)

incrementIter :: Lens' s Int -> [i] -> StateT s IO ()
incrementIter iterInS pop = do
    s <- get 
    let iter = s ^. iterInS
    put (iterInS .~ (iter + 1) $ s) 

-- Use random number generator

useRG :: StateT (Int, StdGen) IO StdGen
useRG = do
    g <- gets snd
    let (g1,g2) = split g
    modify (\(a,_) -> (a, g2))
    return g1

-- Exemple 1 Composition de breedings: trouver un couple d'entiers (a,b) tels que a < b et qui
-- minimise abs(a + b)

type Genome = (Int,Int)
type Individual = (Int,Genome)
type EAState = (Int,StdGen)

iterInState :: Lens' EAState Int
iterInState = _1 

test1 :: StateT EAState IO [Individual]
test1 = 
    runEAUntil
        -- stop condition
        ( anyReaches ((0 ==) . fst))
        ( stepEA
            -- pre-step
            ( \pop -> do 
                showStateAndPop pop
                incrementIter iterInState pop )
            -- Breeding: breed d'abord 20 nouveaux a entre -10 et 10, 
            -- puis pour chaque nouveau a, breed un b entre a et 10
            ( productWithB (,)
                -- premiers int de chaque génome
                (breedAs (fst . snd) (breedIntWithin (-10) 10 500))
                (\a -> (breedAs (snd . snd) (breedIntWithin a (10) 1))) )
            -- Expression: fitness accompagné du génome (abs(a+b), (a,b))
            ( (withGenomeE . expressWith) (\(a,b) ->  abs (a + b)) ) 
            -- Objective: les 10 génomes ayant la fitness la plus petite
            ( minimise fst 200 ) 
            ( muCommaLambda ) )
        -- Initial population
        [(200,(100,100)), (50,(-50, 0))]

runTest1 :: IO ([Individual], EAState)
runTest1 = runStateT test1 (0, mkStdGen 0)

-- Exemple 2 Composition d'expressions: abs(a+b) et abs(a-b).

type Individual2 = ((Int, Int),Genome)

test2 :: StateT EAState IO [Individual2]
test2 = 
    runEAUntil
        -- condition d'arrêt
        ( anyReaches (\((s,d),_) -> (s + d) == 0) )
        ( stepEA 
            -- pre-step
            ( \pop -> do 
                showStateAndPop pop
                incrementIter iterInState pop )
            -- Breeding
            ( productWithB (,)
                (breedAs (fst . snd) (breedIntWithin (-10) 10 20))
                (\a -> (breedAs (snd . snd) (breedIntWithin a (maxBound) 1))) )
            -- Expression: ((abs(a+b), abs(a-b)), (a,b))
            ( withGenomeE $ zipE
                (expressWith (\(a,b) -> abs $ a + b))
                (expressWith (\(a,b) -> abs $ a - b)) )
            -- Objective: minimiser abs(a+b) + abs(a-b)
            ( minimise (\((s,d),_) -> s + d) 10 )
            ( muCommaLambda ) )
        -- Initial population
        [((200,0),(100,100)), 
         ((50,50),(-100, 50)), 
         ((150,50),(-50, -100)), 
         ((200,0),(-100, -100))]

runTest2 :: IO ([Individual2], EAState)
runTest2 = runStateT test2 (0, mkStdGen 0)

-- Exemple 3 Composition d'objectifs: 2 objectifs d'optimisation l'un à la suite de
-- l'autre: minimiser abssum et minimiser absdiff

test3 :: StateT EAState IO [Individual2]
test3 = 
    runEAUntil
        -- condition d'arrêt
        ( anyReaches (\((s,d),_) -> (s + d) == 0) )
        ( stepEA 
            -- pre-step
            ( \pop -> do 
                showStateAndPop pop
                incrementIter iterInState pop )
            -- Breeding
            ( productWithB (,)
                (breedAs (fst . snd) (breedIntWithin (-10) 10 20))
                (\a -> (breedAs (snd . snd) (breedIntWithin a (maxBound) 1))) )
            -- Expression
            ( withGenomeE $ zipE
                (expressWith (\(a,b) -> abs $ a + b))
                (expressWith (\(a,b) -> abs $ a - b)) )
            -- Objective: minimiser abs(a+b), puis parmi les individus conservés, minimiser abs(a-b)
            ( thenO 
                (minimise (\((s,_),_) -> s ) 15)
                (minimise (\((_,d),_) -> d) 10) )
            ( muCommaLambda ) )
        -- Initial population
        [((200,0),(100,100)), 
         ((50,50),(-100, 50)), 
         ((150,50),(-50, -100)), 
         ((200,0),(-100, -100))]

runTest3 :: IO ([Individual2], EAState)
runTest3 = runStateT test3 (0, mkStdGen 0)

-- Exemple 4 minimise byNiche 


test4 :: StateT EAState IO [Individual2]
test4 = 
    runEAUntil
        -- condition d'arrêt
        ( anyReaches (\((s,d),_) -> (s + d) == 0) )
        ( stepEA 
            -- pre-step
            ( \pop -> do 
                showStateAndPop pop
                incrementIter iterInState pop )
            -- Breeding: même breeding que précédemment, mais 3 individus générés indépendamment dans chaque niche
            ( byNicheB differenceNiche
                (productWithB (,)
                    (breedAs (fst . snd) (breedIntWithin (-10) 10 3))
                    (\a -> (breedAs (snd . snd) (breedIntWithin a (maxBound) 1)))) )
            -- Expression
            ( withGenomeE $ zipE
                (expressWith (\(a,b) -> abs $ a + b))
                (expressWith (\(a,b) -> abs $ a - b)) )
            -- Objective: même objectif que précédemment, mais on sélectionne 1 individu dans chaque niche
            ( byNicheO differenceNiche
                (thenO 
                    (minimise (\((a,_),_) -> a) 2)
                    (minimise (\((_,b),_) -> b) 1)) )
            ( muCommaLambda ) )
        -- Initial population
        [((200,0),(100,100)), 
         ((50,50),(-100, 50)), 
         ((150,50),(-50, -100)), 
         ((200,0),(-100, -100))]

differenceNiche :: Individual2 -> Int
differenceNiche ((_,d),_) = max 0 $ min 10 $ div d 10

runTest4 :: IO ([Individual2], EAState)
runTest4 = runStateT test4 (0, mkStdGen 0)

