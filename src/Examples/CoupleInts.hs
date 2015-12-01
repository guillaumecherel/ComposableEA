{-# LANGUAGE RankNTypes #-}

module Examples.CoupleInts where


import Data.Ord
import Data.List
import System.Random
import Control.Monad.State
import Control.Lens

import ComposableEA

---- Exemples ----

-- Composition de breedings: trouver un couple d'entiers (a,b) tels que a < b et qui
-- minimise abs(a + b)

type Genome = (Int,Int)
type Individual = (Int,Genome)
type EAState = (Int,StdGen)

iterInState :: Lens' EAState Int
iterInState = _1 -- lens fst (\(iter, g) iter2 -> (iter2, g))

test1 :: StateT EAState IO [Individual]
test1 = 
    runEAUntil
        -- condition d'arrêt
        ( anyReaches fst 0 )
        -- pre-step
        ( \pop -> do 
            showStateAndPop pop
            incrementIter iterInState pop )
        -- Breeding: les voisins de chaque a et, pour chaque nouveau a, les voisins de chaque b tel que a < b
        ( productWithB (,)
            -- premiers int de chaque génome
            -- (breedAs (fst . snd) (neighbourGenomes 1 id))
            (breedAs (fst . snd) (breedIntWithin (-10) 10 20))
            (\a -> (breedAs (snd . snd) (breedIntWithin a (maxBound) 1))) )
        -- Expression: la fitness accompagné du génome (abs(a+b), (a,b))
        ( withGenomeE (\(a,b) -> return $ abs (a + b)) ) 
        -- Objective: les génomes correspondant aux 10 plus basses fitnesses
        ( minimise fst 10 )
        -- Initial population
        [(200,(100,100)), (50,(-50, 0))]

runTest1 :: IO ([Individual], EAState)
runTest1 = runStateT test1 (0, mkStdGen 0)

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
    
showStateAndPop :: (Show s, Show i) => [i] -> StateT s IO ()
showStateAndPop pop = do
    s <- get
    lift $ putStrLn $ "state: " ++ (show s) ++ " pop " ++ (show $ length pop) ++ ": " ++ (show pop)

incrementIter :: Lens' s Int -> [i] -> StateT s IO ()
incrementIter iterInS pop = do
    s <- get 
    let iter = s ^. iterInS
    put (iterInS .~ (iter + 1) $ s) 

useRG :: StateT (Int, StdGen) IO StdGen
useRG = do
    g <- gets snd
    let (g1,g2) = split g
    modify (\(a,_) -> (a, g2))
    return g1

-- Composition d'expressions: on veut trouver le couple d'entiers (a,b) qui
-- minimise à la fois la somme de a et b et leur distance. Il faut donc
-- exprimer 2 variables à partir d'un génome: abs(a+b) et abs(a-b). On devrait
-- tomber sur le couple (0,0). On utilise toujours comme contexte une writer
-- monad qui enregistre cette fois les deux variables (abssum, absdiff, (a,b)).

type Individual2 = ((Int, Int), Genome)

test2 :: StateT EAState IO [Individual2]
test2 = 
    runEAUntil
        -- condition d'arrêt
        ( anyReaches (\((s,d),_) -> s + d) 0 )
        -- pre-step
        ( \pop -> do 
            showStateAndPop pop
            incrementIter iterInState pop )
        -- Breeding: les voisins de chaque a et, pour chaque nouveau a, les voisins de chaque b tel que a < b
        ( productWithB (,)
            -- premiers int de chaque génome
            -- (breedAs (fst . snd) (neighbourGenomes 1 id))
            (breedAs (fst . snd) (breedIntWithin (-10) 10 20))
            (\a -> (breedAs (snd . snd) (breedIntWithin a (maxBound) 1))) )
        -- Expression: la fitness accompagné du génome (abs(a+b), (a,b))
        --( withGenomeE (\(a,b) -> return $ abs (a + b)) ) 
        ( withGenomeE $ zipE
            (expressWith (\(a,b) -> abs $ a + b))
            (expressWith (\(a,b) -> abs $ a - b)) )
        -- Objective: les génomes correspondant aux 10 plus basses fitnesses
        ( minimise (\((a,b),_) -> a + b) 10 )
        -- Initial population
        [((200,0),(100,100)), 
         ((50,50),(-100, 50)), 
         ((150,50),(-50, -100)), 
         ((200,0),(-100, -100))]

runTest2 :: IO ([Individual2], EAState)
runTest2 = runStateT test2 (0, mkStdGen 0)

-- Composition d'objectifs: 2 objectifs d'optimisation: minimiser abssum et minimiser absdiff

test3 :: StateT EAState IO [Individual2]
test3 = 
    runEAUntil
        -- condition d'arrêt
        ( anyReaches (\((s,d),_) -> s + d) 0 )
        -- pre-step
        ( \pop -> do 
            showStateAndPop pop
            incrementIter iterInState pop )
        -- Breeding: les voisins de chaque a et, pour chaque nouveau a, les voisins de chaque b tel que a < b
        ( productWithB (,)
            -- premiers int de chaque génome
            -- (breedAs (fst . snd) (neighbourGenomes 1 id))
            (breedAs (fst . snd) (breedIntWithin (-10) 10 20))
            (\a -> (breedAs (snd . snd) (breedIntWithin a (maxBound) 1))) )
        -- Expression: la fitness accompagné du génome (abs(a+b), (a,b))
        --( withGenomeE (\(a,b) -> return $ abs (a + b)) ) 
        ( withGenomeE $ zipE
            (expressWith (\(a,b) -> abs $ a + b))
            (expressWith (\(a,b) -> abs $ a - b)) )
        -- Objective: les génomes correspondant aux 10 plus basses fitnesses
        ( thenO 
            (minimise (\((a,_),_) -> a ) 15)
            (minimise (\((_,b),_) -> b) 10) )
        -- Initial population
        [((200,0),(100,100)), 
         ((50,50),(-100, 50)), 
         ((150,50),(-50, -100)), 
         ((200,0),(-100, -100))]

runTest3 :: IO ([Individual2], EAState)
runTest3 = runStateT test3 (0, mkStdGen 0)

-- minimise byNiche 


test4 :: StateT EAState IO [Individual2]
test4 = 
    runEAUntil
        -- condition d'arrêt
        ( anyReaches (\((s,d),_) -> s + d) 0 )
        -- pre-step
        ( \pop -> do 
            showStateAndPop pop
            incrementIter iterInState pop )
        -- Breeding: les voisins de chaque a et, pour chaque nouveau a, les voisins de chaque b tel que a < b
        ( byNicheB differenceNiche
            (productWithB (,)
                -- premiers int de chaque génome
                -- (breedAs (fst . snd) (neighbourGenomes 1 id))
                (breedAs (fst . snd) (breedIntWithin (-10) 10 3))
                (\a -> (breedAs (snd . snd) (breedIntWithin a (maxBound) 1)))) )
        -- Expression: la fitness accompagné du génome (abs(a+b), (a,b))
        --( withGenomeE (\(a,b) -> return $ abs (a + b)) ) 
        ( withGenomeE $ zipE
            (expressWith (\(a,b) -> abs $ a + b))
            (expressWith (\(a,b) -> abs $ a - b)) )
        -- Objective: les génomes correspondant aux 10 plus basses fitnesses
        ( byNicheO differenceNiche
            (thenO 
                (minimise (\((a,_),_) -> a) 2)
                (minimise (\((_,b),_) -> b) 1)) )
        -- Initial population
        [((200,0),(100,100)), 
         ((50,50),(-100, 50)), 
         ((150,50),(-50, -100)), 
         ((200,0),(-100, -100))]

differenceNiche :: Individual2 -> Int
differenceNiche ((_,d),_) = max 0 $ min 10 $ div d 10

runTest4 :: IO ([Individual2], EAState)
runTest4 = runStateT test4 (0, mkStdGen 0)

