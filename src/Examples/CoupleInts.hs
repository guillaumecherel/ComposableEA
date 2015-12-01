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
            (breedAs (fst . snd) (breedIntWithin (-10) 10 10))
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
    lift $ putStrLn $ "state: " ++ (show s) ++ " pop: " ++ (show pop)

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

test2 :: StateT Int IO [(Int, Int, (Int, Int))] -- IO (Writer (Sum Int) [(Int, Int, (Int, Int))])
test2 = 
    runEAUntil
        -- condition d'arrêt: les deux critères abssum et absdiff atteignent 0
        ( \pop -> (lift . return) $ any (\(s,d,_) -> s + d == 0) pop )
        -- step context: increment iteration
        ( \pop -> do 
            -- affichage population
            iter <- get
            lift $ putStrLn $ "iter " ++ (show iter) ++ " " ++ (show pop)
            -- increment iteration
            put (iter + 1) )
        -- Breeding: les voisins de chaque a et, pour chaque nouveau a, les voisins de chaque b
        ( bindB 
            (breedAs (\(_,_,g) -> fst g) (neighbourGenomes 1 id))
            (\as -> fmap (\bs -> as >>= \a -> bs >>= \b -> return (a,b) ) . breedAs (\(_,_,g) -> snd g) (neighbourGenomes 1 id) ) )
        -- Expression: les deux critères accompagnés du génome (abs(a+b), abs(a-b), (a,b))
        ( bindE
            (\(a,b) -> return $ abs(a + b))
            (\s (a,b) -> return $ (s, abs(a - b), (a,b))) )
        -- Objective: les génomes correspondant aux 10 plus basses fitnesses abssum + absdiff
        ( return . take 10 . sortBy (comparing $ \(s,d,_) -> s + d) ) 
        -- Initial population
        [(200,0,(100,100)), 
         (50,50,(-100, 50)), 
         (150,50,(-50, -100)), 
         (200,0,(-100, -100))]

runTest2 :: IO ([(Int, Int, (Int, Int))], Int)
runTest2 = runStateT test2 0

-- Composition d'objectifs: 2 objectifs d'optimisation: minimiser abssum et minimiser absdiff

test3 :: StateT Int IO [(Int, Int, (Int, Int))]
test3 =
    runEAUntil
        -- condition d'arrêt: les deux critères abssum et absdiff atteignent 0
        ( \pop -> (lift . return) $ any (\(s,d,_) -> s + d == 0) pop )
        -- step context: increment iteration
        ( \pop -> do 
            -- affichage population
            iter <- get
            lift $ putStrLn $ "iter " ++ (show iter) ++ " " ++ (show pop)
            -- increment iteration
            put (iter + 1) )
        -- Breeding: les voisins de chaque a et, pour chaque nouveau a, les voisins de chaque b
        ( bindB 
            (breedAs (\(_,_,g) -> fst g) (neighbourGenomes 1 id))
            (\as -> fmap (\bs -> as >>= \a -> bs >>= \b -> return (a,b) ) . breedAs (\(_,_,g) -> snd g) (neighbourGenomes 1 id) ) )
        -- Expression: les deux critères accompagnés du génome (abs(a+b), abs(a-b), (a,b))
        ( bindE
            (\(a,b) -> return $ abs(a + b))
            (\s (a,b) -> return $ (s, abs(a - b), (a,b))) )
        -- Objective: garder 5 meilleurs individus pour chaque critère
        ( thenO 
            (minimise (\(s,_,_) -> s) 5) 
            (minimise (\(_,d,_) -> d) 5) )
        -- Initial population
        [(200,0,(100,100)), 
         (50,50,(-100, 50)), 
         (150,50,(-50, -100)), 
         (200,0,(-100, -100))]

runTest3 :: IO ([(Int, Int, (Int, Int))], Int)
runTest3 = runStateT test3 0

-- minimise byNiche 

test4 ::  StateT Int IO [(Int, Int, (Int, Int))]
test4 =
    runEAUntil
        -- condition d'arrêt: les deux critères abssum et absdiff atteignent 0
        ( \pop -> (lift . return) False ) 
            -- any (\(s,d,_) -> s + d == 0) pop )
        -- affichage: population
        ( \pop -> do 
            -- affichage population and state
            iter <- get
            lift $ putStrLn $ "iter " ++ (show iter) ++ " " ++ (show pop)
            -- increment iteration
            put (iter + 1) )
        -- Breeding: les voisins de chaque a et, pour chaque nouveau a, les voisins de chaque b
        ( bindB 
            (breedAs (\(_,_,g) -> fst g) (neighbourGenomes 1 id))
            (\as -> fmap (\bs -> as >>= \a -> bs >>= \b -> return (a,b) ) . breedAs (\(_,_,g) -> snd g) (neighbourGenomes 1 id) ) )
        -- Expression: les deux critères accompagnés du génome (abs(a+b), abs(a-b), (a,b))
        ( bindE
            (\(a,b) -> return $ abs(a + b))
            (\s (a,b) -> return $ (s, abs(a - b), (a,b))) )
        -- Objective: minimiser s + d dans chaque niche
        ( byNiche (\(s,d,_) -> let n = div d 10 in if n < 0 then 0 else if n > 10 then 10 else n) $ minimise (\(s,d,_) -> s + d) 1 ) -- randomSelect (gets snd) (\newg -> modify $ \(a,g) -> (a, newg)) 1 ) 
        -- Initial population
        [(200,0,(100,100)), 
         (50,50,(-100, 50)), 
         (150,50,(-50, -100)), 
         (200,0,(-100, -100))]

runTest4 :: IO ([(Int, Int, (Int, Int))], Int)
runTest4 = runStateT test4 0

-- random by niche

test5 ::  StateT (Int, StdGen) IO [(Int, Int, (Int, Int))]
test5 =
    runEAUntil
        -- condition d'arrêt: les deux critères abssum et absdiff atteignent 0
        ( \pop -> do
            (iter ,g) <- get
            if iter >= 100
            then (lift . return) True
            else (lift . return) False )
        -- affichage: population
        ( \pop -> do 
            -- affichage population and state
            (iter,g) <- get
            lift $ putStrLn $ "iter " ++ (show iter) ++ " g " ++ (show g) ++ " " ++ (show pop)
            -- increment iteration
            put (iter + 1, g) )
        -- Breeding: les voisins de chaque a et, pour chaque nouveau a, les voisins de chaque b
        ( bindB 
            (breedAs (\(_,_,g) -> fst g) (neighbourGenomes 1 id))
            (\as -> fmap (\bs -> as >>= \a -> bs >>= \b -> return (a,b) ) . breedAs (\(_,_,g) -> snd g) (neighbourGenomes 1 id) ) )
        -- Expression: les deux critères accompagnés du génome (abs(a+b), abs(a-b), (a,b))
        ( bindE
            (\(a,b) -> return $ abs(a + b))
            (\s (a,b) -> return $ (s, abs(a - b), (a,b))) )
        -- Objective: minimiser s + d dans chaque niche
        ( byNiche (\(s,d,_) -> let n = div d 10 in if n < 0 then 0 else if n > 10 then 10 else n) $ randomSelect useRG 1 ) 
        -- Initial population
        [(200,0,(100,100)), 
         (50,50,(-100, 50)), 
         (150,50,(-50, -100)), 
         (200,0,(-100, -100))]

runTest5 :: IO ([(Int, Int, (Int, Int))], (Int, StdGen))
runTest5 = runStateT test5 (0, mkStdGen 0)

