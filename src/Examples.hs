module Examples where

import Data.Ord
import Data.List
import qualified Data.Map.Strict as Map
import Control.Lens
import System.Random
import Control.Monad.Identity
import Control.Monad.Trans
import Control.Monad.Writer
import Control.Monad.State
import Data.Monoid

import ComposableEA

---- Exemples ----

-- Composition de breedings: trouver un couple d'entiers (a,b) tels que a < b et qui
-- minimise abs(a + b)

-- Le contexte de l'évolution est une writer monad de type Writer [(Int, (Int,
-- Int))] qui accumule au fur et à mesure de l'évolution le meilleur individu
-- de chaque iteration. À chaque iteration, on ajoute à la liste un nouveau
-- tuple (f, (a,b)) où f donne la fitness du génome (a,b)

test1 :: StateT Int IO [(Int,(Int,Int))]
test1 = 
    runEAUntil
        -- condition d'arrêt: la meilleure fitness = 0
        ( \pop -> (lift . return) $ (any (\(f, _) -> f == 0) pop) )
        ( \pop -> do 
            -- affichage population
            iter <- get
            lift $ putStrLn $ "iter " ++ (show iter) ++ " " ++ (show pop)
            -- increment iteration
            put (iter + 1) )
        -- Breeding: les voisins de chaque a et, pour chaque nouveau a, les voisins de chaque b tel que a < b
        ( bindB 
            (breedAs (fst . snd) (neighbourGenomes 1 id))
            (\as -> fmap (\bs -> as >>= \a -> bs >>= \b -> if a < b then return (a,b) else []) . breedAs (snd.snd) (neighbourGenomes 1 id) ) )
        -- Expression: la fitness accompagné du génome (abs(a+b), (a,b))
        ( withGenomeE (\(a,b) -> return $ abs (a + b)) ) 
        -- Objective: les génomes correspondant aux 10 plus basses fitnesses
        ( return . take 10 . sortBy (comparing fst) )
        -- Initial population
        [(200,(100,100)), (50,(-50, 0))]

runTest1 :: IO ([(Int,(Int,Int))], Int)
runTest1 = runStateT test1 0

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
        ( bindO 
            (minimise (\(s,_,_) -> s) 5) 
            (thenO (minimise (\(_,d,_) -> d) 5)) )
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
        ( byNiche (\(s,d,_) -> let n = div d 10 in if n < 0 then 0 else if n > 10 then 10 else n) $ randomSelect (gets snd) (\newg -> modify $ \(a,g) -> (a, newg)) 1 ) 
        -- Initial population
        [(200,0,(100,100)), 
         (50,50,(-100, 50)), 
         (150,50,(-50, -100)), 
         (200,0,(-100, -100))]

runTest5 :: IO ([(Int, Int, (Int, Int))], (Int, StdGen))
runTest5 = runStateT test5 (0, mkStdGen 0)

