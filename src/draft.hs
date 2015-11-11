{-# LANGUAGE RankNTypes #-}

import Data.Ord
import Data.List
import Control.Lens
import System.Random
import Control.Monad.Identity
import Control.Monad.Trans
import Control.Monad.Writer
import Data.Monoid

-- TODO: enlever les duplicats dans la composition de breedings

---- Types correspondant aux différentes étapes de l'EA ----

type Breeding i m g = [i] -> m [g]

-- C'est l'étape distribuée en parallèle (équivalent à évaluation dans mgo). Elle ne fait pas intervenir la monad m qui contient l'état de l'évolution
-- car cet état peut avoir besoin d'être synchronisé entre les différents éléments évalués. La mise à jour de l'état devra donc être traité à une étape suivante (objective) si besoin
-- Ceci impose la restriction que les expressions des individus ne peut pas dépendre les unes des autres, mais c'est une restriction nécessaire pour le parallelisme.
type Expression g m p = g -> m p 

type Objective p m i = [p] -> m [i]

------------------------------------------------------------

---- Composition functions ----

bindB :: (Monad m) => Breeding i m g1 -> ([g1] -> Breeding i m g2) -> Breeding i m g2
bindB b1 b2 = \individuals -> do
    g1s <- b1 individuals
    b2 g1s individuals 

breedAs :: (i -> i1) -> Breeding i1 m g1 -> Breeding i m g1
breedAs itoi1 b = b . (fmap itoi1)

-- zipBindB :: (Monad m) => ([g1] -> Breeding i m g2) -> ([g1] -> Breeding ig1 (, g2) m) 
-- zipBindB b2 = \g1s indivs -> fmap (zip g1s) (b2 g1s indivs) 

bindE :: (Monad m) => Expression g m p1 -> (p1 -> Expression g m p2) -> Expression g m p2
bindE e1 e2 = \genome -> do
    p1 <- e1 genome
    e2 p1 genome

expressAs :: (g -> g1) -> Expression g1 m p1 -> Expression g m p1
expressAs gtog1 e = e . gtog1

withGenomeE :: (Monad m) => Expression g m p -> Expression g m (p,g)
withGenomeE express = \g -> (express g) >>= \p -> return (p, g)
-- withGenomeE express = bindE (return . id) (\g -> (fmap (\p -> (p,g))) . express )

bindO :: (Monad m) => Objective p m i1 -> ([i1] -> Objective p m i2) -> Objective p m i2
bindO o1 o2 = \phenotypes -> do
    i1s <- o1 phenotypes
    o2 i1s phenotypes

objectiveAs :: (Monad m) => (p -> p1) -> (p -> i1 -> i) -> Objective p1 m i1 -> Objective p m i
objectiveAs getp1 seti1 o = undefined -- (fmap (fmap (\(p,i1) -> set p i1))) . ( otup ) . (fmap (\p -> (p,getp1)))

-- select the i that also respect the second objective 

-- intersectionO    

-- unionO

-- paretoO

-------------------------------

---- Functions for running an EA ----

stepEA :: (Monad m) => m () -> Breeding i m g -> Expression g m p -> Objective p m i -> [i] -> m [i]
stepEA stepContext breeding expression objective pop = do
    stepContext
    breeded <- breeding pop
    expressed <- mapM expression breeded --mapM est l'étape parallelisable
    objective expressed

runEAUntil :: (Monad m) => (m [i] -> Bool) -> (m [i] -> IO () ) -> m () -> Breeding i m g -> Expression g m p -> Objective p m i  -> m [i] -> IO (m [i])
runEAUntil stopCondition outputF stepContext b e o mpop = do
   if (stopCondition mpop)
   then return mpop
   else do
       outputF mpop
       let newmpop = mpop >>= (stepEA stepContext b e o)
       runEAUntil stopCondition outputF stepContext b e o newmpop

runEA :: (Monad m) => (m [i] -> IO () ) -> m () -> Breeding i m g -> Expression g m p -> Objective p m i -> m [i] -> IO (m [i])
runEA = runEAUntil (\_ -> False)

-------------------------------------

---- Functions for displaying things at each step ----

writempop :: (Show m) => m -> IO ()
writempop mpop = putStrLn $ "Pop " ++ show mpop

writeiterpop :: (Show i) => Writer (Sum Int) [i] -> IO ()
writeiterpop mpop = 
    let (pop,Sum iter) = runWriter mpop 
    in putStrLn $ "Iter " ++ (show iter) ++ " Pop " ++ (show pop)

------------------------------------------------------

---- Breedings ----

class Neighbourhood a where
    neighbours :: Int -> a -> [a]

instance Neighbourhood Int where
    neighbours size a = [a - size .. a + size]

neighbourGenomes :: (Monad m, Neighbourhood g, Eq g) => Int -> (i -> g) -> Breeding i m g
neighbourGenomes size gini = return . nub . join . (map ((neighbours size) . gini))

-------------------

---- Expressions ----

---------------------

---- Objectives ----

minimise :: (Monad m, Ord p1) => (p -> p1) -> Int -> Objective p m p
minimise on keep = return . (take keep) . (sortBy (comparing on) )

--------------------

---- Exemples ----

-- Composition de breedings: trouver un couple d'entiers (a,b) tels que a < b et qui
-- minimise abs(a + b)

-- Le contexte de l'évolution est une writer monad de type Writer [(Int, (Int,
-- Int))] qui accumule au fur et à mesure de l'évolution le meilleur individu
-- de chaque iteration. À chaque iteration, on ajoute à la liste un nouveau
-- tuple (f, (a,b)) où f donne la fitness du génome (a,b)

test1 :: IO (Writer [(Int,(Int,Int))] [(Int,Int)])
test1 = 
    runEAUntil
        -- condition d'arrêt: la dernière meilleure fitness = 0
        ( \mpop -> let (_, bests) = runWriter mpop
                  in case bests of 
                      [] -> False
                      l -> if ((fst . last) l) == 0 then True else False )
        -- affichage: population
        ( \mpop -> let (pop, _) = runWriter mpop
                  in putStrLn $ show pop )
        -- step context: rien
        ( return () )
        -- Breeding: les voisins de chaque a et, pour chaque nouveau a, les voisins de chaque b tel que a < b
        ( bindB 
            (breedAs fst (neighbourGenomes 1 id))
            (\as -> (fmap (\bs -> as >>= \a -> bs >>= \b -> if a < b then return (a,b) else [])) . (breedAs snd (neighbourGenomes 1 id)) ) )
        -- Expression: la fitness accompagné du génome (abs(a+b), (a,b))
        ( withGenomeE (\(a,b) -> return $ abs (a + b)) ) 
        -- Objective: les génomes correspondant aux 10 plus basses fitnesses
        ( \ps -> let best = take 100 (sortBy (comparing fst) ps) 
                 in writer (map snd best, [head best]) ) 
        -- Initial population
        ( return [(100,100), (-50, 0)] ) -- initial population

-- Composition d'expressions: on veut trouver le couple d'entiers (a,b) qui
-- minimise à la fois la somme de a et b et leur distance. Il faut donc
-- exprimer 2 variables à partir d'un génome: abs(a+b) et abs(a-b). On devrait
-- tomber sur le couple (0,0). On utilise toujours comme contexte une writer
-- monad qui enregistre cette fois les deux variables (abssum, absdiff, (a,b)).

test2 :: IO (Writer [(Int, Int, (Int, Int))] [(Int, Int)])
test2 = 
    runEAUntil
        -- condition d'arrêt: les deux critères abssum et absdiff atteignent 0
        ( \mpop -> let (_, bests) = runWriter mpop
                   in case bests of 
                      [] -> False
                      l -> case (last l) of 
                            (0,0,_) -> True 
                            _ -> False )
        -- affichage: population
        ( \mpop -> let (pop, _) = runWriter mpop
                  in putStrLn $ show pop )
        -- step context: rien
        ( return () )
        -- Breeding: les voisins de chaque a et, pour chaque nouveau a, les voisins de chaque b
        ( bindB 
            (breedAs fst (neighbourGenomes 1 id))
            (\as -> (fmap (\bs -> as >>= \a -> bs >>= \b -> return (a,b) )) . (breedAs snd (neighbourGenomes 1 id)) ) )
        -- Expression: les deux critères accompagnés du génome (abs(a+b), abs(a-b), (a,b))
        ( bindE
            (\(a,b) -> return $ abs(a + b))
            (\s (a,b) -> return $ (s, abs(a - b), (a,b))) )
        -- Objective: les génomes correspondant aux 10 plus basses fitnesses abssum + absdiff
        ( \ps -> let best = take 10 (sortBy (comparing $ \(s,d,_) -> s + d) ps) 
                 in writer (map (\(_,_,x) -> x) best, [head best]) ) 
        -- Initial population
        ( return [(100,100), (-100, 50), (-50, -100), (-100, -100)] ) -- initial population
    
-- Composition d'objectifs: 2 objectifs d'optimisation: minimiser abssum et minimiser absdiff

test3 :: IO (Writer [(Int, Int, (Int, Int))] [(Int, Int)])
test3 =
    runEAUntil
        -- condition d'arrêt: les deux critères abssum et absdiff atteignent 0
        ( \mpop -> let (_, bests) = runWriter mpop
                   in case bests of 
                      [] -> False
                      l -> case (last l) of 
                            (0,0,_) -> True 
                            _ -> False )
        -- affichage: population
        ( \mpop -> let (pop, _) = runWriter mpop
                  in putStrLn $ show pop )
        -- step context: rien
        ( return () )
        -- Breeding: les voisins de chaque a et, pour chaque nouveau a, les voisins de chaque b
        ( bindB 
            (breedAs fst (neighbourGenomes 1 id))
            (\as -> (fmap (\bs -> as >>= \a -> bs >>= \b -> return (a,b) )) . (breedAs snd (neighbourGenomes 1 id)) ) )
        -- Expression: les deux critères accompagnés du génome (abs(a+b), abs(a-b), (a,b))
        ( bindE
            (\(a,b) -> return $ abs(a + b))
            (\s (a,b) -> return $ (s, abs(a - b), (a,b))) )
        -- Objective: garder 5 meilleurs individus pour chaque critère
        ( \ps -> let best = take 10 (sortBy (comparing $ \(s,d,_) -> s + d) ps) 
                in writer (map (\(_,_,x) -> x) best, [head best]) ) 
        -- ( bindO 
        --    (minimise (\(s,_,_) -> s) 5) 
        --    (\selected1s -> (fmap (union selected1s)) . (minimise (\(_,d,_) -> d) 5)) )
        -- Initial population
        ( return [(100,100), (-100, 50), (-50, -100), (-100, -100)] ) -- initial population
        

------------------

