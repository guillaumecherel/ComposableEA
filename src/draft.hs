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

type Breeding i g m = [i] -> m [g]

-- C'est l'étape distribuée en parallèle (équivalent à évaluation dans mgo). Elle ne fait pas intervenir la monad m qui contient l'état de l'évolution
-- car cet état peut avoir besoin d'être synchronisé entre les différents éléments évalués. La mise à jour de l'état devra donc être traité à une étape suivante (objective) si besoin
-- Ceci impose la restriction que les expressions des individus ne peut pas dépendre les unes des autres, mais c'est une restriction nécessaire pour le parallelisme.
type Expression g p m = g -> m p 

type Objective p i m = [p] -> m [i]

------------------------------------------------------------

---- Composition functions ----

composeB :: (Monad m) => (i3 -> i1) -> (i3 -> i2) -> (g1 -> g2 -> g3) -> Breeding i1 g1 m -> (g1 -> Breeding i2 g2 m) -> Breeding i3 g3 m
composeB i1ini3 i2ini3 g3fromg1g2 b1 b2 = \individuals ->
    do 
        let indivs1 = map i1ini3 individuals
        let indivs2 = map i2ini3 individuals
        breeded1 <- b1 indivs1 
        fmap join $ sequence $ do 
            g1 <- breeded1
            return $ do 
                g2s <- b2 g1 indivs2
                return $ do
                    g2 <- g2s
                    return $ g3fromg1g2 g1 g2 

composeB' :: (Monad m) => (i3 -> i1) -> (i3 -> i2) -> (g1 -> g2 -> g3) -> Breeding i1 g1 m -> Breeding i2 g2 m -> Breeding i3 g3 m
composeB' i1ini3 i2ini3 g3fromg1g2 b1 b2 = composeB i1ini3 i2ini3 g3fromg1g2 b1 (\_ -> b2)

zipB :: (Monad m) => Breeding i1 g1 m -> (g1 -> Breeding i2 g2 m) -> Breeding (i1, i2) (g1, g2) m
zipB = composeB fst snd (\g1 g2 -> (g1, g2))

zipB' :: (Monad m) => Breeding i1 g1 m -> Breeding i2 g2 m -> Breeding (i1, i2) (g1, g2) m
zipB' b1 b2 = zipB b1 (\_ -> b2)

bindB :: (Monad m) => Breeding i g1 m -> ([g1] -> Breeding i g2 m) -> Breeding i g2 m
bindB b1 b2 = \individuals -> do
    g1s <- b1 individuals
    b2 g1s individuals 

breedAs :: (i -> i1) -> Breeding i1 g1 m -> Breeding i g1 m
breedAs itoi1 b = b . (fmap itoi1)

zipBindB :: (Monad m) => ([g1] -> Breeding i g2 m) -> ([g1] -> Breeding i (g1, g2) m) 
zipBindB b2 = \g1s indivs -> fmap (zip g1s) (b2 g1s indivs) 

composeE :: (Monad m) => (g3 -> g1) -> (g3 -> g2) -> (p1 -> p2 -> p3) -> Expression g1 p1 m -> (p1 -> Expression g2 p2 m) -> Expression g3 p3 m
composeE g1ing3 g2ing3 p3fromp1p2 e1 e2 = \genome -> do
    expressed1 <- e1 $ g1ing3 genome
    expressed2 <- e2 expressed1 $ g2ing3 genome
    return $ p3fromp1p2 expressed1 expressed2

composeE' :: (Monad m) => (g3 -> g1) -> (g3 -> g2) -> (p1 -> p2 -> p3) -> Expression g1 p1 m -> Expression g2 p2 m -> Expression g3 p3 m
composeE' g1ing3 g2ing3 p3fromp1p2 e1 e2 = composeE g1ing3 g2ing3 p3fromp1p2 e1 (\_ -> e2)

zipE :: (Monad m) => Expression g1 p1 m -> (p1 -> Expression g2 p2 m) -> Expression (g1,g2) (p1,p2) m
zipE = composeE fst snd (\p1 p2 -> (p1,p2))

zipE' :: (Monad m) => Expression g1 p1 m -> Expression g2 p2 m -> Expression (g1,g2) (p1,p2) m
zipE' e1 e2 = zipE e1 (\_ -> e2)

withGenomeE :: (Monad m) => Expression g p m -> Expression g (p,g) m
-- expressWithGenome express = \g -> (express g) >>= \p -> return (p, g)
withGenomeE express = 
    composeE' 
        id
        id
        (\p g -> (p,g))
        express 
        (return . id)

composeO :: (Monad m) => (p3 -> p1) -> (p3 -> p2) -> (i1 -> i2 -> i3) -> Objective p1 i1 m -> ([i1] -> Objective p2 i2 m) -> Objective p3 i3 m
composeO p1inp3 p2inp3 i3fromi1i2 o1 o2 = \phenotypes -> do
    let phenotypes1 = map p1inp3 phenotypes
    let phenotypes2 = map p2inp3 phenotypes
    indivs1 <- o1 phenotypes1
    indivs2 <- o2 indivs1 phenotypes2
    return $ zipWith i3fromi1i2 indivs1 indivs2

zipO :: (Monad m) => Objective p1 i1 m -> ([i1] -> Objective p2 i2 m) -> Objective (p1,p2) (i1,i2) m
zipO = composeO fst snd (\i1 i2 -> (i1,i2))

zipO' :: (Monad m) => Objective p1 i1 m -> Objective p2 i2 m -> Objective (p1,p2) (i1,i2) m
zipO' o1 o2 = zipO o1 (\_ -> o2)

-------------------------------

---- Functions for running an EA ----

stepEA :: (Monad m) => m () -> Breeding i g m -> Expression g p m -> Objective p i m -> [i] -> m [i]
stepEA stepContext breeding expression objective pop = do
    stepContext
    breeded <- breeding pop
    expressed <- mapM expression breeded --mapM est l'étape parallelisable
    objective expressed

runEAUntil :: (Monad m) => (m [i] -> Bool) -> (m [i] -> IO () ) -> m () -> Breeding i g m -> Expression g p m -> Objective p i m  -> m [i] -> IO (m [i])
runEAUntil stopCondition outputF stepContext b e o mpop = do
   if (stopCondition mpop)
   then return mpop
   else do
       outputF mpop
       let newmpop = mpop >>= (stepEA stepContext b e o)
       runEAUntil stopCondition outputF stepContext b e o newmpop

runEA :: (Monad m) => (m [i] -> IO () ) -> m () -> Breeding i g m -> Expression g p m -> Objective p i m -> m [i] -> IO (m [i])
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

---- Exemples Breeding----

breedInt :: (Monad m) => Breeding Int Int m
breedInt x = return $ map (1 +) x

breedListDouble :: (Monad m) => Breeding [Double] [Double] m
breedListDouble [] = return mempty
breedListDouble (x:[]) = return [x]
breedListDouble l = return $ (pair l) >>= \(x,y) -> [alternate x y, alternate y x]
    where 
        alternate x [] = x
        alternate [] y = y
        alternate (x:xs) (y:ys) = x:(alternate ys xs)
        pair [] = []
        pair (x:[]) = [(x,x)]
        pair (x:y:l) = (x,y) : pair l

zippedBreedings :: (Monad m) => Breeding (Int, (Int, [Double])) (Int, (Int, [Double])) m
zippedBreedings = zipB' breedInt (zipB' breedInt breedListDouble)

ex1 = do
    putStrLn . show $ (zippedBreedings [(1, (1, [1.1,1.1,1.1,1.1])), (8, (8, [8.8,8.8,8.8,8.8]))] :: Identity [(Int, (Int, [Double]))])
    --(putStrLn . show) ( (zippedBreedings [(1, (1, [1.1,1.1,1.1,1.1])), (8, (8, [8.8,8.8,8.8,8.8]))]) :: Breeding (Int, (Int, [Double])) (Int, (Int, [Double])) Identity)

-- On veut faire un génome composé d'un Int et d'une liste, tel que les n
-- premiers elements de la liste soit composés de ceux du premier parents, et les suivants du second parent.
-- On commence par définir le breeding sur Int et le breeding sur [a] indépendamment l'un de l'autre.

-- Un Breeding qui opère sur des individus décrits par un Int (génome) et un Double (fitness). On breed plusieurs individus en prenant simplement 
-- le génome des individus qui ont la plus grande valeur de fitness
breedIntFittest :: (Monad m) => Int -> Breeding (Int, Double) Int m
breedIntFittest topN individuals =
    let fittest = map (\(g,_) -> g) $ take topN $ sortBy (comparing $ (\(_,fit) -> -fit)) individuals 
    in return $ take (length individuals) (cycle fittest)

-- Un Breeding qui opère sur un génome constitué d'une chaîne de caractères. On breed 2 individus en prenant les n premiers
-- caractères du premier et tous sauf les n premiers caractères du second.
breedListCrossAt :: (Monad m) => Int -> Breeding [a] [a] m
breedListCrossAt _ [] = return mempty
breedListCrossAt _ (x:[]) = return [x]
breedListCrossAt n (x:y:l) = undefined -- (breedListCrossAt (n2:ns) (y:l)) >>= \breeded -> return $ ((take n1 x) ++ (drop n1 y)):breeded 

-- On assemble les deux Breeding précédents en un seul nouveau Breeding avec la fonction bind ci-dessus.
-- L'entier sélectionné par le premier breeding détermine le point de rupture n dans le second breeding.
-- La fonction bind s'occupe d'appeler correctement chaque Breeding, et passe le génome sélectionné par le premier
-- en entrée du deuxième.

geti3i1 :: Getter (Int, [Char], Double) (Int, Double)
geti3i1 = to (\(i,_,d) -> (i,d))

geti3i2 :: Getter (Int, [Char], Double) [Char]
geti3i2 = to (\(_,s,_) -> s)

lensg2g1 :: Lens Int (Int, [Char]) () [Char]
lensg2g1 = lens (\g1 -> ()) (\g1 g2 -> (g1, g2))

bound :: (Monad m ) => Breeding (Int, [Char], Double) (Int, [Char]) m
--bound = composeB geti3i1 geti3i2 lensg2g1 (breedIntFittest 1) breedListCrossAt
bound = composeB i1ini3 i2ini3 g3fromg1g2 (breedIntFittest 1) breedListCrossAt
    where
        i1ini3 (i,_,d) = (i,d) 
        i2ini3 (_,s,_) = s
        g3fromg1g2 g1 g2 = (g1,g2)

ex2 = do
    putStrLn . show $ (bound [(1, "abcdef", 2.0), (3, "zzzzzz", 3.0)] :: Identity [(Int, String)])

---- Exemples Expression ----
expressId :: (Monad m ) => Expression a a m
expressId = return . id

expressListLength :: (Monad m ) => Expression [a] Int m
expressListLength = return . length

expressReplicateInt :: (Monad m) => Int -> Expression Int [Int] m
expressReplicateInt size i = return [i | _ <- [1..size]]

-- boundExpress = Expression ()
-- boundExpress = composeE g1ing3 g2ing3 p2inp1 expressListIntAsMax expressIntAsRange
--     where g1ing3 = to (\(

-- composeE :: Getter g3 g1 -> Getter g3 g2 -> Setter p1 p3 () p2 -> Expression g1 p1 -> (p1 -> Expression g2 p2) -> Expression g3 p3


---- Breedings ----

class Neighbourhood a where
    neighbours :: Int -> a -> [a]

instance Neighbourhood Int where
    neighbours size a = [a - size .. a + size]

neighbourGenomes :: (Monad m, Neighbourhood g, Eq g) => Int -> (i -> g) -> Breeding i g m
neighbourGenomes size gini = return . nub . join . (map ((neighbours size) . gini))

-------------------

---- Expressions ----

---------------------

---- Objectives ----

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
        -- ( zipB 
        --     (neighbourGenomes 1 id)
        --     (\newa -> (fmap (filter (newa <)) ) . (neighbourGenomes 1 id)) )
        ( bindB 
            (breedAs fst (neighbourGenomes 1 id))
            (\as -> (fmap (\bs -> as >>= \a -> bs >>= \b -> return (a,b))) . (breedAs snd (neighbourGenomes 1 id)) ) )
        -- Expression: la fitness accompagné du génome (abs(a+b), (a,b))
        ( withGenomeE (\(a,b) -> return $ abs (a + b)) ) 
        -- Objective: les génomes correspondant aux 10 plus basses fitnesses
        ( \ps -> let best = take 10 (sortBy (comparing fst) ps) 
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
        ( zipB 
            (neighbourGenomes 10 id)
            (\newa -> (neighbourGenomes 10 id)) )
        -- Expression: les deux critères accompagnés du génome (abs(a+b), abs(a-b), (a,b))
        -- ( withGenomeE (\(a,b) -> return $ abs (a + b)) ) 
        ( composeE'
            id
            id
            (\s (d, g) -> (s, d, g))
            (\(a,b) -> return $ abs(a + b)) 
            (withGenomeE (\(a,b) -> return $ abs(a - b))))
        -- Objective: les génomes correspondant aux 10 plus basses fitnesses abssum + absdiff
        ( \ps -> let best = take 20 (sortBy (comparing $ \(s,d,_) -> s + d) ps) 
                 in writer (map (\(_,_,x) -> x) best, [head best]) ) 
        -- Initial population
        ( return [(100,100)] ) -- initial population
    

testRun :: IO (Writer (Sum Int) ([(Int, (Int, [Double]))]))
testRun = 
    runEA 
        writeiterpop
        (tell $ Sum 1)
        zippedBreedings 
        (return . id) 
        (return . id) 
        (return [(1, (1, [1.1,1.1,1.1,1.1])), (8, (8, [8.8,8.8,8.8,8.8]))])

testRun2 :: IO (Writer (Sum Int) ([(Int, (Int, [Double]))]))
testRun2 = 
    runEAUntil
        (\mpop -> let (_, Sum iter) = runWriter mpop in if iter >= 10 then True else False)
        writeiterpop
        (tell $ Sum 1)
        zippedBreedings 
        (return . id) 
        (return . id) 
        (return [(1, (1, [1.1,1.1,1.1,1.1])), (8, (8, [8.8,8.8,8.8,8.8]))])

------------------


