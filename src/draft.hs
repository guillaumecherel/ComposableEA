{-# LANGUAGE RankNTypes #-}

import Data.Ord
import Data.List
import Control.Lens

data Individual g p = Individual g p deriving (Show, Eq)
newtype Breeding g p = Breeding {breed :: [Individual g p] -> g} 


bind :: g3 -> Lens' g3 g1 -> Lens' p3 p1 -> Lens' g3 g2 -> Lens' p3 p2 -> Breeding g1 p1 -> (g1 -> Breeding g2 p2) -> Breeding g3 p3
bind g3empty g3g1 p3p1 g3g2 p3p2 b1 b2 = Breeding { breed = \individuals ->
    let breeded1 = breed b1 $ map (\(Individual g p) -> Individual (g ^. g3g1) (p ^. p3p1)) individuals 
        breeded2 = breed (b2 breeded1) $ map (\(Individual g p) -> Individual (g ^. g3g2) (p ^. p3p2)) individuals
    in (g3g1 .~ breeded1) $ (g3g2 .~ breeded2) $ g3empty }

-- Un Breeding qui opère sur un génome constitué d'un seul entier. On breed plusieurs individus en prenant simplement 
-- le génome de l'individu qui a le phénotype (Double) le plus grand
b1 :: Breeding Int Double
b1 = Breeding {breed = \individuals -> 
    let Individual g _ = maximumBy (comparing $ (\(Individual _ p) -> p)) individuals 
    in g }

-- Un Breeding qui opère sur un génome constitué d'une chaine de caractères. On breed plusieurs individus en prenant les n premiers
-- caractères du premier et tous sauf les n premiers caractères du second.
b2 :: Int -> Breeding [Char] ()
b2 n = Breeding {breed = \individuals -> (firstpart individuals) ++ (secondpart individuals)}
    where firstpart [] = ""
          firstpart (Individual g p:_) = take n g
          secondpart [] = ""
          secondpart (_:[]) = ""
          secondpart (_:Individual g p:_) = drop n g

-- On assemble les deux Breeding précédents en un seul nouveau Breeding avec la fonction bind ci-dessus.
-- L'entier sélectionné par le premier breeding détermine le point de rupture n dans le second breeding.
-- La fonction bind s'occupe d'appeler correctement chaque Breeding, et passe le génome sélectionné par le premier
-- en entrée du deuxième.

bound :: Breeding (Int,[Char]) Double
bound = bind (0,"") _1 (lens id (\i i' -> i')) _2 (lens (\_ -> ()) (\d _ -> d)) b1 b2


---- Exemples ----

-- un génome de type (Int, String), le pivot du crossover du string est donné par son int. Le breeding de l'int
-- est fait en selectionnant le parent avec le plus grand phénotype (Double).
ex1 = do
    putStrLn . show $ (breed bound) [Individual (1, "abcdef") 7.0, Individual (3, "zzzzzz") 3.0]


main :: IO ()
main = do
  putStrLn "hello world"
