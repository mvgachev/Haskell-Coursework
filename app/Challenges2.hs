{-# LANGUAGE DeriveGeneric #-}
-- comp2209 Functional Programming Challenges
-- (c) University of Southampton 2020
-- Skeleton code to be updated with your solutions
-- The dummy functions here simply return an arbitrary value that is usually wrong 

-- DO NOT MODIFY THE FOLLOWING LINES OF CODE
module Challenges (WordSearchGrid,Placement,Posn,Orientation(..),solveWordSearch, createWordSearch) where

-- Import standard library and parsing definitions from Hutton 2016, Chapter 13
-- We import System.Random - make sure that your installation has it installed - use stack ghci and stack ghc
import Data.Char
import Control.Monad
import Data.List
import GHC.Generics (Generic,Generic1)
import Control.DeepSeq
import System.IO
import System.Random
import Data.Set (Set)
import qualified Data.Set as Set


-- types for Part I
type WordSearchGrid = [[ Char ]]
type Placement = (Posn,Orientation)
type Posn = (Int,Int)
data Orientation = Forward | Back | Up | Down | UpForward | UpBack | DownForward | DownBack deriving (Eq,Ord,Show,Read)

-- types for Parts II and III
data LamMacroExpr = LamDef [ (String,LamExpr) ] LamExpr deriving (Eq,Show,Read)
data LamExpr = LamMacro String | LamApp LamExpr LamExpr  |
               LamAbs Int LamExpr  | LamVar Int deriving (Eq,Show,Read)

-- END OF CODE YOU MUST NOT MODIFY

-- ADD YOUR OWN CODE HERE

-- Challenge 1 --

solveWordSearch :: [ String ] -> WordSearchGrid -> [ (String,Maybe Placement) ]
solveWordSearch [] _ = []
solveWordSearch _ [] = error "There is no grid."
solveWordSearch (w:ws) grid = (w,checkWord w grid) : solveWordSearch ws grid

checkWord :: String -> WordSearchGrid -> Maybe Placement
checkWord (l:ls) grid 
         | answer == [] = Nothing
         | length answer > 1 = Nothing
         | otherwise = head answer
         where answer = [ o | o <- options, o /= Nothing]
               options = [ checkPosition [Forward,Back,Up,Down,UpForward,UpBack,DownForward,DownBack] i j grid ls  | i <- [0..len], j <- [0..len], grid !! i !! j == l]
               len = length grid - 1

checkPosition :: [Orientation] -> Int -> Int -> WordSearchGrid -> String -> Maybe Placement
checkPosition [] _ _ _ _ = Nothing
checkPosition os i j grid ls 
             | length allDirections == 1 = head allDirections 
             | otherwise = Nothing
             where allDirections = [ Just ((j,i),fromJust (checkOrientation o i j grid ls)) | o <- os, checkOrientation o i j grid ls /= Nothing]

checkOrientation :: Orientation -> Int -> Int -> WordSearchGrid -> String -> Maybe Orientation
checkOrientation o _ _ _ [] = Just o
checkOrientation Forward i j grid (l:ls) 
              | j == len = Nothing
              | grid !! i !! (j+1) == l = checkOrientation Forward i (j+1) grid ls 
              | grid !! i !! (j+1) == '*' = checkOrientation Forward i (j+1) grid ls 
              | otherwise = Nothing
              where len = (length grid) - 1
checkOrientation Back i j grid (l:ls)
              | j == 0 = Nothing
              | grid !! i !! (j-1) == l = checkOrientation Back i (j-1) grid ls 
              | grid !! i !! (j-1) == '*' = checkOrientation Back i (j-1) grid ls 
              | otherwise = Nothing
checkOrientation Up i j grid (l:ls)
              | i == 0 = Nothing
              | grid !! (i-1) !! j == l = checkOrientation Up (i-1) j grid ls 
              | grid !! (i-1) !! j == '*' = checkOrientation Up (i-1) j grid ls 
              | otherwise = Nothing
checkOrientation Down i j grid (l:ls)
              | i == len = Nothing
              | grid !! (i+1) !! j == l = checkOrientation Down (i+1) j grid ls 
              | grid !! (i+1) !! j == '*' = checkOrientation Down (i+1) j grid ls 
              | otherwise = Nothing
              where len = (length grid) - 1
checkOrientation UpForward i j grid (l:ls)
              | (i == 0) || (j == len) = Nothing
              | grid !! (i-1) !! (j+1) == l = checkOrientation UpForward (i-1) (j+1) grid ls 
              | grid !! (i-1) !! (j+1) == '*' = checkOrientation UpForward (i-1) (j+1) grid ls 
              | otherwise = Nothing
              where len = (length grid) - 1
checkOrientation UpBack i j grid (l:ls)
              | (i == 0) || (j == 0) = Nothing
              | grid !! (i-1) !! (j-1) == l = checkOrientation UpBack (i-1) (j-1) grid ls
              | grid !! (i-1) !! (j-1) == '*' = checkOrientation UpBack (i-1) (j-1) grid ls  
              | otherwise = Nothing
checkOrientation DownForward i j grid (l:ls)
              | (i == len) || (j == len) = Nothing
              | grid !! (i+1) !! (j+1) == l = checkOrientation DownForward (i+1) (j+1) grid ls 
              | grid !! (i+1) !! (j+1) == '*' = checkOrientation DownForward (i+1) (j+1) grid ls 
              | otherwise = Nothing
              where len = (length grid) - 1
checkOrientation DownBack i j grid (l:ls)
              | (i == len) || (j == 0) = Nothing
              | grid !! (i+1) !! (j-1) == l = checkOrientation DownBack (i+1) (j-1) grid ls
              | grid !! (i+1) !! (j-1) == '*' = checkOrientation DownBack (i+1) (j-1) grid ls  
              | otherwise = Nothing
              where len = (length grid) - 1

fromJust :: Maybe a -> a
fromJust Nothing = error "The value should not be equal to Nothing"
fromJust (Just a) = a

-- Two examples for you to try out, the first of which is in the instructions

exGrid1'1 = [ "HAGNIRTSH" , "SACAGETAK", "GCSTACKEL","MGHKMILKI","EKNLETGCN","TNIRTLETE","IRAAHCLSR","MAMROSAGD","GIZKDDNRG" ] 
exWords1'1 = [ "HASKELL","STRING","STACK","MAIN","METHOD"]

exGrid1'2 = ["ROBREUMBR","AURPEPSAN","UNLALMSEE","YGAUNPYYP","NLMNBGENA","NBLEALEOR","ALRYPBBLG","NREPBEBEP","YGAYAROMR"]
exWords1'2 = [ "BANANA", "ORANGE", "MELON", "RASPBERRY","APPLE","PLUM","GRAPE" ]

exWords = [ "METHODe","STACK","MAIN" ]
-- Challenge 2 --

createWordSearch :: [ String ] -> Double -> IO WordSearchGrid
createWordSearch [] 0 = return [[]]
createWordSearch _ 0 = error "The maximum density can not be zero when we have words to hide."
createWordSearch ws d = do
                let allChars = Set.toList $ foldl Set.union Set.empty (map Set.fromList ws)
                let lenLongestWord = maximum (map length ws)
                let gridSize = findGridSize (length allChars) lenLongestWord d
                let startGrid = replicate gridSize (replicate gridSize '*')

                g <- getStdGen
                let halfGrid = tryPosition g ws startGrid
               
                g <- getStdGen
                let finalGrid = genRandBackgrnd g 0 0 allChars halfGrid
                let solvedGrid = [ s | s <- map snd (solveWordSearch ws finalGrid), s == Nothing]

                if length solvedGrid /= 0
                 then createWordSearch ws d
                else return finalGrid
               
--                (putStr $ unlines $ finalGrid)

findGridSize :: Int -> Int -> Double -> Int
findGridSize numChars size d = 
            if (fromIntegral numChars) / (fromIntegral (size ^ 2)) > d
              then findGridSize numChars (size + 1) d
            else size

getRandIndxs :: RandomGen g => g -> Int -> ((Int,Int), g)
getRandIndxs g size = ((fst $ fstRand, fst $ sndRand), snd $ sndRand) 
              where fstRand = randomR (0, size - 1) g
                    sndRand = randomR (0, size - 1) (snd $ fstRand)

tryPosition :: RandomGen g => g -> [String] -> WordSearchGrid -> WordSearchGrid
tryPosition _ [] grid = grid
tryPosition g (w:ws) grid
          | (i > marg && i < (size - marg)) && (j > marg && j < (size - marg)) = tryPosition newG (w:ws) grid
          | (currPos /= '*') && (currPos /= head w) = tryPosition newG (w:ws) grid
          | newGrid == Nothing = tryPosition newG (w:ws) grid
          | otherwise = tryPosition newG ws (fromJust newGrid)
          
          where i = fst $ fst $ getRandIndxs g size
                j = snd $ fst $ getRandIndxs g size 
                currPos = (grid !! i) !! j
                newG = snd $ getRandIndxs g size
                size = length grid
                marg = size - (length w)
                newGrid = tryOrientation newG i j w grid

getRandOr :: RandomGen g => g -> [Orientation] -> (Orientation,g)
getRandOr g avOr = (avOr !! (fst rand), snd rand)
         where rand = randomR (0, length avOr - 1) g

-- Checks if the word can be put on the random direction
tryOrientation :: RandomGen g => g -> Int -> Int -> String -> WordSearchGrid -> Maybe WordSearchGrid
tryOrientation g i j w grid 
              | length avOr == 0 = Nothing
              | otherwise = Just (putWord i j (fromJust $ checkOrientation randOr i j grid (tail w)) w grid)

              where avOr = [ o | o <- [Forward, Back, Up, Down, UpForward, UpBack, DownForward, DownBack], checkOrientation o i j grid (tail w) /= Nothing]
                    randOr = fst $ getRandOr g avOr
                    newG = snd $ getRandOr g avOr                   

putWord :: Int -> Int -> Orientation -> String -> WordSearchGrid -> WordSearchGrid
putWord _ _ _ [] grid = grid
putWord i j Forward (l:ls) grid = putWord i (j+1) Forward ls (replace i j l grid)
putWord i j Back (l:ls) grid = putWord i (j-1) Back ls (replace i j l grid)
putWord i j Up (l:ls) grid = putWord (i-1) j Up ls (replace i j l grid)
putWord i j Down (l:ls) grid = putWord (i+1) j Down ls (replace i j l grid)
putWord i j UpForward (l:ls) grid = putWord (i-1) (j+1) UpForward ls (replace i j l grid)
putWord i j UpBack (l:ls) grid = putWord (i-1) (j-1) UpBack ls (replace i j l grid)
putWord i j DownForward (l:ls) grid = putWord (i+1) (j+1) DownForward ls (replace i j l grid)
putWord i j DownBack (l:ls) grid = putWord (i+1) (j-1) DownBack ls (replace i j l grid)

genRandLett :: RandomGen g => g -> [Char] -> (Char, g)
genRandLett g allChars = (allChars !! randLett, newG)
           where randLett = fst $ randomR (0, length allChars - 1) g
                 newG = snd $ randomR (0, length allChars - 1) g

genRandBackgrnd :: RandomGen g => g -> Int -> Int -> [Char] -> WordSearchGrid -> WordSearchGrid
genRandBackgrnd g i j allChars grid 
               | grid !! i !! j == '*' = 
                     if i == size && j == size 
                        then newGrid
                     else if j == size 
                            then genRandBackgrnd newG (i+1) 0 allChars newGrid
                          else genRandBackgrnd newG i (j+1) allChars newGrid
               | otherwise = 
                     if i == size && j == size 
                        then grid
                     else if j == size 
                            then genRandBackgrnd g (i+1) 0 allChars grid
                          else genRandBackgrnd g i (j+1) allChars grid
               
               where newL = fst $ genRandLett g allChars
                     newG = snd $ genRandLett g allChars
                     size = length grid - 1
                     newGrid = replace i j newL grid
                     
replace :: Int -> Int -> Char -> WordSearchGrid -> WordSearchGrid
replace 0 j newL ((l:ls):ws) = replaceInRow j newL (l:ls) : ws
replace i j newL (w:ws) = w : replace (i-1) j newL ws

replaceInRow :: Int -> Char -> [Char] -> [Char]
replaceInRow 0 newL (l:ls) = newL : ls
replaceInRow j newL (l:ls) = l : replaceInRow (j-1) newL ls        


exGrid = ["*******","*******","*******","*******","*******","*******","*******"]



--Challenge 4
