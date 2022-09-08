{-# LANGUAGE DeriveGeneric #-}
-- comp2209 Functional Programming Challenges
-- (c) University of Southampton 2020
-- Skeleton code to be updated with your solutions
-- The dummy functions here simply return an arbitrary value that is usually wrong 

-- DO NOT MODIFY THE FOLLOWING LINES OF CODE
module Challenges (WordSearchGrid,Placement,Posn,Orientation(..),solveWordSearch, createWordSearch,
    LamMacroExpr(..),LamExpr(..),prettyPrint, parseLamMacro,
    cpsTransform,innerRedn1,outerRedn1,compareInnerOuter) where

-- Import standard library and parsing definitions from Hutton 2016, Chapter 13
-- We import System.Random - make sure that your installation has it installed - use stack ghci and stack ghc
import Data.Char
import Control.Monad
import Data.List
import GHC.Generics (Generic,Generic1)
import Control.DeepSeq
import System.IO
import System.Random
import Parsing
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
         | otherwise = head answer
         where answer = [ o | o <- options, o /= Nothing]
               options = [ checkPosition [Forward,Back,Up,Down,UpForward,UpBack,DownForward,DownBack] i j grid ls  | i <- [0..len], j <- [0..len], grid !! i !! j == l]
               len = length grid - 1

checkPosition :: [Orientation] -> Int -> Int -> WordSearchGrid -> String -> Maybe Placement
checkPosition [] _ _ _ _ = Nothing
checkPosition (o:os) i j grid ls 
             | checkOrientation o i j grid ls /= Nothing = Just ((j,i),fromJust (checkOrientation o i j grid ls))
             | otherwise = checkPosition os i j grid ls

checkOrientation :: Orientation -> Int -> Int -> WordSearchGrid -> String -> Maybe Orientation
checkOrientation o _ _ _ [] = Just o
checkOrientation Forward i j grid (l:ls) 
              | j == len = Nothing
              | grid !! i !! (j+1) == l = checkOrientation Forward i (j+1) grid ls 
              | otherwise = Nothing
              where len = (length grid) - 1
checkOrientation Back i j grid (l:ls)
              | j == 0 = Nothing
              | grid !! i !! (j-1) == l = checkOrientation Back i (j-1) grid ls 
              | otherwise = Nothing
checkOrientation Up i j grid (l:ls)
              | i == 0 = Nothing
              | grid !! (i-1) !! j == l = checkOrientation Up (i-1) j grid ls 
              | otherwise = Nothing
checkOrientation Down i j grid (l:ls)
              | i == len = Nothing
              | grid !! (i+1) !! j == l = checkOrientation Down (i+1) j grid ls 
              | otherwise = Nothing
              where len = (length grid) - 1
checkOrientation UpForward i j grid (l:ls)
              | (i == 0) || (j == len) = Nothing
              | grid !! (i-1) !! (j+1) == l = checkOrientation UpForward (i-1) (j+1) grid ls 
              | otherwise = Nothing
              where len = (length grid) - 1
checkOrientation UpBack i j grid (l:ls)
              | (i == 0) || (j == 0) = Nothing
              | grid !! (i-1) !! (j-1) == l = checkOrientation UpBack (i-1) (j-1) grid ls 
              | otherwise = Nothing
checkOrientation DownForward i j grid (l:ls)
              | (i == len) || (j == len) = Nothing
              | grid !! (i+1) !! (j+1) == l = checkOrientation DownForward (i+1) (j+1) grid ls 
              | otherwise = Nothing
              where len = (length grid) - 1
checkOrientation DownBack i j grid (l:ls)
              | (i == len) || (j == 0) = Nothing
              | grid !! (i+1) !! (j-1) == l = checkOrientation DownBack (i+1) (j-1) grid ls 
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


-- Challenge 2 --

createWordSearch :: [ String ] -> Double -> IO WordSearchGrid
--createWordSearch _ _ = return []
createWordSearch [] 0 = return []
createWordSearch _ 0 = error "The maximum density can not be zero when we have words to hide."
--createWordSearch (w:ws) d = findRandPosition w [[]]

--findRandPosition :: String -> WordSearchGrid

--- Convenience functions supplied for testing purposes
createAndSolve :: [ String ] -> Double -> IO [ (String, Maybe Placement) ]
createAndSolve words maxDensity =   do g <- createWordSearch words maxDensity
                                       let soln = solveWordSearch words g
                                       printGrid g
                                       return soln

printGrid :: WordSearchGrid -> IO ()
printGrid [] = return ()
printGrid (w:ws) = do putStrLn w
                      printGrid ws



-- Challenge 3 --

prettyPrint :: LamMacroExpr -> String
prettyPrint (LamDef [] e) = transform [] e
prettyPrint (LamDef ms e) = (printAllMacros ms) ++ (transform ms e)


printAllMacros :: [(String,LamExpr)] -> String
printAllMacros [] = ""
printAllMacros (m:ms) = "def " ++ ((fst m) ++ " = " ++ (transform [] (snd m)))  ++ " in " ++ printAllMacros ms

transform :: [(String,LamExpr)] -> LamExpr -> String
transform _ (LamMacro s) = s
transform ms (LamVar v)  
         | checkForMacro ms e == Nothing = "x" ++ (show v)
         | otherwise = fromJust $ checkForMacro ms e
         where e = LamVar v
transform ms (LamAbs v e) 
         | checkForMacro ms (LamAbs v e) == Nothing = "\\" ++ "x" ++ (show v) ++ "-" ++ ">" ++ transform ms e
         | otherwise = (fromJust $ checkForMacro ms (LamAbs v e))
transform ms (LamApp (LamAbs v e) (LamApp e1 e2)) 
         | checkForMacro ms e == Nothing = "(" ++ (transform ms fst) ++ ") " ++ "(" ++ (transform ms snd) ++ ")"
         | otherwise = fromJust $ checkForMacro ms e
         where e = LamApp (LamAbs v e) (LamApp e1 e2)
               fst = LamAbs v e
               snd = LamApp e1 e2
transform ms (LamApp (LamAbs v e) snd) 
         | checkForMacro ms e == Nothing = "(" ++ (transform ms fst) ++ ") " ++ transform ms snd
         | otherwise = fromJust $ checkForMacro ms e
         where curre = LamApp fst snd
               fst = LamAbs v e
transform ms (LamApp fst (LamApp e1 e2)) 
         | checkForMacro ms e == Nothing = transform ms fst ++ " (" ++ (transform ms snd) ++ ")"
         | otherwise = fromJust $ checkForMacro ms e
         where e = LamApp fst snd
               snd = LamApp e1 e2
transform ms (LamApp fst snd)
         | checkForMacro ms e == Nothing = transform ms fst ++ " " ++ transform ms snd
         | otherwise = fromJust $ checkForMacro ms e
         where e = LamApp fst snd

checkForMacro :: [(String,LamExpr)] -> LamExpr -> Maybe String
checkForMacro [] e = Nothing
checkForMacro (m:ms) e
             | snd m == e = Just (" " ++ fst m ++ " ")
             | otherwise = checkForMacro ms e
             

-- examples in the instructions
ex3'1 = LamDef [] (LamApp (LamAbs 1 (LamVar 1)) (LamAbs 1 (LamVar 1)))
ex3'2 = LamDef [] (LamAbs 1 (LamApp (LamVar 1) (LamAbs 1 (LamVar 1))))
ex3'3 = LamDef [ ("F", LamAbs 1 (LamVar 1) ) ] (LamAbs 2 (LamApp (LamVar 2) (LamMacro "F")))
ex3'4 = LamDef [ ("F", LamAbs 1 (LamVar 1) ) ] (LamAbs 2 (LamApp (LamAbs 1 (LamVar 1)) (LamVar 2))) 


-- Challenge 4 --
--  TODO: Scobes in the macro (not reading them); Closed terms in the macro
parseLamMacro :: String -> Maybe LamMacroExpr
parseLamMacro s
             | macro == Nothing = Nothing
             | expression == Nothing = Nothing
             | checkRepMacros result == True = Nothing 
             | otherwise = Just (LamDef result (fromJust $ expression))
             where macro = parseMacro s
                   result = fst $ fromJust $ macro
                   remainder = snd $ fromJust $ macro
                   expression = parseLamExpr remainder

parseLamExpr :: String -> Maybe LamExpr
parseLamExpr s
            | remainder == "" = Just result
            | length nextApp /= 0 = recursiveLamApp result nextResult nextRemainder
            | otherwise = Nothing
            where result = fst $ head $ parse expr s
                  remainder = snd $ head $ parse expr s
                  nextApp = parse applicationHalf remainder
                  nextResult = fst $ head $ nextApp
                  nextRemainder = snd $ head $ nextApp
         
parseMacro :: String -> Maybe ([(String, LamExpr)],String)
parseMacro s
                 | parse (string "def") s == [] = Just ([], s)
                 | length d == 0 = Nothing
                 | otherwise = Just (head d)
                 where d = parse (some def) s
                       result = fst $ head $ d
                       remainder = snd $ head $ d

checkRepMacros :: [(String, LamExpr)] -> Bool
checkRepMacros l
              | length stringList /= length set = True
              | otherwise = False 
              where stringList = map fst l
                    set = Set.fromList stringList
recursiveLamApp :: LamExpr -> LamExpr -> String -> Maybe LamExpr
recursiveLamApp preResult result remainder
               | length nextApp /= 0 = recursiveLamApp (LamApp preResult result) nextResult nextRemainder
               | remainder == "" = Just (LamApp preResult result)
               | otherwise = Nothing
               where nextApp = parse applicationHalf remainder
                     nextResult = fst $ head $ nextApp
                     nextRemainder = snd $ head $ nextApp

def :: Parser (String, LamExpr)
def = do string "def "
         mn <- macroName
         string " = "
         e <- abstraction
         string " in "
         return (mn, e)

expr :: Parser LamExpr
expr = do e <- application <|> abstraction <|> factor
          return e

application :: Parser LamExpr
application = do a1 <- factor <|> abstraction
                 sat isSpace
                 a2 <- factor <|> abstraction
                 return (LamApp a1 a2)

applicationHalf :: Parser LamExpr
applicationHalf = do sat isSpace
                     e <- factor <|> abstraction
                     return e 

abstraction :: Parser LamExpr
abstraction = do v <- varInt
                 string " -> "
                 e <- expr
                 return (LamAbs v e)      

factor :: Parser LamExpr
factor = do char '('
            e <- expr
            char ')'
            return e
        <|> var
        <|> macroExpr

macroExpr :: Parser LamExpr
macroExpr = do uc <- some uChar
               return (LamMacro uc)

macroName :: Parser String
macroName = do uc <- some uChar
               return uc

uChar :: Parser Char
uChar = do uc <- upper
           return uc

var :: Parser LamExpr
var = do char 'x'
         ds <- digits
         return (LamVar ds)

varInt :: Parser Int
varInt = do string "\\"
            char 'x'
            ds <- digits
            return ds

digits :: Parser Int
digits = do d <- some digit
            return (read d :: Int)

-- Challenge 5

--(LamDef [ ("F", LamAbs 1 (LamVar 1) ) ] (LamAbs 2 (LamApp (LamVar 2) (LamMacro "F"))))


cpsTransform :: LamMacroExpr -> LamMacroExpr
cpsTransform (LamDef [] e) = LamDef [] (fst $ transformExpr e maxNum)
            where usedNums = checkUsedNums (Set.empty) e
                  maxNum = Set.findMax usedNums
cpsTransform (LamDef ms e) = LamDef transMacros transExpr
            where macroExpr = map snd ms
                  usedNums = foldl checkUsedNums Set.empty (e : macroExpr)
                  maxNum = Set.findMax usedNums
                  allTrans = transformAll (ms ++ [("expr", e)])  maxNum
                  transMacros = init allTrans
                  transExpr = snd $ last $ allTrans
                  
transformAll :: [(String, LamExpr)] -> Int -> [(String, LamExpr)]
transformAll [] _ = []
transformAll (m:ms) maxNum = (fst m, fst transExpr) : transformAll ms newMaxNum
                  where transExpr = transformExpr (snd m) maxNum
                        newMaxNum = snd transExpr

                  
checkUsedNums :: Set Int -> LamExpr -> Set Int
checkUsedNums s (LamMacro str) = s
checkUsedNums s (LamVar v) = Set.insert v s
checkUsedNums s (LamAbs i e) = checkUsedNums s e
checkUsedNums s (LamApp e1 e2) = Set.union (checkUsedNums s e1) (checkUsedNums s e2)

transformExpr :: LamExpr -> Int -> (LamExpr,Int)
transformExpr (LamMacro s) prevIndx = (LamMacro s, prevIndx)
transformExpr (LamVar v) prevIndx = (LamAbs k (LamApp (LamVar k) (LamVar v)), k)
             where k = prevIndx + 1
transformExpr (LamAbs i e) prevIndx = ((LamAbs k (LamApp varK (LamAbs i convE))), currIndx)
             where k = prevIndx + 1
                   varK = LamVar k
                   convE = fst (transformExpr e k)
                   currIndx = snd (transformExpr e k)
transformExpr (LamApp e1 e2) prevIndx = ( (LamAbs k (LamApp convE1 (LamAbs f (LamApp convE2 (LamAbs e (LamApp (LamApp varF varE) varK)))))), e)
             where k = prevIndx + 1
                   varK = LamVar k
                   convE1 = fst (transformExpr e1 k)
                   f = snd (transformExpr e1 k) + 1
                   varF = LamVar f
                   convE2 = fst (transformExpr e2 f)
                   e = snd (transformExpr e2 f) + 1
                   varE = LamVar e
                   
                   


-- Examples in the instructions
exId =  LamAbs 1 (LamVar 1)
ex5'1 = (LamDef [] (LamApp (LamVar 1) (LamVar 2)))
ex5'2 = (LamDef [ ("F", exId) ] (LamVar 2) )
ex5'3 = (LamDef [ ("F", exId) ] (LamMacro "F") )
ex5'4 = (LamDef [ ("F", exId) ] (LamApp (LamMacro "F") (LamMacro "F")))


-- Challenge 6

-- innerRedn1 :: LamMacroExpr -> Maybe LamMacroExpr
-- innerRedn1 (LamDef ms e)
--       | outerRednEx1 ms e == Nothing = Nothing
--       | otherwise = Just (LamDef newMs newE)
--       where newE = snd $ fromJust $ outerRednEx1 ms e
--             newMs = fst $ fromJust $ outerRednEx1 ms e

-- innerRednEx1 :: [(String,LamExpr)] -> LamExpr -> Maybe ([(String,LamExpr)], LamExpr)
-- --Case with only one macro
-- innerRednEx1 ms (LamMacro mac) = Just ([], (snd $ head $ ms))
-- --Case with abstraction with macro 
-- innerRednEx1 ms (LamAbs x (LamMacro m')) = Just (newMs, (LamAbs x (snd mac)))
--         where mac = head $ [ m | m <- ms, fst m == m']
--               newMs = delete mac ms
-- --Normal abstraction
-- innerRednEx1 ms (LamAbs x e) = Just (ms, (LamAbs x e))
-- --Case with application with macro
-- innerRednEx1 ms (LamApp (LamMacro m') e2) = Just (newMs, (LamApp (snd mac) e2))
--         where mac = head $ [ m | m <- ms, fst m == m']
--               newMs = delete mac ms
-- --Second case with app with macro
-- innerRednEx1 ms (LamApp e1 (LamMacro m')) = Just (newMs, (LamApp e1 (snd mac)))
--         where mac = head $ [ m | m <- ms, fst m == m']
--               newMs = delete mac ms
-- --Case with macro inside an abstraction inside an application
-- innerRednEx1 ms (LamApp (LamAbs x (LamMacro m')) e2) = Just (newMs, (LamApp (LamAbs x (snd mac)) e2))
--         where mac = head $ [ m | m <- ms, fst m == m']
--               newMs = delete mac ms
-- --Second case of macro inside an abstraction
-- innerRednEx1 ms (LamApp (LamAbs x (LamMacro m')) e2) = Just (newMs, (LamApp (LamAbs x (snd mac)) e2))
--         where mac = head $ [ m | m <- ms, fst m == m']
--               newMs = delete mac ms
-- innerRednEx1 ms (LamApp (LamAbs x e1) e2) = Just (ms, subst e1 x e2)
-- innerRednEx1 ms (LamApp e1 e2) 
--         | outerRednEx1 ms e1 == Nothing = Nothing
--         | otherwise = Just (newMs, LamApp newE e2)
--         where newE = snd $ fromJust $ innerRednEx1 ms e1
--               newMs = fst $ fromJust $ innerRednEx1 ms e1


outerRedn1 :: LamMacroExpr -> Maybe LamMacroExpr
outerRedn1 (LamDef ms e)
      | outerRednEx1 ms e == Nothing = Nothing
      | otherwise = Just (LamDef newMs newE)
      where newE = snd $ fromJust $ outerRednEx1 ms e
            newMs = fst $ fromJust $ outerRednEx1 ms e

outerRednEx1 :: [(String,LamExpr)] -> LamExpr -> Maybe ([(String,LamExpr)], LamExpr)
--Case with only one macro
outerRednEx1 ms (LamMacro mac) = Just ([], (snd $ head $ ms))
--Case with abstraction with macro 
outerRednEx1 ms (LamAbs x (LamMacro m')) = Just (newMs, (LamAbs x (snd mac)))
        where mac = head $ [ m | m <- ms, fst m == m']
              newMs = delete mac ms
--Normal abstraction
outerRednEx1 ms (LamAbs x e) = Just (ms, (LamAbs x e))
--Case with application with macro
outerRednEx1 ms (LamApp (LamMacro m') e2) = Just (newMs, (LamApp (snd mac) e2))
        where mac = head $ [ m | m <- ms, fst m == m']
              newMs = delete mac ms
--Second case with app with macro
outerRednEx1 ms (LamApp e1 (LamMacro m')) = Just (newMs, (LamApp e1 (snd mac)))
        where mac = head $ [ m | m <- ms, fst m == m']
              newMs = delete mac ms
--Case with macro inside an abstraction inside an application
-- outerRednEx1 ms (LamApp (LamAbs x (LamMacro m')) e2) = Just (newMs, (LamApp (LamAbs x (snd mac)) e2))
--         where mac = head $ [ m | m <- ms, fst m == m']
--               newMs = delete mac ms
outerRednEx1 ms (LamApp (LamAbs x e1) e2) = Just (ms, subst ms e1 x e2)
outerRednEx1 ms (LamApp e1 e2) 
        | outerRednEx1 ms e1 == Nothing = Nothing
        | otherwise = Just (newMs, LamApp newE e2)
        where newE = snd $ fromJust $ outerRednEx1 ms e1
              newMs = fst $ fromJust $ outerRednEx1 ms e1

subst :: [(String,LamExpr)] -> LamExpr -> Int -> LamExpr -> LamExpr
subst ms (LamMacro m') y e = subst newMs newE y e
    where mac = head $ [ m | m <- ms, fst m == m']
          newE = snd mac
          newMs = delete mac ms
subst ms (LamVar x) y e 
    | x == y = e
    | otherwise = LamVar x
subst ms (LamAbs x e1) y e
    | x /= y && not (free x e) = LamAbs x (subst ms e1 y e)
    | x /= y && (free x e) = subst ms (LamAbs x' (subst ms e1 x (LamVar x'))) y e
    | otherwise = LamAbs x e1
    where x' = rename x e1
subst ms (LamApp e1 e2) y e = LamApp (subst ms e1 y e) (subst ms e2 y e)

rename :: Int -> LamExpr -> Int
rename x e
    | free (x+1) e = rename (x+1) e
    | otherwise = x + 1

free :: Int -> LamExpr -> Bool
free x (LamVar y) = x == y
free x (LamAbs y e)
    | x == y = False
    | x /= y = free x e
free x (LamApp e1 e2) = (free x e1) || (free x e2)

compareInnerOuter :: LamMacroExpr -> Int -> (Maybe Int,Maybe Int,Maybe Int,Maybe Int)
compareInnerOuter _ _ = (Nothing,Nothing,Nothing,Nothing) 

-- Examples in the instructions

-- (\x1 -> x1 x2)
ex6'1 = LamDef [] (LamAbs 1 (LamApp (LamVar 1) (LamVar 2)))

--  def F = \x1 -> x1 in F  
ex6'2 = LamDef [ ("F",exId) ] (LamMacro "F")

--  (\x1 -> x1) (\x2 -> x2)   
ex6'3 = LamDef [] ( LamApp exId (LamAbs 2 (LamVar 2)))

--  (\x1 -> x1 x1)(\x1 -> x1 x1)  
wExp = (LamAbs 1 (LamApp (LamVar 1) (LamVar 1)))
ex6'4 = LamDef [] (LamApp wExp wExp)

--  def ID = \x1 -> x1 in def FST = (\x1 -> λx2 -> x1) in FST x3 (ID x4) 
ex6'5 = LamDef [ ("ID",exId) , ("FST",LamAbs 1 (LamAbs 2 (LamVar 1))) ] ( LamApp (LamApp (LamMacro "FST") (LamVar 3)) (LamApp (LamMacro "ID") (LamVar 4)))

--  def FST = (\x1 -> λx2 -> x1) in FST x3 ((\x1 ->x1) x4))   
ex6'6 = LamDef [ ("FST", LamAbs 1 (LamAbs 2 (LamVar 1)) ) ]  ( LamApp (LamApp (LamMacro "FST") (LamVar 3)) (LamApp (exId) (LamVar 4)))

-- def ID = \x1 -> x1 in def SND = (\x1 -> λx2 -> x2) in SND ((\x1 -> x1) (\x2 -> x2)) ID
ex6'7 = LamDef [ ("ID",exId) , ("SND",LamAbs 1 (LamAbs 2 (LamVar 2))) ]  (LamApp (LamApp (LamMacro "SND") (LamApp wExp wExp) ) (LamMacro "ID") ) 

ex6'8 = LamDef [ ("ID",exId) ] (LamApp (LamAbs 2 (LamMacro"ID")) (LamVar 3))