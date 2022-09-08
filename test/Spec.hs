import Lib
--import HUnit.hs
import Test.QuickCheck
-- Challenge 1--

exGrid1'1 = [ "TQMSN","GRAOP","UWTUO","TAHTT","FLMHS" ] 
exWords1'1 = [ "SOUTH","MATH","HAT","SAW","STOP"]
answer1'1 = [("SOUTH",Just ((0,3),DownBackward)),("MATH",Just ((0,2),Down)),("HAT",Just ((3,2),BACK)),("SAW",Just ((0,3),DownBackward)),("STOP",Just ((4,4),UP))]

exGrid1'2 = [ "HAGNIRTSH" , "SACAGETAK", "GCSTACKEL","MGHKMILKI","EKNLETGCN","TNIRTLETE","IRAAHCLSR","MAMROSAGD","GIZKDDNRG" ] 
exWords1'2 = [ "HASKELL","STRING","STACK","MAIN","METHOD"]

exGrid1'3 = ["ROBREUMBR","AURPEPSAN","UNLALMSEE","YGAUNPYYP","NLMNBGENA","NBLEALEOR","ALRYPBBLG","NREPBEBEP","YGAYAROMR"]
exWords1'3 = [ "BANANA", "ORANGE", "MELON", "RASPBERRY","APPLE","PLUM","GRAPE" ]
 

assert :: Bool -> String -> String -> IO()
assert test passInfo failInfo = if test then putStrLn passInfo else putStrLn failInfo

-- test1'1 :: Test
-- test1'1 =  TestCase (asserEqual "for exGrid=" answer1 (solveWordSearch exWords1'1 exGrid1'1))

test1'1 = solveWordSearch exWords1'1 exGrid1'1

main :: IO Counts
main = do 
	putStrLn "========Test Starts========"
	putStrLn "Test 5x5 table: "
	assert (solveWordSearch exWords1'1 exGrid1'1 == answer1'1) "passed" "failed"
	putStrLn "========Test Ends========"
