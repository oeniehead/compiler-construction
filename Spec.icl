implementation module Spec

// Testing framework made by Markus Klinik

import StdEnv
import GenPrint

(shouldBe) :: a a -> TestResult
  | == a
  & toString a
(shouldBe) x y = shouldBeImpl x y toString

(shouldBe_) :: a a -> TestResult | == a & gPrint{|*|} a
(shouldBe_) x y = shouldBeImpl x y printToString

(shouldBeL) :: [a] [a] -> TestResult | == a & toString a
(shouldBeL) x y = (TestableList x) shouldBe (TestableList y)

shouldBeImpl :: a a (a -> String) -> TestResult | == a
shouldBeImpl x y print
  | x == y    = Passed
  | otherwise = Failed ("\n expected: '" +++ print y +++
                       "'\n  but got: '" +++ print x +++ "'")

(shouldSatisfy) :: a (a -> Bool) -> TestResult | toString a
(shouldSatisfy) x p
  | p x       = Passed
  | otherwise = Failed ("\n " +++ toString x +++ " doesn't satisfy the condition.")

assert :: Bool -> TestResult
assert True  = Passed
assert False = Failed "failed"

unlines :: [String] -> String
unlines xs = foldr (\x y = x +++ "\n" +++ y) "" xs

runTests :: [Testcase] (Testcase -> Bool) *World -> *World
runTests tests filtr world
  # wantedTests = filter filtr tests
  # (output, world) = runTests` wantedTests filtr "" world
  # (console, world) = stdio world
  # console = fwrites (if (output == "")
      (toString (length wantedTests) +++ " test(s) passed\n")
      output) console
  # (ok, world) = fclose console world
  | not ok = abort "Cannot close console\n"
  | otherwise = world

runTests` :: [Testcase] (Testcase -> Bool) String *World -> (String, *World)
runTests` [] _ output world = (output, world)
runTests` [(IOTestcase description test):rest] filtr output world
  # (result, world) = test world
  = case result of
      Passed -> runTests` rest filtr output world
      (Failed reason) ->
        runTests` rest filtr (output +++ description +++ ": " +++ reason +++ "\n") world
runTests` [(Testcase description test):rest] filtr output world
  # (result, world) = (test, world)
  = case result of
      Passed -> runTests` rest filtr output world
      (Failed reason) ->
        runTests` rest filtr (output +++ description +++ ": " +++ reason +++ "\n") world

listToString :: (a -> String) [a] -> String
listToString f xs = listToString` f xs (\x = "[" +++ x)
  where
    listToString` :: (a -> String) [a] (String -> String) -> String
    listToString` _ []     c = c "]"
    listToString` f [s:[]] c = c (listToString` f [] (\x = f s +++ x))
    listToString` f [s:ss] c = c (listToString` f ss (\x = f s +++ "," +++ x))

:: TestableList a = TestableList [a]

instance toString (TestableList a) | toString a
  where toString (TestableList l) = listToString toString l

instance == (TestableList a) | == a
  where (==) (TestableList l) (TestableList r) = l == r
