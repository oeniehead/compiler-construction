definition module Spec

// Testing framework made by Markus Klinik

import StdOverloaded
import GenPrint

:: Testcase
  = Testcase String TestResult
  | IOTestcase String (*World -> *(TestResult, *World))
:: TestResult = Passed | Failed String

(shouldBe) :: a a -> TestResult | == a & toString a
(shouldBeL) :: [a] [a] -> TestResult | == a & toString a
(shouldBe_) :: a a -> TestResult | == a & gPrint{|*|} a
(shouldSatisfy) :: a (a -> Bool) -> TestResult | toString a
assert :: Bool -> TestResult

// Evaluate tests and print results to stdout
runTests :: [Testcase] (Testcase -> Bool) *World -> *World

// How to use:
//
// Start = runTests
//     [ Testcase "toColor o fromColor" $ assert $
//         and [ c == toColor (fromColor c) \\ c <- [Red, Yellow, Blue]]
//     , Testcase "show for Color Red" $ StringList (show Red) shouldBe TestableList ["Red"]
//     ]
