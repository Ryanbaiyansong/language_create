module Main where

import Test.Tasty

import CheckTest
import EvalTest
import ParserTest

  main = 
    do 
        setEnv "TASTY_TIMEOUT" "40s"
        setEnv "TASTY_QUICKCHECK_TESTS" "1000" --TODO: I never trust less than 10000
        setEnv "TASTY_QUICKCHECK_MAX_SIZE" "50"
        defaultMain allTests
        unsetEnv "TASTY_TIMEOUT"
        unsetEnv "TASTY_QUICKCHECK_TESTS"
        unsetEnv "TASTY_QUICKCHECK_MAX_SIZE"

  allTests =
    testGroup
      "allTests"
      [
        errorTest,
        tests,
        operatorTest,
        stdLibTest,
        partests
      ]
