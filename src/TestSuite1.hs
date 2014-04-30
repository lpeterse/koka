module TestSuite1 ( tests ) where

import Distribution.TestSuite

import Kind.Assumption

tests :: IO [Test]
tests = return [ Test test1 ]
  where
    test1 = TestInstance
      { run       = return $ Finished
                           $ if kgammaIsEmpty kgammaEmpty
                               then Pass
                               else Fail "kgammaEmpty is not empty"
      , name      = "Kind.Assumption.isEmpty"
      , tags      = []
      , options   = []
      , setOption = \_ _ -> Right test1
      }
