module Calendarios (calendariosSuite) where
-- Tasty makes it easy to test your code. It is a test framework that can
-- combine many different types of tests into one suite. See its website for
-- help: <http://documentup.com/feuerbach/tasty>.
-- Hspec is one of the providers for Tasty. It provides a nice syntax for
-- writing tests. Its website has more info: <https://hspec.github.io>.
-- import Test.Tasty.Hspec
import              Test.Tasty                      (testGroup, TestTree)
-- import              Test.Tasty.SmallCheck           (forAll)
-- import qualified    Test.Tasty.SmallCheck       as  SC
-- import qualified    Test.Tasty.QuickCheck       as  QC
-- import              Test.SmallCheck.Series          (Serial)
import qualified    Test.Tasty.HUnit            as HU                         
import qualified    Library.Calendarios      as Cal


calendariosSuite :: TestTree
calendariosSuite = testGroup "Calendarios"
        [testDibEsCorrecto]
    
-- in Swallow: swallow = (++)
testDibEsCorrecto :: TestTree
testDibEsCorrecto = testGroup [testCase "Dibujo vacío es correcto" @? Cal.dibEsCorrecto [],
                     testCase "Dibujo correcto" @? Cal.dibEsCorrecto ["aaa","bbs","ccc"],
                     testCase "Dibujo incorrecto" @? Cal.dibEsCorrecto ["aaa","bbs","cccc"],
                     testCase "Dibujo incorrecto" @? Cal.dibEsCorrecto ["","bbs","cccc"]]

    
-- stringUtilsSuite :: TestTree
-- stringUtilsSuite = testGroup "StringUtils"
--     [ SC.testProperty "SC projectNameFromString idempotent" $
--             idempotent projectNameFromString


-- deeperIdempotent :: (Eq a, Show a, Serial m a) => (a -> a) -> SC.Property m
-- deeperIdempotent f = forAll $ SC.changeDepth1 (+1) $ \s -> f s == f (f s)