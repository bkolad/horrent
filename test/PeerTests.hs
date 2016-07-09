module PeerTests where

import qualified Test.Tasty.HUnit as THU
import qualified Test.Tasty.QuickCheck as QC
import Test.Tasty
import qualified Peer as P
import qualified Data.List as L

intLsToIntLs :: [Int] -> [Int]
intLsToIntLs ls = (P.bsToPieceLs . P.pieceLsToBS) ls

qcProp :: QC.Property
qcProp  = QC.forAll sortedPosLsNub
            (\ls -> intLsToIntLs ls == P.rmdups ls)

sortedPosLsNub :: QC.Gen [Int]
sortedPosLsNub = do
    l <-  QC.arbitrary
    return $ L.sort (abs <$>l)


qcTest :: TestTree
qcTest = QC.testProperty "QC test convert [Int] to BS and back " qcProp


huTest :: TestTree
huTest = THU.testCase "HUnit test" $
    let ls =[0,99,188]
        msg = "HU test convert [Int] to BS and back"
    in THU.assertEqual msg (intLsToIntLs ls) ls


allTests = testGroup "PeerTests" [qcTest, huTest]

main = defaultMain allTests
