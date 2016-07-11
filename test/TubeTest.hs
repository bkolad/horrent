module TubeTest where

import qualified Data.ByteString.Char8 as BC
import qualified PeersControlerSimulator as S
import qualified Types as TP
import qualified Interpreter.ST as IPST
import qualified Data.List as L
import qualified Data.ByteString as B
import qualified Test.Tasty.HUnit as THU
import qualified Test.Tasty.QuickCheck as QC
import Control.Applicative
import Test.Tasty
import qualified Data.Sequence as S






pSizeG = 2593--3*16384

fileG = BC.pack (concat $ map show [0.. 1573])


recFile (pieceSize, fileLS) =
    let file = BC.pack (concat $ map show [0.. fileLS])
        (pieces, state) =  S.runSimulation pieceSize file
        gl = S.findIndicesL (\a-> a /= TP.Done) (IPST.global state)
        cont = L.foldl' (\acc (x, b) -> B.append acc b) B.empty pieces
    in  (cont == file) && (gl == [])

pSizeGen :: QC.Gen Int
pSizeGen = QC.elements [100.. 1000]

fileLSGen :: QC.Gen Int
fileLSGen = QC.elements [100.. 1000]

pp = liftA2 (,) pSizeGen fileLSGen

qcProp :: QC.Property
qcProp  = QC.forAll pp recFile

qcTest :: TestTree
qcTest =  localOption (QC.QuickCheckTests 1000) $ QC.testProperty "QC test " qcProp


allTests = testGroup "PeerTests" [qcTest]

main = defaultMain allTests
