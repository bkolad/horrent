module TubeTest where

import qualified Data.ByteString.Char8 as BC
import qualified PeersControlerSimulator as S
import qualified Types as TP
import qualified InterpretST as IPST
import qualified Data.List as L
import qualified Data.ByteString as B
import qualified Test.Tasty.HUnit as THU
import qualified Test.Tasty.QuickCheck as QC
import Control.Applicative
import Test.Tasty




--2593,1573

pSizeG = 2593--3*16384

fileG = BC.pack (concat $ map show [0.. 1573])


checkIfCorrect =
    let pieces =  fst $ S.runSimulation pSizeG fileG
        cont = L.foldl' (\acc (x, b) -> B.append acc b) B.empty pieces
    in cont == fileG


-- (49827,44663)

recFile (pieceSize, fileLS) =
    let file = BC.pack (concat $ map show [0.. fileLS])
        pieces =  fst $ S.runSimulation pieceSize file
        cont = L.foldl' (\acc (x, b) -> B.append acc b) B.empty pieces
    in  cont == file

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
