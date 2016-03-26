module ParserTests (testAllTorrents) where

import qualified BencodeParser as BP
import Control.Monad.Except (ExceptT, liftIO, runExceptT)
import qualified System.Directory as Dir
import qualified Data.List as L
import qualified Test.Tasty.HUnit as THU
import Test.Tasty

torrents = "/test/TestTorrents/"

getTorrentsDir = do
    currentDir <- Dir.getCurrentDirectory
    return $ currentDir ++ torrents



torrentContent :: String -> IO (Either (String, String) (String, BP.BEncode))
torrentContent torrent = do
    torrentsDir <- getTorrentsDir
    content <- runExceptT $ BP.parseFromFile (torrentsDir ++ torrent)
    case content of
        Left s -> return $ Left (torrent, s)
        Right r -> return $ Right (torrent, r)


pieceSizeNotZero :: (String, BP.BEncode) -> THU.Assertion
pieceSizeNotZero (torrent, content) =
    let notZ = (/=0) <$> (BP.piceSize content)
        msg = "Piece has size 0 for " ++ torrent
    in THU.assertEqual msg (Right True) notZ


convertToBsAndParseBack :: (String, BP.BEncode) -> THU.Assertion
convertToBsAndParseBack (torrent, content) =
    let recContent = BP.parseFromBS (BP.toByteString content)
        msg = "Converting error for " ++ torrent
    in THU.assertEqual msg (Right content) recContent


testAllTorrents :: IO TestTree
testAllTorrents = do
    torrentsDir <- getTorrentsDir
    allFilles <- Dir.listDirectory torrentsDir
    let allTorrents = filter (L.isInfixOf ".torrent") allFilles
    torrentList <- sequence <$> (traverse torrentContent allTorrents)

    case torrentList of
        Left err ->
            let msgErr = "Parsing problem " ++ (show err)
            in  return $ THU.testCase "Failure" (THU.assertFailure msgErr)
        Right tL ->
            let tN = "Test Name"
                sizeNotZeroTests =
                    testGroup "Piece size not zero" $
                        map (THU.testCase tN . pieceSizeNotZero) tL

                convTest =
                    testGroup "Convert to BS and Back" $
                        map (THU.testCase tN . convertToBsAndParseBack) tL

                tests = testGroup "All Tests"
                        [sizeNotZeroTests ,convTest]

            in return tests


main = testAllTorrents >>= defaultMain
