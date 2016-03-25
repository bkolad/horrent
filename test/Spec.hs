import qualified BencodeParser as BP
import Control.Monad.Except (ExceptT, liftIO, runExceptT)
import qualified System.Directory as Dir
import qualified Data.List as L
import Test.HUnit

pathToTorrents = "TestTorrents/"

torrentContent :: String -> IO (Either (String, String) (String, BP.BEncode))
torrentContent torrent = do
    content <- runExceptT $ BP.parseFromFile (pathToTorrents ++torrent)
    case content of
        Left s -> return $ Left (torrent, s)
        Right r -> return $ Right (torrent, r)


pieceSizeNotZero :: (String, BP.BEncode) -> Assertion
pieceSizeNotZero (torrent, content) =
    let notZ = (/=0) <$> (BP.piceSize content)
        msg = "Piece has size 0 for " ++ torrent
    in assertEqual msg (Right True) notZ


convertToBsAndParseBack :: (String, BP.BEncode) -> Assertion
convertToBsAndParseBack (torrent, content) =
    let recContent = BP.parseFromBS (BP.toByteString content)
        msg = "Converting error for " ++ torrent
    in assertEqual msg (Right content) recContent


testAllTorrents = do
    allFilles <- Dir.listDirectory pathToTorrents
    let allTorrents = filter (L.isInfixOf ".torrent") allFilles
    torrentList <- sequence <$> (traverse torrentContent allTorrents)

    case torrentList of
        Left err ->
            let msgErr = "Parsing problem " ++ (show err)
            in runTestTT $ TestCase $ assertFailure msgErr
        Right tL ->
            let sizeNotZeroTests = TestList $ map (TestCase . pieceSizeNotZero) tL
                convTest = TestList $ map (TestCase . convertToBsAndParseBack) tL
                tests = [TestLabel "Size Not Zero" sizeNotZeroTests
                        ,TestLabel "Converter Test" convTest]

            in runTestTT $ TestList tests
