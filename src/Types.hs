{-# LANGUAGE NoMonomorphismRestriction #-}

module Types  ( GlobalPiceInfo
              , newGlobalBitField
              , PiceInfo (..)
              , SizeInfo (..)
            --  , PeerStatus (..)
              , PeerException (..)
              , HashInfo
              , liftEither
              , ExceptT
              , liftIO
              , runExceptT
              , hashInfoFromList
              , NumberOfPieces
              , NormalPieceSize
              , LastPieceSize
              , Perhaps
              , showGlobal
              , getSizeData
              ) where


import qualified Control.Concurrent.STM.TArray as TA
import qualified Data.ByteString as B

import qualified Data.Sequence as Seq
import Control.Concurrent.STM
import Data.Array.MArray
import Control.Monad.Trans.Except
import Control.Monad.IO.Class (liftIO)
import qualified Network as N
import Control.Exception



data PeerException = PeerException N.HostName (Maybe Int)
    --deriving Typeable
instance Show PeerException where
    show (PeerException n i) = "Peer Exception " ++ n ++ " "++ (show i)

instance Exception PeerException



type NumberOfPieces = Int
type NormalPieceSize = Int
type LastPieceSize = Int

data SizeInfo = SizeInfo { numberOfPieces :: NumberOfPieces
                         , normalPieceSize :: NormalPieceSize
                         , lastPieceSize :: LastPieceSize
                         } deriving Show

type Perhaps a = Either String a

{--
data PeerStatus a = PeerOK a
                  | PeerTimeOut N.HostName
                  | PeerError N.HostName

--}
data PiceInfo = Done
              | InProgress
              | NotHave
              | Registered
              | TimeOut --N.HostName
  deriving (Show, Eq)

type GlobalPiceInfo = TA.TArray Int PiceInfo
--    deriving Show

type HashInfo = Seq.Seq B.ByteString

showGlobal :: GlobalPiceInfo -> IO ([(Int, PiceInfo)])
showGlobal global = atomically $ getAssocs global



hashInfoFromList :: [B.ByteString] -> HashInfo
hashInfoFromList = Seq.fromList


newGlobalBitField ::Int -> IO GlobalPiceInfo
newGlobalBitField size = atomically $ newArray (0, size-1) NotHave





liftEither :: Either e a -> ExceptT e IO a
liftEither = ExceptT . return


getSizeData :: Int
            -> Int
            -> SizeInfo
getSizeData torrentSize pieceSize =
  let tSize = fromIntegral torrentSize
      pSize = fromIntegral pieceSize
      numberOfPieces = ceiling $ tSize / pSize
      lps = tSize `mod` pSize
      lastPieceSize = if lps == 0 then  pSize else lps
  in SizeInfo numberOfPieces pSize lastPieceSize
