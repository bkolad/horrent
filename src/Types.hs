{-# LANGUAGE NoMonomorphismRestriction #-}

module Types  ( GlobalPiceInfo
              , newGlobalBitField
              , PiceInfo (..)
              , SizeInfo (..)
              , FileInfo (..)
            --  , PeerStatus (..)
              , PeerException (..)
              , ExeptionType (..)
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
import Control.Monad.Trans.Except (ExceptT(..), runExceptT)
import Control.Monad.IO.Class (liftIO)
import qualified Network as N
import Control.Exception (Exception(..))



data PeerException = PeerException ExeptionType N.HostName (Maybe Int)

data ExeptionType = TimeOutException
                   | NetworkException
                   | NoDataException
                   | PacketParseException
                   | MsgNotSupportedException
                   | ChokeException
                   deriving Show


instance Show PeerException where
    show (PeerException e n i) = "Peer Exception "
                               ++ show e
                               ++ " "
                               ++ n
                               ++ " "
                               ++ show i

instance Exception PeerException



type NumberOfPieces = Int
type NormalPieceSize = Int
type LastPieceSize = Int

data SizeInfo = SizeInfo { numberOfPieces  :: NumberOfPieces
                         , normalPieceSize :: NormalPieceSize
                         , lastPieceSize   :: LastPieceSize
                         } deriving Show

data FileInfo = FileInfo { fFileName :: B.ByteString
                         , fSize     :: Int
                         } deriving Show

type Perhaps a = Either String a


data PiceInfo = Done
              | InProgress
              | NotHave

  deriving (Show, Eq)

type GlobalPiceInfo = TA.TArray Int PiceInfo

type HashInfo = Seq.Seq B.ByteString

showGlobal :: GlobalPiceInfo -> IO [(Int, PiceInfo)]
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
