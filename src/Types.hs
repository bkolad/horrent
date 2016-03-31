{-# LANGUAGE NoMonomorphismRestriction #-}

module Types  ( GlobalPiceInfo
              , newGlobalBitField
              , PiceInfo (..)
              , SizeInfo (..)
              , HashInfo
              , liftEither
              , ExceptT
              , liftIO
              , runExceptT
              , hashInfoFromList
              , NumberOfPieces
              , NormalPieceSize
              , LastPieceSize
              , SizeInfo
              , Perhaps
              , getSizeData
              ) where


import qualified Control.Concurrent.STM.TArray as TA
import qualified Data.ByteString.Char8 as BC
import qualified Data.Sequence as Seq
import Control.Concurrent.STM
import Data.Array.MArray
import Control.Monad.Trans.Except
import Control.Monad.IO.Class (liftIO)



type NumberOfPieces = Int
type NormalPieceSize = Int
type LastPieceSize = Int

data SizeInfo = SizeInfo { numberOfPieces :: NumberOfPieces
                         , normalPieceSize :: NormalPieceSize
                         , lastPieceSize :: LastPieceSize
                         } deriving Show

type Perhaps a = Either String a


data PiceInfo = Done | InProgress | NotHave
  deriving (Show, Eq)

type GlobalPiceInfo = TA.TArray Int PiceInfo

type HashInfo = Seq.Seq BC.ByteString


hashInfoFromList :: [BC.ByteString] -> HashInfo
hashInfoFromList = Seq.fromList


newGlobalBitField ::Int-> IO GlobalPiceInfo
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
