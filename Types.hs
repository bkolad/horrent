module Types (GlobalPiceInfo, newGlobalBitField, PiceInfo(..), Buffer) where


import qualified Control.Concurrent.STM.TArray as TA
import qualified Data.ByteString.Char8 as BC
import qualified Data.Sequence as Seq
import Control.Concurrent.STM
import Data.Array.MArray


data PiceInfo = Done | InProgress | NotHave deriving Show

type GlobalPiceInfo = TA.TArray Int PiceInfo 

type Buffer = Seq.Seq BC.ByteString
              

newGlobalBitField ::Int-> IO GlobalPiceInfo
newGlobalBitField size = atomically $ newArray (0, size-1) NotHave  