module Tracker.TrackerUtils (getIPandPort, get32and16b) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Network.Socket as N
import Data.Binary.Get



getIPandPort :: B.ByteString -> [(N.HostName, N.PortNumber)]
getIPandPort bs = runGet get32and16b (BL.fromChunks [bs])

get32and16b :: Get [(N.HostName, N.PortNumber)]
get32and16b =
    do empty <- isEmpty
       if empty then
           return []
       else do
           ip <- show <$> getWord32be
           port <- fromIntegral <$> getWord16be
           rest <- get32and16b
           return $ (ip, port):rest
