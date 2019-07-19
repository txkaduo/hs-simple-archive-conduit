{-# LANGUAGE UndecidableInstances #-}
module Codec.Archive.Smooth.GZ where

import Prelude 
import Codec.Archive.Smooth.Types
import qualified Data.ByteString.Lazy       as LB
import qualified Data.Conduit.Binary        as CB
import qualified Data.Conduit.Zlib          as Zlib

data SimpleGZ = SimpleGZ
                deriving (Show)


instance FormatDetect SimpleGZ where
    magicMatch _ = do
        bs <- CB.take 2
        return $ (bs == magic_bs, bs)
        where
            magic_bs =  LB.pack [ 0x1f, 0x8b ]

    mimeMatch _ ct = ct == "application/gzip"

    formatName _ = "gzip"


instance CompressFormat SimpleGZ where
    decompress _ = Zlib.ungzip

