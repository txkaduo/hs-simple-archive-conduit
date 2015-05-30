{-# LANGUAGE UndecidableInstances #-}
module Codec.Archive.Smooth.XZ where

import Prelude 
import Codec.Archive.Smooth.Types
import qualified Data.ByteString.Lazy       as LB
import qualified Data.Conduit.Binary        as CB
import qualified Data.Conduit.Lzma          as LZMA


data SimpleXZ = SimpleXZ
                deriving (Show)


instance FormatDetect SimpleXZ where
    magicMatch _ = do
        bs <- CB.take 6
        return $ (bs == magic_bs, bs)
        where
            magic_bs =  LB.pack [ 0xfd, 0x37, 0x7a, 0x58, 0x5a, 0x00 ]

    mimeMatch _ ct = ct == "application/x-xz"

    formatName _ = "xz"


instance CompressFormat SimpleXZ where
    decompress _ = LZMA.decompress Nothing
