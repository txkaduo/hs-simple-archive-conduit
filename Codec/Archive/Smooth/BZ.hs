{-# LANGUAGE UndecidableInstances #-}
module Codec.Archive.Smooth.BZ where

import Prelude 
import Codec.Archive.Smooth.Types
import qualified Data.Conduit.Binary        as CB
import qualified Data.Conduit.BZlib         as BZlib

data SimpleBZ = SimpleBZ
                deriving (Show)


instance FormatDetect SimpleBZ where
    magicMatch _ = do
        bs <- CB.take 3
        return $ (bs == "BZh", bs)

    mimeMatch _ ct = ct == "application/x-bzip2"

    formatName _ = "bzip2"


instance CompressFormat SimpleBZ where
    decompress _ = BZlib.bunzip2
