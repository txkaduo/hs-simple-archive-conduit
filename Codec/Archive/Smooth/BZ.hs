{-# LANGUAGE UndecidableInstances #-}
module Codec.Archive.Smooth.BZ where

import Prelude 
import Codec.Archive.Smooth.Types
import Control.Monad
import Data.Conduit.Binary                  (sinkLbs)
import Data.Conduit                         (yield)
import Control.Monad.Error.Class
import Data.Monoid                          (mconcat)
import qualified Data.ByteString.Lazy       as LB
import qualified Data.Conduit.List          as CL
import qualified Data.Conduit.Binary        as CB
import qualified Blaze.ByteString.Builder   as Blaze
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
