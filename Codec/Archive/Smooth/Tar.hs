{-# LANGUAGE UndecidableInstances #-}
module Codec.Archive.Smooth.Tar where

import Prelude 
import Control.Exception.Safe
import Control.Monad.Error.Class
import Control.Monad (when)
import Codec.Archive.Smooth.Types
import Data.Conduit
import qualified Data.ByteString.Lazy       as LB
import qualified Data.ByteString.Lazy.Char8 as LBC8
import qualified Data.Conduit.Tar           as CT
import qualified Data.Conduit.Binary        as CB

data SimpleTar = SimpleTar
                deriving (Show)


instance FormatDetect SimpleTar where
    magicMatch _ = do
        bs <- CB.take (257 + 8)
        let magic_bs = LB.take 8 $ LB.drop 257 bs
            match = magic_bs == LBC8.pack"ustar\NUL00"
                        || magic_bs == LBC8.pack"ustar  \NUL"
        return $ (match, bs)

    mimeMatch _ ct = ct == "application/x-tar"

    formatName _ = "tar"


instance HasCodecError SimpleTar where
    type SimpleCodecError SimpleTar = SomeException


instance SimpleArchive SimpleTar where
  extractEntries _ = transPipe (either throwError pure) (CT.untarChunks .| CT.withEntries conv_entry)
        where
            conv_entry hdr = do
              when (CT.headerFileType hdr == CT.FTNormal) $ do
                lbs <- CB.sinkLbs
                yield $ FileEntry (CT.headerFilePath hdr) lbs

