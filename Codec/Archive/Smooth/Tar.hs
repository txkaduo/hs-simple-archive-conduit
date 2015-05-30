{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}
module Codec.Archive.Smooth.Tar where

import Prelude 

import Codec.Archive.Smooth.Types
import Data.Conduit
import Codec.Archive.Tar.Entry              (fromTarPath, entryTarPath)
import qualified Data.ByteString.Lazy       as LB
import qualified Data.ByteString.Lazy.Char8 as LBC8
import qualified Codec.Archive.Tar          as Tar
-- import qualified Data.Conduit.List          as CL
import qualified Data.Conduit.Binary        as CB
-- import qualified Blaze.ByteString.Builder   as Blaze

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
    type SimpleCodecError SimpleTar = Tar.FormatError


instance SimpleArchive SimpleTar where
    extractEntries _ = mapOutputMaybe to_fe Tar.conduitEntry
        where
            to_fe tfe = case Tar.entryContent tfe of
                Tar.NormalFile content _    ->
                            return $ FileEntry
                                        (fromTarPath $ entryTarPath tfe)
                                        content
                _   -> Nothing

