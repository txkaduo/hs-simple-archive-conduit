{-# LANGUAGE UndecidableInstances #-}
module Codec.Archive.Smooth.Zip where

import Prelude 
import Codec.Archive.Smooth.Types
import Control.Monad
import Data.Conduit.Binary                  (sinkLbs)
import Data.Conduit                         (yield)
import Control.Monad.Error.Class
import Data.Monoid                          (mconcat)
import qualified Data.ByteString.Lazy       as LB
import qualified Codec.Archive.Zip          as Zip
import qualified Data.Conduit.List          as CL
import qualified Data.Conduit.Binary        as CB
import qualified Blaze.ByteString.Builder   as Blaze

data SimpleZip = SimpleZip
                deriving (Show)


instance FormatDetect SimpleZip where
    magicMatch _ = do
        bs <- CB.take 4
        return $ (bs `elem` zip_magic_bs, bs)
        where
            zip_magic_bs =  [ LB.pack [ 0x50, 0x4b, 0x03, 0x04 ]
                            , LB.pack [ 0x50, 0x4b, 0x05, 0x06 ]
                            , LB.pack [ 0x50, 0x4b, 0x07, 0x08 ]
                            ]

    mimeMatch _ ct = ct == "application/zip" ||
                    ct == "application/x-zip-compressed"

    formatName _ = "zip"



instance HasCodecError SimpleZip where
    type SimpleCodecError SimpleZip = String


instance SimpleArchive SimpleZip where
    extractEntries _ = do
        bs <- fmap (Blaze.toLazyByteString . mconcat . map Blaze.fromByteString)
                    CL.consume
        arc <- either throwError return $ Zip.toArchiveOrFail bs
        forM_ (Zip.zEntries arc) $ \entry -> do
            let file_bs     = Zip.fromEntry entry
                file_name   = Zip.eRelativePath entry
                ae          = FileEntry file_name file_bs
            yield ae
