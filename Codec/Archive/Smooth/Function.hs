module Codec.Archive.Smooth.Function where

import Prelude
import Data.Conduit
import Control.Monad.Error.Class
import qualified Data.ByteString.Lazy       as LB
import Data.ByteString                      (ByteString)
import Control.Monad.Catch                  (MonadThrow)
import Control.Monad.Primitive              (PrimMonad)
import Control.Monad.Base                   (MonadBase)
import Control.Monad.Trans.Resource         (MonadResource)
import Control.Monad.Trans.Except           (runExceptT)

import Codec.Archive.Smooth.Types

autoDecompress ::
        (MonadBase base m, PrimMonad base, MonadThrow m, MonadResource m
        , CompressFormat a, FormatDetect a) =>
        a -> Conduit ByteString m ByteString
autoDecompress x = do
    (matched, bs) <- magicMatch x
    leftover $ LB.toStrict bs
    if matched
        then decompress x
        else awaitForever yield


autoDecompressByCompressors ::
        (MonadBase base m, PrimMonad base, MonadThrow m, MonadResource m) =>
        [SomeDetectiveCompressor] -> Conduit ByteString m ByteString
autoDecompressByCompressors = go
    where
        go []       = awaitForever yield
        go (SomeDetectiveCompressor x:xs)   = do
                        (matched, bs) <- magicMatch x
                        leftover $ LB.toStrict bs
                        if matched
                            then decompress x
                            else go xs


autoExtractFiles :: (MonadError String m, Functor m) =>
    [SomeDetectiveArchive]
    -> Conduit ByteString m FileEntry
autoExtractFiles []                                         = mempty
autoExtractFiles (ax@(SomeDetectiveArchive x _):xs)    = do
    (matched, bs) <- magicMatch x
    leftover $ LB.toStrict bs
    if matched
        then extractFilesByDetectiveArchive ax
        else autoExtractFiles xs


extractFilesByDetectiveArchive :: (MonadError String m, Functor m) =>
    SomeDetectiveArchive
    -> Conduit ByteString m FileEntry
extractFilesByDetectiveArchive (SomeDetectiveArchive x convert_err) =
    transPipe (\me -> runExceptT me >>= either (throwError . err_to_str) return) $
                    transPipe convert_err $ extractEntries x
    where
        err_to_str e = "Failed to extract files by format "
                            ++ show (formatName x)
                            ++ ": " ++ e


-- | detect archive format from bytestring
-- NOTE: This functions will put back any bytes it possible consumed.
--       So the upstream conduit can be reused from the begining.
autoDetectArchive :: (Monad m) =>
    [SomeDetectiveArchive]
    -> Sink ByteString m (Maybe SomeDetectiveArchive)
autoDetectArchive []                                    = return Nothing
autoDetectArchive (ax@(SomeDetectiveArchive x _):xs)    = do
    (matched, bs) <- magicMatch x
    leftover $ LB.toStrict bs
    if matched
        then return $ Just ax
        else autoDetectArchive xs
