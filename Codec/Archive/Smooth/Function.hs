module Codec.Archive.Smooth.Function where

import Prelude
import Data.Conduit
import Control.Monad.Error.Class
import qualified Data.ByteString.Lazy       as LB
import Data.ByteString                      (ByteString)
import Control.Monad.Catch                  (MonadThrow)
import Control.Monad.Trans.Resource         (MonadResource)
import Control.Monad.Trans.Except           (runExceptT)

#if MIN_VERSION_conduit(1, 3, 0)
import Control.Monad.Primitive              (PrimMonad)
#endif

import Codec.Archive.Smooth.Types

autoDecompress :: ( MonadThrow m, MonadResource m
#if MIN_VERSION_conduit(1, 3, 0)
                  , PrimMonad m
#endif
                  , CompressFormat a, FormatDetect a)
               => a
#if MIN_VERSION_conduit(1, 3, 0)
               -> ConduitT ByteString ByteString m ()
#else
               -> Conduit ByteString m ByteString
#endif
autoDecompress x = do
    (matched, bs) <- magicMatch x
    leftover $ LB.toStrict bs
    if matched
       then decompress x
       else awaitForever yield


autoDecompressByCompressors :: ( MonadThrow m, MonadResource m
#if MIN_VERSION_conduit(1, 3, 0)
                               , PrimMonad m
#endif
                               )
                            => [SomeDetectiveCompressor]
#if MIN_VERSION_conduit(1, 3, 0)
                            -> ConduitT ByteString ByteString m ()
#else
                            -> Conduit ByteString m ByteString
#endif
autoDecompressByCompressors = go
    where
        go []       = awaitForever yield
        go (SomeDetectiveCompressor x:xs)   = do
                        (matched, bs) <- magicMatch x
                        leftover $ LB.toStrict bs
                        if matched
                            then decompress x
                            else go xs


autoExtractFiles :: (MonadError String m, Functor m)
                 => [SomeDetectiveArchive]
#if MIN_VERSION_conduit(1, 3, 0)
                 -> ConduitT ByteString FileEntry m ()
#else
                 -> Conduit ByteString m FileEntry
#endif
autoExtractFiles []                                         = mempty
autoExtractFiles (ax@(SomeDetectiveArchive x _):xs)    = do
    (matched, bs) <- magicMatch x
    leftover $ LB.toStrict bs
    if matched
        then extractFilesByDetectiveArchive ax
        else autoExtractFiles xs


extractFilesByDetectiveArchive :: (MonadError String m, Functor m)
                               => SomeDetectiveArchive
#if MIN_VERSION_conduit(1, 3, 0)
                               -> ConduitT ByteString FileEntry m ()
#else
                               -> Conduit ByteString m FileEntry
#endif
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
autoDetectArchive :: (Monad m)
                  => [SomeDetectiveArchive]
#if MIN_VERSION_conduit(1, 3, 0)
                  -> ConduitT ByteString Void m (Maybe SomeDetectiveArchive)
#else
                  -> Sink ByteString m (Maybe SomeDetectiveArchive)
#endif
autoDetectArchive []                                    = return Nothing
autoDetectArchive (ax@(SomeDetectiveArchive x _):xs)    = do
    (matched, bs) <- magicMatch x
    leftover $ LB.toStrict bs
    if matched
        then return $ Just ax
        else autoDetectArchive xs
