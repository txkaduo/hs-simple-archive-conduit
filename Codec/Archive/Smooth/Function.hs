module Codec.Archive.Smooth.Function where

import Prelude
import Data.Conduit
import Data.Monoid
import Control.Monad                        (liftM)
import Control.Monad.Error.Class
import Control.Monad.Trans.Class
import qualified Data.ByteString.Lazy       as LB
import Data.ByteString                      (ByteString)
import Control.Monad.Catch                  (MonadThrow)
import Control.Monad.Primitive              (PrimMonad)
import Control.Monad.Base                   (MonadBase)
import Control.Monad.Trans.Resource         (MonadResource)
import Control.Monad.Trans.Except           (runExceptT, ExceptT(..))

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
        else mempty


autoDecompressByCompressors ::
        (MonadBase base m, PrimMonad base, MonadThrow m, MonadResource m) =>
        [SomeDetectiveCompressor] -> Conduit ByteString m ByteString
autoDecompressByCompressors = mconcat . map (\(SomeDetectiveCompressor x) -> autoDecompress x)


autoExtractFiles :: (MonadError String m, Functor m) =>
    [SomeDetectiveArchive]
    -> Conduit ByteString m FileEntry
autoExtractFiles []                                         = mempty
autoExtractFiles (SomeDetectiveArchive x convert_err:xs)    = do
    (matched, bs) <- magicMatch x
    leftover $ LB.toStrict bs
    if matched
        then do
            transPipe (\me -> runExceptT me >>= either (throwError . err_to_str) return) $
                            transPipe convert_err $ extractEntries x
        else autoExtractFiles xs
    where
        err_to_str e = "Failed to extract files by format "
                            ++ show (formatName x)
                            ++ ": " ++ e
