{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Codec.Archive.Smooth.Types where

import Prelude
import Control.Monad.Error.Class
import Data.ByteString                      (ByteString)
import qualified Data.ByteString.Lazy       as LB
import Data.Conduit
import Control.Monad.Trans.Resource         (MonadResource)
import Control.Monad.Catch                  (MonadThrow)
#if MIN_VERSION_conduit(1, 3, 0)
import Control.Monad.Primitive              (PrimMonad)
#endif
import Control.Monad.Trans.Except           (ExceptT(..))


-- | a File in archive
data FileEntry = FileEntry
                  { feName          :: String
                  , feContent     :: LB.ByteString
                  }
                  deriving (Show)


class FormatDetect a where
    -- | test a stream by magic number
    magicMatch :: Monad m =>
        a
#if MIN_VERSION_conduit(1, 3, 0)
        -> (forall o. ConduitT ByteString o m (Bool, LB.ByteString))
#else
        -> Consumer ByteString m (Bool, LB.ByteString)
#endif
            -- ^ Bool: if the bytestring looks like current format
            --   ByteString: what is consumed from upstream during computation.

    -- | test MIME type
    mimeMatch :: a -> ByteString -> Bool

    formatName :: a -> String


class HasCodecError a where
    -- | Tar/Zip 都有定义某种错误报告的类型
    type SimpleCodecError a

class HasCodecError a => SimpleArchive a where
    -- | extract FileEntry from stream
    extractEntries :: MonadError (SimpleCodecError a) m
                   => a
#if MIN_VERSION_conduit(1, 3, 0)
                   -> ConduitT ByteString FileEntry m ()
#else
                   -> Conduit ByteString m FileEntry
#endif


-- | some kind of compress/decompress algorithm
class CompressFormat a where
    -- | 目前看，所用到的库都没有定义自己的错误类型
    -- 可能都是抛exception来报错
    -- constraints are from Zlib.ungzip
    decompress :: ( MonadThrow m, MonadResource m
#if MIN_VERSION_conduit(1, 3, 0)
                  , PrimMonad m
#endif
                  )
               => a
#if MIN_VERSION_conduit(1, 3, 0)
               -> ConduitT ByteString ByteString m ()
#else
               -> Conduit ByteString m ByteString
#endif


data SomeDetectiveCompressor = forall a. (FormatDetect a, CompressFormat a) =>
                                    SomeDetectiveCompressor a

data SomeDetectiveArchive = forall a. (FormatDetect a, SimpleArchive a) =>
                                SomeDetectiveArchive
                                    a
                                    (forall b n. Functor n => ExceptT (SimpleCodecError a) n b -> ExceptT String n b)
