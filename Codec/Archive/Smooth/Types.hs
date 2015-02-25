{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Codec.Archive.Smooth.Types where

import Prelude
import Control.Monad.Error.Class
import Data.ByteString                      (ByteString)
import qualified Data.ByteString.Lazy       as LB
import Data.Conduit                         (Conduit, Consumer)
import Control.Monad.Trans.Resource         (MonadResource)
import Control.Monad.Catch                  (MonadThrow)
import Control.Monad.Primitive              (PrimMonad)
import Control.Monad.Base                   (MonadBase)
import Control.Monad.Trans.Except           (ExceptT(..))


-- | a File in archive
data FileEntry = FileEntry {
                    feName          :: String
                    , feContent     :: LB.ByteString
                }
                deriving (Show)


class FormatDetect a where
    -- | test a stream by magic number
    magicMatch :: Monad m =>
        a
        -> Consumer ByteString m (Bool, LB.ByteString)
            -- ^ Bool: if the bytestring looks like current format
            --   ByteString: what is consumed from upstream during computation.

    -- | test MIME type
    mimeMatch :: a -> ByteString -> Bool

    formatName :: a -> String


class HasCodecError a where
    -- | Tar/Zip 都有定义某种错误报告的类型
    type SimpleCodecError a

class HasCodecError a => SimpleArchive a where
    -- | extrace FileEntry from stream
    extractEntries :: MonadError (SimpleCodecError a) m => a -> Conduit ByteString m FileEntry


-- | some kind of compress/decompress algorithm
class CompressFormat a where
    -- | 目前看，所用到的库都没有定义自己的错误类型
    -- 可能都是抛exception来报错
    decompress ::
        (MonadBase base m, PrimMonad base, MonadThrow m, MonadResource m) =>
        a
        -> Conduit ByteString m ByteString


data SomeDetectiveCompressor = forall a. (FormatDetect a, CompressFormat a) =>
                                    SomeDetectiveCompressor a

data SomeDetectiveArchive = forall a. (FormatDetect a, SimpleArchive a) =>
                                SomeDetectiveArchive
                                    a
                                    (forall b n. Functor n => ExceptT (SimpleCodecError a) n b -> ExceptT String n b)
