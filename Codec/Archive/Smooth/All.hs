{-# LANGUAGE CPP #-}
module Codec.Archive.Smooth.All
    ( module Codec.Archive.Smooth.All
    , module Codec.Archive.Smooth.Types
    , module Codec.Archive.Smooth.Tar
    , module Codec.Archive.Smooth.Zip
    , module Codec.Archive.Smooth.GZ
    , module Codec.Archive.Smooth.Function
#ifdef HAVE_BZ
    , module Codec.Archive.Smooth.BZ
#endif

#ifdef HAVE_XZ
    , module Codec.Archive.Smooth.XZ
#endif
    ) where

import Codec.Archive.Smooth.Types
import Codec.Archive.Smooth.Zip
import Codec.Archive.Smooth.GZ
import Codec.Archive.Smooth.Tar
import Codec.Archive.Smooth.Function
import Control.Monad.Trans.Except           (withExceptT)

#ifdef HAVE_BZ
import Codec.Archive.Smooth.BZ
#endif

#ifdef HAVE_XZ
import Codec.Archive.Smooth.XZ
#endif

allKnownDetectiveCompressors :: [SomeDetectiveCompressor]
allKnownDetectiveCompressors =   [ SomeDetectiveCompressor SimpleGZ
#ifdef HAVE_BZ
                                , SomeDetectiveCompressor SimpleBZ
#endif
#ifdef HAVE_XZ
                                , SomeDetectiveCompressor SimpleXZ
#endif
                                ]

allKnownDetectiveArchives :: [SomeDetectiveArchive]
allKnownDetectiveArchives =
    [ SomeDetectiveArchive SimpleTar (withExceptT show)
    , SomeDetectiveArchive SimpleZip id
    ]
