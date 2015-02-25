import Prelude
import System.Environment
import Codec.Archive.Smooth.All
import System.IO
import Control.Monad
import Data.Conduit
import Data.Conduit.Binary (sourceFile, sourceHandle)
import qualified Data.Conduit.List as CL
import Control.Monad.Trans.Except
import Control.Monad.Trans.Resource

testFile :: String -> IO ()
testFile fp = do
    err_or_names <- runExceptT $ runResourceT $
                        (if fp == "-"
                            then sourceHandle stdin
                            else sourceFile fp)
                        $= autoDecompressByCompressors allKnownDetectiveCompressors
                        $= autoExtractFiles allKnownDetectiveArchives
                        $= CL.map feName
                        $$ CL.consume
    case err_or_names of
        Left err -> do
            hPutStrLn stderr $ "error when reading file '" ++ fp
                                ++ "': " ++ show err
        Right names -> do
            putStrLn $ "File: " ++ fp
            forM_ names $ \name -> do
                putStrLn name


main :: IO ()
main = do
    args <- getArgs
    mapM_ testFile args

