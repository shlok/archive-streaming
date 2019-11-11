--------------------------------------------------------------------------------

module Codec.Archive.Streaming.Tests (tests) where

--------------------------------------------------------------------------------

import Codec.Archive.Streaming (Header (..), readArchive)
import Codec.Archive.Streaming.Foreign (blockSize)
import qualified Codec.Archive.Tar as Tar (pack, write)
import Codec.Compression.GZip (compress)
import Control.Monad (forM)
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Crypto.Random.Entropy (getEntropy)
import Data.Bifunctor (first)
import Data.ByteString (ByteString, append)
import qualified Data.ByteString as B (writeFile)
import Data.ByteString.Char8 (unpack)
import qualified Data.ByteString.Lazy as LB (writeFile)
import Data.Char (chr, ord)
import Data.Either (isRight)
import Data.Function ((&))
import Data.List (nub, sort)
import Data.Maybe (fromJust)
import qualified Streaming.Prelude as S (fold, groupBy, map, mapped, toList_)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (addTrailingPathSeparator, joinPath, takeDirectory)
import System.IO.Temp (withSystemTempDirectory)
import Test.QuickCheck (Gen, choose, frequency, vectorOf)
import Test.QuickCheck.Monadic (monadicIO, pick, run)
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (testProperty)

--------------------------------------------------------------------------------

tests :: [TestTree]
tests =
    [ testTar False
    , testTar True ]

--------------------------------------------------------------------------------

-- | Use other libraries to create a tar (or tar.gz) file containing random data,
-- read the file back using our library, and check if the results are as expected.
testTar :: Bool -> TestTree
testTar gz = testProperty "tar" $ monadicIO $  do
    -- Generate a random file system hierarchy.
    hierarchy <- pick $ randomHierarchy "" 4 4 5

    -- Create a new temporary directory, write our hierarchy into
    -- a "files" subdirectory of the temporary directory, use other
    -- libraries to create files.tar (or files.tar.gz), read the file
    -- back using our library, and check if the results are as expected.
    run . withSystemTempDirectory "archive-streaming-testZip" $ \tmpDir -> do
        let filesDir = joinPath [tmpDir, "files"]
        createDirectoryIfMissing True filesDir
        pathsAndByteStrings <- writeHierarchy filesDir hierarchy

        let archFile = joinPath [tmpDir, "files.tar" ++ (if gz then ".gz" else "")]
        LB.writeFile archFile . (if gz then compress else id) . Tar.write =<< Tar.pack tmpDir ["files"]

        pathsAndByteStrings2
            <- readArchive archFile
             & S.groupBy (const isRight)
             & S.mapped (S.fold (\(mfp, mbs) e ->
                 case e of
                     Left h -> (unpack <$> headerPathName h, mbs)
                     Right bs -> (mfp, case mbs of
                                             Nothing -> Just bs
                                             Just bs' -> Just $ bs' `append` bs)) (Nothing, Nothing) id)
             & S.map (\(mfp, mbs) -> (fromJust mfp, mbs))
             & S.toList_
             & runResourceT

        -- Make the two lists comparable and compare them.
        let pathsAndByteStrings_ = sort $ map (first ("files/"++)) (("", Nothing) : pathsAndByteStrings)
        let pathAndByteStrings2_ = sort $ pathsAndByteStrings2

        return $ pathsAndByteStrings_ == pathAndByteStrings2_

--------------------------------------------------------------------------------

-- | Writes a given hierarchy of relative paths (created with 'randomHierarchy') to disk
-- in the specified directory and returns the same hierarchy except with actual ByteStrings
-- instead of lengths. Note: The original relative paths are returned back unaltered.
writeHierarchy :: FilePath -> [(FilePath, Maybe Int)]  -> IO [(FilePath, Maybe ByteString)]
writeHierarchy writeDir = mapM $ \(p, mBsLen) ->
    let fullp = joinPath [writeDir, p]
     in case mBsLen of
            Just bsLen -> do
                createDirectoryIfMissing True (takeDirectory fullp)
                bs <- getEntropy (fromIntegral bsLen)
                B.writeFile fullp bs
                return (p, if bsLen == 0
                            then Nothing -- Our library yields no ByteString at all for empty files.
                            else Just bs)
            Nothing -> createDirectoryIfMissing True fullp >> return (p, Nothing)

-- | Recursively generates a random hierarchy of relative paths to file and
-- directories. (Nothing is written to disk; only the paths are returned.)
-- The initial dirPath should be "". A random bytestring length is
-- provided in case of a file; 'Nothing' in the case of a directory.
randomHierarchy :: FilePath -> Int -> Int -> Int -> Gen [(FilePath, Maybe Int)]
randomHierarchy dirPath maxFiles maxDirs maxDepth = do
    numFiles <- choose (0, maxFiles)
    fileComps <- nub <$> vectorOf numFiles pathComponent
    let filePaths = map (\c -> joinPath [dirPath, c]) fileComps
    bsLengths <- (map Just) <$> vectorOf (length filePaths)
                                    ( frequency [ (1, choose (0, 5))
                                                , (1, choose (blockSize - 5, blockSize + 5))
                                                , (1, choose (0, 3 * blockSize)) ] )

    numDirs <- choose (0, maxDirs)
    dirComps <- (nub . filter (not . (`elem` fileComps)))
                    <$> (vectorOf (if maxDepth <= 0 then 0 else numDirs) $ pathComponent)
    -- libarchive reads back directory paths with a trailing separator.
    let dirPaths = map (\c -> addTrailingPathSeparator $ joinPath [dirPath, c]) dirComps

    recursion <- concat <$> (forM dirPaths $ \dirPath' ->
                                randomHierarchy dirPath' (maxFiles `div` 2) (maxDirs `div` 2) (maxDepth - 1))

    return $ zip filePaths bsLengths ++ zip dirPaths (repeat Nothing) ++ recursion

-- | Generates a random path component of length between 1 and 10, e.g., "HO53UVKQ".
-- For compatibility with case-insensitive file systems, uses only one case.
pathComponent :: Gen String
pathComponent = do
    len <- choose (1, 10)
    vectorOf len $ chr <$> frequency [ (1, choose (ord 'A', ord 'Z'))
                                     , (1, choose (ord '0', ord '9')) ]

--------------------------------------------------------------------------------
