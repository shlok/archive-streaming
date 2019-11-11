--------------------------------------------------------------------------------

module Codec.Archive.Streaming
    ( Header (..)
    , FileType (..)
    , readArchive ) where

--------------------------------------------------------------------------------

import Codec.Archive.Streaming.Foreign (Archive, FileType (..), archive_entry_filetype, archive_entry_pathname,
                                        archive_entry_pathname_utf8, archive_read_data, archive_read_free,
                                        archive_read_new, archive_read_next_header, archive_read_open_filename,
                                        archive_read_support_filter_all, archive_read_support_format_all)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Resource (MonadResource, allocate, release)
import Data.ByteString (ByteString)
import Streaming.Prelude (Of, Stream, yield)

--------------------------------------------------------------------------------

data Header
   = Header { headerFileType     :: !(Maybe FileType)
            , headerPathName     :: !(Maybe ByteString)
            , headerPathNameUtf8 :: !(Maybe ByteString) }

readArchive :: (MonadResource m) => FilePath -> Stream (Of (Either Header ByteString)) m ()
readArchive fp = do
    (archKey, arch) <- lift $ allocate archive_read_new archive_read_free
    liftIO $ archive_read_support_filter_all arch
    liftIO $ archive_read_support_format_all arch
    liftIO $ archive_read_open_filename arch fp
    yieldAll arch True
    release archKey

yieldAll :: (MonadResource m) => Archive -> Bool -> Stream (Of (Either Header ByteString)) m ()
yieldAll arch readHeader =
    if readHeader
        then do
            me <- liftIO $ archive_read_next_header arch
            case me of
                Nothing -> return ()
                Just e -> do
                    fileType <- liftIO $ archive_entry_filetype e
                    pathName <- liftIO $ archive_entry_pathname e
                    pathNameUtf8 <- liftIO $ archive_entry_pathname_utf8 e
                    yield . Left $ Header { headerFileType = fileType
                                          , headerPathName = pathName
                                          , headerPathNameUtf8 = pathNameUtf8 }
                    yieldAll arch False
        else do
            md <- liftIO $ archive_read_data arch
            case md of
                Nothing -> yieldAll arch True
                Just d -> do
                    yield $ Right d
                    yieldAll arch False

--------------------------------------------------------------------------------
