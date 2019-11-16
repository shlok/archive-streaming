--------------------------------------------------------------------------------

module Codec.Archive.Streaming.Foreign
    ( Archive
    , Entry
    , archive_read_new
    , archive_read_support_filter_all
    , archive_read_support_format_all
    , blockSize
    , archive_read_open_filename
    , archive_read_next_header
    , FileType (..)
    , archive_entry_filetype
    , archive_entry_pathname
    , archive_entry_pathname_utf8
    , archive_read_data
    , archive_read_free ) where

--------------------------------------------------------------------------------

import Control.Monad (when)
import Data.Bits ((.&.))
import Data.ByteString (ByteString, packCString, packCStringLen)
import Foreign (Ptr, nullPtr)
import Foreign.C.String (CString, peekCString, withCString)
import Foreign.C.Types (CInt (CInt), CSize (CSize))
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Storable (peek)
import System.Posix.Types (CSsize (CSsize), CMode (CMode))
import UnliftIO.Exception (Exception, throwIO, throwString)

--------------------------------------------------------------------------------

data CArchive
data CEntry

foreign import ccall unsafe "archive.h archive_errno"
    c_archive_errno :: Ptr CArchive -> IO CInt

foreign import ccall unsafe "archive.h archive_error_string"
    c_archive_error_string :: Ptr CArchive -> IO CString

foreign import ccall unsafe "archive.h archive_read_new"
    c_archive_read_new :: IO (Ptr CArchive)

foreign import ccall unsafe "archive.h archive_read_support_filter_all"
    c_archive_read_support_filter_all :: Ptr CArchive -> IO CInt

foreign import ccall unsafe "archive.h archive_read_support_format_all"
    c_archive_read_support_format_all :: Ptr CArchive -> IO CInt

foreign import ccall unsafe "archive.h archive_read_open_filename"
    c_archive_read_open_filename :: Ptr CArchive -> CString -> CSize -> IO CInt

foreign import ccall unsafe "archive.h archive_read_next_header"
    c_archive_read_next_header :: Ptr CArchive -> Ptr (Ptr CEntry) -> IO CInt

foreign import ccall unsafe "archive_entry.h archive_entry_filetype"
    c_archive_entry_filetype :: Ptr CEntry -> IO CMode -- Todo: Think about type on non-POSIX.

foreign import ccall unsafe "archive_entry.h archive_entry_pathname"
    c_archive_entry_pathname :: Ptr CEntry -> IO CString

foreign import ccall unsafe "archive_entry.h archive_entry_pathname_utf8"
    c_archive_entry_pathname_utf8 :: Ptr CEntry -> IO CString

foreign import ccall unsafe "archive.h archive_read_data"
    c_archive_read_data :: Ptr CArchive -> Ptr a -> CSize -> IO CSsize -- Todo: Think about la_ssize_t on non-POSIX.

foreign import ccall unsafe "archive.h archive_read_free"
    c_archive_read_free :: Ptr CArchive -> IO CInt

--------------------------------------------------------------------------------

-- Documented libarchive return codes.
data RetCode
    = RetCodeEOF
    | RetCodeOK
    | RetCodeRETRY
    | RetCodeWARN
    | RetCodeFAILED
    | RetCodeFATAL
    deriving (Show)

retCodes :: [(CInt, RetCode)]
retCodes =
    [ (1, RetCodeEOF)
    , (0, RetCodeOK)
    , (-10, RetCodeRETRY)
    , (-20, RetCodeWARN)
    , (-25, RetCodeFAILED)
    , (-30, RetCodeFATAL) ]

data ArchiveError =
     ArchiveError { err_function :: !String
                  , err_retcode  :: !(Either CInt RetCode)
                  , err_number   :: !Int
                  , err_string   :: !String }
                  deriving (Show)
instance Exception ArchiveError

archive_error_string :: Ptr CArchive -> IO String
archive_error_string ptr = do
    cstr <- c_archive_error_string ptr
    if cstr == nullPtr
        then return "archive_error_string returned NULL"
        else peekCString cstr

throwArchiveError :: String -> CInt -> Ptr CArchive -> IO noReturn
throwArchiveError fn rc ptr = do
    num <- fromIntegral <$> c_archive_errno ptr
    str <-  archive_error_string ptr
    throwIO $ ArchiveError
        { err_function = fn
        , err_retcode = maybe (Left rc) Right (lookup rc retCodes)
        , err_number = num
        , err_string = str }

--------------------------------------------------------------------------------

data Archive = Archive !(Ptr CArchive)
data Entry = Entry !(Ptr CEntry)

archive_read_new :: IO Archive
archive_read_new = do
    ptr_archive <- c_archive_read_new
    if ptr_archive == nullPtr
        then throwString "archive_read_new returned NULL"
        else return $ Archive ptr_archive

archive_read_support_filter_all :: Archive -> IO ()
archive_read_support_filter_all (Archive ptr) = do
    rc <- c_archive_read_support_filter_all ptr
    when (rc /= 0) $ throwArchiveError "archive_read_support_filter_all" rc ptr

archive_read_support_format_all :: Archive -> IO ()
archive_read_support_format_all (Archive ptr) = do
    rc <- c_archive_read_support_format_all ptr
    when (rc /= 0) $ throwArchiveError "archive_read_support_format_all" rc ptr

-- Fixed block size for now. (Exported for the tests to use.)
blockSize :: (Num a) => a
blockSize = 40960

archive_read_open_filename :: Archive -> FilePath -> IO ()
archive_read_open_filename (Archive ptr) fp =
    withCString fp $ \cstr -> do
        rc <- c_archive_read_open_filename ptr cstr blockSize
        when (rc /= 0) $ throwArchiveError "archive_read_open_filename" rc ptr

-- | Returns 'Nothing' if we have reached the end of the archive.
-- Use the desired "archive_entry..." functions directly after this one;
-- otherwise you run the risk of querying a wrong or non-existent entry.
archive_read_next_header :: Archive -> IO (Maybe Entry)
archive_read_next_header (Archive ptr) =
    alloca $ \ppe -> do
        rc <- c_archive_read_next_header ptr ppe
        if rc == 1
            then return Nothing
            else if rc < 0
                then throwArchiveError "archive_read_next_header" rc ptr
                else Just . Entry <$> peek ppe

data FileType = FileTypeRegular
              | FileTypeSymlink
              | FileTypeSocket
              | FileTypeCharDevice
              | FileTypeBlockDevice
              | FileTypeDirectory
              | FileTypeNamedPipe
              deriving (Show, Eq)

fileTypeAeIFMT :: CMode
fileTypeAeIFMT = 0o0170000

fileTypes :: [(CMode, FileType)]
fileTypes =
    [ (0o0100000, FileTypeRegular)
    , (0o0120000, FileTypeSymlink)
    , (0o0140000, FileTypeSocket)
    , (0o0020000, FileTypeCharDevice)
    , (0o0060000, FileTypeBlockDevice)
    , (0o0040000, FileTypeDirectory)
    , (0o0010000, FileTypeNamedPipe) ]

archive_entry_filetype :: Entry -> IO (Maybe FileType)
archive_entry_filetype (Entry ptr) = do
    i <- c_archive_entry_filetype ptr
    return $ lookup (i .&. fileTypeAeIFMT) fileTypes

archive_entry_pathname :: Entry -> IO (Maybe ByteString)
archive_entry_pathname (Entry ptr) = do
    cstr <- c_archive_entry_pathname ptr
    if cstr == nullPtr
        then return Nothing
        else Just <$> packCString cstr

archive_entry_pathname_utf8 :: Entry -> IO (Maybe ByteString)
archive_entry_pathname_utf8 (Entry ptr) = do
    cstr <- c_archive_entry_pathname_utf8 ptr
    if cstr == nullPtr
        then return Nothing
        else Just <$> packCString cstr

-- | Returns 'Nothing' if there is no more data for the current entry.
archive_read_data :: Archive -> IO (Maybe ByteString)
archive_read_data (Archive ptr) =
    allocaBytes blockSize $ \pw -> do
        rb <- c_archive_read_data ptr pw blockSize
        if rb == 0
            then return Nothing
            else if rb < 0
                then throwArchiveError "archive_read_data" (fromIntegral rb) ptr
                else Just <$> packCStringLen (pw, fromIntegral rb)

archive_read_free :: Archive -> IO ()
archive_read_free (Archive ptr) = do
    rc <- c_archive_read_free ptr
    when (rc /= 0) $ throwArchiveError "archive_read_free" rc ptr

--------------------------------------------------------------------------------
