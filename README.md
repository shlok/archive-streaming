# archive-streaming

Stream data from an archive (tar, tar.gz, zip, or any other format [supported by libarchive](https://github.com/libarchive/libarchive/wiki/LibarchiveFormats)) using the Haskell [streaming](https://hackage.haskell.org/package/streaming) library.

This library provides, within `Codec.Archive.Streaming`, the following main data type and function:

```haskell
import Control.Monad.Trans.Resource (MonadResource)
import Data.ByteString (ByteString)
import Streaming.Prelude (Of, Stream)

data Header

readArchive :: (MonadResource m) => FilePath -> Stream (Of (Either Header ByteString)) m ()
```

The stream is a header followed by zero or more data chunks, and so on for each header in the archive.

This library also provides the following auxiliary functions to extract information from headers ([many more](https://github.com/libarchive/libarchive/blob/v3.4.0/libarchive/archive_entry.h#L238) could be added):

```haskell
headerFileType     :: Header -> Maybe FileType
headerPathName     :: Header -> Maybe ByteString
headerPathNameUtf8 :: Header -> Maybe ByteString

data FileType = FileTypeRegular
              | FileTypeSymlink
              | FileTypeSocket
              | FileTypeCharDevice
              | FileTypeBlockDevice
              | FileTypeDirectory
              | FileTypeNamedPipe
```

## Example

```haskell
myStream :: (MonadResource m) => Stream (Of (Either Header ByteString)) m ()
myStream = readArchive "animals.tar.gz"
```

You will get a stream like this: `Left <bear1.jpg header>`, `Right <bear1.jpg 1st data chunk>`, …, `Right <bear1.jpg last data chunk>`, and so on for all the other files. Calling `headerPathName` on a `Header` could return, e.g., `Just "bears/bear1.jpg"`.

What you do with the stream is of course up to you. One scenario is to [`groupBy`](https://hackage.haskell.org/package/streaming-0.2.3.0/docs/Streaming-Prelude.html#v:groupBy) at the `Header`s, giving you a stream of substreams, and [`fold`](https://hackage.haskell.org/package/streaming-0.2.3.0/docs/Streaming-Prelude.html#v:fold) or [`foldM`](https://hackage.haskell.org/package/streaming-0.2.3.0/docs/Streaming-Prelude.html#v:foldM) over the substreams.
