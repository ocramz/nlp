module NLP.WordNet31 where

import Foreign.Ptr (Ptr(..), plusPtr)
import Foreign.ForeignPtr (ForeignPtr(..)) 
import System.IO.MMap (mmapFileByteStringLazy)
-- import Data.Int (Int64(..))

import qualified Data.ByteString.Lazy as LBS

import NLP.WordNet31.Parsers
import NLP.WordNet31.Types

-- | mmap a file in read-only mode, starting at a specific byte offset and copying n bytes into a lazy ByteString
mmapFile ::
     FilePath
  -> ByteOffset  -- ^ Offset in bytes
  -> Int    -- ^ Size in bytes
  -> IO LBS.ByteString
mmapFile fp offs n = mmapFileByteStringLazy fp (Just (offs, offs + fromIntegral n))
