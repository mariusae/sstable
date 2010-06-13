# SSTables for Haskell

An SSTable implementation for Haskell. In its infancy, but supports
the basic functionality, including efficient range scans.

SSTables are described in the [the Bigtable paper](http://labs.google.com/papers/bigtable-osdi06.pdf)

# Data layout

Absolute offsets are 64-bit words (`Word64`), while relative ones are
32-bit (`Word32`). All values are serialized in big-endian byte order.

## Header

The header contains the very first data in the file, and contains
metadata on the SSTable as well as the offset to the block index.

    VERSION      :: Word32
    NUM-BLOCKS   :: Word32
    INDEX-OFFSET :: Word64

## Entry

    KEY-LENGTH   :: Word32
    ENTRY-LENGTH :: Word32
    KEY-BYTES    :: [Word8]
    ENTRY-BYTES  :: [Word8]

## Block index entry

    KEY-LENGTH   :: Word32
    BLOCK-OFFSET :: Word64
    BLOCK-LENGTH :: Word32
    KEY-BYTES    :: [Word8]
    
