module YHSeq where

  import Prelude

  -- | Optparsed arguments.
  data OpArg = OpArg String OpSeqNum Bool Bool

  -- | Optparsed sequence and number.
  data OpSeqNum = Spr String String | Glu String

  -- | Implemented versions of Y-sequence Hexirp edition.
  data Ver = V0110 | V0111 | V0200 | V0210

  -- | Arguments.
  data Arg = Arg Ver SeqNum Bool Bool

  -- | Sequence and Number.
  data SeqNum = SeqNum [Integer] Integer
