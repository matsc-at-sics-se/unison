module Unison.Target.X86.X86ResourceDecl (X86Resource (..)) where

data X86Resource =
  BundleWidth |
  Pipe
  deriving (Eq, Ord, Show, Read)
