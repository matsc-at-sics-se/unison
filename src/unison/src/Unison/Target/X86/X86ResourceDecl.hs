module Unison.Target.X86.X86ResourceDecl (X86Resource (..)) where

data X86Resource =
  BundleWidth |
  Pipe |
  SKLPort0 |
  SKLPort1 |
  SKLPort4 |
  SKLPort5 |
  SKLPort6 |
  SKLPort01 |
  SKLPort05 |
  SKLPort06 |
  SKLPort15 |
  SKLPort16 |
  SKLPort23 |
  SKLPort015 |
  SKLPort237 |
  SKLPort0156 |
  SKLDivider |
  SKLFPDivider |
  ExePort
  deriving (Eq, Ord, Show, Read)
