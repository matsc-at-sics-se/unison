module Unison.Target.X86.X86ResourceDecl (X86Resource (..)) where

data X86Resource =
  BundleWidth |
  Pipe |
  SKLPort1 |
  SKLPort0 |
  SKLPort01 |
  SKLPort06 |
  SKLPort4 |
  SKLPort05 |
  SKLPort015 |
  SKLPort6 |
  SKLDivider |
  SKLPort16 |
  SKLPort237 |
  SKLPort5 |
  SKLPort15 |
  SKLPort23 |
  SKLPort0156 |
  ExePort
  deriving (Eq, Ord, Show, Read)
