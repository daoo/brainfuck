{-# LANGUAGE LambdaCase #-}
module Brainfuck.Data.Tarpit where

import Brainfuck.Data.Expr
import Control.Applicative
import Data.Monoid
import Test.QuickCheck

data Function = Assign Int Expr | Shift Int | PutChar Expr | GetChar Int
  deriving (Eq, Show)

data Control = Forever | Once | Never | If Expr | While Expr
  deriving (Eq, Show)

-- data Definition = Variable Identifier Expr

data Tarpit = Nop
            | Instruction Function Tarpit
            | Flow Control Tarpit Tarpit
  deriving (Eq, Show)

instance Arbitrary Function where
  arbitrary = frequency [ (4, liftA2 Assign (choose (-4, 10)) arbitrary)
                        , (2, liftA Shift (choose (-4, 10)))
                        , (1, liftA PutChar arbitrary)
                        ]
  shrink = \case
    Assign i e -> map (Assign i) $ shrink e
    Shift i    -> map Shift $ shrink i
    PutChar e  -> map PutChar $ shrink e
    GetChar i  -> map GetChar $ shrink i

instance Arbitrary Control where
  arbitrary = oneof [ return Once
                    , return Never
                    , liftA If arbitrary
                    ]

  shrink = \case
    Never   -> []
    Once    -> [Never]
    Forever -> [Once, Never]

    If e    -> map If (shrink e) ++ [Once, Never]
    While e -> map While (shrink e) ++ [Once, Never]

instance Arbitrary Tarpit where
  arbitrary = sized tree
    where
      tree 0 = return Nop
      tree n = frequency [ (4, liftA2 Instruction arbitrary subtree)
                         , (1, liftA3 Flow arbitrary subtree subtree) ]
        where
          subtree = tree (n `div` 2)

  shrink = initsTarpit

instance Monoid Tarpit where
  mempty = Nop

  mappend a b = case a of
    Nop                  -> b
    Instruction fun next -> Instruction fun (mappend next b)
    Flow ctrl inner next -> Flow ctrl inner (mappend next b)

initTarpit :: Tarpit -> Tarpit
initTarpit = \case
  Nop               -> Nop
  Instruction _ Nop -> Nop
  Flow _ Nop _      -> Nop
  Flow _ _ Nop      -> Nop

  Instruction fun next -> Instruction fun (initTarpit next)
  Flow ctrl inner next -> Flow ctrl (initTarpit inner) (initTarpit next)

initsTarpit :: Tarpit -> [Tarpit]
initsTarpit = \case
  Nop -> []
  x   -> let x' = initTarpit x in x' : initsTarpit x'

mapTarpit :: (Function -> Function) -> (Control -> Control) -> Tarpit -> Tarpit
mapTarpit f g = \case
  Nop                  -> Nop
  Flow ctrl inner next -> Flow (g ctrl) (mapTarpit f g inner) (mapTarpit f g next)
  Instruction fun next -> Instruction (f fun) (mapTarpit f g next)
