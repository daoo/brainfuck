{-# LANGUAGE LambdaCase #-}
module Brainfuck.Data.AST where

import Brainfuck.Data.Expr
import Control.Applicative
import Test.QuickCheck

data Function = Set Int Expr | Shift Int | PutChar Expr | GetChar Int
  deriving (Eq, Show)

data Control = Forever | Once | Never | If Expr | While Expr
  deriving (Eq, Show)

-- data Definition = Variable Identifier Expr

data AST = Nop
         | Instruction Function AST
         | Flow Control AST AST
         -- | Scope Definition AST
  deriving (Eq, Show)

instance Arbitrary Function where
  arbitrary = frequency [ (4, liftA2 Set (choose (-4, 10)) arbitrary)
                        , (2, liftA Shift (choose (-4, 10)))
                        , (1, liftA PutChar arbitrary)
                        ]
  shrink = \case
    Set i e   -> map (Set i) $ shrink e
    Shift i   -> map Shift $ shrink i
    PutChar e -> map PutChar $ shrink e
    GetChar i -> map GetChar $ shrink i

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

instance Arbitrary AST where
  arbitrary = sized tree
    where
      tree 0 = return Nop
      tree n = frequency [ (4, liftA2 Instruction arbitrary subtree)
                         , (1, liftA3 Flow arbitrary subtree subtree) ]
        where
          subtree = tree (n `div` 2)

  shrink = initsAST

initAST :: AST -> AST
initAST = \case
  Nop               -> Nop
  Instruction _ Nop -> Nop
  Flow _ Nop _      -> Nop
  Flow _ _ Nop      -> Nop

  Instruction fun next -> Instruction fun (initAST next)
  Flow ctrl inner next -> Flow ctrl (initAST inner) (initAST next)

initsAST :: AST -> [AST]
initsAST = \case
  Nop -> []
  x   -> let x' = initAST x in x' : initsAST x'

join :: AST -> AST -> AST
join a b = case a of
  Nop                  -> b
  Instruction fun next -> Instruction fun (join next b)
  Flow ctrl inner next -> Flow ctrl inner (join next b)

filterAST :: (Function -> Bool) -> (Control -> Bool) -> AST -> AST
filterAST f g = \case
  Nop                              -> Nop
  Flow ctrl inner next | g ctrl    -> Flow ctrl (filterAST f g inner) (filterAST f g next)
                       | otherwise -> filterAST f g next
  Instruction fun next | f fun     -> Instruction fun (filterAST f g next)
                       | otherwise -> filterAST f g next

mapAST :: (Function -> Function) -> (Control -> Control) -> AST -> AST
mapAST f g = \case
  Nop                  -> Nop
  Flow ctrl inner next -> Flow (g ctrl) (mapAST f g inner) (mapAST f g next)
  Instruction fun next -> Instruction (f fun) (mapAST f g next)
