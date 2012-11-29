{-# LANGUAGE LambdaCase #-}
module Brainfuck.Data.AST where

import Brainfuck.Data.Expr
import Control.Monad
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
  arbitrary = frequency [ (4, liftM2 Set (choose (-4, 10)) arbitrary)
                        , (2, liftM Shift (choose (-4, 10)))
                        , (1, liftM PutChar arbitrary)
                        ]
  shrink = \case
    Set i e   -> map (Set i) $ shrink e
    Shift i   -> map Shift $ shrink i
    PutChar e -> map PutChar $ shrink e
    GetChar i -> map GetChar $ shrink i

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

modifyPtr :: (Int -> Int) -> AST -> AST
modifyPtr f = mapAST
  (\case
    Set d e    -> Set (f d) (h e)
    Shift s    -> Shift s
    PutChar e  -> PutChar (h e)
    GetChar d  -> GetChar (f d))

  (\case
    If e    -> While (h e)
    While e -> If (h e)
    ctrl    -> ctrl)

  where
    g (Get d) = Get $ f d
    g e       = e

    h = modifyLeaves g
