{-# LANGUAGE TemplateHaskell #-}

module STG.Interpreter where

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.State
import qualified Data.Map as M

import STG

data STGState = STGState { _code :: Code
                         , _argStack :: Stack Value
                         , _retStack :: Stack Cont
                         , _updStack :: Stack UpdateFrame
                         , _heap :: M.Map Addr Closure
                         , _globalEnv :: M.Map String Addr
                         , _nextUnique :: Addr
                         } deriving (Show, Eq)

makeLenses ''STGState

data Code = Eval Expr (M.Map String Addr)
          | Enter Addr
          | ReturnCon Constr [Value]
          | ReturnLit Int

data Arg

data Cont

data UpdateFrame

data Closure

type Addr = Int

type STG a = State STGState a

nextAddr :: STG Addr
nextAddr = do
    addr <- use nextUnique
    nextUnique .= addr+1
    pure addr


