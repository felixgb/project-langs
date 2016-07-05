{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Codegen where

import Data.Word
import Data.List
import Data.String
import Data.Function
import qualified Data.Map as Map

import Control.Monad.State
import Control.Applicative

import LLVM.General.AST
import LLVM.General.AST.Global
import qualified LLVM.General.AST as AST

import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST.Attribute as A
import qualified LLVM.General.AST.CallingConvention as CC
import qualified LLVM.General.AST.FloatingPointPredicate as FP

type SymbolTable = [(String, Operand)]

type Names = Map.Map String Int

instance IsString Name where
    fromString = Name . fromString

data BlockState
    = BlockState {
      idx       :: Int                          -- Block index
    , stack     :: [Named Instruction]          -- Stack of Instructions
    , term      :: Maybe (Named Terminator)     -- Block Terminator
    } deriving (Show)

data CodegenState
    = CodegenState {
      currentBlock      :: Name                         -- Name of current block
    , blocks            :: Map.Map Name BlockState      -- Blocks for functions
    , symTable          :: SymbolTable                  -- Function scope symbol table
    , blockCount        :: Int                          -- Count of basic blocks
    , count             :: Word                         -- Count of unnamed instructions
    , names             :: Names                        -- Name supply
    } deriving (Show)

newtype Codegen a = Codegen {runCodegen :: State CodegenState a}
    deriving (Functor, Applicative, Monad, MonadState CodegenState)

newtype LLVM a = LLVM {unLLVM :: State AST.Module a}
    deriving (Functor, Applicative, Monad, MonadState AST.Module)

runLLVM :: AST.Module -> LLVM a -> AST.Module
runLLVM = flip (execState . unLLVM)

emptyModule :: String -> AST.Module
emptyModule label = defaultModule {moduleName = label}

addDefn :: Definition -> LLVM ()
addDefn d = do
    defs <- gets moduleDefinitions
    modify $ \s -> s {moduleDefinitions = defs ++ [d]}

define :: Type -> String -> [(Type, Name)] -> [BasicBlock] -> LLVM ()
define retty label argtys body = addDefn $
    GlobalDefinition $ functionDefaults {
      name = Name label
    , parameters = ([Parameter ty nm [] | (ty, nm) <- argtys], False)
    , returnType = retty
    , basicBlocks = body
    }

external :: Type -> String -> [(Type, Name)] -> LLVM ()
external retty label argtys = addDefn $
    GlobalDefinition $ functionDefaults {
      name = Name label
    , parameters = ([Parameter ty nm [] | (ty, nm) <- argtys], False)
    , returnType = retty
    , basicBlocks = []
    }

entry :: Codegen Name
entry = gets currentBlock

addBlock :: String -> Codegen Name
addBlock bname = do
    bls <- gets blocks
    ix <- gets blockCount
    nms <- gets names

    let new = emptyBlock ix
        (qname, supply) = uniqueName bname nms

    modify $ \s -> s { blocks = Map.insert (Name qname) new bls
                     , blockCount = ix + 1
                     , names = supply
                     }
    return (Name qname)

setBlock :: Name -> Codegen Name
setBlock bname = do
    modify $ \s -> s { currentBlock = bname }
    return bname

getsBlock :: Codegen Name
getsBlock = gets currentBlock

modifyBlock :: BlockState -> Codegen ()
modifyBlock new = do
    active <- gets currentBlock
    modify $ \s -> s { blocks = Map.insert active new (blocks s) }

current :: Codegen BlockState
current = do
    c <- gets currentBlock
    blks <- gets blocks
    case Map.lookup c blks of
        Just x -> return x
        Nothing -> error $ "No such block: " ++ show c

emptyBlock :: Int -> BlockState
emptyBlock i = BlockState i [] Nothing

createBlocks :: CodegenState -> [BasicBlock]
createBlocks m = map makeBlock $ sortBlocks $ Map.toList (blocks m)

sortBlocks :: [(Name, BlockState)] -> [(Name, BlockState)]
sortBlocks = sortBy (compare `on` (idx . snd))

makeBlock :: (Name, BlockState) -> BasicBlock
makeBlock (l, (BlockState _ s t)) = BasicBlock l s (maketerm t)
    where
        maketerm (Just x) = x
        maketerm Nothing = error $ "Block has no terminator: " ++ (show l)

entryBlockName :: String
entryBlockName = "entry"

emptyCodegen :: CodegenState
emptyCodegen = CodegenState (Name entryBlockName) Map.empty [] 1 0 Map.empty

execCodegen :: Codegen a -> CodegenState
execCodegen m = execState (runCodegen m) emptyCodegen

uniqueName :: String -> Names -> (String, Names)
uniqueName nm ns =
    case Map.lookup nm ns of
        Nothing -> (nm, Map.insert nm 1 ns)
        Just ix -> (nm ++ show ix, Map.insert nm (ix + 1) ns)

fresh :: Codegen Word
fresh = do
    i <- gets count
    modify $ \s -> s { count = 1 + i }
    return $ i + 1

local :: Name -> Operand
local = LocalReference double

global :: Name -> C.Constant
global = C.GlobalReference double

externf :: Name -> Operand
externf = ConstantOperand . C.GlobalReference double

assign :: String -> Operand -> Codegen ()
assign var x = do
    lcls <- gets symTable
    modify $ \s -> s { symTable = [(var, x)] ++ lcls }

getvar :: String -> Codegen Operand
getvar var = do
    syms <- gets symTable
    case lookup var syms of
        Just x -> return x
        Nothing -> error $ "Can't find var: " ++ var

instr :: Instruction -> Codegen Operand
instr ins = do
    n <- fresh
    blk <- current
    let i = stack blk
    let ref = (UnName n)
    modifyBlock $ blk { stack = i ++ [ref := ins] }
    return $ local ref

terminator :: Named Terminator -> Codegen (Named Terminator)
terminator trm = do
    blk <- current
    modifyBlock $ blk { term = Just trm }
    return trm

-- Types -----------------------------------------------------------------------

double :: Type
double = FloatingPointType 64 IEEE

struct :: [Type] -> Type
struct elemTypes = StructureType False elemTypes


fadd :: Operand -> Operand -> Codegen Operand
fadd a b = instr $ FAdd NoFastMathFlags a b []

fsub :: Operand -> Operand -> Codegen Operand
fsub a b = instr $ FSub NoFastMathFlags a b []

fmul :: Operand -> Operand -> Codegen Operand
fmul a b = instr $ FMul NoFastMathFlags a b []

fdiv :: Operand -> Operand -> Codegen Operand
fdiv a b = instr $ FDiv NoFastMathFlags a b []

fcmp :: FP.FloatingPointPredicate -> Operand -> Operand -> Codegen Operand
fcmp cond a b = instr $ FCmp cond a b []

constant :: C.Constant -> Operand
constant = ConstantOperand

uitofp :: Type -> Operand -> Codegen Operand
uitofp ty a = instr $ UIToFP a ty []

branch :: Name -> Codegen (Named Terminator)
branch val = terminator $ Do $ Br val []

condBranch :: Operand -> Name -> Name -> Codegen (Named Terminator)
condBranch cond true false = terminator $ Do $ CondBr cond true false []

toArgs :: [Operand] -> [(Operand, [A.ParameterAttribute])]
toArgs = map (\x -> (x, []))

ret :: Operand -> Codegen (Named Terminator)
ret val = terminator $ Do $ Ret (Just val) []

call :: Operand -> [Operand] -> Codegen Operand
call fn args = instr $ Call Nothing CC.C [] (Right fn) (toArgs args) [] []

-- Alloca instruction allocates memory on the stack frame of the currently
-- executing instruction, to be automatically released when this fuction
-- returns to caller.
alloca :: Type -> Codegen Operand
alloca ty = instr $ Alloca ty Nothing 0 []

-- The store instuction writes to memory.
-- The first argument is the value to store and the second is an address at
-- which to store it. (for llvm, the args are backwards for the function
store :: Operand -> Operand -> Codegen Operand
store ptr val = instr $ Store False ptr val Nothing 0 []

-- The load instruction reads from memory.
load :: Operand -> Codegen Operand
load ptr = instr $ Load False ptr Nothing 0 []
