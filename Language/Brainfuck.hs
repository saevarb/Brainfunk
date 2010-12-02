{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.Brainfuck
    ( runProgram
    , Brainfunk (..)
    )
    where

import Data.Word
import Control.Monad
import Control.Monad.State
import Data.Char  

import ListZipper
    
type Buffer = ListZipper Word8
type Program = ListZipper Char
type BFContext = (Buffer, Program)

newtype Brainfunk a =
    Brainfunk {
        unFunk :: StateT BFContext IO a
    } deriving 
        ( Monad, Functor, MonadIO
        , MonadState BFContext )

runProgram :: BFContext -> IO BFContext 
runProgram = runBrainfunk runCode 

runBrainfunk :: Brainfunk a -> BFContext -> IO BFContext
runBrainfunk funk = 
    execStateT (unFunk funk)

runCode :: Brainfunk ()
runCode = do
    i <- getInstruction
    case i of
      (Just i') -> parseInstruction i' >> nextInstruction >> runCode
      _         -> return ()

parseInstruction :: Char -> Brainfunk ()
parseInstruction '>' = nextByte 
parseInstruction '<' = prevByte
parseInstruction '+' = incByte
parseInstruction '-' = decByte
parseInstruction '.' = outputByte
parseInstruction ',' = inputByte
parseInstruction '[' = do
    b <- getByte
    when (b == 0) skipLoop
parseInstruction ']' = do
    b <- getByte
    when (b /= 0) repeatLoop
parseInstruction _  = return ()

nextInstruction :: Brainfunk ()
nextInstruction = do
    (buf, prog) <- get
    case forward prog of 
      (Just p) -> put (buf, p)
      _        -> fail "Attempt to get next instruction at end."

prevInstruction :: Brainfunk ()
prevInstruction = do
    (buf, prog) <- get
    case back prog of
      (Just p) -> put (buf, p)
      _        -> fail "Attempt to go back at beginning of instruction buffer."

getInstruction :: Brainfunk (Maybe Char)
getInstruction = do
    s <- get
    case s of
      (_, (_, []))  -> return Nothing
      (_, (_, p:_)) -> return $ Just p 

repeatLoop :: Brainfunk ()
repeatLoop = do
    i <- getInstruction
    case i of
      (Just '[') -> return ()
      (Just _  ) -> prevInstruction >> repeatLoop
      Nothing    -> fail "Unexpected error."

skipLoop :: Brainfunk ()
skipLoop = do
    i <- getInstruction
    case i of
      (Just ']') -> return ()
      (Just _  ) -> nextInstruction >> skipLoop
      Nothing    -> fail "Unexpected error."

nextByte :: Brainfunk ()
nextByte = do
    (buf, prog) <- get
    case forward buf of
      (Just b) -> put (b, prog)
      _        -> fail "Attempt to get next byte at end of buffer."

prevByte :: Brainfunk ()
prevByte = do
    (buf, prog) <- get
    case back buf of
      (Just b) -> put (b, prog)
      _        -> fail "Attempt to go back at beginning of buffer."

getByte :: Brainfunk Word8
getByte = do
    g <- get
    case g of
      ((_, b:_), _) -> return b  
      _             -> fail "Out of buffer space."

putByte :: Word8 -> Brainfunk ()
putByte b = do
    ((t, _:bs), p) <- get
    put ((t, b:bs), p)

incByte :: Brainfunk ()
incByte = 
    getByte >>= putByte . succ

decByte :: Brainfunk ()
decByte = 
    getByte >>= putByte . pred

outputByte :: Brainfunk ()
outputByte = do
    b <- getByte
    liftIO . putChar . chr . fromIntegral $ b

inputByte :: Brainfunk ()
inputByte = do
    c <- liftIO getChar
    putByte . fromIntegral . ord $ c
    
