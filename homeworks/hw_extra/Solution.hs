{-# LANGUAGE BlockArguments #-}

import Control.Monad.State (State, runState,get,put, execState,evalState, withState, modify)
import Language.Haskell.TH (safe)
import Control.Monad.Writer (Writer,runWriter,tell)
--newtype Writer w a = Writer { runWriter :: (a, w) }
--newtype State  s a = State  { runState  :: s -> (a, s) }
-- tell :: Monoid w => w -> Writer w ()
newtype WriterS w a = WriterS { unWriterS :: State w a }

instance Functor (WriterS w) where
    -- fmap :: (a -> b) -> WriterS w a ->WriterS w b
    fmap f (WriterS act)= WriterS $  do
        a<-act
        return (f a)


instance Monoid w => Applicative (WriterS w) where
    -- pure::a->WriterS w a
       pure a = WriterS (pure a)
   -- (<*>) :: WriterS w(a -> b) -> WriterS w a -> WriterS w b 
       (<*>) (WriterS fState) (WriterS aState)=WriterS $ do
        a<-aState
        f<-fState
        return (f a)

instance Monoid w=> Monad (WriterS w) where

    -- >>=:: WriterS w a -> (a-> WriterS w b) -> WriterS w b  
    (>>=) (WriterS aState) f = WriterS $ do
        a<- aState
        let WriterS bstate = f a
        bstate
tellS       :: Monoid w => w -> WriterS w ()
tellS new = WriterS $ modify (\old-> old<>new)

runWriterS  :: Monoid w => WriterS w a -> (a, w)
runWriterS (WriterS wSt)=
    let (a,w ) = runState wSt mempty in
    (a,w)

execWriterS :: Monoid w => WriterS w a -> w

execWriterS (WriterS wSt) =
    let (_,w ) = runState wSt mempty in
    w
toWriterS   :: Monoid w => Writer w a  -> WriterS w a

toWriterS wr= do
    let (a,w)=runWriter wr
    tellS w
    return a
fromWriterS :: Monoid w => WriterS w a -> Writer w a
fromWriterS wrS = do
    let (a,w) = runWriterS wrS
    tell w
    return a  
-- Writer  w a -> WriterS w a -> Writer w a
-- for toWriterS :
---- let input be Writer <msg> val
---- we extract the <msg> and val with runWriter,
---- then we create an empty WriterS and use tellS to add the <msg> to it, then we return the val as the result of the WriterS
---- finally we return to change context from ()
-- for fromWriterS:
---- let input be WriterS <msg> val
---- we extract the <msg> and val with runWriterS,
---- then we create an empty Writer and use tell to add the <msg> to it,
---- then we return the val as the result of the Writer
--by changing the order of the steps we get 
-- WriterS w a -> Writer w a -> WriterS w a
-- most importtant part is the tell/tellS.
-- it relies on the fact that runWriter returns whole log as the answer.
-- however if it didnt (w wasnt a monad) the implementation would most porbably crash.
-- the rest is changing context/wrapping resulting value.
-- the impelemtation of WriterS is based on keeping explicit buffer of messages as state.
-- as the calculation doesnt depend on it, we are free to use state as a buffer.
-- 
complexTask :: WriterS [String] Int
complexTask = do
    tellS ["Starting in WriterS"]
    -- Convert a standard Writer action into our WriterS
    val <- toWriterS (tell ["Logging in standard Writer"] >> return 42)
    tellS ["Finishing in WriterS"]
    return val

main::IO()
main=do
    print $  execWriterS complexTask