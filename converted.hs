{-# LANGUAGE GADTs, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TypeOperators, DataKinds, RankNTypes #-} 
import Control.Eff
import Control.Eff.Extend
import Debug.Trace

import Prelude hiding (print,safedivision, throw, fun, flip, max, get, fibonnaci, set, test, aaa, bbb, or, xor, foo)


data Console x where
    Print :: x -> Console ()

print x = send (Print x)

data Exception x where
   Throw :: String -> Exception () 

throw x1  = send (Throw x1 )

data Amb x where
   Flip :: Amb Bool 

flip () = send Flip

data State x where
   Get :: State Int 
   Set :: Int -> State () 

get () = send Get
set x1  = send (Set x1 )

eapp :: Monad m => m (a -> m b) -> m a -> m b
eapp f x = do
   res <- f <*> x
   res


safedivision :: (Member Exception t1) => Int -> Eff t0 (Int -> Eff t1 Int )
safedivision = \a0 -> 
 return $ \b0 -> do
           let bl2 _ = do 
                   let bl3 _ = do 
                           _ <- ( return throw `eapp` return "\"can't divide by zero\"")
                           ( return bl4 `eapp` return ())
                       bl4 _ = do 
                           return (a0  `div`  b0)
                   t19 <- ( ( == ) <$> return b0 <*> return 0)
                   if t19
                   then ( return bl3 `eapp` return ())
                   else ( return bl4 `eapp` return ())
           ( return bl2 `eapp` return ())

fun :: a -> Eff t0 Int 
fun = \_ -> do
 let bl2 _ = do 
         return 2
 ( return bl2 `eapp` return ())

max :: Int -> Eff t0 (Int -> Eff t1 Int )
max = \n0 -> 
 return $ \m0 -> do
           let bl2 _ = do 
                   let bl3 _ = do 
                           return n0
                       bl4 _ = do 
                           return m0
                   t19 <- return (n0  >  m0)
                   if t19
                   then ( return bl3 `eapp` return ())
                   else ( return bl4 `eapp` return ())
           ( return bl2 `eapp` return ())

fibonnaci :: Int -> Eff t0 Int 
fibonnaci = \n0 -> do
 let bl2 _ = do 
         x1 <- return 0
         let bl3 _ = do 
                 y1 <- return 1
                 let bl4 _ = do 
                         z1 <- return 3
                         let bl5 z2 = 
                                 return $ \y2 -> 
                                 return $ \x2 -> 
                                 return $ \w1 -> 
                                 return $ \_ -> do 
                                 let bl6 _ = do 
                                         w2 <- return (x2  +  y2)
                                         let bl7 _ = do 
                                                 x3 <- return y2
                                                 let bl8 _ = do 
                                                         y3 <- return w2
                                                         let bl9 _ = do 
                                                                 z3 <- ( ( + ) <$> return z2 <*> return 1)
                                                                 ((((( return bl5 `eapp` return z3) `eapp` return y3) `eapp` return x3) `eapp` return w2) `eapp` return ())
                                                         ( return bl9 `eapp` return ())
                                                 ( return bl8 `eapp` return ())
                                         ( return bl7 `eapp` return ())
                                     bl10 _ = do 
                                          return y2
                                 t33 <- return (z2  <  n0)
                                 if t33
                                 then ( return bl6 `eapp` return ())
                                 else ( return bl10 `eapp` return ())
                         ((((( return bl5 `eapp` return z1) `eapp` return y1) `eapp` return x1) `eapp` ( return error `eapp` return "uninitialized")) `eapp` return ())
                 ( return bl4 `eapp` return ())
         ( return bl3 `eapp` return ())
 ( return bl2 `eapp` return ())

test :: a -> Eff t0 (Int -> Eff t1 Int )
test = \_ -> do
 let bl2 _ = do 
         ( return max `eapp` ( return fibonnaci `eapp` return 10))
 ( return bl2 `eapp` return ())

aaa :: (Member Amb t3) => Int -> Eff t0 (Int -> Eff t1 (Int -> Eff t2 (Int -> Eff t3 Int )))
aaa = \a0 -> 
 return $ \b0 -> 
           return $ \c0 -> 
                     return $ \d0 -> do
                               let bl2 _ = do 
                                       let bl3 _ = do 
                                               return (a0  +  b0)
                                           bl4 _ = do 
                                               return (c0  +  d0)
                                       t39 <- ( return flip `eapp` return ())
                                       if t39
                                       then ( return bl3 `eapp` return ())
                                       else ( return bl4 `eapp` return ())
                               ( return bl2 `eapp` return ())

bbb :: (Member State t1,Member Amb t3) => Int -> Eff t0 (Int -> Eff t1 (Int -> Eff t2 (Int -> Eff t3 Int )))
bbb = \a0 -> 
 return $ \b0 -> do
           let bl2 _ = do 
                   _ <- ( return set `eapp` return 10)
                   let bl3 _ = do 
                           (( return aaa `eapp` return a0) `eapp` return b0)
                   ( return bl3 `eapp` return ())
           ( return bl2 `eapp` return ())

or :: (Member Console t1) => Bool -> Eff t0 (b -> Eff t1 () )
or = \a0 -> 
 return $ \b0 -> do
           let bl2 _ = do 
                   let bl0 _ = do 
                           return ()
                       bl3 _ = do 
                           _ <- ( return print `eapp` return True)
                           ( return bl0 `eapp` return ())
                       bl4 _ = do 
                           _ <- ( return print `eapp` return b0)
                           ( return bl0 `eapp` return ())
                   t19 <- ( ( == ) <$> return a0 <*> return True)
                   if t19
                   then ( return bl3 `eapp` return ())
                   else ( return bl4 `eapp` return ())
           ( return bl2 `eapp` return ())

xor :: (Member Amb t0) => a -> Eff t0 Bool 
xor = \_ -> do
 let bl2 _ = do 
         p1 <- ( return flip `eapp` return ())
         let bl3 _ = do 
                 q1 <- ( return flip `eapp` return ())
                 let bl4 _ = do 
                         return p1
                 ( return bl4 `eapp` return ())
         ( return bl3 `eapp` return ())
 ( return bl2 `eapp` return ())

foo :: (Member Amb t0,Member State t0) => Int -> Eff t0 Bool 
foo = \threshold0 -> do
 let bl2 _ = do 
         p1 <- ( return flip `eapp` return ())
         let bl3 _ = do 
                 i1 <- ( return get `eapp` return ())
                 let bl4 _ = do 
                         _ <- ( return set `eapp` ( ( + ) <$> return i1 <*> return 1))
                         let bl5 _ = do 
                                 let bl6 _ = do 
                                         ( return xor `eapp` return ())
                                     bl7 _ = do 
                                         return False
                                 t33 <- return (i1  >  threshold0)
                                 if t33
                                 then ( return bl6 `eapp` return ())
                                 else ( return bl7 `eapp` return ())
                         ( return bl5 `eapp` return ())
                 ( return bl4 `eapp` return ())
         ( return bl3 `eapp` return ())
 ( return bl2 `eapp` return ())

