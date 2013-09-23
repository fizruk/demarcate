{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
---------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Trans.Demarcate.Internal
-- Copyright   :  (c) Nickolay Kudasov 2013
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  nickolay.kudasov@gmail.com
-- Stability   :  experimental
-- Portability :  ghc
--
-- Internals of the 'Demarcate' monad transformer.
---------------------------------------------------------------------------
module Control.Monad.Trans.Demarcate.Internal where

import Control.Monad.Free
import Control.Monad.Trans.Class
import Control.Monad (join)

import Control.Monad.State.Class
import Control.Monad.Writer.Class
import Control.Monad.Reader.Class
import Control.Monad.RWS.Class
import Control.Monad.Error.Class
import Control.Monad.Cont.Class

-- | Demarcate functor.
data DemarcateF t m next
    = forall a. DemarcateMonad (  m a) (a -> next)  -- ^ Unlifted monadic computation.
    | forall a. DemarcateTrans (t m a) (a -> next)  -- ^ Transformed monadic computation.

instance Functor (DemarcateF t m) where
    fmap f (DemarcateMonad m g) = DemarcateMonad m (f . g)
    fmap f (DemarcateTrans m g) = DemarcateTrans m (f . g)

-- | Demarcate monad transformer.
newtype Demarcate t m a = Demarcate
    { unDemarcate :: Free (DemarcateF t m) a }

instance Functor (Demarcate t m) where
    fmap f = Demarcate . fmap f . unDemarcate

instance Monad (Demarcate t m) where
    return  = Demarcate . return
    m >>= f = Demarcate $ unDemarcate m >>= unDemarcate . f

instance MonadFree (DemarcateF t m) (Demarcate t m) where
    wrap = Demarcate . wrap . fmap unDemarcate

instance MonadTrans (Demarcate t) where
    lift m = liftF $ DemarcateMonad m id

instance (MonadState s (t m)) => MonadState s (Demarcate t m) where
    get = demarcateT get
    put = demarcateT . put

instance (MonadReader e (t m)) => MonadReader e (Demarcate t m) where
    ask    = demarcateT ask
    reader = demarcateT . reader
    local f = hoistDemarcateT $ local f

-- | [Warning: 'listen' and 'pass' arguments are not visible for 'transformDemarcateM'.]
instance (Monad m, MonadTrans t, MonadWriter w (t m)) => MonadWriter w (Demarcate t m) where
    tell   = demarcateT . tell
    listen = demarcateT . listen . execDemarcate
    pass   = demarcateT . pass . execDemarcate

-- | [Warning: 'listen' and 'pass' arguments are not visible for 'transformDemarcateM'.]
instance (Monad m, MonadTrans t, MonadRWS r w s (t m)) => MonadRWS r w s (Demarcate t m)

-- | [Warning: 'catchError' arguments are not visible for 'transformDemarcateM'.]
instance (Monad m, MonadTrans t, MonadError e (t m)) => MonadError e (Demarcate t m) where
    throwError = demarcateT . throwError
    catchError t c = demarcateT $
      catchError (execDemarcate t) (execDemarcate . c)

-- | [Warning: 'callCC' argument is not visible for 'transformDemarcateM'.]
instance (Monad m, MonadTrans t, MonadCont (t m)) => MonadCont (Demarcate t m) where
    callCC f = demarcateT $ callCC (\k ->
                                   execDemarcate (f (\x ->
                                                 demarcateT $ k x)))

-- | Lift pure monadic computation into @Demarcate t m a@
demarcateM :: m a -> Demarcate t m a
demarcateM m = liftF $ DemarcateMonad m id

-- | Lift transformed monadic computation into @Demarcate t m a@
demarcateT :: t m a -> Demarcate t m a
demarcateT m = liftF $ DemarcateTrans m id

-- | Execute demarcated computation.
execDemarcate :: (Monad (t m), Monad m, MonadTrans t) => Demarcate t m a -> t m a
execDemarcate = iterM execDemarcateF . unDemarcate
  where
    execDemarcateF (DemarcateMonad m next) = lift m >>= next
    execDemarcateF (DemarcateTrans m next) = m >>= next

-- | Subsitute monad transformer.
hoistDemarcateT :: (forall b. t m b -> t' m b) -> Demarcate t m a -> Demarcate t' m a
hoistDemarcateT phi = iterM hoistDemarcateF . unDemarcate
  where
    hoistDemarcateF (DemarcateMonad m next) = demarcateM m >>= next
    hoistDemarcateF (DemarcateTrans m next) = demarcateT (phi m) >>= next

-- | Substitute monad computations with demarcated.
transformDemarcateM :: (forall b. m b -> Demarcate t m b) -> Demarcate t m a -> Demarcate t m a
transformDemarcateM phi = iterM transformF . unDemarcate
  where
    transformF (DemarcateMonad m next) = phi m >>= next
    transformF (DemarcateTrans m next) = demarcateT m >>= next

-- | Substitute free monad actions with demarcated monad computations.
transformDemarcateFree :: (Functor f) =>
  (forall b. f (Demarcate t (Free f) b) -> Demarcate t (Free f) b) -> Demarcate t (Free f) a -> Demarcate t (Free f) a
transformDemarcateFree phi = transformDemarcateM (iterM phi)

-- | Helper function (useful with @transformDemarcateFree@).
-- I believe it should be somewhere in @Control.Monad.Free@
wrapT :: (Functor f, MonadFree f m, MonadTrans t, Monad (t m)) => f (t m a) -> t m a
wrapT = join . lift . liftF

