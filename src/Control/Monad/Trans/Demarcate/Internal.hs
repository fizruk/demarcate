{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.Monad.Trans.Demarcate.Internal (
    Demarcate,
    demarcateM, demarcateT,
    execDemarcate,
    hoistDemarcateT,
    transformDemarcateM,
) where

import Control.Monad.Free
import Control.Monad.Trans.Class

data DemarcateF t m next
    = forall a. DemarcateMonad (  m a) (a -> next)
    | forall a. DemarcateTrans (t m a) (a -> next)

instance Functor (DemarcateF t m) where
    fmap f (DemarcateMonad m g) = DemarcateMonad m (f . g)
    fmap f (DemarcateTrans m g) = DemarcateTrans m (f . g)

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

demarcateM :: m a -> Demarcate t m a
demarcateM m = liftF $ DemarcateMonad m id

demarcateT :: t m a -> Demarcate t m a
demarcateT m = liftF $ DemarcateTrans m id

execDemarcate :: (Monad (t m), Monad m, MonadTrans t) => Demarcate t m a -> t m a
execDemarcate = iterM execDemarcateF . unDemarcate
  where
    execDemarcateF (DemarcateMonad m next) = lift m >>= next
    execDemarcateF (DemarcateTrans m next) = m >>= next

hoistDemarcateT :: (forall b. t m b -> t' m b) -> Demarcate t m a -> Demarcate t' m a
hoistDemarcateT phi = iterM hoistDemarcateF . unDemarcate
  where
    hoistDemarcateF (DemarcateMonad m next) = demarcateM m >>= next
    hoistDemarcateF (DemarcateTrans m next) = demarcateT (phi m) >>= next

transformDemarcateM :: (forall b. m b -> Demarcate t m b) -> Demarcate t m a -> Demarcate t m a
transformDemarcateM phi = iterM transformDemarcateF . unDemarcate
  where
    transformDemarcateF (DemarcateMonad m next) = phi m >>= next
    transformDemarcateF (DemarcateTrans m next) = demarcateT m >>= next

wrapInner :: Functor f => f (t (Free f) a) -> t (Free f) a
wrapInner = join . lift . liftF
