{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ExistentialQuantification #-}

module Control.Monad.Trans.Demarcate.Internal (
    Demarcate,
    demarcateM, demarcateT,
    execDemarcate,
    transformDemarcateM,
) where

import Control.Monad.Trans.Class

data Demarcate t m a
    = Result a
    | forall b. DemarcateMonad (  m b) (b -> Demarcate t m a)
    | forall b. DemarcateTrans (t m b) (b -> Demarcate t m a)

instance Functor (Demarcate t m) where
    fmap f (Result x) = Result (f x)
    fmap f (DemarcateMonad m g) = DemarcateMonad m (fmap f . g)
    fmap f (DemarcateTrans m g) = DemarcateTrans m (fmap f . g)

instance Monad (Demarcate t m) where
    return = Result
    Result x >>= f = f x
    DemarcateMonad m g >>= f = DemarcateMonad m $ (>>= f) . g
    DemarcateTrans m g >>= f = DemarcateTrans m $ (>>= f) . g

instance MonadTrans (Demarcate t) where
    lift = demarcateM

-- | Lift pure monadic computation into @Demarcate t m a@
demarcateM :: m a -> Demarcate t m a
demarcateM m = DemarcateMonad m Result

-- | Lift transformed monadic computation into @Demarcate t m a@
demarcateT :: t m a -> Demarcate t m a
demarcateT m = DemarcateTrans m Result

-- | Execute demarcated computation.
execDemarcate :: (Monad (t m), Monad m, MonadTrans t) => Demarcate t m a -> t m a
execDemarcate (Result x) = return x
execDemarcate (DemarcateMonad m next) = lift m >>= execDemarcate . next
execDemarcate (DemarcateTrans m next) = m >>= execDemarcate . next

-- | Substitute monad computations with demarcated.
transformDemarcateM :: (forall b. m b -> Demarcate t m b) -> Demarcate t m a -> Demarcate t m a
transformDemarcateM phi (DemarcateMonad m next) = phi m >>= transformDemarcateM phi . next
transformDemarcateM phi (DemarcateTrans m next) = DemarcateTrans m (transformDemarcateM phi . next)
transformDemarcateM _ m = m
