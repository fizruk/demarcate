{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.Monad.Trans.Demarcate.Internal (
    Demarcate,
    demarcateM, demarcateT,
    execDemarcate,
    transformDemarcateM,
) where

import Control.Monad.Trans.Class

newtype Demarcate t m a = Demarcate
    { unDemarcate :: forall r.
        (a -> r) ->
        (forall b.   m b -> (b -> Demarcate t m a) -> r) ->
        (forall b. t m b -> (b -> Demarcate t m a) -> r) ->
        r }

instance Functor (Demarcate t m) where
    fmap f (Demarcate g) = Demarcate $ \rf mf tf ->
      g (rf . f)
        (\mm mh -> mf mm (fmap f . mh))
        (\tm th -> tf tm (fmap f . th))

instance Monad (Demarcate t m) where
    return x = Demarcate $ \rf _ _ -> rf x
    Demarcate g >>= f =
      g f
        (\m h -> demarcateMonad m ((>>= f) . h))
        (\m h -> demarcateTrans m ((>>= f) . h))

instance MonadTrans (Demarcate t) where
    lift = demarcateM

demarcateMonad :: m b -> (b -> Demarcate t m a) -> Demarcate t m a
demarcateMonad m g = Demarcate $ \_ mf _ -> mf m g

demarcateTrans :: t m b -> (b -> Demarcate t m a) -> Demarcate t m a
demarcateTrans m g = Demarcate $ \_ _ tf -> tf m g

-- | Lift pure monadic computation into @Demarcate t m a@
demarcateM :: m a -> Demarcate t m a
demarcateM m = demarcateMonad m return

-- | Lift transformed monadic computation into @Demarcate t m a@
demarcateT :: t m a -> Demarcate t m a
demarcateT m = demarcateTrans m return

-- | Execute demarcated computation.
execDemarcate :: (Monad (t m), Monad m, MonadTrans t) => Demarcate t m a -> t m a
execDemarcate (Demarcate g) =
    g return
      (\m h -> lift m >>= execDemarcate . h)
      (\m h -> m >>= execDemarcate . h)

-- | Substitute monad computations with demarcated.
transformDemarcateM :: (forall b. m b -> Demarcate t m b) -> Demarcate t m a -> Demarcate t m a
transformDemarcateM phi (Demarcate g) =
    g return
      (\m h -> phi m >>= transformDemarcateM phi . h)
      (\m h -> demarcateTrans m (transformDemarcateM phi . h))

