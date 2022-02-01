
module Helpers where

import Control.Monad
import Data.Monoid
import System.FilePath

infixr 5 +/+
(+/+) :: String -> String -> String
a +/+ b = a ++ [pathSeparator] ++ b

altErrors :: Either String a -> Either String a -> Either String a
altErrors (Left a) (Left b) = Left (a ++ "\n" ++ b)
altErrors (Left _) (Right b) = Right b
altErrors (Right a) _ = Right a

-- SingleRes

data SingleRes a = NoRes | Res a | ManyRes

instance Semigroup (SingleRes a) where
  NoRes <> t = t
  t <> NoRes = t
  ManyRes <> t = ManyRes
  t <> ManyRes = ManyRes
  Res _ <> Res _ = ManyRes

instance Monoid (SingleRes a) where
  mempty = NoRes

instance Monad SingleRes  where
  return = Res
  Res a >>= k = k a
  NoRes >>= _ = NoRes
  ManyRes >>= _ = ManyRes

instance Functor SingleRes where
  fmap = liftM

instance Applicative SingleRes where
  pure = return
  (<*>) = ap

singleRes :: r -> r -> (a -> r) -> SingleRes a -> r
singleRes handleNoRes handleManyRes handleRes = \case
  NoRes -> handleNoRes
  Res a -> handleRes a
  ManyRes -> handleManyRes

resToMaybe = singleRes Nothing Nothing Just
