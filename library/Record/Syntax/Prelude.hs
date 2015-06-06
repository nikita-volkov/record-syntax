module Record.Syntax.Prelude
( 
  module Exports,
  LazyText,
  TextBuilder,
  (∘),
  (∘∘),
  zipTraversableWith,
  zipTraversableWithM,
)
where


-- base-prelude
-------------------------
import BasePrelude as Exports hiding (left, right, isLeft, isRight)

-- transformers
-------------------------
import Control.Monad.Trans.State.Strict as Exports hiding (liftCallCC, liftCatch, liftListen, liftPass)
import Control.Monad.Trans.Reader as Exports hiding (liftCallCC, liftCatch, liftListen, liftPass)
import Control.Monad.Trans.Writer as Exports hiding (liftCallCC, liftCatch, liftListen, liftPass)
import Control.Monad.Trans.Maybe as Exports hiding (liftCallCC, liftCatch, liftListen, liftPass)
import Control.Monad.Trans.Class as Exports
import Control.Monad.IO.Class as Exports
import Data.Functor.Identity as Exports

-- text
-------------------------
import Data.Text as Exports (Text)

-- conversion
-------------------------
import Conversion as Exports
import Conversion.Text as Exports


import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB


type LazyText = TL.Text
type TextBuilder = TLB.Builder


(∘) :: (a -> b) -> (c -> a) -> (c -> b)
(∘) =
  (.)

(∘∘) :: (a -> b) -> (c -> d -> a) -> (c -> d -> b)
(∘∘) a b =
  \c d -> a (b c d)


zipTraversableWith :: (Traversable t1, Foldable t2) => (a1 -> a2 -> a3) -> t1 a1 -> t2 a2 -> t1 a3
zipTraversableWith f t1 t2 =
  flip evalState (toList t2) $ flip traverse t1 $ \a1 -> state $ \case
    a2 : tail -> (f a1 a2, tail)

zipTraversableWithM :: (Traversable t1, Foldable t2, Applicative m, Monad m) => (a1 -> a2 -> m a3) -> t1 a1 -> t2 a2 -> m (t1 a3)
zipTraversableWithM f t1 t2 =
  flip evalStateT (toList t2) $ flip traverse t1 $ \a1 -> StateT $ \case
    a2 : tail -> (,) <$> f a1 a2 <*> pure tail



