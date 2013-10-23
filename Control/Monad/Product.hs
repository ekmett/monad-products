-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Product
-- Copyright   :  (C) 2011-2013 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- Monad Products 
----------------------------------------------------------------------------

module Control.Monad.Product
  ( Product(..)
  ) where

import Control.Applicative
import Data.Functor.Bind
import Data.Functor.Alt
import Control.Monad

-- | disjoint 'Graph' union as a 'Monad' product
newtype Product g h a = Product { runProduct :: (g a, h a) } 

instance (Functor g, Functor h) => Functor (Product g h) where
  fmap f (Product (g, h)) = Product (fmap f g, fmap f h)
  b <$ Product (g, h) = Product (b <$ g, b <$ h)

instance (Apply g, Apply h) => Apply (Product g h) where
  Product (gf, hf) <.> Product (ga, ha) = Product (gf <.> ga, hf <.> ha)
  Product (gf, hf) <.  Product (ga, ha) = Product (gf <.  ga, hf <.  ha)
  Product (gf, hf)  .> Product (ga, ha) = Product (gf  .> ga, hf  .> ha)

instance (Applicative g, Applicative h) => Applicative (Product g h) where
  pure a = Product (pure a, pure a)
  Product (gf, hf) <*> Product (ga, ha) = Product (gf <*> ga, hf <*> ha)
  Product (gf, hf) <*  Product (ga, ha) = Product (gf <*  ga, hf <*  ha)
  Product (gf, hf)  *> Product (ga, ha) = Product (gf  *> ga, hf  *> ha)

instance (Bind g, Bind h) => Bind (Product g h) where
  Product (g, h) >>- k = Product (g >>- fst . runProduct . k, h >>- snd . runProduct . k)
  
instance (Monad g, Monad h) => Monad (Product g h) where
  return a = Product (return a, return a)
  Product (g, h) >>= k = Product (g >>= fst . runProduct . k, h >>= snd . runProduct . k)
  Product (ga, ha) >> Product (gb, hb) = Product (ga >> gb, ha >> hb)

instance (Alt g, Alt h) => Alt (Product g h) where
  Product (ga, ha) <!> Product (gb, hb) = Product (ga <!> gb, ha <!> hb)

instance (Alternative g, Alternative h) => Alternative (Product g h) where
  empty = Product (empty, empty)
  Product (ga, ha) <|> Product (gb, hb) = Product (ga <|> gb, ha <|> hb)

instance (MonadPlus g, MonadPlus h) => MonadPlus (Product g h) where
  mzero = Product (mzero, mzero)
  Product (ga, ha) `mplus` Product (gb, hb) = Product (ga `mplus` gb, ha `mplus` hb)
