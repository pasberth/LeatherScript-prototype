{-# LANGUAGE TypeFamilies #-}

module Language.LeatherScript.Types where

import qualified GHC.Exts
import qualified Data.Vector as Vector

instance GHC.Exts.IsList (Vector.Vector a) where
  type Item (Vector.Vector a) = a
  fromList  = Vector.fromList
  toList    = Vector.toList
  fromListN = Vector.fromListN

