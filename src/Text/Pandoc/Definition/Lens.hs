{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

------------------------------------------------------------------------
-- |
-- Module      :  Text.Pandoc.Definition.Lens
-- Copyright   :  (C) 2015 Christopher Chalmers
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Christopher Chalmers <c.chalmers@me.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- 'Prism's, 'Lens'es and 'Traversal's for and orphan instances 'Pandoc'
-- types.
------------------------------------------------------------------------

module Text.Pandoc.Definition.Lens
  ( -- $orphans

    -- * Documents
    body
  , meta

  -- * Generics traversals
  -- | These traversals are simply 'template'.
  , attributes
  , inlines
  , blocks
  , citations
  , formats
  , targets

  -- * Block prisms
  , _Plain
  , _Para
  , _CodeBlock
  , _RawBlock
  , _BlockQuote
  , _OrderedList
  , _BulletList
  , _DefinitionList
  , _Header
  , _HorizontalRule
  , _Table
  , _Div
  , _Null

  -- ** Inline prisms
  , _Str
  , _Emph
  , _Strong
  , _Strikeout
  , _Superscript
  , _Subscript
  , _SmallCaps
  , _Quoted
  , _Cite
  , _Code
  , _Space
  , _LineBreak
  , _Math
  , _RawInline
  , _Link
  , _Image
  , _Note
  , _Span

  -- ** Meta prisms
  , _MetaMap
  , _MetaList
  , _MetaBool
  , _MetaString
  , _MetaInlines
  , _MetaBlocks

  -- ** Citation lenses
  , citeId
  , citePrefix
  , citeSuffix
  , citeMode
  , citeNoteNum
  , citeHash

  , module Text.Pandoc.Definition

  ) where

import Control.Applicative
import Control.Lens
import Data.Data              (Data)
import Data.Data.Lens
import Data.Semigroup

import Text.Pandoc.Builder
import Text.Pandoc.Definition


-- | The 'Block's of a 'Pandoc'.
body :: Lens' Pandoc [Block]
body f (Pandoc m bs) = f bs <&> \bs' -> Pandoc m bs'
{-# INLINE body #-}

-- | The 'Meta' of a 'Pandoc'.
meta :: Lens' Pandoc Meta
meta f (Pandoc m bs) = f m <&> \m' -> Pandoc m' bs
{-# INLINE meta #-}

-- Generics

-- | Traversal over all 'Attr's of a structure (usually 'Pandoc',
--   'Block' or 'Inline').
attributes :: Data a => Traversal' a Attr
attributes = template

-- | Traversal over all 'Block's of a structure that are not contained
--   in another 'Block'. (usually @a@ is 'Pandoc', 'Block' or 'Inline').
blocks :: Data a => Traversal' a Block
blocks = template

-- | Traversal over all 'Inline's of a structure that are not contained
--   in another 'Inline'. (usually @a@ is 'Pandoc', 'Block' or 'Inline').
inlines :: Data a => Traversal' a Inline
inlines = template

-- | Traversal over all 'Citation's of a structure (usually @a@ is 'Pandoc',
--   'Block' or 'Inline').
citations :: Data a => Traversal' a Citation
citations = template

-- | Traversal over all 'Target's of a structure (usually @a@ is 'Pandoc',
--   'Block' or 'Inline').
targets :: Data a => Traversal' a Target
targets = template

-- | Traversal over all 'Format's of a structure (usually @a@ is 'Pandoc',
--   'Block' or 'Inline').
formats :: Data a => Traversal' a Format
formats = template

-- Template haskell

makeWrapped ''Meta
makeWrapped ''Format
makeWrapped ''Many

makePrisms ''Block
makePrisms ''Inline
makePrisms ''MetaValue

makeLensesFor
  [ ("citationId"     , "citeId")
  , ("citationPrefix" , "citePrefix")
  , ("citationSuffix" , "citeSuffix")
  , ("citationMode"   , "citeMode")
  , ("citationNoteNum", "citeNoteNum")
  , ("citationHash"   , "citeHash")
  ] ''Citation

-- $orphans
-- The following orphan instances are provided:
-- * 'Plated': 'Block', 'Inline'
-- * 'Wrapped': 'Meta', 'Format', 'Many'
-- * 'At' and 'Ixed': Meta
-- * 'Semigroup': 'Pandoc', 'Meta', 'Inlines', 'Blocks'
-- * 'Each': 'Meta', 'Many'

instance Plated Inline
instance Plated Block

type instance IxValue Meta = MetaValue
type instance Index Meta   = String
instance Ixed Meta where
  ix = ixAt
instance At Meta where
  at i = _Wrapped . at i

instance Each Meta Meta MetaValue MetaValue where
  each = _Wrapped . each
instance Each (Many a) (Many b) a b where
  each = _Wrapped . each

instance Semigroup Meta
instance Semigroup Pandoc

-- not sure why Pandoc doesn't have Monoid (Many a) instance
instance Semigroup (Many Inline)
instance Semigroup (Many Block)

-- Potential 'Many' instances: Snoc Cons Ixed

class HasAttr a where
  -- | Traversal over the top level attributes of an object.
  attr :: Traversal' a Attr

instance HasAttr Block where
  attr f (CodeBlock a s) = f a <&> \a' -> CodeBlock a' s
  attr f (Header n a s)  = f a <&> \a' -> Header n a' s
  attr f (Div a s)       = f a <&> \a' -> Div a' s
  attr _ x               = pure x

instance HasAttr Inline where
  attr f (Code a s) = f a <&> \a' -> Code a' s
  attr f (Span a s) = f a <&> \a' -> Span a' s
  attr _ x          = pure x

-- attrKeysAt :: String -> Lens Attr String
-- attrKeysAt s = _3 . _Unwrapping M.fromList . at s

-- attrKeysIx :: String -> Traversal' Attr String
-- attrKeysIx s = _3 . _Unwrapping M.fromList . ix s
