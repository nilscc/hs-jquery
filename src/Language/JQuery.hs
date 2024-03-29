{-# LANGUAGE OverloadedStrings #-}

module Language.JQuery
  ( JQuery, JQueryStmts, JQueryVar, JQueryObject
     -- * JQuery functions
  , module Language.JQuery.Functions
    -- * HTML conversion
  , module Language.JQuery.JavaScript
  ) where

import Language.JQuery.Functions
import Language.JQuery.Types
import Language.JQuery.JavaScript
