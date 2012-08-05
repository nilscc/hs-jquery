{-# LANGUAGE MultiParamTypeClasses, GeneralizedNewtypeDeriving,
             FlexibleInstances, UndecidableInstances, GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ViewPatterns #-}

module Language.JQuery.Internal.Types where

import Control.Monad.State

import Text.Blaze.Html
import Text.Blaze.Html.Renderer.String
import Language.JavaScript.AST

data JQueryStmt where

  JQS_jQuery       :: JQuerySelector sel =>                sel -> JQueryStmts t -> JQueryStmt
  JQS_bind         :: JQuerySelector sel => JQueryVar t -> sel -> JQueryStmts t -> JQueryStmt


data JQueryStmts t where

  JQSs_chain        :: JQueryStmts a -> JQueryStmts b -> JQueryStmts b
  JQSs_call         :: String -> [Expr] -> JQueryStmts a

newtype JQuery a = JQuery { unJQ :: State JQueryS a }

data JQueryS = JQueryS
  { jqueryLastStmt  :: Maybe JQueryStmt
  , jqueryLastVarId :: Int
  , jqueryChain     :: JQueryChain
  }

data JQueryChain where

  JQC_empty  ::                               JQueryChain
  JQC_single ::                 JQueryStmt -> JQueryChain
  JQC_chain  ::  JQueryChain -> JQueryStmt -> JQueryChain


--------------------------------------------------------------------------------
-- newtypes

newtype JQueryObject = JQueryObject Name


--------------------------------------------------------------------------------
-- Variables

newtype JQueryVar a = JQueryVar Name


--------------------------------------------------------------------------------
-- Values

class JavaScriptValue t a | a -> t where
  valToExpr :: a -> Expr

instance JavaScriptValue String String where
  valToExpr (jsString -> Right jss) = ExprLit (LitString jss)
  valToExpr s = error $ "`JavaScriptValue valToExpr' conversion error of String \"" ++ s ++ "\""

instance JavaScriptValue Html Html where
  valToExpr h = valToExpr (renderHtml h)

instance JavaScriptValue Double Double where
  valToExpr d = ExprLit (LitNumber (Number d))

instance JavaScriptValue Bool Bool where
  valToExpr b = ExprLit (LitBool b)

instance JavaScriptValue t a => JavaScriptValue t (JQueryVar a) where
  valToExpr (JQueryVar n) = ExprName n

-- | Class \"alias\" for @JavaScriptValue String a@
class    JavaScriptValue String a => StringValue a where
instance JavaScriptValue String a => StringValue a where

-- | Class \"alias\" for @JavaScriptValue Html a@
class    JavaScriptValue Html   a => HtmlValue   a where
instance JavaScriptValue Html   a => HtmlValue   a where

--------------------------------------------------------------------------------
-- Selectors

-- type Selector = JQuerySelector a => a

class JQuerySelector a where
  selToExpr :: a -> Expr

instance JQuerySelector String where
  selToExpr (jsString -> Right jss) = ExprLit (LitString jss)
  selToExpr s = error $ "`JQuerySelector selToExpr' conversion error of String \"" ++ s ++ "\""

instance JQuerySelector JQueryObject where
  selToExpr (JQueryObject n) = ExprName n
