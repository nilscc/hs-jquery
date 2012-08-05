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

data JQueryStmt t where

  JQS_chain        :: JQueryStmt a -> JQueryStmt b -> JQueryStmt b

  JQS_call         :: String -> [Expr] -> JQueryStmt ()
  JQS_str_call     :: String -> [Expr] -> JQueryStmt String
  JQS_double_call  :: String -> [Expr] -> JQueryStmt Double
  JQS_html_call    :: String -> [Expr] -> JQueryStmt Html

  -- CSS
  --JQS_get_css      :: StringValue str => str -> JQueryStmt String
  --JQS_set_css      :: (StringValue str1, StringValue str2) => str1 -> str2 -> JQueryStmt ()

  -- Manipulation
  --JQS_get_html     :: JQueryStmt Html
  --JQS_set_html     :: HtmlValue html => html -> JQueryStmt ()

data JQuery t where

  JQ_chain  :: JQuery a -> JQuery b        -> JQuery b
  JQ_bind   :: JQuery a -> (a -> JQuery b) -> JQuery b
  JQ_return :: a                           -> JQuery a

  JQ_select :: JQuerySelector sel => sel -> JQueryStmt a -> JQuery (Var a)

instance Monad JQuery where
  a >>  b = JQ_chain a b
  a >>= b = JQ_bind  a b
  return  = JQ_return

--------------------------------------------------------------------------------
-- newtypes

newtype Object = Object Name


--------------------------------------------------------------------------------
-- Variables

class JQueryVariables a where
  type Var a

instance JQueryVariables () where
  type Var () = ()

newtype JQueryVar a = JQueryVar Name

instance JQueryVariables String where
  type Var String = JQueryVar String

instance JQueryVariables Double where
  type Var Double = JQueryVar Double

instance JQueryVariables Bool where
  type Var Bool = JQueryVar Bool

instance JQueryVariables Html where
  type Var Html = JQueryVar Html

--instance JQueryVariables Object where
  --type Var Object = JQVar


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

instance JQuerySelector Object where
  selToExpr (Object n) = ExprName n
