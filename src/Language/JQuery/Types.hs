{-# LANGUAGE MultiParamTypeClasses, GeneralizedNewtypeDeriving,
             FlexibleInstances, UndecidableInstances, GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Language.JQuery.Types where

import Control.Monad.State

import Text.Blaze.Html
import Language.JavaScript.AST

data JQueryStmt t where

  JQS_chain        :: JQueryStmt a -> JQueryStmt b -> JQueryStmt b

  -- CSS
  JQS_get_css      :: String -> JQueryStmt String
  JQS_set_css      :: String -> String -> JQueryStmt ()

  -- Manipulation
  JQS_get_html     :: JQueryStmt Html
  JQS_set_html     :: Html -> JQueryStmt ()

type VarCount = Int

data JQuery t where

  JQ_chain  :: JQuery a -> JQuery b        -> JQuery b
  JQ_bind   :: JQuery a -> (a -> JQuery b) -> JQuery b
  JQ_return :: a                           -> JQuery a

  JQ_select :: String -> JQueryStmt a -> JQuery (Var a)

instance Monad JQuery where
  a >>  b = JQ_chain a b
  a >>= b = JQ_bind  a b
  return  = JQ_return

--
-- Variables
--
class JQueryVariables a where
  type Var a

instance JQueryVariables () where
  type Var () = ()

newtype JQVar a = JQVar Name

instance JQueryVariables String where
  type Var String = JQVar String

instance JQueryVariables Double where
  type Var Double = JQVar Double

instance JQueryVariables Bool where
  type Var Bool = JQVar Bool

instance JQueryVariables Html where
  type Var Html = JQVar Html

--instance JQueryVariables Object where
  --type Var Object = JQVar
