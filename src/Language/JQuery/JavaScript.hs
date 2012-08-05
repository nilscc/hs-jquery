{-# LANGUAGE GADTs #-}
{-# LANGUAGE ViewPatterns #-}

module Language.JQuery.JavaScript
  ( jqueryToHtml ) where

import Control.Monad.State

import Text.Blaze.Html
import Text.Blaze.Html.Renderer.String
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Language.JavaScript.AST
--import Language.JavaScript.Pretty
import Language.JavaScript.NonEmptyList

import Language.JQuery.Types

jqueryToHtml :: JQuery () -> Html
jqueryToHtml jq =
  let _p = evalState (mkProgram jq) 0
   in H.script ! A.type_ (toValue "text/javascript") $ undefined

--------------------------------------------------------------------------------
-- Helper

toJSString :: String  -- ^ name of the calling function for error messages
           -> String  -- ^ string to convert
           -> JSString
toJSString n s =
  case jsString s of
       Right jss -> jss
       Left  err -> error $ "Error in `" ++ n ++ "': " ++ err

toName :: String -- ^ name of the calling function
       -> String
       -> Name
toName n s =
  case name s of
       Right n'  -> n'
       Left  err -> error $ "Error in `" ++ n ++ "': " ++ err

--------------------------------------------------------------------------------
-- Javascript generation

mkProgram :: JQuery () -> State VarCount Program

mkProgram (JQ_return _) =
  return $ Program [] []

-- TODO: JQ_chain JQ_bind

mkProgram (JQ_select (toJSString "mkProgram" -> s) st) =
  return $ Program [ ]
                   [ StmtExpr $ ESApply (singleton lval) rval ]
 where
  (lval, rval) = mkSelect s st

mkProgram _ = error "Error in `mkProgram': Not implemented yet."

-- $(..) statements
mkSelect :: JSString -> JQueryStmt a -> (LValue, RValue)
mkSelect s st = (LValue (toN "$") l, RVInvoke (singleton r))
 where
  toN = toName     "mkSelect"
  toS = toJSString "mkSelect"
  str = ExprLit . LitString . toS

  (l,r) = mkSel []
                (Invocation [ExprLit (LitString s)]) -- $("...")
                st

  mkSel :: [([Invocation],Refinement)]
        -> Invocation
        -> JQueryStmt a
        -> ( [([Invocation],Refinement)], Invocation )
  mkSel ls li stmt = case stmt of
    JQS_chain    a b -> let (ls',li') = mkSel ls li a
                         in mkSel ls' li' b
    JQS_get_html     -> invoke "html" [ ]
    JQS_set_html h   -> invoke "html" [ str (renderHtml h) ]
    JQS_get_css  n   -> invoke "css"  [ str n ]
    JQS_set_css  n c -> invoke "css"  [ str n, str c ]
   where
    invoke n expr = ( ls ++ [([li], Property (toN n))], Invocation expr)

