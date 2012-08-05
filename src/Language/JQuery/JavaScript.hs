{-# LANGUAGE GADTs #-}
{-# LANGUAGE ViewPatterns #-}

module Language.JQuery.JavaScript
  ( jqueryToString, jqueryToHtml
  , jqueryToCompactString, jqueryToCompactHtml
  ) where

import Control.Monad.State

import Text.Blaze.Html
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Language.JavaScript.AST
import Language.JavaScript.Pretty
import Language.JavaScript.NonEmptyList
import Text.PrettyPrint.Leijen

import Language.JQuery.Internal.Types

-- | Generate JavaScript code from `JQuery'.
jqueryToString :: JQuery () -> String
jqueryToString jq =
  let p = evalState (mkProgram jq) 0
   in show $ pretty p

-- | Generate JavaScript code from `JQuery' and wrap it into a @<script>@ tag.
jqueryToHtml :: JQuery () -> Html
jqueryToHtml jq =
  let src = jqueryToString jq
   in H.script ! A.type_ (toValue "text/javascript") $ toHtml src

-- | Generate JavaScript code from `JQuery'. Removes all spaces between
-- delimiters and identifiers and removes all new
-- lines.
jqueryToCompactString :: JQuery () -> String
jqueryToCompactString jq =
  let p = evalState (mkProgram jq) 0
   in displayS (compacter . renderCompact $ pretty p) ""
 where
  compacter d =
    case d of
         SEmpty       -> SEmpty
         SLine   _ d' -> compacter d'
         SChar ' ' d' -> compacter d'
         SChar   c d' -> SChar c (compacter d')
         SText _ s d' -> SText 0 s (compacter d')

-- | Generate JavaScript code from `JQuery' using `jqueryToCompactString' and
-- wrap it into a @<script>@ tag.
jqueryToCompactHtml :: JQuery () -> Html
jqueryToCompactHtml jq =
  let src = jqueryToCompactString jq
   in H.script ! A.type_ (toValue "text/javascript") $ toHtml src


--------------------------------------------------------------------------------
-- Helper

{-
toJSString :: String  -- ^ name of the calling function for error messages
           -> String  -- ^ string to convert
           -> JSString
toJSString n s =
  case jsString s of
       Right jss -> jss
       Left  err -> error $ "Error in `" ++ n ++ "': " ++ err
-}

toName :: String -- ^ name of the calling function
       -> String
       -> Name
toName n s =
  case name s of
       Right n'  -> n'
       Left  err -> error $ "Error in `" ++ n ++ "': " ++ err

--------------------------------------------------------------------------------
-- Javascript generation

type VarCount = Int

mkProgram :: JQuery () -> State VarCount Program

mkProgram (JQ_return _) =
  return $ Program [] []

-- TODO: JQ_chain JQ_bind

mkProgram (JQ_select sel st) =
  return $ Program [ ]
                   [ StmtExpr $ ESApply (singleton lval) rval ]
 where
  (lval, rval) = mkSelect (selToExpr sel) st

mkProgram _ = error "Error in `mkProgram': Not implemented yet."

-- $(..) statements
mkSelect :: Expr -> JQueryStmt a -> (LValue, RValue)
mkSelect s st = (LValue (toN "$") l, RVInvoke (singleton r))
 where
  toN = toName "mkSelect"

  (l,r) = mkSel []
                (Invocation [s]) -- \\$("...")
                st

  mkSel :: [([Invocation],Refinement)]
        -> Invocation
        -> JQueryStmt a
        -> ( [([Invocation],Refinement)], Invocation )
  mkSel ls li stmt = case stmt of
    JQS_chain       a b    -> let (ls',li') = mkSel ls li a
                               in mkSel ls' li' b
    JQS_call        f args -> invoke f args
    JQS_str_call    f args -> invoke f args
    JQS_double_call f args -> invoke f args
    JQS_html_call   f args -> invoke f args
   where
    invoke n expr = ( ls ++ [([li], Property (toN n))], Invocation expr)
