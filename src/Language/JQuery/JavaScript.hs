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

import Language.JQuery.Internal.Functions
import Language.JQuery.Internal.Types

runJQ :: JQuery () -> Program
runJQ jq = mkProgram . jqueryChain $ execState (unJQ jq >> discardBinding) nullJQueryS

-- | Generate JavaScript code from `JQuery'.
jqueryToString :: JQuery () -> String
jqueryToString jq = show $ pretty (runJQ jq)

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
  displayS (compacter . renderCompact . pretty $ runJQ jq) ""
 where
  compacter d =
    case d of
         SEmpty       -> SEmpty
         SLine   _ d' -> compacter d'
         SChar ' ' d' -> compacter d'
         SChar   c d' -> SChar c (compacter d')
         SText _ s d'
           | s == "var" -> SText 0 s (varDec d')
           | otherwise  -> SText 0 s (compacter d')
  varDec d =
    case d of
         SChar ' ' d' -> SChar ' ' (compacter d')
         _            -> compacter d

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

mkProgram :: JQueryChain -> Program
mkProgram (JQC_empty)        = Program [] []
mkProgram (JQC_single  stmt) = mkProgram' stmt
mkProgram (JQC_chain c stmt) =
  let Program v1 s1 = mkProgram c
      Program v2 s2 = mkProgram' stmt
   in Program (v1 ++ v2) (s1 ++ s2)

mkProgram' :: JQueryStmt -> Program
mkProgram' (JQS_jQuery sel stmts) =
  Program [] [StmtExpr $ ESApply (singleton lval) rval]
 where
  (lval, rval)  = mkSelect (selToExpr sel) stmts
  toN           = toName "mkSelect"
  mkSelect s st = (LValue (toN "$") l, RVInvoke (singleton r))
   where
    (l,r) = mkSel []
                  (Invocation [s]) -- \\$("...")
                  st
  
    mkSel :: [([Invocation],Refinement)]
          -> Invocation
          -> JQueryStmts a
          -> ( [([Invocation],Refinement)], Invocation )
    mkSel ls li stmts' = case stmts' of
      JQSs_chain a b    -> let (ls',li') = mkSel ls li a
                            in mkSel ls' li' b
      JQSs_call  f args -> invoke f args
     where
      invoke n expr = ( ls ++ [([li], Property (toN n))], Invocation expr)

mkProgram' (JQS_bind (JQueryVar n) sel stmts) =
  Program [VarStmt (singleton $ VarDecl n Nothing)]
          [StmtExpr $ ESApply (singleton $ LValue n []) (RVAssign expr)]
 where
  toN  = toName "mkSelect"
  jqs  = ExprInvocation (ExprName (toN "$")) (Invocation [selToExpr sel])
  expr = mkExp jqs stmts
  mkExp :: Expr -> JQueryStmts t -> Expr
  mkExp e s = case s of
    JQSs_chain a b -> let e' = mkExp e a in mkExp e' b
    JQSs_call  f a -> ExprInvocation (ExprRefinement e (Property (toN f)))
                                     (Invocation a)

{-
mkProgram' :: JQuery a -> State JQueryS Program

mkProgram' (JQ_return _) = return $ Program [] [] -- FIX ME

-- TODO: JQ_chain JQ_bind

mkProgram' (JQ_chain a b) = do
  Program v1 s1 <- mkProgram' a
  Program v2 s2 <- mkProgram' b
  return $ Program (v1 ++ v2) (s1 ++ s2)

mkProgram' (JQ_bind a b) = do
  Program v1 s1 <- mkProgram' a
  v <- newVar
  let v1' = v1
      s1' = case s1 of
                 [] -> error "Error in `mkProgram': Empty bind statement."
                 _  -> init s1 ++ [bindVar v $ last s1]
  Program v2 s2 <- mkProgram' $ b undefined -- FIXME: (JQueryVar $ toName "mkProgram'" ?)
  return $ Program (v1' ++ v2) (s1' ++ s2)
 where
  bindVar v stmt =
    case stmt of
         StmtExpr (ESApply ls r) -> undefined

mkProgram' (JQ_select sel st) =
  return $ Program [ ]
                   [ StmtExpr $ ESApply (singleton lval) rval ]
 where
  (lval, rval) = mkSelect (selToExpr sel) st

mkProgram' _ = error "Error in `mkProgram': Not implemented yet."
-}
