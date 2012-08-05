{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}

module Language.JQuery.Functions where

import Text.Blaze.Html

--import Language.JavaScript.AST
import Language.JQuery.Internal.Functions
import Language.JQuery.Internal.Types

(~>) :: JQueryStmts a -> JQueryStmts b -> JQueryStmts b
(~>) = JQSs_chain

jQuery :: JQuerySelector sel => sel -> JQueryStmts a -> JQuery (JQueryVar a)
jQuery sel stmts = addJQuery sel stmts


--------------------------------------------------------------------------------
-- ** CSS

getCSS :: StringValue s => s -> JQueryStmts String
getCSS s = JQSs_call "css" [valToExpr s]

setCSS :: (StringValue s1, StringValue s2) => s1 -> s2 -> JQueryStmts ()
setCSS s1 s2 = JQSs_call "css" [valToExpr s1, valToExpr s2]


--------------------------------------------------------------------------------
-- ** Manipulation

getHtml :: JQueryStmts Html
getHtml = JQSs_call "html" []

setHtml :: HtmlValue html => html -> JQueryStmts ()
setHtml h = JQSs_call "html" [valToExpr h]
