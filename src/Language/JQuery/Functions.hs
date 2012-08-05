{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Language.JQuery.Functions where

import Text.Blaze.Html

--import Language.JavaScript.AST
import Language.JQuery.Internal.Types

(~>) :: JQueryStmt a -> JQueryStmt b -> JQueryStmt b
(~>) = JQS_chain

jQuery :: JQuerySelector sel => sel -> JQueryStmt a -> JQuery (Var a)
jQuery = JQ_select


--------------------------------------------------------------------------------
-- ** CSS

getCSS :: StringValue s => s -> JQueryStmt String
getCSS s = JQS_str_call "css" [valToExpr s]

setCSS :: (StringValue s1, StringValue s2) => s1 -> s2 -> JQueryStmt ()
setCSS s1 s2 = JQS_call "css" [valToExpr s1, valToExpr s2]


--------------------------------------------------------------------------------
-- ** Manipulation

getHtml :: JQueryStmt Html
getHtml = JQS_html_call "html" []

setHtml :: HtmlValue html => html -> JQueryStmt ()
setHtml h = JQS_call "html" [valToExpr h]
