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
getCSS = JQS_get_css

setCSS :: (StringValue s1, StringValue s2) => s1 -> s2 -> JQueryStmt ()
setCSS = JQS_set_css


--------------------------------------------------------------------------------
-- ** Manipulation

getHtml :: JQueryStmt Html
getHtml = JQS_get_html

setHtml :: HtmlValue html => html -> JQueryStmt ()
setHtml = JQS_set_html
