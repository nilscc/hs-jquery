{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

module Language.JQuery.Functions where

import Text.Blaze.Html

--import Language.JavaScript.AST
import Language.JQuery.Types

(~>) :: JQueryStmt a -> JQueryStmt b -> JQueryStmt b
(~>) = JQS_chain

jQuery :: String -> JQueryStmt a -> JQuery (Var a)
jQuery = JQ_select


--------------------------------------------------------------------------------
-- CSS

getCSS :: String -> JQueryStmt String
getCSS = JQS_get_css

setCSS :: String -> String -> JQueryStmt ()
setCSS = JQS_set_css


--------------------------------------------------------------------------------
-- Manipulation

getHtml :: JQueryStmt Html
getHtml = JQS_get_html

setHtml :: Html -> JQueryStmt ()
setHtml = JQS_set_html
