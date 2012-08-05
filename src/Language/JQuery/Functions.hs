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

jQuery :: (JQuerySelector sel, JQueryVariable a) => sel -> JQueryStmts a -> JQuery (Var a)
jQuery sel stmts = addJQuery sel stmts

-- | The typical jQuery chain operator:
--
-- > jQuery ".button" $ setCSS "display" "block"
-- >                 ~> setHTML (toHtml "Click me!")
-- >                 ~> addClass "visible"
(~>) :: JQueryStmts a -> JQueryStmts b -> JQueryStmts b
(~>) = JQSs_chain


--------------------------------------------------------------------------------
-- ** CSS

getCSS :: StringValue s => s -> JQueryStmts String
getCSS s = call "css" [valToExpr s]

setCSS :: (StringValue s1, StringValue s2) => s1 -> s2 -> JQueryStmts ()
setCSS s1 s2 = call "css" [valToExpr s1, valToExpr s2]


--------------------------------------------------------------------------------
-- ** Manipulation

--
-- *** Class Attribute
--
addClass :: StringValue s => s -> JQueryStmts ()
addClass s = call "addClass" [valToExpr s]

--
-- *** DOM Insertion, Inside
--
getHtml :: JQueryStmts Html
getHtml = call "html" []

setHtml :: HtmlValue html => html -> JQueryStmts ()
setHtml h = call "html" [valToExpr h]
