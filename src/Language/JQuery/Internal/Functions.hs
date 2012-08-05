{-# OPTIONS -fno-warn-orphans #-}

module Language.JQuery.Internal.Functions where

import Control.Monad.State
import Language.JQuery.Internal.Types
import Language.JavaScript.AST

-- | Empty JQuery state
nullJQueryS :: JQueryS
nullJQueryS = JQueryS
  { jqueryLastVarId = 0
  , jqueryLastStmt  = Nothing
  , jqueryChain     = JQC_empty
  }

call :: String -> [Expr] -> JQueryStmts a
call = JQSs_call

-- | Add \"\$(...)\" function
addJQuery :: (JQuerySelector sel, JQueryVariable a) => sel -> JQueryStmts a -> JQuery (Var a)
addJQuery sel stmt = JQuery $ do
  s <- get
  put s{ jqueryLastStmt = Just $ JQS_jQuery sel stmt }
  let Right n = name $ "hs_jquery_var_" ++ show (jqueryLastVarId s)
  return $ toVar stmt n

discardBinding :: State JQueryS ()
discardBinding = modify $ \s -> s
  { jqueryLastStmt = Nothing
  , jqueryChain    = let c = jqueryChain s in case jqueryLastStmt s of
      Just stmt -> case c of
                        JQC_empty     -> JQC_single  stmt
                        JQC_single {} -> JQC_chain c stmt
                        JQC_chain  {} -> JQC_chain c stmt
      Nothing   -> c
  }

bind :: State JQueryS ()
bind = do
  s <- get
  case jqueryLastStmt s of
       Just (JQS_jQuery sel stmts)
         | bindable stmts -> bind' s sel stmts
         | otherwise      -> discardBinding
       _ -> error "JQuery: Invalid `bind'."
       
 where
  bind' s sel stmts = do
    let stmt'   = JQS_bind var sel stmts
        Right n = name $ "hs_jquery_var_" ++ show (jqueryLastVarId s)
        var     = JQueryVar n
    put s { jqueryLastStmt = Nothing
          , jqueryChain = let c = jqueryChain s in case c of
              JQC_empty     -> JQC_single  stmt'
              JQC_single {} -> JQC_chain c stmt'
              JQC_chain  {} -> JQC_chain c stmt'
          , jqueryLastVarId = jqueryLastVarId s + 1
          }

instance Monad JQuery where
  (JQuery a) >> (JQuery b) = JQuery $
    a >> discardBinding >> b
  (JQuery a) >>= f         = JQuery $ do
    r <- a
    bind
    let JQuery b = f r
    b
  return a = JQuery $ return a
