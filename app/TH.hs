{-# LANGUAGE TemplateHaskellQuotes #-}
module TH where

import Language.Haskell.TH
import Class

deriveFormatter :: Q Exp -> Name -> Q [Dec]
deriveFormatter opts name = do
        inst <- instanceD ctx typ decls
        return [inst]
    where 
        ctx = return []
        typ = appT (conT ''Formatter) (conT name)
        decls = [funD 'doFmt [
                    clause [] (normalB $ appE (varE 'doFmtOptions) opts) []
                ]]
