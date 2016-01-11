{-# LANGUAGE TemplateHaskell #-}
module TH where

import           Language.Haskell.TH

class MyShow a where
    myShow :: a -> String

deriveShowType :: Name -> Q [Dec]
deriveShowType name = return [ InstanceD []
                               (AppT (ConT ''MyShow) (ConT name))
                               [ FunD 'myShow [ Clause [ WildP ]
                                                (NormalB (LitE
                                                          (StringL
                                                           (nameBase name))))
                                                []
                                              ]
                               ]
                             ]

deriveShowType' :: Name -> Q [Dec]
deriveShowType' name = do
    ids <- [d| instance MyShow String where myShow _ = "Mock" |]
    let [InstanceD ctx (AppT showt (ConT _)) _] = ids
        name' = nameBase name
    fds <- [d| myShow _ = name' |]
    return [InstanceD ctx (AppT showt (ConT name)) fds]
