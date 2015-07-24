module Data.OpenDataTable.LiftNoTH
       () where

import Data.OpenDataTable

import Language.Haskell.TH.Syntax (lift)
import qualified Language.Haskell.TH.Syntax
import qualified Language.Haskell.TH.Lib

-- OpenDataTable.hs:261:3-28: Splicing declarations
instance Language.Haskell.TH.Syntax.Lift OpenDataTable where
  lift
    (OpenDataTable x0 x1 x2 x3 x4 x5)
    = Language.Haskell.TH.Lib.appE
        (Language.Haskell.TH.Lib.appE
           (Language.Haskell.TH.Lib.appE
              (Language.Haskell.TH.Lib.appE
                 (Language.Haskell.TH.Lib.appE
                    (Language.Haskell.TH.Lib.appE
                       (Language.Haskell.TH.Lib.conE
                          (Language.Haskell.TH.Syntax.Name
                             (Language.Haskell.TH.Syntax.mkOccName "OpenDataTable")
                             (Language.Haskell.TH.Syntax.NameG
                                Language.Haskell.TH.Syntax.DataName
                                (Language.Haskell.TH.Syntax.mkPkgName "main")
                                (Language.Haskell.TH.Syntax.mkModName "Data.OpenDataTable"))))
                       (lift x0))
                    (lift x1))
                 (lift x2))
              (lift x3))
           (lift x4))
        (lift x5)
-- OpenDataTable.hs:262:3-28: Splicing declarations
instance Language.Haskell.TH.Syntax.Lift SecurityLevel where
  lift SecurityLevelAny
    = Language.Haskell.TH.Lib.conE
        (Language.Haskell.TH.Syntax.Name
           (Language.Haskell.TH.Syntax.mkOccName "SecurityLevelAny")
           (Language.Haskell.TH.Syntax.NameG
              Language.Haskell.TH.Syntax.DataName
              (Language.Haskell.TH.Syntax.mkPkgName "main")
              (Language.Haskell.TH.Syntax.mkModName "Data.OpenDataTable")))
  lift SecurityLevelApp
    = Language.Haskell.TH.Lib.conE
        (Language.Haskell.TH.Syntax.Name
           (Language.Haskell.TH.Syntax.mkOccName "SecurityLevelApp")
           (Language.Haskell.TH.Syntax.NameG
              Language.Haskell.TH.Syntax.DataName
              (Language.Haskell.TH.Syntax.mkPkgName "main")
              (Language.Haskell.TH.Syntax.mkModName "Data.OpenDataTable")))
  lift
    SecurityLevelUser
    = Language.Haskell.TH.Lib.conE
        (Language.Haskell.TH.Syntax.Name
           (Language.Haskell.TH.Syntax.mkOccName "SecurityLevelUser")
           (Language.Haskell.TH.Syntax.NameG
              Language.Haskell.TH.Syntax.DataName
              (Language.Haskell.TH.Syntax.mkPkgName "main")
              (Language.Haskell.TH.Syntax.mkModName "Data.OpenDataTable")))
-- OpenDataTable.hs:263:3-21: Splicing declarations
instance Language.Haskell.TH.Syntax.Lift Delete where
  lift
    (Delete x0 x1 x2 x3 x4)
    = Language.Haskell.TH.Lib.appE
        (Language.Haskell.TH.Lib.appE
           (Language.Haskell.TH.Lib.appE
              (Language.Haskell.TH.Lib.appE
                 (Language.Haskell.TH.Lib.appE
                    (Language.Haskell.TH.Lib.conE
                       (Language.Haskell.TH.Syntax.Name
                          (Language.Haskell.TH.Syntax.mkOccName "Delete")
                          (Language.Haskell.TH.Syntax.NameG
                             Language.Haskell.TH.Syntax.DataName
                             (Language.Haskell.TH.Syntax.mkPkgName "main")
                             (Language.Haskell.TH.Syntax.mkModName "Data.OpenDataTable"))))
                    (lift x0))
                 (lift x1))
              (lift x2))
           (lift x3))
        (lift x4)
-- OpenDataTable.hs:264:3-19: Splicing declarations
instance Language.Haskell.TH.Syntax.Lift Meta where
  lift
    (Meta x0 x1 x2 x3 x4)
    = Language.Haskell.TH.Lib.appE
        (Language.Haskell.TH.Lib.appE
           (Language.Haskell.TH.Lib.appE
              (Language.Haskell.TH.Lib.appE
                 (Language.Haskell.TH.Lib.appE
                    (Language.Haskell.TH.Lib.conE
                       (Language.Haskell.TH.Syntax.Name
                          (Language.Haskell.TH.Syntax.mkOccName "Meta")
                          (Language.Haskell.TH.Syntax.NameG
                             Language.Haskell.TH.Syntax.DataName
                             (Language.Haskell.TH.Syntax.mkPkgName "main")
                             (Language.Haskell.TH.Syntax.mkModName "Data.OpenDataTable"))))
                    (lift x0))
                 (lift x1))
              (lift x2))
           (lift x3))
        (lift x4)
-- OpenDataTable.hs:265:3-22: Splicing declarations
instance Language.Haskell.TH.Syntax.Lift Binding where
  lift
    (SelectBinding x0)
    = Language.Haskell.TH.Lib.appE
        (Language.Haskell.TH.Lib.conE
           (Language.Haskell.TH.Syntax.Name
              (Language.Haskell.TH.Syntax.mkOccName "SelectBinding")
              (Language.Haskell.TH.Syntax.NameG
                 Language.Haskell.TH.Syntax.DataName
                 (Language.Haskell.TH.Syntax.mkPkgName "main")
                 (Language.Haskell.TH.Syntax.mkModName "Data.OpenDataTable"))))
        (lift x0)
  lift
    (InsertBinding x0)
    = Language.Haskell.TH.Lib.appE
        (Language.Haskell.TH.Lib.conE
           (Language.Haskell.TH.Syntax.Name
              (Language.Haskell.TH.Syntax.mkOccName "InsertBinding")
              (Language.Haskell.TH.Syntax.NameG
                 Language.Haskell.TH.Syntax.DataName
                 (Language.Haskell.TH.Syntax.mkPkgName "main")
                 (Language.Haskell.TH.Syntax.mkModName "Data.OpenDataTable"))))
        (lift x0)
  lift
    (UpdateBinding x0)
    = Language.Haskell.TH.Lib.appE
        (Language.Haskell.TH.Lib.conE
           (Language.Haskell.TH.Syntax.Name
              (Language.Haskell.TH.Syntax.mkOccName "UpdateBinding")
              (Language.Haskell.TH.Syntax.NameG
                 Language.Haskell.TH.Syntax.DataName
                 (Language.Haskell.TH.Syntax.mkPkgName "main")
                 (Language.Haskell.TH.Syntax.mkModName "Data.OpenDataTable"))))
        (lift x0)
  lift
    (DeleteBinding x0)
    = Language.Haskell.TH.Lib.appE
        (Language.Haskell.TH.Lib.conE
           (Language.Haskell.TH.Syntax.Name
              (Language.Haskell.TH.Syntax.mkOccName "DeleteBinding")
              (Language.Haskell.TH.Syntax.NameG
                 Language.Haskell.TH.Syntax.DataName
                 (Language.Haskell.TH.Syntax.mkPkgName "main")
                 (Language.Haskell.TH.Syntax.mkModName "Data.OpenDataTable"))))
        (lift x0)
  lift
    (FunctionBinding x0)
    = Language.Haskell.TH.Lib.appE
        (Language.Haskell.TH.Lib.conE
           (Language.Haskell.TH.Syntax.Name
              (Language.Haskell.TH.Syntax.mkOccName "FunctionBinding")
              (Language.Haskell.TH.Syntax.NameG
                 Language.Haskell.TH.Syntax.DataName
                 (Language.Haskell.TH.Syntax.mkPkgName "main")
                 (Language.Haskell.TH.Syntax.mkModName "Data.OpenDataTable"))))
        (lift x0)
-- OpenDataTable.hs:266:3-21: Splicing declarations
instance Language.Haskell.TH.Syntax.Lift Select where
  lift
    (Select x0 x1 x2 x3 x4 x5 x6)
    = Language.Haskell.TH.Lib.appE
        (Language.Haskell.TH.Lib.appE
           (Language.Haskell.TH.Lib.appE
              (Language.Haskell.TH.Lib.appE
                 (Language.Haskell.TH.Lib.appE
                    (Language.Haskell.TH.Lib.appE
                       (Language.Haskell.TH.Lib.appE
                          (Language.Haskell.TH.Lib.conE
                             (Language.Haskell.TH.Syntax.Name
                                (Language.Haskell.TH.Syntax.mkOccName "Select")
                                (Language.Haskell.TH.Syntax.NameG
                                   Language.Haskell.TH.Syntax.DataName
                                   (Language.Haskell.TH.Syntax.mkPkgName "main")
                                   (Language.Haskell.TH.Syntax.mkModName "Data.OpenDataTable"))))
                          (lift x0))
                       (lift x1))
                    (lift x2))
                 (lift x3))
              (lift x4))
           (lift x5))
        (lift x6)
-- OpenDataTable.hs:267:3-21: Splicing declarations
instance Language.Haskell.TH.Syntax.Lift Paging where
  lift
    (Paging x0 x1 x2 x3 x4 x5)
    = Language.Haskell.TH.Lib.appE
        (Language.Haskell.TH.Lib.appE
           (Language.Haskell.TH.Lib.appE
              (Language.Haskell.TH.Lib.appE
                 (Language.Haskell.TH.Lib.appE
                    (Language.Haskell.TH.Lib.appE
                       (Language.Haskell.TH.Lib.conE
                          (Language.Haskell.TH.Syntax.Name
                             (Language.Haskell.TH.Syntax.mkOccName "Paging")
                             (Language.Haskell.TH.Syntax.NameG
                                Language.Haskell.TH.Syntax.DataName
                                (Language.Haskell.TH.Syntax.mkPkgName "main")
                                (Language.Haskell.TH.Syntax.mkModName "Data.OpenDataTable"))))
                       (lift x0))
                    (lift x1))
                 (lift x2))
              (lift x3))
           (lift x4))
        (lift x5)
-- OpenDataTable.hs:268:3-26: Splicing declarations
instance Language.Haskell.TH.Syntax.Lift PagingModel where
  lift
    PagingModelOffset
    = Language.Haskell.TH.Lib.conE
        (Language.Haskell.TH.Syntax.Name
           (Language.Haskell.TH.Syntax.mkOccName "PagingModelOffset")
           (Language.Haskell.TH.Syntax.NameG
              Language.Haskell.TH.Syntax.DataName
              (Language.Haskell.TH.Syntax.mkPkgName "main")
              (Language.Haskell.TH.Syntax.mkModName "Data.OpenDataTable")))
  lift PagingModelPage
    = Language.Haskell.TH.Lib.conE
        (Language.Haskell.TH.Syntax.Name
           (Language.Haskell.TH.Syntax.mkOccName "PagingModelPage")
           (Language.Haskell.TH.Syntax.NameG
              Language.Haskell.TH.Syntax.DataName
              (Language.Haskell.TH.Syntax.mkPkgName "main")
              (Language.Haskell.TH.Syntax.mkModName "Data.OpenDataTable")))
  lift PagingModelURL
    = Language.Haskell.TH.Lib.conE
        (Language.Haskell.TH.Syntax.Name
           (Language.Haskell.TH.Syntax.mkOccName "PagingModelURL")
           (Language.Haskell.TH.Syntax.NameG
              Language.Haskell.TH.Syntax.DataName
              (Language.Haskell.TH.Syntax.mkPkgName "main")
              (Language.Haskell.TH.Syntax.mkModName "Data.OpenDataTable")))
-- OpenDataTable.hs:269:3-25: Splicing declarations
instance Language.Haskell.TH.Syntax.Lift PagingSize where
  lift
    (PagingSize x0 x1)
    = Language.Haskell.TH.Lib.appE
        (Language.Haskell.TH.Lib.appE
           (Language.Haskell.TH.Lib.conE
              (Language.Haskell.TH.Syntax.Name
                 (Language.Haskell.TH.Syntax.mkOccName "PagingSize")
                 (Language.Haskell.TH.Syntax.NameG
                    Language.Haskell.TH.Syntax.DataName
                    (Language.Haskell.TH.Syntax.mkPkgName "main")
                    (Language.Haskell.TH.Syntax.mkModName "Data.OpenDataTable"))))
           (lift x0))
        (lift x1)
-- OpenDataTable.hs:270:3-26: Splicing declarations
instance Language.Haskell.TH.Syntax.Lift PagingStart where
  lift
    (PagingStart x0 x1)
    = Language.Haskell.TH.Lib.appE
        (Language.Haskell.TH.Lib.appE
           (Language.Haskell.TH.Lib.conE
              (Language.Haskell.TH.Syntax.Name
                 (Language.Haskell.TH.Syntax.mkOccName "PagingStart")
                 (Language.Haskell.TH.Syntax.NameG
                    Language.Haskell.TH.Syntax.DataName
                    (Language.Haskell.TH.Syntax.mkPkgName "main")
                    (Language.Haskell.TH.Syntax.mkModName "Data.OpenDataTable"))))
           (lift x0))
        (lift x1)
-- OpenDataTable.hs:271:3-26: Splicing declarations
instance Language.Haskell.TH.Syntax.Lift PagingTotal where
  lift (PagingTotal x0)
    = Language.Haskell.TH.Lib.appE
        (Language.Haskell.TH.Lib.conE
           (Language.Haskell.TH.Syntax.Name
              (Language.Haskell.TH.Syntax.mkOccName "PagingTotal")
              (Language.Haskell.TH.Syntax.NameG
                 Language.Haskell.TH.Syntax.DataName
                 (Language.Haskell.TH.Syntax.mkPkgName "main")
                 (Language.Haskell.TH.Syntax.mkModName "Data.OpenDataTable"))))
        (lift x0)
-- OpenDataTable.hs:272:3-23: Splicing declarations
instance Language.Haskell.TH.Syntax.Lift NextPage where
  lift (NextPage x0)
    = Language.Haskell.TH.Lib.appE
        (Language.Haskell.TH.Lib.conE
           (Language.Haskell.TH.Syntax.Name
              (Language.Haskell.TH.Syntax.mkOccName "NextPage")
              (Language.Haskell.TH.Syntax.NameG
                 Language.Haskell.TH.Syntax.DataName
                 (Language.Haskell.TH.Syntax.mkPkgName "main")
                 (Language.Haskell.TH.Syntax.mkModName "Data.OpenDataTable"))))
        (lift x0)
-- OpenDataTable.hs:273:3-21: Splicing declarations
instance Language.Haskell.TH.Syntax.Lift Insert where
  lift
    (Insert x0 x1 x2 x3 x4)
    = Language.Haskell.TH.Lib.appE
        (Language.Haskell.TH.Lib.appE
           (Language.Haskell.TH.Lib.appE
              (Language.Haskell.TH.Lib.appE
                 (Language.Haskell.TH.Lib.appE
                    (Language.Haskell.TH.Lib.conE
                       (Language.Haskell.TH.Syntax.Name
                          (Language.Haskell.TH.Syntax.mkOccName "Insert")
                          (Language.Haskell.TH.Syntax.NameG
                             Language.Haskell.TH.Syntax.DataName
                             (Language.Haskell.TH.Syntax.mkPkgName "main")
                             (Language.Haskell.TH.Syntax.mkModName "Data.OpenDataTable"))))
                    (lift x0))
                 (lift x1))
              (lift x2))
           (lift x3))
        (lift x4)
-- OpenDataTable.hs:274:3-21: Splicing declarations
instance Language.Haskell.TH.Syntax.Lift Update where
  lift
    (Update x0 x1 x2 x3 x4)
    = Language.Haskell.TH.Lib.appE
        (Language.Haskell.TH.Lib.appE
           (Language.Haskell.TH.Lib.appE
              (Language.Haskell.TH.Lib.appE
                 (Language.Haskell.TH.Lib.appE
                    (Language.Haskell.TH.Lib.conE
                       (Language.Haskell.TH.Syntax.Name
                          (Language.Haskell.TH.Syntax.mkOccName "Update")
                          (Language.Haskell.TH.Syntax.NameG
                             Language.Haskell.TH.Syntax.DataName
                             (Language.Haskell.TH.Syntax.mkPkgName "main")
                             (Language.Haskell.TH.Syntax.mkModName "Data.OpenDataTable"))))
                    (lift x0))
                 (lift x1))
              (lift x2))
           (lift x3))
        (lift x4)
-- OpenDataTable.hs:275:3-22: Splicing declarations
instance Language.Haskell.TH.Syntax.Lift Product where
  lift ProductXML
    = Language.Haskell.TH.Lib.conE
        (Language.Haskell.TH.Syntax.Name
           (Language.Haskell.TH.Syntax.mkOccName "ProductXML")
           (Language.Haskell.TH.Syntax.NameG
              Language.Haskell.TH.Syntax.DataName
              (Language.Haskell.TH.Syntax.mkPkgName "main")
              (Language.Haskell.TH.Syntax.mkModName "Data.OpenDataTable")))
  lift ProductJSON
    = Language.Haskell.TH.Lib.conE
        (Language.Haskell.TH.Syntax.Name
           (Language.Haskell.TH.Syntax.mkOccName "ProductJSON")
           (Language.Haskell.TH.Syntax.NameG
              Language.Haskell.TH.Syntax.DataName
              (Language.Haskell.TH.Syntax.mkPkgName "main")
              (Language.Haskell.TH.Syntax.mkModName "Data.OpenDataTable")))
-- OpenDataTable.hs:276:3-23: Splicing declarations
instance Language.Haskell.TH.Syntax.Lift Function where
  lift (Function x0 x1)
    = Language.Haskell.TH.Lib.appE
        (Language.Haskell.TH.Lib.appE
           (Language.Haskell.TH.Lib.conE
              (Language.Haskell.TH.Syntax.Name
                 (Language.Haskell.TH.Syntax.mkOccName "Function")
                 (Language.Haskell.TH.Syntax.NameG
                    Language.Haskell.TH.Syntax.DataName
                    (Language.Haskell.TH.Syntax.mkPkgName "main")
                    (Language.Haskell.TH.Syntax.mkModName "Data.OpenDataTable"))))
           (lift x0))
        (lift x1)
-- OpenDataTable.hs:277:3-27: Splicing declarations
instance Language.Haskell.TH.Syntax.Lift FunctionType where
  lift
    FunctionTypeStream
    = Language.Haskell.TH.Lib.conE
        (Language.Haskell.TH.Syntax.Name
           (Language.Haskell.TH.Syntax.mkOccName "FunctionTypeStream")
           (Language.Haskell.TH.Syntax.NameG
              Language.Haskell.TH.Syntax.DataName
              (Language.Haskell.TH.Syntax.mkPkgName "main")
              (Language.Haskell.TH.Syntax.mkModName "Data.OpenDataTable")))
  lift
    FunctionTypeReduce
    = Language.Haskell.TH.Lib.conE
        (Language.Haskell.TH.Syntax.Name
           (Language.Haskell.TH.Syntax.mkOccName "FunctionTypeReduce")
           (Language.Haskell.TH.Syntax.NameG
              Language.Haskell.TH.Syntax.DataName
              (Language.Haskell.TH.Syntax.mkPkgName "main")
              (Language.Haskell.TH.Syntax.mkModName "Data.OpenDataTable")))
-- OpenDataTable.hs:278:3-20: Splicing declarations
instance Language.Haskell.TH.Syntax.Lift Input where
  lift (InputKey x0)
    = Language.Haskell.TH.Lib.appE
        (Language.Haskell.TH.Lib.conE
           (Language.Haskell.TH.Syntax.Name
              (Language.Haskell.TH.Syntax.mkOccName "InputKey")
              (Language.Haskell.TH.Syntax.NameG
                 Language.Haskell.TH.Syntax.DataName
                 (Language.Haskell.TH.Syntax.mkPkgName "main")
                 (Language.Haskell.TH.Syntax.mkModName "Data.OpenDataTable"))))
        (lift x0)
  lift (InputValue x0)
    = Language.Haskell.TH.Lib.appE
        (Language.Haskell.TH.Lib.conE
           (Language.Haskell.TH.Syntax.Name
              (Language.Haskell.TH.Syntax.mkOccName "InputValue")
              (Language.Haskell.TH.Syntax.NameG
                 Language.Haskell.TH.Syntax.DataName
                 (Language.Haskell.TH.Syntax.mkPkgName "main")
                 (Language.Haskell.TH.Syntax.mkModName "Data.OpenDataTable"))))
        (lift x0)
  lift (InputMap x0)
    = Language.Haskell.TH.Lib.appE
        (Language.Haskell.TH.Lib.conE
           (Language.Haskell.TH.Syntax.Name
              (Language.Haskell.TH.Syntax.mkOccName "InputMap")
              (Language.Haskell.TH.Syntax.NameG
                 Language.Haskell.TH.Syntax.DataName
                 (Language.Haskell.TH.Syntax.mkPkgName "main")
                 (Language.Haskell.TH.Syntax.mkModName "Data.OpenDataTable"))))
        (lift x0)
-- OpenDataTable.hs:279:3-24: Splicing declarations
instance Language.Haskell.TH.Syntax.Lift InputInfo where
  lift
    (InputInfo x0 x1 x2 x3 x4 x5 x6 x7 x8 x9)
    = Language.Haskell.TH.Lib.appE
        (Language.Haskell.TH.Lib.appE
           (Language.Haskell.TH.Lib.appE
              (Language.Haskell.TH.Lib.appE
                 (Language.Haskell.TH.Lib.appE
                    (Language.Haskell.TH.Lib.appE
                       (Language.Haskell.TH.Lib.appE
                          (Language.Haskell.TH.Lib.appE
                             (Language.Haskell.TH.Lib.appE
                                (Language.Haskell.TH.Lib.appE
                                   (Language.Haskell.TH.Lib.conE
                                      (Language.Haskell.TH.Syntax.Name
                                         (Language.Haskell.TH.Syntax.mkOccName "InputInfo")
                                         (Language.Haskell.TH.Syntax.NameG
                                            Language.Haskell.TH.Syntax.DataName
                                            (Language.Haskell.TH.Syntax.mkPkgName "main")
                                            (Language.Haskell.TH.Syntax.mkModName
                                               "Data.OpenDataTable"))))
                                   (lift x0 :: Language.Haskell.TH.Lib.ExpQ))
                                (lift x1))
                             (lift x2))
                          (lift x3))
                       (lift x4))
                    (lift x5))
                 (lift x6))
              (lift x7))
           (lift x8))
        (lift x9 :: Language.Haskell.TH.Lib.ExpQ)
-- OpenDataTable.hs:280:3-24: Splicing declarations
instance Language.Haskell.TH.Syntax.Lift InputType where
  lift InputTypeBool
    = Language.Haskell.TH.Lib.conE
        (Language.Haskell.TH.Syntax.Name
           (Language.Haskell.TH.Syntax.mkOccName "InputTypeBool")
           (Language.Haskell.TH.Syntax.NameG
              Language.Haskell.TH.Syntax.DataName
              (Language.Haskell.TH.Syntax.mkPkgName "main")
              (Language.Haskell.TH.Syntax.mkModName "Data.OpenDataTable")))
  lift InputTypeDate
    = Language.Haskell.TH.Lib.conE
        (Language.Haskell.TH.Syntax.Name
           (Language.Haskell.TH.Syntax.mkOccName "InputTypeDate")
           (Language.Haskell.TH.Syntax.NameG
              Language.Haskell.TH.Syntax.DataName
              (Language.Haskell.TH.Syntax.mkPkgName "main")
              (Language.Haskell.TH.Syntax.mkModName "Data.OpenDataTable")))
  lift InputTypeDouble
    = Language.Haskell.TH.Lib.conE
        (Language.Haskell.TH.Syntax.Name
           (Language.Haskell.TH.Syntax.mkOccName "InputTypeDouble")
           (Language.Haskell.TH.Syntax.NameG
              Language.Haskell.TH.Syntax.DataName
              (Language.Haskell.TH.Syntax.mkPkgName "main")
              (Language.Haskell.TH.Syntax.mkModName "Data.OpenDataTable")))
  lift InputTypeFloat
    = Language.Haskell.TH.Lib.conE
        (Language.Haskell.TH.Syntax.Name
           (Language.Haskell.TH.Syntax.mkOccName "InputTypeFloat")
           (Language.Haskell.TH.Syntax.NameG
              Language.Haskell.TH.Syntax.DataName
              (Language.Haskell.TH.Syntax.mkPkgName "main")
              (Language.Haskell.TH.Syntax.mkModName "Data.OpenDataTable")))
  lift InputTypeInt
    = Language.Haskell.TH.Lib.conE
        (Language.Haskell.TH.Syntax.Name
           (Language.Haskell.TH.Syntax.mkOccName "InputTypeInt")
           (Language.Haskell.TH.Syntax.NameG
              Language.Haskell.TH.Syntax.DataName
              (Language.Haskell.TH.Syntax.mkPkgName "main")
              (Language.Haskell.TH.Syntax.mkModName "Data.OpenDataTable")))
  lift InputTypeString
    = Language.Haskell.TH.Lib.conE
        (Language.Haskell.TH.Syntax.Name
           (Language.Haskell.TH.Syntax.mkOccName "InputTypeString")
           (Language.Haskell.TH.Syntax.NameG
              Language.Haskell.TH.Syntax.DataName
              (Language.Haskell.TH.Syntax.mkPkgName "main")
              (Language.Haskell.TH.Syntax.mkModName "Data.OpenDataTable")))
-- OpenDataTable.hs:281:3-24: Splicing declarations
instance Language.Haskell.TH.Syntax.Lift ParamType where
  lift ParamTypeQuery
    = Language.Haskell.TH.Lib.conE
        (Language.Haskell.TH.Syntax.Name
           (Language.Haskell.TH.Syntax.mkOccName "ParamTypeQuery")
           (Language.Haskell.TH.Syntax.NameG
              Language.Haskell.TH.Syntax.DataName
              (Language.Haskell.TH.Syntax.mkPkgName "main")
              (Language.Haskell.TH.Syntax.mkModName "Data.OpenDataTable")))
  lift ParamTypeMatrix
    = Language.Haskell.TH.Lib.conE
        (Language.Haskell.TH.Syntax.Name
           (Language.Haskell.TH.Syntax.mkOccName "ParamTypeMatrix")
           (Language.Haskell.TH.Syntax.NameG
              Language.Haskell.TH.Syntax.DataName
              (Language.Haskell.TH.Syntax.mkPkgName "main")
              (Language.Haskell.TH.Syntax.mkModName "Data.OpenDataTable")))
  lift ParamTypeHeader
    = Language.Haskell.TH.Lib.conE
        (Language.Haskell.TH.Syntax.Name
           (Language.Haskell.TH.Syntax.mkOccName "ParamTypeHeader")
           (Language.Haskell.TH.Syntax.NameG
              Language.Haskell.TH.Syntax.DataName
              (Language.Haskell.TH.Syntax.mkPkgName "main")
              (Language.Haskell.TH.Syntax.mkModName "Data.OpenDataTable")))
  lift ParamTypePath
    = Language.Haskell.TH.Lib.conE
        (Language.Haskell.TH.Syntax.Name
           (Language.Haskell.TH.Syntax.mkOccName "ParamTypePath")
           (Language.Haskell.TH.Syntax.NameG
              Language.Haskell.TH.Syntax.DataName
              (Language.Haskell.TH.Syntax.mkPkgName "main")
              (Language.Haskell.TH.Syntax.mkModName "Data.OpenDataTable")))
  lift
    ParamTypeVariable
    = Language.Haskell.TH.Lib.conE
        (Language.Haskell.TH.Syntax.Name
           (Language.Haskell.TH.Syntax.mkOccName "ParamTypeVariable")
           (Language.Haskell.TH.Syntax.NameG
              Language.Haskell.TH.Syntax.DataName
              (Language.Haskell.TH.Syntax.mkPkgName "main")
              (Language.Haskell.TH.Syntax.mkModName "Data.OpenDataTable")))
