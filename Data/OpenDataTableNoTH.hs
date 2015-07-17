{-# LANGUAGE TemplateHaskell #-}

module Data.OpenDataTableNoTH
       ( Binding(..)
       , Delete(..)
       , Function(..)
       , FunctionType(..)
       , Input(..)
       , InputInfo(..)
       , InputType(..)
       , Insert(..)
       , NextPage(..)
       , Meta(..)
       , OpenDataTable(..)
       , Paging(..)
       , PagingModel(..)
       , PagingSize(..)
       , PagingStart(..)
       , PagingTotal(..)
       , ParamType(..)
       , Product(..)
       , SecurityLevel(..)
       , Select(..)
       , Update(..) ) where

import Language.Haskell.TH.Syntax (lift)
import qualified Language.Haskell.TH.Syntax
import qualified Language.Haskell.TH.Lib

import Text.Read (choice, readPrec)
import qualified Text.Read (lift)
import Text.ParserCombinators.ReadP (string)

strValMap = map (\(x, y) -> Text.Read.lift $ string x >> return y)

data OpenDataTable
  = OpenDataTable
    { openDataTableXmlns         :: Maybe String
    , openDataTableSecurityLevel :: Maybe SecurityLevel
    , openDataTableHttps         :: Maybe Bool
    , openDataTableMeta          :: Meta
    , openDataTableExecute       :: Maybe String
    , openDataTableBindings      :: [Binding]
    } deriving (Read, Show, Eq)

data SecurityLevel
  = SecurityLevelAny | SecurityLevelApp | SecurityLevelUser
  deriving (Eq)

instance Read SecurityLevel where
  readPrec = choice $ strValMap
             [ ("any", SecurityLevelAny)
             , ("app", SecurityLevelApp)
             , ("user", SecurityLevelUser) ]

instance Show SecurityLevel where
  showsPrec _ SecurityLevelAny  = showString "any"
  showsPrec _ SecurityLevelApp  = showString "app"
  showsPrec _ SecurityLevelUser = showString "user"

data Meta
  = Meta
    { metaAuthor           :: Maybe String
    , metaDescription      :: Maybe String
    , metaDocumentationURL :: Maybe String
    , metaApiKeyURL        :: Maybe String
    , metaSampleQuery      :: [String]
    } deriving (Read, Show, Eq)

data Binding
  = SelectBinding Select
  | InsertBinding Insert
  | UpdateBinding Update
  | DeleteBinding Delete
  | FunctionBinding Function
  deriving (Read, Show, Eq)

data Select
  = Select
    { selectItemPath                :: Maybe String
    , selectProduces                :: Maybe Product
    , selectPollingFrequencySeconds :: Maybe Integer
    , selectUrl                     :: Maybe String
    , selectInputs                  :: [Input]
    , selectExecute                 :: Maybe String
    , selectPaging                  :: Maybe Paging
    }  deriving (Read, Show, Eq)

data Paging
  = Paging
    { pagingModel    :: Maybe PagingModel
    , pagingMatrix   :: Maybe Bool
    , pagingPageSize :: Maybe PagingSize
    , pagingStart    :: Maybe PagingStart
    , pagingTotal    :: Maybe PagingTotal
    , pagingNextPage :: Maybe NextPage
    } deriving (Read, Show, Eq)

data PagingModel
  = PagingModelOffset | PagingModelPage | PagingModelURL
  deriving (Eq)

instance Read PagingModel where
  readPrec = choice $ strValMap
             [ ("offset", PagingModelOffset)
             , ("page", PagingModelPage)
             , ("url", PagingModelURL) ]

instance Show PagingModel where
  showsPrec _ PagingModelOffset  = showString "offset"
  showsPrec _ PagingModelPage  = showString "page"
  showsPrec _ PagingModelURL = showString "url"

data PagingSize
  = PagingSize
    { pagingSizeId  :: String
    , pagingSizeMax :: Integer
    } deriving (Read, Show, Eq)

data PagingStart
  = PagingStart
    { pagingStartId      :: String
    , pagingStartDefault :: Integer
    } deriving (Read, Show, Eq)

data PagingTotal
  = PagingTotal
    { pagingTotalDefault :: Integer
    } deriving (Read, Show, Eq)

data NextPage
  = NextPage
    { nextPagePath :: String
    } deriving (Read, Show, Eq)

data Insert
  = Insert
    { insertItemPath                :: Maybe String
    , insertPollingFrequencySeconds :: Maybe Integer
    , insertProduces                :: Maybe Product
    , insertUrls                    :: [String]
    , insertInputs                  :: [Input]
    }  deriving (Read, Show, Eq)

data Update
  = Update
    { updateItemPath                :: Maybe String
    , updatePollingFrequencySeconds :: Maybe Integer
    , updateProduces                :: Maybe Product
    , updateUrls                    :: [String]
    , updateInputs                  :: [Input]
    }  deriving (Read, Show, Eq)

data Delete
  = Delete
    { deleteItemPath                :: Maybe String
    , deletePollingFrequencySeconds :: Maybe Integer
    , deleteProduces                :: Maybe Product
    , deleteUrls                    :: [String]
    , deleteInputs                  :: [Input]
    }  deriving (Read, Show, Eq)

data Product
  = ProductXML | ProductJSON
  deriving (Eq)

instance Read Product where
  readPrec = choice $ strValMap
             [ ("XML", ProductXML)
             , ("JSON", ProductJSON) ]

instance Show Product where
  showsPrec _ ProductXML  = showString "XML"
  showsPrec _ ProductJSON = showString "JSON"

data Function
  = Function
    { functionName :: Maybe String
    , functionType :: FunctionType
    } deriving (Read, Show, Eq)

data FunctionType
  = FunctionTypeStream | FunctionTypeReduce
  deriving (Eq)

instance Read FunctionType where
  readPrec = choice $ strValMap
             [ ("stream", FunctionTypeStream)
             , ("reduce", FunctionTypeReduce) ]

instance Show FunctionType where
  showsPrec _ FunctionTypeStream = showString "stream"
  showsPrec _ FunctionTypeReduce = showString "reduce"

data Input
  = InputKey InputInfo
  | InputValue InputInfo
  | InputMap InputInfo
  deriving (Read, Show, Eq)

data InputInfo
  = InputInfo
    { inputInfoId            :: String
    , inputInfoAs            :: Maybe String
    , inputInfoType          :: InputType
    , inputInfoParamType     :: ParamType
    , inputInfoRequired      :: Bool
    , inputInfoDefault       :: Maybe String
    , inputInfoPrivate       :: Maybe Bool
    , inputInfoConst         :: Maybe Bool
    , inputInfoBatchable     :: Maybe Bool
    , inputInfoMaxBatchItems :: Maybe Integer
    } deriving (Read, Show, Eq)

data InputType =
  InputTypeBool   |
  InputTypeDate   |
  InputTypeDouble |
  InputTypeFloat  |
  InputTypeInt    |
  InputTypeString
  deriving (Eq)

instance Read InputType where
  readPrec = choice $ strValMap
             [ ("xs:bool", InputTypeBool)
             , ("xs:boolean", InputTypeBool)
             , ("xs:date", InputTypeDate)
             , ("xs:double", InputTypeDouble)
             , ("xs:float", InputTypeFloat)
             , ("xs:int", InputTypeInt)
             , ("xs:integer", InputTypeInt)
             , ("xs:number", InputTypeDouble)
             , ("xs:string", InputTypeString)]

instance Show InputType where
  showsPrec _ InputTypeBool   = showString "xs:bool"
  showsPrec _ InputTypeDate   = showString "xs:date"
  showsPrec _ InputTypeDouble = showString "xs:double"
  showsPrec _ InputTypeFloat  = showString "xs:float"
  showsPrec _ InputTypeInt    = showString "xs:int"
  showsPrec _ InputTypeString = showString "xs:string"

data ParamType
  = ParamTypeQuery | ParamTypeMatrix | ParamTypeHeader
  | ParamTypePath  | ParamTypeVariable
  deriving (Eq)

instance Read ParamType where
  readPrec = choice $ strValMap
             [ ("query", ParamTypeQuery)
             , ("matrix", ParamTypeMatrix)
             , ("header", ParamTypeHeader)
             , ("path", ParamTypePath)
             , ("variable", ParamTypeVariable) ]

instance Show ParamType where
  showsPrec _ ParamTypeQuery = showString "query"
  showsPrec _ ParamTypeMatrix = showString "matrix"
  showsPrec _ ParamTypeHeader = showString "header"
  showsPrec _ ParamTypePath = showString "path"
  showsPrec _ ParamTypeVariable = showString "variable"

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
