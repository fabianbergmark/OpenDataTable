module Data.OpenDataTable
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

import Text.Read
import Text.ParserCombinators.ReadP hiding (choice)

strValMap = map (\(x, y) -> lift $ string x >> return y)

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
