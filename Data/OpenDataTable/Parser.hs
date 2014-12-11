{-# LANGUAGE Arrows  #-}

module Data.OpenDataTable.Parser
       ( parseBindings
       , parseInput
       , parseInputInfo
       , parsePaging
       , parsePagingNextPage
       , parsePagingPageSize
       , parsePagingStart
       , parsePagingTotal
       , parseMeta
       , parseOpenDataTable
       , parseSelect ) where

import Text.Read
import Data.Maybe

import Text.XML.HXT.Core (arr, deep, constA, isElem, orElse, returnA,
                          isA, listA, getText, none,
                          hasName, getChildren,  getAttrValue0,
                          (<<<),  (>>>),
                          XmlTree, ArrowXml, ArrowList, ArrowChoice)

import Data.OpenDataTable

readBoolMaybe :: String -> Maybe Bool
readBoolMaybe "true"  = Just True
readBoolMaybe "false" = Just False
readBoolMaybe _       = Nothing

atTag :: ArrowXml a => String -> a XmlTree XmlTree
atTag tag = deep (isElem >>> hasName tag)

atOptionalTag :: ArrowXml a => String -> a XmlTree (Maybe XmlTree)
atOptionalTag tag = deep (isElem >>> hasName tag >>> arr Just)
                `orElse` (constA Nothing)

text :: ArrowXml a => a XmlTree String
text = getChildren >>> getText

textAtTag :: ArrowXml a => String -> a XmlTree String
textAtTag tag = atTag tag >>> text

getList :: ArrowXml a => String -> a XmlTree c -> a XmlTree [c]
getList tag cont = listA ( atTag tag >>> cont)

getOptionalList :: (ArrowChoice a, ArrowXml a) =>
                   String -> a XmlTree b -> a (Maybe XmlTree) [b]
getOptionalList tag cont =
  proc x -> do
    case x of
      Nothing -> returnA -< []
      Just xml -> returnA  <<< getList tag cont -< xml

significant :: String -> Bool
significant = not . all (`elem` " \n\r\t")

optionalAttr :: ArrowXml a => String -> a XmlTree (Maybe String)
optionalAttr attr =
  (getAttrValue0 attr >>> isA significant >>> arr Just)
  `orElse` (constA Nothing)

optionalTextAtTag :: ArrowXml a => String -> a XmlTree (Maybe String)
optionalTextAtTag tag =
  (textAtTag tag >>> arr Just)
  `orElse` (constA Nothing)

optionalTextAtOptionalTag :: (ArrowChoice a, ArrowXml a) =>
                             String -> a (Maybe XmlTree) (Maybe String)
optionalTextAtOptionalTag tag =
  proc mx -> do
    case mx of
     Nothing -> returnA -< Nothing
     Just x -> optionalTextAtTag tag -< x

readOptionalAttrVal :: (ArrowChoice a, ArrowList a) =>
                       (b -> Maybe c) -> a (Maybe b) (Maybe c)
readOptionalAttrVal reader =
  proc x -> do
    case x of
      Nothing   -> returnA -< Nothing
      Just attr -> do
        let mRead = reader attr
        case mRead of
          Nothing  -> none    -< Nothing
          Just val -> returnA -< Just val

readOptionalAttr :: (ArrowList a, ArrowChoice a, Read c) =>
                    a (Maybe String) (Maybe c)
readOptionalAttr = readOptionalAttrVal readMaybe

readOptionalAttrBool :: (ArrowList a, ArrowChoice a) =>
                        a (Maybe String) (Maybe Bool)
readOptionalAttrBool = readOptionalAttrVal readBoolMaybe

readAttrVal :: (ArrowChoice a, ArrowList a) => (b -> Maybe c) -> a b c
readAttrVal reader =
  proc x -> do
    let mRead = reader x
    case mRead of
      Nothing ->  none    -< Nothing
      Just val -> returnA -< val

readAttr :: (ArrowChoice a, ArrowList a, Read b) =>
            a String b
readAttr = readAttrVal readMaybe

readAttrBool :: (ArrowChoice a, ArrowList a) =>
                a String Bool
readAttrBool = readAttrVal readBoolMaybe

parseOpenDataTable :: (ArrowXml a, ArrowChoice a) =>
                  a XmlTree OpenDataTable
parseOpenDataTable =
  proc x -> do
    mHttps        <- optionalAttr "https"             -< x
    https         <- readOptionalAttr                 -< mHttps
    xmlns         <- optionalAttr "xmlns"             -< x
    securityLevel <- readOptionalAttr
                     <<< optionalAttr "securityLevel" -< x
    meta          <- parseMeta                        -< x
    mExecute      <- optionalAttr "execute"           -< x
    execute       <- readOptionalAttr                 -< mExecute
    bindings      <- parseBindings                    -< x
    returnA -< OpenDataTable
      xmlns
      securityLevel
      https
      meta
      execute
      bindings

parseMeta :: (ArrowXml a) => a XmlTree Meta
parseMeta =
  proc x -> do
    meta             <- atTag "meta"                          -< x
    author           <- optionalTextAtTag "author"            -< meta
    apiKeyUrl        <- optionalTextAtTag "apiKeyUrl"         -< meta
    description      <- optionalTextAtTag "description"       -< meta
    sampleQueries    <- getList "sampleQuery" text            -< meta
    documentationUrl <- optionalTextAtTag "documentationURL"  -< meta
    returnA -< Meta
      author
      description
      documentationUrl
      apiKeyUrl
      sampleQueries

parseBindings :: (ArrowXml a, ArrowChoice a) => a XmlTree [Binding]
parseBindings =
  proc x -> do
    bindings <- atTag "bindings"                       -< x
    selects  <- listA (atTag "select" >>> parseSelect) -< bindings
    returnA -< SelectBinding `fmap` selects

parseSelect :: (ArrowXml a, ArrowChoice a) => a XmlTree Select
parseSelect =
  proc x -> do
    itemPath                <- optionalAttr "itemPath"                    -< x
    produces                <- readOptionalAttr
                               <<< optionalAttr "produces"                -< x
    pollingFrequencySeconds <- readOptionalAttr
                               <<< optionalAttr "pollingFrequencySeconds" -< x
    url                     <- optionalTextAtOptionalTag "url"
                               <<< atOptionalTag "urls"                   -< x
    inputs                  <- parseInput                                 -< x
    execute                 <- optionalTextAtTag "execute"                -< x
    paging                  <- (arr Just <<< parsePaging)
                               `orElse` (constA Nothing)                  -< x
    returnA -< Select
      itemPath
      produces
      pollingFrequencySeconds
      url
      inputs
      execute
      paging

parseInput :: (ArrowXml a, ArrowChoice a) => a XmlTree [Input]
parseInput =
  proc x -> do
    inputs <- atOptionalTag "inputs"               -< x
    keys   <- getOptionalList "key" parseInputInfo -< inputs
    returnA -< InputKey `fmap` keys

parseInputInfo :: (ArrowXml a, ArrowChoice a) => a XmlTree InputInfo
parseInputInfo =
  proc x -> do
    id            <- getAttrValue0 "id"                               -< x
    as            <- optionalAttr "as"                                -< x
    typ           <- readAttr
                     <<< getAttrValue0 "type"                         -< x
    paramType     <- readAttr <<< getAttrValue0 "paramType"           -< x
    required      <- readOptionalAttrBool <<< optionalAttr "required" -< x
    def           <- optionalAttr "default"                           -< x
    private       <- readOptionalAttrBool
                     <<< optionalAttr "private"                       -< x
    const         <- readOptionalAttrBool
                     <<< optionalAttr "const"                         -< x
    batchable     <- readOptionalAttrBool
                     <<< optionalAttr "batchable"                     -< x
    maxBatchItems <- readOptionalAttr
                     <<< optionalAttr "maxBatchItems"                 -< x
    returnA -< InputInfo
      id
      as
      typ
      paramType
      (fromMaybe False required)
      def
      private
      const
      batchable
      maxBatchItems

parsePaging :: (ArrowXml a, ArrowChoice a) => a XmlTree Paging
parsePaging =
  proc x -> do
    paging    <- atTag "paging"                   -< x
    mModel    <- optionalAttr "model"             -< paging
    model     <- readOptionalAttr                 -< mModel
    matrix    <- readOptionalAttrBool
                 <<< optionalAttr "matrix"        -< paging
    pageSize  <- (arr Just <<< parsePagingPageSize)
                 `orElse` (constA Nothing)        -< paging
    pageStart <- (arr Just <<< parsePagingStart)
                 `orElse` (constA Nothing)        -< paging
    pageTotal <- (arr Just <<< parsePagingTotal)
                 `orElse` (constA Nothing)        -< paging
    nextPage  <- (arr Just <<< parsePagingNextPage)
                 `orElse` (constA Nothing)        -< paging
    returnA -< Paging
      model
      matrix
      pageSize
      pageStart
      pageTotal
      nextPage

parsePagingPageSize :: ArrowXml a => a XmlTree PagingSize
parsePagingPageSize =
  proc x -> do
    pageSize <- atTag "pagesize"    -< x
    id       <- getAttrValue0 "id"  -< pageSize
    max      <- getAttrValue0 "max" -< pageSize
    returnA -< PagingSize id $ read max

parsePagingStart :: ArrowXml a => a XmlTree PagingStart
parsePagingStart =
  proc x -> do
    pagingStart <- atTag "start"           -< x
    id          <- getAttrValue0 "id"      -< pagingStart
    def         <- getAttrValue0 "default" -< pagingStart
    returnA -< PagingStart id $ read def

parsePagingTotal :: ArrowXml a => a XmlTree PagingTotal
parsePagingTotal =
  proc x -> do
    pagingTotal <- atTag "total"           -< x
    def         <- getAttrValue0 "default" -< pagingTotal
    returnA -< PagingTotal $ read def

parsePagingNextPage :: ArrowXml a => a XmlTree NextPage
parsePagingNextPage =
  proc x -> do
    nextPage <- atTag "nextpage"    -< x
    path     <- getAttrValue0 "path" -< nextPage
    returnA -< NextPage path
