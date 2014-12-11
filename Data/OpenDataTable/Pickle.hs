module Data.OpenDataTable.Pickle
       () where

import Text.XML.HXT.Core
import Text.XML.HXT.Arrow.Pickle.Schema

import Data.OpenDataTable

instance XmlPickler OpenDataTable where
  xpickle = xpOpenDataTable

xpOpenDataTable :: PU OpenDataTable
xpOpenDataTable
  = xpWrap ( \ ((xmlns, securityLevel, https, meta, execute, bindings)) ->
              OpenDataTable xmlns securityLevel https meta execute bindings
           , \ ot -> (openDataTableXmlns ot, openDataTableSecurityLevel ot,
                     openDataTableHttps ot, openDataTableMeta ot,
                     openDataTableExecute ot, openDataTableBindings ot)) $
    xpElem "table" $
    xp6Tuple
    (xpOption $ xpAddFixedAttr "xmlns" "http://query.yahooapis.com/v1/schema/table.xsd" xpText)
    (xpOption $ xpAttr "securityLevel" xpPrim)
    (xpOption $ xpAttr "https" xpPrim)
    xpMeta
    (xpOption (xpElem "execute" $ xpTextDT scString))
    xpBindings

instance XmlPickler Meta where
  xpickle = xpMeta

xpMeta :: PU Meta
xpMeta
  = xpElem "meta" $
    xpWrap ( \ ((aut, des, doc, sam, api)) -> Meta api aut doc des sam
           , \ m -> (metaAuthor m, metaDescription m, metaDocumentationURL m,
                     metaSampleQuery m, metaApiKeyURL m)) $
    xp5Tuple
    (xpOption $ xpElem "author" xpText)
    (xpOption $ xpElem "description" xpText)
    (xpOption $ xpElem "documentationURL" xpText)
    (xpList $ xpElem "sampleQuery" xpText)
    (xpOption $ xpElem "apiKeyUrl" xpText)

xpBindings :: PU [Binding]
xpBindings = xpElem "bindings" $
             xpList xpBinding

instance XmlPickler Binding where
  xpickle = xpBinding

xpBinding :: PU Binding
xpBinding = xpWrap ( SelectBinding, \ (SelectBinding s) -> s ) $
            xpickle

instance XmlPickler Select where
  xpickle = xpSelect

xpSelect :: PU Select
xpSelect = xpElem "select" $
           xpWrap ( \ ((ite, pro, pol, url, inp, exe, pag)) ->
                     Select ite pro pol url inp exe pag,
                    \ s -> (selectItemPath s, selectProduces s,
                            selectPollingFrequencySeconds s,
                            selectUrl s, selectInputs s,
                            selectExecute s, selectPaging s) ) $
           xp7Tuple
           (xpOption (xpAttr "itemPath" xpText))
           (xpOption (xpAttr "produces" xpPrim))
           (xpOption (xpAttr "pollingFrequencySeconds" xpPrim))
           (xpOption (xpElem "urls" $ xpElem "url" xpText))
           (xpElem "inputs" $ xpList $ xpInput)
           (xpOption (xpElem "execute" $ xpTextDT scString))
           (xpElem "paging" xpickle)

xpInput :: PU Input
xpInput = xpWrap ( InputKey, \ (InputKey i) -> i ) $
          xpElem "key" xpickle

instance XmlPickler InputInfo where
  xpickle = xpInputInfo

xpInputInfo :: PU InputInfo
xpInputInfo = xpWrap ( \ ((id, as, typ, par, req, def, pri, con, bat, max)) ->
                        InputInfo id as typ par req def pri con bat max,
                       \ i -> (inputInfoId i, inputInfoAs i, inputInfoType i,
                               inputInfoParamType i, inputInfoRequired i,
                               inputInfoDefault i, inputInfoPrivate i,
                               inputInfoConst i, inputInfoBatchable i,
                               inputInfoMaxBatchItems i)) $
              xp10Tuple
              (xpAttr "id" xpText)
              (xpOption $ xpAttr "as" xpText)
              (xpAttr "type" xpPrim)
              (xpAttr "paramType" xpPrim)
              (xpAttr "required" xpPrim)
              (xpOption $ xpAttr "default" xpText)
              (xpOption $ xpAttr "private" xpPrim)
              (xpOption $ xpAttr "const" xpPrim)
              (xpOption $ xpAttr "batchable" xpPrim)
              (xpOption $ xpAttr "maxBatchItems" xpPrim)

instance XmlPickler Paging where
  xpickle = xpPaging

xpPaging :: PU Paging
xpPaging = xpWrap ( \ ((mod, mat, siz, sta, tot, nex)) ->
                   Paging mod mat siz sta tot nex,
                  \ p -> (pagingModel p, pagingMatrix p, pagingPageSize p,
                          pagingStart p, pagingTotal p, pagingNextPage p)) $
         xp6Tuple
         (xpOption $ xpAttr "model" xpPrim)
         (xpOption $ xpAttr "matrix" xpPrim)
         (xpOption $ xpickle)
         (xpOption $ xpickle)
         (xpOption $ xpickle)
         (xpOption $ xpickle)

instance XmlPickler PagingSize where
  xpickle = xpElem "pagesize" $
            xpWrap ( \ ((id, max)) -> PagingSize id max,
                     \ ps -> (pagingSizeId ps, pagingSizeMax ps)) $
            xpPair
            (xpAttr "id" xpText)
            (xpAttr "max" xpPrim)

instance XmlPickler PagingStart where
  xpickle = xpElem "start" $
            xpWrap ( \ ((id, def)) -> PagingStart id def,
                     \ ps -> (pagingStartId ps, pagingStartDefault ps)) $
            xpPair
            (xpAttr "id" xpText)
            (xpAttr "default" xpPrim)

instance XmlPickler PagingTotal where
  xpickle = xpElem "total" $
            xpWrap ( PagingTotal, pagingTotalDefault) $
            (xpAttr "default" xpPrim)

instance XmlPickler NextPage where
  xpickle = xpElem "nextpage" $
            xpWrap ( NextPage, nextPagePath) $
            (xpAttr "path" xpText)
