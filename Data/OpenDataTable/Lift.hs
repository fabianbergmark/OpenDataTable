{-# LANGUAGE TemplateHaskell #-}

module Data.OpenDataTable.Lift
       () where

import Data.OpenDataTable

import Language.Haskell.TH.Lift (deriveLift)

$(deriveLift ''OpenDataTable)
$(deriveLift ''SecurityLevel)
$(deriveLift ''Delete)
$(deriveLift ''Meta)
$(deriveLift ''Binding)
$(deriveLift ''Select)
$(deriveLift ''Paging)
$(deriveLift ''PagingModel)
$(deriveLift ''PagingSize)
$(deriveLift ''PagingStart)
$(deriveLift ''PagingTotal)
$(deriveLift ''NextPage)
$(deriveLift ''Insert)
$(deriveLift ''Update)
$(deriveLift ''Product)
$(deriveLift ''Function)
$(deriveLift ''FunctionType)
$(deriveLift ''Input)
$(deriveLift ''InputInfo)
$(deriveLift ''InputType)
$(deriveLift ''ParamType)
