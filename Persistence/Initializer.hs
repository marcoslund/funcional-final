{-# LANGUAGE TemplateHaskell, TypeFamilies #-}

module Persistence.Initializer where

import Models.Store
import Persistence.Cart
import Persistence.Category
import Persistence.Product
import Persistence.User

import Data.Acid            (makeAcidic)

$(makeAcidic ''Store
  [ 'createProduct
  , 'updateProduct
  , 'productById
  , 'productsByStatus
  , 'productsByPriceAsc
  , 'productsByPriceDesc
  , 'productsByNameAsc
  , 'productsByCategoryId
  , 'productsByCategory
  , 'newCategory
  , 'buildCategories
  , 'categoryById
  , 'getCategories
  , 'categoriesByNameAsc
  , 'newUser
  , 'userById
  , 'userByEmailAsc
  , 'appendProduct
  ])
