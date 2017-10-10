{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Types where

import Data.Aeson.TH

data Nag = Nag
  { action :: String
  , tags :: [ String ]
  , status :: Status
  } deriving ( Eq, Ord, Show )

data Status
  = Complete
  | Active
  | Queued
  | Cancelled
  deriving ( Eq, Ord, Show )

$(deriveJSON defaultOptions ''Status)
$(deriveJSON defaultOptions ''Nag)
