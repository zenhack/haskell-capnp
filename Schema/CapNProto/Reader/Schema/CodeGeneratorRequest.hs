{-# LANGUAGE ConstraintKinds, TemplateHaskell #-}
module Schema.CapNProto.Reader.Schema.CodeGeneratorRequest where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.Quota (MonadQuota)

import Data.CapNProto.Blob (Blob)
import qualified Schema.CapNProto.Reader.Schema as S
import qualified Data.CapNProto.Untyped as U

import Data.CapNProto.TH

$(mkStructWrappers ["RequestedFile", "Import"])

$(mkListReaders
    'S.CodeGeneratorRequest
    [ ("nodes",          'S.Node)
    , ("requestedFiles", 'RequestedFile)
    , ("imports",        'Import)
    ])
