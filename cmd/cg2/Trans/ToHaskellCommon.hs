{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
-- Things used by both RawToHaskell and PureToHaskell.
module Trans.ToHaskellCommon where

import qualified Data.Text as T

import Data.Word

import Data.Char       (toUpper)
import System.FilePath (splitDirectories)
import Text.Printf     (printf)

import qualified IR.Common as C
import qualified IR.Name   as Name

import IR.Haskell

-- Misc shortcuts for common constructs:

std_ :: Name.UnQ -> Name.GlobalQ
std_ name = gName ["Std_"] (Name.mkLocal Name.emptyNS name)

eStd_ :: Name.UnQ -> Exp
eStd_ = EGName . std_

tStd_ :: Name.UnQ -> Type
tStd_ = TGName . std_

gName :: [T.Text] -> Name.LocalQ -> Name.GlobalQ
gName parts local = Name.GlobalQ
    { globalNS = Name.NS parts
    , local
    }

egName :: [T.Text] -> Name.LocalQ -> Exp
egName parts local = EGName $ gName parts local

euName :: Name.UnQ -> Exp
euName = ELName . Name.mkLocal Name.emptyNS

tgName :: [T.Text] -> Name.LocalQ -> Type
tgName parts local = TGName $ gName parts local

pgName :: [T.Text] -> Name.LocalQ -> [Pattern] -> Pattern
pgName parts local = PGCtor (gName parts local)

tuName :: Name.UnQ -> Type
tuName = TLName . Name.mkLocal Name.emptyNS

iValue :: Name.UnQ -> [Pattern] -> Exp -> InstanceDef
iValue name params value = IdValue DfValue {name, params, value}

iType :: Name.UnQ -> [Type] -> Type -> InstanceDef
iType name params value = IdType $ TypeAlias name params value

readCtx :: T.Text -> T.Text -> Type
readCtx m msg = TApp
    (tgName ["Untyped"] "ReadCtx")
    [ TVar m
    , TVar msg
    ]

rwCtx :: T.Text -> T.Text -> Type
rwCtx m s = TApp
    (tgName ["Untyped"] "RWCtx")
    [ TVar m
    , TVar s
    ]

eGetWordField :: Exp -> C.DataLoc -> Exp
eGetWordField struct C.DataLoc{dataIdx, dataOff, dataDef} =
    EApp
        (egName ["GenHelpers"] "getWordField")
        [ struct
        , EInt $ fromIntegral dataIdx
        , EInt $ fromIntegral dataOff
        , EInt $ fromIntegral dataDef
        ]

idToModule :: Word64 -> [Name.UnQ]
idToModule fileId =
    ["Capnp", "Gen", "ById", Name.UnQ $ T.pack $ printf "X%x" fileId]

instance_ :: [Type] -> [T.Text] -> Name.LocalQ -> [Type] -> [InstanceDef] -> Decl
instance_ ctx classNS className tys defs = DcInstance
    { ctx
    , typ = TApp
        (tgName classNS className)
        tys
    , defs
    }

-- | Transform the file path into a valid haskell module name.
-- TODO: this is a best-effort transformation; it gives good
-- results on the schema I(zenhack) have found in the wild, but
-- may fail to generate valid/non-overlapping module names in
-- all cases.
--
-- This generates the bit that is unique to the specific file
-- name and common to both raw and pure backends, so e.g. for
-- @myorg/example.capnp@ it generates @["Myorg", "Example"]@.
makeModName :: FilePath -> [Name.UnQ]
makeModName fileName =
    [ Name.UnQ (T.pack (mangleSegment seg)) | seg <- splitDirectories fileName ]
  where
    mangleSegment "c++.capnp" = "Cxx"
    mangleSegment ""          = error "Unexpected empty file name"
    mangleSegment (c:cs)      = go (toUpper c : cs) where
        go ('-':c:cs) = toUpper c : go cs
        go ".capnp"   = ""
        go []         = ""
        go (c:cs)     = c : go cs
