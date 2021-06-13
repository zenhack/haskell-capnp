{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ViewPatterns          #-}
-- Things used by both RawToHaskell and PureToHaskell.
module Trans.ToHaskellCommon where

import Data.Word

import Data.Char       (toUpper)
import System.FilePath (splitDirectories)
import Text.Printf     (printf)

import qualified Data.Set  as S
import qualified Data.Text as T

import qualified IR.Common as C
import qualified IR.Name   as Name

import IR.Haskell

-- Misc shortcuts for common constructs:

std_ :: Name.UnQ -> Name.GlobalQ
std_ name = gName ["Std_"] (Name.mkLocal Name.emptyNS name)

eStd_ :: Name.UnQ -> Exp
eStd_ = EGName . std_

ePureUnit :: Exp
ePureUnit = EApp (eStd_ "pure") [ETup []]

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

puName :: Name.UnQ -> [Pattern] -> Pattern
puName = PLCtor . Name.unQToLocal

tuName :: Name.UnQ -> Type
tuName = TLName . Name.unQToLocal

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
instance_ ctx [] className tys defs = DcInstance
    { ctx
    , typ = TApp (TLName className) tys
    , defs
    }
instance_ ctx classNS className tys defs = DcInstance
    { ctx
    , typ = TApp (tgName classNS className) tys
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


-- | Fix the capnp imports of a module, so that they correspond to the
-- imports actually used in the body of the module and/or export list.
--
-- Note that this only looks at imports of the form Capnp.Gen....; other
-- imports are not touched.
fixImports :: Module -> Module
fixImports m@Module{modImports} =
    let namespaces = S.toList $ S.fromList -- deduplicate
            [ nsParts
            | Name.GlobalQ
                { globalNS = Name.NS nsParts@(map T.unpack -> "Capnp":"Gen":_)
                }
            <- S.toList (findGNames m)
            ]
        neededImports =
            [ ImportQual { parts = map Name.UnQ nsParts }
            | nsParts <- namespaces
            ]
    in
    m { modImports = modImports ++ neededImports }

toTVars :: [Name.UnQ] -> [Type]
toTVars = map (TVar . Name.typeVarName)

class HasGNames a where
    -- | Collect all of the 'Name.GlobalQ's used in the module.
    --
    -- This seems like it would be the perfect use case for something
    -- like syb or similar libraries, but I(zenhack) haven't taken the
    -- time to fully wrap my head around how to use those yet, so we do
    -- it the boilerplate-heavy way.
    findGNames :: a -> S.Set Name.GlobalQ

instance HasGNames Module where
    findGNames Module{modExports=Just exports, modDecls} =
        S.unions $ map findGNames exports ++ map findGNames modDecls
    findGNames Module{modExports=Nothing, modDecls} =
        S.unions $ map findGNames modDecls

instance HasGNames Export where
    findGNames (ExportGCtors name) = S.singleton name
    findGNames (ExportGName name)  = S.singleton name
    findGNames _                   = S.empty

instance HasGNames Decl where
    findGNames (DcData d)        = findGNames d
    findGNames DcValue{typ, def} = findGNames typ `S.union` findGNames def
    findGNames DcInstance{ctx, typ, defs} = S.unions
        [ S.unions $ map findGNames ctx
        , findGNames typ
        , S.unions $ map findGNames defs
        ]
    findGNames (DcTypeInstance alias orig) = findGNames alias `S.union` findGNames orig
    findGNames (DcDeriveInstance ctx typ) = findGNames (TCtx ctx typ)
    findGNames DcClass{ctx, decls} =
        S.unions $ map findGNames ctx ++ map findGNames decls

instance HasGNames DataDecl where
    findGNames Data{typeArgs, dataVariants} =
        S.unions $ map findGNames typeArgs ++ map findGNames dataVariants

instance HasGNames ClassDecl where
    findGNames (CdValueDecl _ ty) = findGNames ty
    findGNames (CdValueDef d)     = findGNames d
    findGNames (CdMinimal _)      = S.empty

instance HasGNames InstanceDef where
    findGNames (IdValue d) = findGNames d
    findGNames (IdData d)  = findGNames d
    findGNames (IdType t)  = findGNames t

instance HasGNames TypeAlias where
    findGNames (TypeAlias _ ts t) = S.unions $ map findGNames (t:ts)

instance HasGNames ValueDef where
    findGNames DfValue{value, params} =
        S.unions $ findGNames value : map findGNames params

instance HasGNames DataVariant where
    findGNames DataVariant{dvArgs} = findGNames dvArgs

instance HasGNames DataArgs where
    findGNames (APos tys)    = S.unions $ map findGNames tys
    findGNames (ARec fields) = S.unions $ map (findGNames . snd) fields

instance HasGNames Type where
    findGNames (TGName n)  = S.singleton n
    findGNames (TApp t ts) = S.unions $ map findGNames (t:ts)
    findGNames (TFn ts)    = S.unions $ map findGNames ts
    findGNames (TCtx ts t) = S.unions $ map findGNames (t:ts)
    findGNames _           = S.empty

instance HasGNames Exp where
    findGNames (EApp e es)  = S.unions $ map findGNames (e:es)
    findGNames (EFApp e es) = S.unions $ map findGNames (e:es)
    findGNames (EDo ds e)   = S.unions $ findGNames e : map findGNames ds
    findGNames (EBind x y)  = findGNames x `S.union` findGNames y
    findGNames (ETup es)    = S.unions $ map findGNames es
    findGNames (ECase e arms) = S.unions
        [ findGNames e
        , S.unions $ map (findGNames . fst) arms
        , S.unions $ map (findGNames . snd) arms
        ]
    findGNames (ETypeAnno e t) = findGNames e `S.union` findGNames t
    findGNames (ELambda ps e) = S.unions $ findGNames e : map findGNames ps
    findGNames (ERecord e fields) = S.unions $ findGNames e : map (findGNames . snd) fields
    findGNames _ = S.empty

instance HasGNames Do where
    findGNames (DoBind _ e) = findGNames e
    findGNames (DoE e)      = findGNames e

instance HasGNames Pattern where
    findGNames (PLCtor _ ps) = S.unions $ map findGNames ps
    findGNames (PGCtor n ps) = S.unions $ S.singleton n : map findGNames ps
    findGNames _             = S.empty
