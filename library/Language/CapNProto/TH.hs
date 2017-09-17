{-# LANGUAGE TemplateHaskell #-}
module Language.CapNProto.TH
    ( mkStructWrappers
    , mkListReaders
    , mkWordReaders
    )
  where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.Bits
import Control.Monad.Catch(throwM)

import qualified Data.CapNProto.Errors as E
import qualified Data.CapNProto.Untyped as U

-- | For a type with one data constructor, with the same name as its type
-- constructor, convert a 'Name' for the data constructor to a 'Name' for
-- the type constructor.
inferTypeName :: Name -> Name
inferTypeName (Name occ (NameG DataName pkgName modName)) =
    Name occ (NameG TcClsName pkgName modName)
inferTypeName name = name

-- | @mkStructWrapper name@ Defines a newtype wrapper around a struct type,
-- e.g. @newtype MyStruct b = MyStruct (U.Struct b)@
mkStructWrapper :: String -> DecQ
mkStructWrapper name = do
    let name' = mkName name
    let b = mkName "b"
    return $ NewtypeD [] name' [PlainTV b] Nothing
                (NormalC name' [ ( Bang NoSourceUnpackedness
                                         NoSourceStrictness
                                  , AppT (ConT ''U.Struct) (VarT b)
                                  )
                                ])
                []

mkStructWrappers :: [String] -> DecsQ
mkStructWrappers = mapM mkStructWrapper

-- | @mkReaderType parentType childType@ emits the type for a reader that reads
-- values of type @childType b@ from values of type @parentType b@, i.e.
-- @U.ReadCtx m b => parentType b -> m (returnType b)@
mkReaderType :: (TypeQ -> TypeQ) -> (TypeQ -> TypeQ) -> TypeQ
mkReaderType parentType returnType = do
    m <- varT <$> newName "m"
    b <- varT <$> newName "b"
    [t| U.ReadCtx $m $b => $(parentType b) -> $m $(returnType b) |]

-- | Like @mkReaderType@, except that the return type is wrapped in a @Maybe@,
-- as is typical of pointer types.
mkPtrReaderType :: (TypeQ -> TypeQ) -> (TypeQ -> TypeQ) -> TypeQ
mkPtrReaderType parentType returnType =
    mkReaderType parentType $ \b -> [t| Maybe $(returnType b) |]

mkPtrReaderVal parentConName ptrOffset withPtr = do
    struct <- newName "struct"
    ptr' <- newName "ptr'"
    [| \ $(conP parentConName [varP struct]) -> do
            ptrSec <- U.ptrSection $(varE struct)
            ptr <- U.index $(litE $ IntegerL ptrOffset) ptrSec
            case ptr of
                Nothing -> return Nothing
                Just $(varP ptr') -> $(withPtr ptr') |]

mkListReaderVal parentConName ptrOffset listConName withList = do
    list <- newName "list"
    mkPtrReaderVal parentConName ptrOffset $ \ptr' ->
        [| case $(varE ptr') of
                U.PtrList $(conP listConName [varP list]) ->
                    $(withList list)
                _ -> throwM $ E.SchemaViolationError $ $(litE $ StringL $
                            "Expected PtrList (" ++ show listConName ++ " ...)") |]


mkListReaderType parentType childType = mkPtrReaderType
    (\b -> [t| $parentType $b |])
    (\b -> [t| U.ListOf $b ($childType $b) |])

mkReader :: Name -> TypeQ -> ExpQ -> DecsQ
mkReader name ty val = do
    ty' <- ty
    val' <- val
    return [ SigD name ty'
           , ValD (VarP name) (NormalB val') []
           ]

-- | @mkListReader@ generates a reader which extracts a list from a struct.
mkListReader :: String -- ^ The name of the reader
            -> Name    -- ^ The data constructor for the parent type.
            -> Integer -- ^ The offset into the struct's pointer section.
            -> Name    -- ^ The 'List' data constructor that we expect
            -> Name    -- ^ A data constructor to apply to the elements of
                       --   the list.
            -> DecsQ
mkListReader readerName parentConName ptrOffset listConName childConName = mkReader
    (mkName readerName)
    (mkListReaderType
        (conT $ inferTypeName parentConName)
        (conT $ inferTypeName childConName))
    (mkListReaderVal parentConName ptrOffset listConName $
        (\list -> [| return $ Just $ fmap $(conE childConName) $(varE list) |]))

-- | @mkListReaders name args@ calls mkListReader once for each tuple in
-- @args@. @parent@ is always passed as the first argument. the values
-- in the tuple are the remaining arguments.
mkListReaders :: Name -> [(String, Integer, Name, Name)] -> DecsQ
mkListReaders parent readers =
    concat <$> mapM (uncurry4 $ \arg -> mkListReader arg parent) readers
  where
    uncurry4 f (a, b, c, d) = f a b c d

mkWordReaders :: Name -> [(String, Integer, Name, Integer, ExpQ)] -> DecsQ
mkWordReaders con readers =
    concat <$> mapM mkReader readers
  where
    mkReader (name, start, typ, defaultVal, transform) = do
        let fnName = mkName name
        struct <- newName "struct"
        body <- mkBody struct
        return $ [ FunD fnName [Clause [ConP con [VarP struct]]
                                       (NormalB body)
                                       []
                               ]
                 ]
      where
        mkBody struct = do
            let dataIndex = LitE $ IntegerL $ start `div` 64
            let bitOffset = LitE $ IntegerL $  start `mod` 64
            let defaultValE = litE $ IntegerL $ defaultVal
            [| do dataSec <- U.dataSection $(varE struct)
                  word <- U.index $(return $ dataIndex) dataSec
                  let rawVal = word `shiftR` $(return $ bitOffset) `xor` $defaultValE
                  return $ $transform $ (fromIntegral rawVal :: $(conT typ)) |]
