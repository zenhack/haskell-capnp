{-# LANGUAGE TemplateHaskell #-}
module Language.CapNProto.TH
    ( mkStructWrappers
    , mkListReaders
    , mkWordReaders
    )
  where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.Word
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


-- | @requireCon con@ constructs a function that matches its argument against
-- the unary data constructor named by @con@, returning the value contained
-- within if the pattern matches, and calling 'throwM' with a
-- 'SchemaViolationError' otherwise.
requireCon :: Name -> ExpQ
requireCon con = do
    result <- newName "result"
    let errmsg = "Expected " ++ show con
    [| \arg -> case arg of
                    $(return $ ConP con [VarP result]) ->
                        return $(return $ VarE result)
                    _ ->
                        throwM $ E.SchemaViolationError $(return $ LitE $ StringL errmsg)
     |]

-- | @mkReaderType parentType childType@ emits the type for a reader that reads
-- values of type @childType b@ from values of type @parentType b@, i.e.
-- @U.ReadCtx m b => parentType b -> m (returnType b)@
mkReaderType :: (TypeQ -> TypeQ) -> (TypeQ -> TypeQ) -> TypeQ
mkReaderType parentType returnType = do
    m <- VarT <$> newName "m"
    b <- VarT <$> newName "b"
    let (m', b') = (return m, return b)
    [t| U.ReadCtx $m' $b' => $(parentType b') -> $m' $(returnType b') |]

-- | Like @mkReaderType@, except that the return type is wrapped in a @Maybe@,
-- as is typical of pointer types.
mkPtrReaderType :: (TypeQ -> TypeQ) -> (TypeQ -> TypeQ) -> TypeQ
mkPtrReaderType parentType returnType =
    mkReaderType parentType $ \b -> [t| Maybe $(returnType b) |]


mkListReader :: String -> Word16 -> Name -> Name -> Name -> DecsQ
mkListReader name offset parentData childData listCon = do
    let parentType = return $ ConT $ inferTypeName parentData
    let childType = return $ ConT $ inferTypeName childData
    let fnName = mkName name
    ty <- mkPtrReaderType
        (\b -> [t| $parentType $b |])
        (\b -> [t| U.ListOf $b ($childType $b) |])
    val <- mkVal
    return [ SigD fnName ty
           , ValD (VarP fnName) (NormalB val) []
           ]
 where
    mkVal = do
        struct <- newName "struct"
        let structE = return $ VarE struct
        let childConE = return $ ConE childData
        let pattern = return $ ConP parentData [VarP struct]
        [| \ $pattern -> do
                ptrSec <- U.ptrSection $structE
                ptr <- U.index offset ptrSec
                case ptr of
                    Nothing -> return Nothing
                    Just ptr' -> do
                        ptrList <- $(requireCon 'U.PtrList) ptr'
                        list <- $(requireCon listCon) ptrList
                        return $ Just $ fmap $childConE list |]

mkListReaders :: Name -> [(String, Name, Word16, Name)] -> DecsQ
mkListReaders parent readers = do
    concat <$> mapM mkReader readers
  where
    mkReader (name, child, offset, listCon) = mkListReader
        name
        offset
        parent
        child
        listCon

mkWordReaders :: Name -> [(String, Integer, Name, Integer, Name)] -> DecsQ
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
            let defaultValE = return $ LitE $ IntegerL $ defaultVal
            let transformE = return $ VarE transform
            [| do dataSec <- U.dataSection $(return $ VarE struct)
                  word <- U.index $(return $ dataIndex) dataSec
                  let rawVal = word `shiftR` $(return $ bitOffset) `xor` $defaultValE
                  return $ $transformE $ (fromIntegral rawVal :: $(return $ ConT typ)) |]
