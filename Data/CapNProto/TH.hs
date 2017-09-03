{-# LANGUAGE TemplateHaskell #-}
module Data.CapNProto.TH
    ( mkStructWrappers
    , mkListReaders
    )
  where

import Language.Haskell.TH
import Data.Word

import qualified Data.CapNProto.Untyped as U

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

mkListReader :: String -> Word16 -> Name -> Name -> DecQ
mkListReader name offset parentType childType = do
    let fnName = mkName name
    struct <- newName "struct"
    body <- mkBody struct
    return $ FunD fnName [Clause [ConP parentType [VarP struct]]
                                 (NormalB body)
                                 []
                         ]
 where
    childCon = return $ ConE childType
    mkBody struct = do
        [| do ptr <- U.ptrSection $(return $ VarE struct) >>= U.index offset
              case ptr of
                    Nothing -> return Nothing
                    Just ptr' -> do
                        listPtr <- U.requireListStruct ptr'
                        return $ Just $ fmap $(childCon) listPtr |]

mkListReaders :: Name -> [(String, Name, Word16)] -> DecsQ
mkListReaders parent = mapM mkReader where
    mkReader (name, child, offset) = mkListReader name offset parent child
