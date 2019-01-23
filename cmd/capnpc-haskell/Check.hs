{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
module Check (reportIssues) where

import Data.Foldable (for_)
import System.IO     (hPutStrLn, stderr)

import qualified Data.Vector as V

import Capnp.Gen.Capnp.Schema.Pure

-- | Scan the code generator request for certain issues, and warn the user
-- if found.
--
-- We still assume the input is *valid*, so these are issues regarding things
-- that our implementation can't handle.
reportIssues :: CodeGeneratorRequest -> IO ()
reportIssues CodeGeneratorRequest{nodes} =
    let problemFields =
            [ (displayName, name)
            | Node{displayName, union'=Node'struct Node'struct'{fields}} <- V.toList nodes
            , Field
                { name
                , union'=Field'slot Field'slot'{hadExplicitDefault, defaultValue}
                } <- V.toList fields
            , hadExplicitDefault && isPtrValue defaultValue
            ]
    in
    for_ problemFields $ \(displayName, name) ->
        hPutStrLn stderr $ concat
            [ "WARNING: the field ", show name, " in ", show displayName, "\n"
            , " has a custom default value, but haskell-capnp does not\n"
            , " support this for pointer-valued fields. The custom\n"
            , " default will be ignored; please be careful. See:\n"
            , "\n"
            , "https://github.com/zenhack/haskell-capnp/issues/28\n"
            , "\n"
            , "for more information.\n"
            ]

isPtrValue :: Value -> Bool
isPtrValue = \case
    Value'void -> False
    Value'bool _ -> False
    Value'int8 _ -> False
    Value'int16 _ -> False
    Value'int32 _ -> False
    Value'int64 _ -> False
    Value'uint8 _ -> False
    Value'uint16 _ -> False
    Value'uint32 _ -> False
    Value'uint64 _ -> False
    Value'float32 _ -> False
    Value'float64 _ -> False
    Value'enum _ -> False
    _ -> True
