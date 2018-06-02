{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module Data.Capnp.ById.Xa93fc509624c72d9.Pure where

-- Code generated by capnpc-haskell. DO NOT EDIT.
-- Generated from schema file: schema/capnp/schema.capnp

import Data.Int
import Data.Word

import Data.Capnp.Untyped.Pure (List)
import Data.Capnp.BuiltinTypes.Pure (Data, Text)
import Control.Monad.Catch (MonadThrow)
import Data.Capnp.TraversalLimit (MonadLimit)

import qualified Data.Capnp.Untyped.Pure
import qualified Data.Capnp.Untyped
import qualified Codec.Capnp

import Data.ByteString as BS

import qualified Data.Capnp.ById.Xa93fc509624c72d9
import qualified Data.Capnp.ById.Xbdf87d7bb8304e81.Pure
import qualified Data.Capnp.ById.Xbdf87d7bb8304e81

data Type'anyPointer'unconstrained
    = Type'anyPointer'unconstrained'anyKind
    | Type'anyPointer'unconstrained'struct
    | Type'anyPointer'unconstrained'list
    | Type'anyPointer'unconstrained'capability
    | Type'anyPointer'unconstrained'unknown' (Word16)
    deriving(Show, Read, Eq)

instance (MonadThrow m, MonadLimit m) => Codec.Capnp.Decerialize m (Data.Capnp.ById.Xa93fc509624c72d9.Type'anyPointer'unconstrained m BS.ByteString) Type'anyPointer'unconstrained where
    decerialize raw = case raw of

        Data.Capnp.ById.Xa93fc509624c72d9.Type'anyPointer'unconstrained'anyKind -> pure Type'anyPointer'unconstrained'anyKind
        Data.Capnp.ById.Xa93fc509624c72d9.Type'anyPointer'unconstrained'struct -> pure Type'anyPointer'unconstrained'struct
        Data.Capnp.ById.Xa93fc509624c72d9.Type'anyPointer'unconstrained'list -> pure Type'anyPointer'unconstrained'list
        Data.Capnp.ById.Xa93fc509624c72d9.Type'anyPointer'unconstrained'capability -> pure Type'anyPointer'unconstrained'capability
        Data.Capnp.ById.Xa93fc509624c72d9.Type'anyPointer'unconstrained'unknown' val -> Type'anyPointer'unconstrained'unknown' <$> Codec.Capnp.decerialize val

data Brand
    = Brand
        { scopes :: List (Brand'Scope)
        }
    deriving(Show, Read, Eq)

instance (MonadThrow m, MonadLimit m) => Codec.Capnp.Decerialize m (Data.Capnp.ById.Xa93fc509624c72d9.Brand m BS.ByteString) Brand where
    decerialize raw = Brand
            <$> (Data.Capnp.ById.Xa93fc509624c72d9.get_Brand'scopes raw >>= Codec.Capnp.decerialize)

data Method
    = Method
        { name :: Text
        , codeOrder :: Word16
        , paramStructType :: Word64
        , resultStructType :: Word64
        , annotations :: List (Annotation)
        , paramBrand :: Brand
        , resultBrand :: Brand
        , implicitParameters :: List (Node'Parameter)
        }
    deriving(Show, Read, Eq)

instance (MonadThrow m, MonadLimit m) => Codec.Capnp.Decerialize m (Data.Capnp.ById.Xa93fc509624c72d9.Method m BS.ByteString) Method where
    decerialize raw = Method
            <$> (Data.Capnp.ById.Xa93fc509624c72d9.get_Method'name raw >>= Codec.Capnp.decerialize)
            <*> (Data.Capnp.ById.Xa93fc509624c72d9.get_Method'codeOrder raw >>= Codec.Capnp.decerialize)
            <*> (Data.Capnp.ById.Xa93fc509624c72d9.get_Method'paramStructType raw >>= Codec.Capnp.decerialize)
            <*> (Data.Capnp.ById.Xa93fc509624c72d9.get_Method'resultStructType raw >>= Codec.Capnp.decerialize)
            <*> (Data.Capnp.ById.Xa93fc509624c72d9.get_Method'annotations raw >>= Codec.Capnp.decerialize)
            <*> (Data.Capnp.ById.Xa93fc509624c72d9.get_Method'paramBrand raw >>= Codec.Capnp.decerialize)
            <*> (Data.Capnp.ById.Xa93fc509624c72d9.get_Method'resultBrand raw >>= Codec.Capnp.decerialize)
            <*> (Data.Capnp.ById.Xa93fc509624c72d9.get_Method'implicitParameters raw >>= Codec.Capnp.decerialize)

data Enumerant
    = Enumerant
        { name :: Text
        , codeOrder :: Word16
        , annotations :: List (Annotation)
        }
    deriving(Show, Read, Eq)

instance (MonadThrow m, MonadLimit m) => Codec.Capnp.Decerialize m (Data.Capnp.ById.Xa93fc509624c72d9.Enumerant m BS.ByteString) Enumerant where
    decerialize raw = Enumerant
            <$> (Data.Capnp.ById.Xa93fc509624c72d9.get_Enumerant'name raw >>= Codec.Capnp.decerialize)
            <*> (Data.Capnp.ById.Xa93fc509624c72d9.get_Enumerant'codeOrder raw >>= Codec.Capnp.decerialize)
            <*> (Data.Capnp.ById.Xa93fc509624c72d9.get_Enumerant'annotations raw >>= Codec.Capnp.decerialize)

data Field
    = Field'
        { name :: Text
        , codeOrder :: Word16
        , annotations :: List (Annotation)
        , discriminantValue :: Word16
        , ordinal :: Field'ordinal
        , union' :: Field'
        }
    deriving(Show, Read, Eq)

instance (MonadThrow m, MonadLimit m) => Codec.Capnp.Decerialize m (Data.Capnp.ById.Xa93fc509624c72d9.Field m BS.ByteString) Field where
    decerialize raw = Field'
            <$> (Data.Capnp.ById.Xa93fc509624c72d9.get_Field''name raw >>= Codec.Capnp.decerialize)
            <*> (Data.Capnp.ById.Xa93fc509624c72d9.get_Field''codeOrder raw >>= Codec.Capnp.decerialize)
            <*> (Data.Capnp.ById.Xa93fc509624c72d9.get_Field''annotations raw >>= Codec.Capnp.decerialize)
            <*> (Data.Capnp.ById.Xa93fc509624c72d9.get_Field''discriminantValue raw >>= Codec.Capnp.decerialize)
            <*> (Data.Capnp.ById.Xa93fc509624c72d9.get_Field''ordinal raw >>= Codec.Capnp.decerialize)
            <*> (Data.Capnp.ById.Xa93fc509624c72d9.get_Field''union' raw >>= Codec.Capnp.decerialize)

data Field'
    = Field'slot
        { offset :: Word32
        , type_ :: Type
        , defaultValue :: Value
        , hadExplicitDefault :: Bool
        }
    | Field'group
        { typeId :: Word64
        }
    | Field'unknown' (Word16)
    deriving(Show, Read, Eq)

instance (MonadThrow m, MonadLimit m) => Codec.Capnp.Decerialize m (Data.Capnp.ById.Xa93fc509624c72d9.Field' m BS.ByteString) Field' where
    decerialize raw = case raw of

        Data.Capnp.ById.Xa93fc509624c72d9.Field'slot raw -> Field'slot
            <$> (Data.Capnp.ById.Xa93fc509624c72d9.get_Field'slot'offset raw >>= Codec.Capnp.decerialize)
            <*> (Data.Capnp.ById.Xa93fc509624c72d9.get_Field'slot'type_ raw >>= Codec.Capnp.decerialize)
            <*> (Data.Capnp.ById.Xa93fc509624c72d9.get_Field'slot'defaultValue raw >>= Codec.Capnp.decerialize)
            <*> (Data.Capnp.ById.Xa93fc509624c72d9.get_Field'slot'hadExplicitDefault raw >>= Codec.Capnp.decerialize)
        Data.Capnp.ById.Xa93fc509624c72d9.Field'group raw -> Field'group
            <$> (Data.Capnp.ById.Xa93fc509624c72d9.get_Field'group'typeId raw >>= Codec.Capnp.decerialize)
        Data.Capnp.ById.Xa93fc509624c72d9.Field'unknown' val -> Field'unknown' <$> Codec.Capnp.decerialize val

data Superclass
    = Superclass
        { id :: Word64
        , brand :: Brand
        }
    deriving(Show, Read, Eq)

instance (MonadThrow m, MonadLimit m) => Codec.Capnp.Decerialize m (Data.Capnp.ById.Xa93fc509624c72d9.Superclass m BS.ByteString) Superclass where
    decerialize raw = Superclass
            <$> (Data.Capnp.ById.Xa93fc509624c72d9.get_Superclass'id raw >>= Codec.Capnp.decerialize)
            <*> (Data.Capnp.ById.Xa93fc509624c72d9.get_Superclass'brand raw >>= Codec.Capnp.decerialize)

data Brand'Scope
    = Brand'Scope'
        { scopeId :: Word64
        , union' :: Brand'Scope'
        }
    deriving(Show, Read, Eq)

instance (MonadThrow m, MonadLimit m) => Codec.Capnp.Decerialize m (Data.Capnp.ById.Xa93fc509624c72d9.Brand'Scope m BS.ByteString) Brand'Scope where
    decerialize raw = Brand'Scope'
            <$> (Data.Capnp.ById.Xa93fc509624c72d9.get_Brand'Scope''scopeId raw >>= Codec.Capnp.decerialize)
            <*> (Data.Capnp.ById.Xa93fc509624c72d9.get_Brand'Scope''union' raw >>= Codec.Capnp.decerialize)

data Brand'Scope'
    = Brand'Scope'bind (List (Brand'Binding))
    | Brand'Scope'inherit
    | Brand'Scope'unknown' (Word16)
    deriving(Show, Read, Eq)

instance (MonadThrow m, MonadLimit m) => Codec.Capnp.Decerialize m (Data.Capnp.ById.Xa93fc509624c72d9.Brand'Scope' m BS.ByteString) Brand'Scope' where
    decerialize raw = case raw of

        Data.Capnp.ById.Xa93fc509624c72d9.Brand'Scope'bind val -> Brand'Scope'bind <$> Codec.Capnp.decerialize val
        Data.Capnp.ById.Xa93fc509624c72d9.Brand'Scope'inherit -> pure Brand'Scope'inherit
        Data.Capnp.ById.Xa93fc509624c72d9.Brand'Scope'unknown' val -> Brand'Scope'unknown' <$> Codec.Capnp.decerialize val

data CodeGeneratorRequest'RequestedFile'Import
    = CodeGeneratorRequest'RequestedFile'Import
        { id :: Word64
        , name :: Text
        }
    deriving(Show, Read, Eq)

instance (MonadThrow m, MonadLimit m) => Codec.Capnp.Decerialize m (Data.Capnp.ById.Xa93fc509624c72d9.CodeGeneratorRequest'RequestedFile'Import m BS.ByteString) CodeGeneratorRequest'RequestedFile'Import where
    decerialize raw = CodeGeneratorRequest'RequestedFile'Import
            <$> (Data.Capnp.ById.Xa93fc509624c72d9.get_CodeGeneratorRequest'RequestedFile'Import'id raw >>= Codec.Capnp.decerialize)
            <*> (Data.Capnp.ById.Xa93fc509624c72d9.get_CodeGeneratorRequest'RequestedFile'Import'name raw >>= Codec.Capnp.decerialize)

data Node'Parameter
    = Node'Parameter
        { name :: Text
        }
    deriving(Show, Read, Eq)

instance (MonadThrow m, MonadLimit m) => Codec.Capnp.Decerialize m (Data.Capnp.ById.Xa93fc509624c72d9.Node'Parameter m BS.ByteString) Node'Parameter where
    decerialize raw = Node'Parameter
            <$> (Data.Capnp.ById.Xa93fc509624c72d9.get_Node'Parameter'name raw >>= Codec.Capnp.decerialize)

data Field'ordinal
    = Field'ordinal'implicit
    | Field'ordinal'explicit (Word16)
    | Field'ordinal'unknown' (Word16)
    deriving(Show, Read, Eq)

instance (MonadThrow m, MonadLimit m) => Codec.Capnp.Decerialize m (Data.Capnp.ById.Xa93fc509624c72d9.Field'ordinal m BS.ByteString) Field'ordinal where
    decerialize raw = case raw of

        Data.Capnp.ById.Xa93fc509624c72d9.Field'ordinal'implicit -> pure Field'ordinal'implicit
        Data.Capnp.ById.Xa93fc509624c72d9.Field'ordinal'explicit val -> Field'ordinal'explicit <$> Codec.Capnp.decerialize val
        Data.Capnp.ById.Xa93fc509624c72d9.Field'ordinal'unknown' val -> Field'ordinal'unknown' <$> Codec.Capnp.decerialize val

data CodeGeneratorRequest
    = CodeGeneratorRequest
        { nodes :: List (Node)
        , requestedFiles :: List (CodeGeneratorRequest'RequestedFile)
        , capnpVersion :: CapnpVersion
        }
    deriving(Show, Read, Eq)

instance (MonadThrow m, MonadLimit m) => Codec.Capnp.Decerialize m (Data.Capnp.ById.Xa93fc509624c72d9.CodeGeneratorRequest m BS.ByteString) CodeGeneratorRequest where
    decerialize raw = CodeGeneratorRequest
            <$> (Data.Capnp.ById.Xa93fc509624c72d9.get_CodeGeneratorRequest'nodes raw >>= Codec.Capnp.decerialize)
            <*> (Data.Capnp.ById.Xa93fc509624c72d9.get_CodeGeneratorRequest'requestedFiles raw >>= Codec.Capnp.decerialize)
            <*> (Data.Capnp.ById.Xa93fc509624c72d9.get_CodeGeneratorRequest'capnpVersion raw >>= Codec.Capnp.decerialize)

data Type'anyPointer
    = Type'anyPointer'unconstrained
        { union' :: Type'anyPointer'unconstrained
        }
    | Type'anyPointer'parameter
        { scopeId :: Word64
        , parameterIndex :: Word16
        }
    | Type'anyPointer'implicitMethodParameter
        { parameterIndex :: Word16
        }
    | Type'anyPointer'unknown' (Word16)
    deriving(Show, Read, Eq)

instance (MonadThrow m, MonadLimit m) => Codec.Capnp.Decerialize m (Data.Capnp.ById.Xa93fc509624c72d9.Type'anyPointer m BS.ByteString) Type'anyPointer where
    decerialize raw = case raw of

        Data.Capnp.ById.Xa93fc509624c72d9.Type'anyPointer'unconstrained raw -> Type'anyPointer'unconstrained
            <$> (Data.Capnp.ById.Xa93fc509624c72d9.get_Type'anyPointer'unconstrained'union' raw >>= Codec.Capnp.decerialize)
        Data.Capnp.ById.Xa93fc509624c72d9.Type'anyPointer'parameter raw -> Type'anyPointer'parameter
            <$> (Data.Capnp.ById.Xa93fc509624c72d9.get_Type'anyPointer'parameter'scopeId raw >>= Codec.Capnp.decerialize)
            <*> (Data.Capnp.ById.Xa93fc509624c72d9.get_Type'anyPointer'parameter'parameterIndex raw >>= Codec.Capnp.decerialize)
        Data.Capnp.ById.Xa93fc509624c72d9.Type'anyPointer'implicitMethodParameter raw -> Type'anyPointer'implicitMethodParameter
            <$> (Data.Capnp.ById.Xa93fc509624c72d9.get_Type'anyPointer'implicitMethodParameter'parameterIndex raw >>= Codec.Capnp.decerialize)
        Data.Capnp.ById.Xa93fc509624c72d9.Type'anyPointer'unknown' val -> Type'anyPointer'unknown' <$> Codec.Capnp.decerialize val

data Brand'Binding
    = Brand'Binding'unbound
    | Brand'Binding'type_ (Type)
    | Brand'Binding'unknown' (Word16)
    deriving(Show, Read, Eq)

instance (MonadThrow m, MonadLimit m) => Codec.Capnp.Decerialize m (Data.Capnp.ById.Xa93fc509624c72d9.Brand'Binding m BS.ByteString) Brand'Binding where
    decerialize raw = case raw of

        Data.Capnp.ById.Xa93fc509624c72d9.Brand'Binding'unbound -> pure Brand'Binding'unbound
        Data.Capnp.ById.Xa93fc509624c72d9.Brand'Binding'type_ val -> Brand'Binding'type_ <$> Codec.Capnp.decerialize val
        Data.Capnp.ById.Xa93fc509624c72d9.Brand'Binding'unknown' val -> Brand'Binding'unknown' <$> Codec.Capnp.decerialize val

data Value
    = Value'void
    | Value'bool (Bool)
    | Value'int8 (Int8)
    | Value'int16 (Int16)
    | Value'int32 (Int32)
    | Value'int64 (Int64)
    | Value'uint8 (Word8)
    | Value'uint16 (Word16)
    | Value'uint32 (Word32)
    | Value'uint64 (Word64)
    | Value'float32 (Float)
    | Value'float64 (Double)
    | Value'text (Text)
    | Value'data_ (Data)
    | Value'list (Maybe (Data.Capnp.Untyped.Pure.PtrType))
    | Value'enum (Word16)
    | Value'struct (Maybe (Data.Capnp.Untyped.Pure.PtrType))
    | Value'interface
    | Value'anyPointer (Maybe (Data.Capnp.Untyped.Pure.PtrType))
    | Value'unknown' (Word16)
    deriving(Show, Read, Eq)

instance (MonadThrow m, MonadLimit m) => Codec.Capnp.Decerialize m (Data.Capnp.ById.Xa93fc509624c72d9.Value m BS.ByteString) Value where
    decerialize raw = case raw of

        Data.Capnp.ById.Xa93fc509624c72d9.Value'void -> pure Value'void
        Data.Capnp.ById.Xa93fc509624c72d9.Value'bool val -> Value'bool <$> Codec.Capnp.decerialize val
        Data.Capnp.ById.Xa93fc509624c72d9.Value'int8 val -> Value'int8 <$> Codec.Capnp.decerialize val
        Data.Capnp.ById.Xa93fc509624c72d9.Value'int16 val -> Value'int16 <$> Codec.Capnp.decerialize val
        Data.Capnp.ById.Xa93fc509624c72d9.Value'int32 val -> Value'int32 <$> Codec.Capnp.decerialize val
        Data.Capnp.ById.Xa93fc509624c72d9.Value'int64 val -> Value'int64 <$> Codec.Capnp.decerialize val
        Data.Capnp.ById.Xa93fc509624c72d9.Value'uint8 val -> Value'uint8 <$> Codec.Capnp.decerialize val
        Data.Capnp.ById.Xa93fc509624c72d9.Value'uint16 val -> Value'uint16 <$> Codec.Capnp.decerialize val
        Data.Capnp.ById.Xa93fc509624c72d9.Value'uint32 val -> Value'uint32 <$> Codec.Capnp.decerialize val
        Data.Capnp.ById.Xa93fc509624c72d9.Value'uint64 val -> Value'uint64 <$> Codec.Capnp.decerialize val
        Data.Capnp.ById.Xa93fc509624c72d9.Value'float32 val -> Value'float32 <$> Codec.Capnp.decerialize val
        Data.Capnp.ById.Xa93fc509624c72d9.Value'float64 val -> Value'float64 <$> Codec.Capnp.decerialize val
        Data.Capnp.ById.Xa93fc509624c72d9.Value'text val -> Value'text <$> Codec.Capnp.decerialize val
        Data.Capnp.ById.Xa93fc509624c72d9.Value'data_ val -> Value'data_ <$> Codec.Capnp.decerialize val
        Data.Capnp.ById.Xa93fc509624c72d9.Value'list val -> Value'list <$> Codec.Capnp.decerialize val
        Data.Capnp.ById.Xa93fc509624c72d9.Value'enum val -> Value'enum <$> Codec.Capnp.decerialize val
        Data.Capnp.ById.Xa93fc509624c72d9.Value'struct val -> Value'struct <$> Codec.Capnp.decerialize val
        Data.Capnp.ById.Xa93fc509624c72d9.Value'interface -> pure Value'interface
        Data.Capnp.ById.Xa93fc509624c72d9.Value'anyPointer val -> Value'anyPointer <$> Codec.Capnp.decerialize val
        Data.Capnp.ById.Xa93fc509624c72d9.Value'unknown' val -> Value'unknown' <$> Codec.Capnp.decerialize val

data CodeGeneratorRequest'RequestedFile
    = CodeGeneratorRequest'RequestedFile
        { id :: Word64
        , filename :: Text
        , imports :: List (CodeGeneratorRequest'RequestedFile'Import)
        }
    deriving(Show, Read, Eq)

instance (MonadThrow m, MonadLimit m) => Codec.Capnp.Decerialize m (Data.Capnp.ById.Xa93fc509624c72d9.CodeGeneratorRequest'RequestedFile m BS.ByteString) CodeGeneratorRequest'RequestedFile where
    decerialize raw = CodeGeneratorRequest'RequestedFile
            <$> (Data.Capnp.ById.Xa93fc509624c72d9.get_CodeGeneratorRequest'RequestedFile'id raw >>= Codec.Capnp.decerialize)
            <*> (Data.Capnp.ById.Xa93fc509624c72d9.get_CodeGeneratorRequest'RequestedFile'filename raw >>= Codec.Capnp.decerialize)
            <*> (Data.Capnp.ById.Xa93fc509624c72d9.get_CodeGeneratorRequest'RequestedFile'imports raw >>= Codec.Capnp.decerialize)

data Type
    = Type'void
    | Type'bool
    | Type'int8
    | Type'int16
    | Type'int32
    | Type'int64
    | Type'uint8
    | Type'uint16
    | Type'uint32
    | Type'uint64
    | Type'float32
    | Type'float64
    | Type'text
    | Type'data_
    | Type'list
        { elementType :: Type
        }
    | Type'enum
        { typeId :: Word64
        , brand :: Brand
        }
    | Type'struct
        { typeId :: Word64
        , brand :: Brand
        }
    | Type'interface
        { typeId :: Word64
        , brand :: Brand
        }
    | Type'anyPointer
        { union' :: Type'anyPointer
        }
    | Type'unknown' (Word16)
    deriving(Show, Read, Eq)

instance (MonadThrow m, MonadLimit m) => Codec.Capnp.Decerialize m (Data.Capnp.ById.Xa93fc509624c72d9.Type m BS.ByteString) Type where
    decerialize raw = case raw of

        Data.Capnp.ById.Xa93fc509624c72d9.Type'void -> pure Type'void
        Data.Capnp.ById.Xa93fc509624c72d9.Type'bool -> pure Type'bool
        Data.Capnp.ById.Xa93fc509624c72d9.Type'int8 -> pure Type'int8
        Data.Capnp.ById.Xa93fc509624c72d9.Type'int16 -> pure Type'int16
        Data.Capnp.ById.Xa93fc509624c72d9.Type'int32 -> pure Type'int32
        Data.Capnp.ById.Xa93fc509624c72d9.Type'int64 -> pure Type'int64
        Data.Capnp.ById.Xa93fc509624c72d9.Type'uint8 -> pure Type'uint8
        Data.Capnp.ById.Xa93fc509624c72d9.Type'uint16 -> pure Type'uint16
        Data.Capnp.ById.Xa93fc509624c72d9.Type'uint32 -> pure Type'uint32
        Data.Capnp.ById.Xa93fc509624c72d9.Type'uint64 -> pure Type'uint64
        Data.Capnp.ById.Xa93fc509624c72d9.Type'float32 -> pure Type'float32
        Data.Capnp.ById.Xa93fc509624c72d9.Type'float64 -> pure Type'float64
        Data.Capnp.ById.Xa93fc509624c72d9.Type'text -> pure Type'text
        Data.Capnp.ById.Xa93fc509624c72d9.Type'data_ -> pure Type'data_
        Data.Capnp.ById.Xa93fc509624c72d9.Type'list raw -> Type'list
            <$> (Data.Capnp.ById.Xa93fc509624c72d9.get_Type'list'elementType raw >>= Codec.Capnp.decerialize)
        Data.Capnp.ById.Xa93fc509624c72d9.Type'enum raw -> Type'enum
            <$> (Data.Capnp.ById.Xa93fc509624c72d9.get_Type'enum'typeId raw >>= Codec.Capnp.decerialize)
            <*> (Data.Capnp.ById.Xa93fc509624c72d9.get_Type'enum'brand raw >>= Codec.Capnp.decerialize)
        Data.Capnp.ById.Xa93fc509624c72d9.Type'struct raw -> Type'struct
            <$> (Data.Capnp.ById.Xa93fc509624c72d9.get_Type'struct'typeId raw >>= Codec.Capnp.decerialize)
            <*> (Data.Capnp.ById.Xa93fc509624c72d9.get_Type'struct'brand raw >>= Codec.Capnp.decerialize)
        Data.Capnp.ById.Xa93fc509624c72d9.Type'interface raw -> Type'interface
            <$> (Data.Capnp.ById.Xa93fc509624c72d9.get_Type'interface'typeId raw >>= Codec.Capnp.decerialize)
            <*> (Data.Capnp.ById.Xa93fc509624c72d9.get_Type'interface'brand raw >>= Codec.Capnp.decerialize)
        Data.Capnp.ById.Xa93fc509624c72d9.Type'anyPointer raw -> Type'anyPointer
            <$> (Data.Capnp.ById.Xa93fc509624c72d9.get_Type'anyPointer'union' raw >>= Codec.Capnp.decerialize)
        Data.Capnp.ById.Xa93fc509624c72d9.Type'unknown' val -> Type'unknown' <$> Codec.Capnp.decerialize val

data ElementSize
    = ElementSize'empty
    | ElementSize'bit
    | ElementSize'byte
    | ElementSize'twoBytes
    | ElementSize'fourBytes
    | ElementSize'eightBytes
    | ElementSize'pointer
    | ElementSize'inlineComposite
    | ElementSize'unknown' (Word16)
    deriving(Show, Read, Eq)

instance (MonadThrow m, MonadLimit m) => Codec.Capnp.Decerialize m (Data.Capnp.ById.Xa93fc509624c72d9.ElementSize m BS.ByteString) ElementSize where
    decerialize raw = case raw of

        Data.Capnp.ById.Xa93fc509624c72d9.ElementSize'empty -> pure ElementSize'empty
        Data.Capnp.ById.Xa93fc509624c72d9.ElementSize'bit -> pure ElementSize'bit
        Data.Capnp.ById.Xa93fc509624c72d9.ElementSize'byte -> pure ElementSize'byte
        Data.Capnp.ById.Xa93fc509624c72d9.ElementSize'twoBytes -> pure ElementSize'twoBytes
        Data.Capnp.ById.Xa93fc509624c72d9.ElementSize'fourBytes -> pure ElementSize'fourBytes
        Data.Capnp.ById.Xa93fc509624c72d9.ElementSize'eightBytes -> pure ElementSize'eightBytes
        Data.Capnp.ById.Xa93fc509624c72d9.ElementSize'pointer -> pure ElementSize'pointer
        Data.Capnp.ById.Xa93fc509624c72d9.ElementSize'inlineComposite -> pure ElementSize'inlineComposite
        Data.Capnp.ById.Xa93fc509624c72d9.ElementSize'unknown' val -> ElementSize'unknown' <$> Codec.Capnp.decerialize val

data CapnpVersion
    = CapnpVersion
        { major :: Word16
        , minor :: Word8
        , micro :: Word8
        }
    deriving(Show, Read, Eq)

instance (MonadThrow m, MonadLimit m) => Codec.Capnp.Decerialize m (Data.Capnp.ById.Xa93fc509624c72d9.CapnpVersion m BS.ByteString) CapnpVersion where
    decerialize raw = CapnpVersion
            <$> (Data.Capnp.ById.Xa93fc509624c72d9.get_CapnpVersion'major raw >>= Codec.Capnp.decerialize)
            <*> (Data.Capnp.ById.Xa93fc509624c72d9.get_CapnpVersion'minor raw >>= Codec.Capnp.decerialize)
            <*> (Data.Capnp.ById.Xa93fc509624c72d9.get_CapnpVersion'micro raw >>= Codec.Capnp.decerialize)

data Node'NestedNode
    = Node'NestedNode
        { name :: Text
        , id :: Word64
        }
    deriving(Show, Read, Eq)

instance (MonadThrow m, MonadLimit m) => Codec.Capnp.Decerialize m (Data.Capnp.ById.Xa93fc509624c72d9.Node'NestedNode m BS.ByteString) Node'NestedNode where
    decerialize raw = Node'NestedNode
            <$> (Data.Capnp.ById.Xa93fc509624c72d9.get_Node'NestedNode'name raw >>= Codec.Capnp.decerialize)
            <*> (Data.Capnp.ById.Xa93fc509624c72d9.get_Node'NestedNode'id raw >>= Codec.Capnp.decerialize)

data Node
    = Node'
        { id :: Word64
        , displayName :: Text
        , displayNamePrefixLength :: Word32
        , scopeId :: Word64
        , nestedNodes :: List (Node'NestedNode)
        , annotations :: List (Annotation)
        , parameters :: List (Node'Parameter)
        , isGeneric :: Bool
        , union' :: Node'
        }
    deriving(Show, Read, Eq)

instance (MonadThrow m, MonadLimit m) => Codec.Capnp.Decerialize m (Data.Capnp.ById.Xa93fc509624c72d9.Node m BS.ByteString) Node where
    decerialize raw = Node'
            <$> (Data.Capnp.ById.Xa93fc509624c72d9.get_Node''id raw >>= Codec.Capnp.decerialize)
            <*> (Data.Capnp.ById.Xa93fc509624c72d9.get_Node''displayName raw >>= Codec.Capnp.decerialize)
            <*> (Data.Capnp.ById.Xa93fc509624c72d9.get_Node''displayNamePrefixLength raw >>= Codec.Capnp.decerialize)
            <*> (Data.Capnp.ById.Xa93fc509624c72d9.get_Node''scopeId raw >>= Codec.Capnp.decerialize)
            <*> (Data.Capnp.ById.Xa93fc509624c72d9.get_Node''nestedNodes raw >>= Codec.Capnp.decerialize)
            <*> (Data.Capnp.ById.Xa93fc509624c72d9.get_Node''annotations raw >>= Codec.Capnp.decerialize)
            <*> (Data.Capnp.ById.Xa93fc509624c72d9.get_Node''parameters raw >>= Codec.Capnp.decerialize)
            <*> (Data.Capnp.ById.Xa93fc509624c72d9.get_Node''isGeneric raw >>= Codec.Capnp.decerialize)
            <*> (Data.Capnp.ById.Xa93fc509624c72d9.get_Node''union' raw >>= Codec.Capnp.decerialize)

data Node'
    = Node'file
    | Node'struct
        { dataWordCount :: Word16
        , pointerCount :: Word16
        , preferredListEncoding :: ElementSize
        , isGroup :: Bool
        , discriminantCount :: Word16
        , discriminantOffset :: Word32
        , fields :: List (Field)
        }
    | Node'enum
        { enumerants :: List (Enumerant)
        }
    | Node'interface
        { methods :: List (Method)
        , superclasses :: List (Superclass)
        }
    | Node'const
        { type_ :: Type
        , value :: Value
        }
    | Node'annotation
        { type_ :: Type
        , targetsFile :: Bool
        , targetsConst :: Bool
        , targetsEnum :: Bool
        , targetsEnumerant :: Bool
        , targetsStruct :: Bool
        , targetsField :: Bool
        , targetsUnion :: Bool
        , targetsGroup :: Bool
        , targetsInterface :: Bool
        , targetsMethod :: Bool
        , targetsParam :: Bool
        , targetsAnnotation :: Bool
        }
    | Node'unknown' (Word16)
    deriving(Show, Read, Eq)

instance (MonadThrow m, MonadLimit m) => Codec.Capnp.Decerialize m (Data.Capnp.ById.Xa93fc509624c72d9.Node' m BS.ByteString) Node' where
    decerialize raw = case raw of

        Data.Capnp.ById.Xa93fc509624c72d9.Node'file -> pure Node'file
        Data.Capnp.ById.Xa93fc509624c72d9.Node'struct raw -> Node'struct
            <$> (Data.Capnp.ById.Xa93fc509624c72d9.get_Node'struct'dataWordCount raw >>= Codec.Capnp.decerialize)
            <*> (Data.Capnp.ById.Xa93fc509624c72d9.get_Node'struct'pointerCount raw >>= Codec.Capnp.decerialize)
            <*> (Data.Capnp.ById.Xa93fc509624c72d9.get_Node'struct'preferredListEncoding raw >>= Codec.Capnp.decerialize)
            <*> (Data.Capnp.ById.Xa93fc509624c72d9.get_Node'struct'isGroup raw >>= Codec.Capnp.decerialize)
            <*> (Data.Capnp.ById.Xa93fc509624c72d9.get_Node'struct'discriminantCount raw >>= Codec.Capnp.decerialize)
            <*> (Data.Capnp.ById.Xa93fc509624c72d9.get_Node'struct'discriminantOffset raw >>= Codec.Capnp.decerialize)
            <*> (Data.Capnp.ById.Xa93fc509624c72d9.get_Node'struct'fields raw >>= Codec.Capnp.decerialize)
        Data.Capnp.ById.Xa93fc509624c72d9.Node'enum raw -> Node'enum
            <$> (Data.Capnp.ById.Xa93fc509624c72d9.get_Node'enum'enumerants raw >>= Codec.Capnp.decerialize)
        Data.Capnp.ById.Xa93fc509624c72d9.Node'interface raw -> Node'interface
            <$> (Data.Capnp.ById.Xa93fc509624c72d9.get_Node'interface'methods raw >>= Codec.Capnp.decerialize)
            <*> (Data.Capnp.ById.Xa93fc509624c72d9.get_Node'interface'superclasses raw >>= Codec.Capnp.decerialize)
        Data.Capnp.ById.Xa93fc509624c72d9.Node'const raw -> Node'const
            <$> (Data.Capnp.ById.Xa93fc509624c72d9.get_Node'const'type_ raw >>= Codec.Capnp.decerialize)
            <*> (Data.Capnp.ById.Xa93fc509624c72d9.get_Node'const'value raw >>= Codec.Capnp.decerialize)
        Data.Capnp.ById.Xa93fc509624c72d9.Node'annotation raw -> Node'annotation
            <$> (Data.Capnp.ById.Xa93fc509624c72d9.get_Node'annotation'type_ raw >>= Codec.Capnp.decerialize)
            <*> (Data.Capnp.ById.Xa93fc509624c72d9.get_Node'annotation'targetsFile raw >>= Codec.Capnp.decerialize)
            <*> (Data.Capnp.ById.Xa93fc509624c72d9.get_Node'annotation'targetsConst raw >>= Codec.Capnp.decerialize)
            <*> (Data.Capnp.ById.Xa93fc509624c72d9.get_Node'annotation'targetsEnum raw >>= Codec.Capnp.decerialize)
            <*> (Data.Capnp.ById.Xa93fc509624c72d9.get_Node'annotation'targetsEnumerant raw >>= Codec.Capnp.decerialize)
            <*> (Data.Capnp.ById.Xa93fc509624c72d9.get_Node'annotation'targetsStruct raw >>= Codec.Capnp.decerialize)
            <*> (Data.Capnp.ById.Xa93fc509624c72d9.get_Node'annotation'targetsField raw >>= Codec.Capnp.decerialize)
            <*> (Data.Capnp.ById.Xa93fc509624c72d9.get_Node'annotation'targetsUnion raw >>= Codec.Capnp.decerialize)
            <*> (Data.Capnp.ById.Xa93fc509624c72d9.get_Node'annotation'targetsGroup raw >>= Codec.Capnp.decerialize)
            <*> (Data.Capnp.ById.Xa93fc509624c72d9.get_Node'annotation'targetsInterface raw >>= Codec.Capnp.decerialize)
            <*> (Data.Capnp.ById.Xa93fc509624c72d9.get_Node'annotation'targetsMethod raw >>= Codec.Capnp.decerialize)
            <*> (Data.Capnp.ById.Xa93fc509624c72d9.get_Node'annotation'targetsParam raw >>= Codec.Capnp.decerialize)
            <*> (Data.Capnp.ById.Xa93fc509624c72d9.get_Node'annotation'targetsAnnotation raw >>= Codec.Capnp.decerialize)
        Data.Capnp.ById.Xa93fc509624c72d9.Node'unknown' val -> Node'unknown' <$> Codec.Capnp.decerialize val

data Annotation
    = Annotation
        { id :: Word64
        , value :: Value
        , brand :: Brand
        }
    deriving(Show, Read, Eq)

instance (MonadThrow m, MonadLimit m) => Codec.Capnp.Decerialize m (Data.Capnp.ById.Xa93fc509624c72d9.Annotation m BS.ByteString) Annotation where
    decerialize raw = Annotation
            <$> (Data.Capnp.ById.Xa93fc509624c72d9.get_Annotation'id raw >>= Codec.Capnp.decerialize)
            <*> (Data.Capnp.ById.Xa93fc509624c72d9.get_Annotation'value raw >>= Codec.Capnp.decerialize)
            <*> (Data.Capnp.ById.Xa93fc509624c72d9.get_Annotation'brand raw >>= Codec.Capnp.decerialize)

