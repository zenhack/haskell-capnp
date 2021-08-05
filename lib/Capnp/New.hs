-- | Module: Capnp.New
-- Description: Re-export commonly used things from elsewhere in the library.
module Capnp.New
    ( module X
    , Parsed

    -- * Working with raw values
    , R.Raw

    -- ** Working with raw lists
    , R.List
    , R.index
    , R.setIndex
    , R.length

    -- * Working with fields
    , F.Field
    , F.FieldKind
    , F.HasField(..)
    , F.HasUnion(..)
    , F.HasVariant(..)

    -- * Working with messages
    , Message.Message
    , Message.Segment
    , Message.Mutability(..)
    , Message.MonadReadMessage(..)
    , Message.newMessage
    , Message.fromByteString
    , Message.toByteString

    -- * Building messages in pure code
    , PureBuilder
    , createPure

    -- * Implementing RPC servers
    , MethodHandler
    , Export(Server)
    , export
    , handleParsed
    , handleRaw
    , methodUnimplemented

    -- * Re-exported from "Data.Default", for convienence.
    , def
    ) where

-- TODO: be more intentional about the ordering of the stuff we're
-- currently exposing as X, so the haddocks are clearer.

import           Capnp.Convert         as X
import qualified Capnp.Fields          as F
import           Capnp.IO              as X
import qualified Capnp.Message         as Message
import           Capnp.New.Accessors   as X
import           Capnp.New.Classes     as X hiding (Parsed)
import           Capnp.New.Constraints as X
import           Capnp.New.Rpc.Server
import qualified Capnp.Repr            as R
import           Capnp.Repr.Methods    as X
import           Capnp.Repr.Parsed     (Parsed)
import           Capnp.TraversalLimit  as X
import           Data.Default          (def)
import           Internal.BuildPure    (PureBuilder, createPure)
