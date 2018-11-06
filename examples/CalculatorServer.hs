{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
module Main where

import Prelude hiding (subtract)

import Data.Int

import Control.Monad      (when)
import Data.Function      ((&))
import Network.Simple.TCP (serve)

import qualified Data.Vector as V

import Capnp     (def)
import Capnp.Rpc
    ( RpcT
    , VatConfig(..)
    , runVat
    , socketTransport
    , throwFailed
    , toClient
    , vatConfig
    )

import Capnp.Gen.Calculator.Pure

newtype LitValue = LitValue Double

instance Value'server_ LitValue where
    value'read _ (LitValue val) = pure Value'read'results { value = val }

newtype OpFunc = OpFunc (Double -> Double -> Double)

instance Function'server_ OpFunc where
    function'call Function'call'params{params} (OpFunc op) = do
        when (V.length params /= 2) $
            throwFailed "Wrong number of parameters."
        pure Function'call'results
            { value = (params V.! 0) `op` (params V.! 1) }

data ExprFunc = ExprFunc
    { paramCount :: !Int32
    , body       :: Expression
    }

instance Function'server_ ExprFunc where
    function'call Function'call'params{params} ExprFunc{..} = do
        when (fromIntegral (V.length params) /= paramCount) $
            throwFailed "Wrong number of parameters."
        Function'call'results <$> eval params body

data MyCalc = MyCalc
    { add      :: Function
    , subtract :: Function
    , multiply :: Function
    , divide   :: Function
    }

instance Calculator'server_ MyCalc where
    calculator'evaluate Calculator'evaluate'params{expression} MyCalc{} =
        Calculator'evaluate'results <$> (eval V.empty expression >>= export_Value . LitValue)

    calculator'getOperator Calculator'getOperator'params{op} MyCalc{..} =
        Calculator'getOperator'results <$> case op of
            Operator'add      -> pure add
            Operator'subtract -> pure subtract
            Operator'multiply -> pure multiply
            Operator'divide   -> pure divide

            Operator'unknown' _ ->
                throwFailed "Unknown operator"

    calculator'defFunction Calculator'defFunction'params{..} _ =
        Calculator'defFunction'results <$> export_Function ExprFunc{..}

newCalculator :: RpcT IO Calculator
newCalculator = do
    add      <- export_Function $ OpFunc (+)
    subtract <- export_Function $ OpFunc (-)
    multiply <- export_Function $ OpFunc (*)
    divide   <- export_Function $ OpFunc (/)
    export_Calculator MyCalc{..}

eval :: V.Vector Double -> Expression -> RpcT IO Double
eval _ (Expression'literal lit) =
    pure lit
eval _ (Expression'previousResult val) = do
    Value'read'results{value} <- val & value'read def
    pure value
eval args (Expression'parameter idx)
    | fromIntegral idx >= V.length args =
        throwFailed "Parameter index out of bounds"
    | otherwise =
        pure $ args V.! fromIntegral idx
eval _ Expression'call{function, params} = do
    args' <- traverse (eval V.empty) params
    Function'call'results{value} <-
        function & function'call def { params = args' }
    pure value
eval _ (Expression'unknown' _) =
    throwFailed "Unknown expression type"

main :: IO ()
main = serve "localhost" "4000" $ \(sock, _addr) ->
    runVat $ (vatConfig $ socketTransport sock)
        { offerBootstrap = Just $ toClient <$> newCalculator
        , debugMode = True
        }
