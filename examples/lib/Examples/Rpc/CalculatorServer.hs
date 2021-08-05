{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeApplications      #-}
module Examples.Rpc.CalculatorServer (main) where

import Prelude hiding (subtract)

import Data.Int

import Control.Concurrent.STM (STM, atomically)
import Control.Monad          (when)
import Data.Function          ((&))
import Data.Functor           ((<&>))
import Network.Simple.TCP     (serve)
import Supervisors            (Supervisor)

import qualified Data.Vector as V

import Capnp.New
    ( Client
    , Pipeline
    , callP
    , def
    , defaultLimit
    , export
    , getField
    , handleParsed
    , waitPipeline
    )
import Capnp.Rpc
    (ConnConfig(..), Server, handleConn, socketTransport, throwFailed, toClient)

import Capnp.Gen.Calculator.New

newtype LitValue = LitValue Double

instance Value'server_ LitValue where
    value'read (LitValue val) = handleParsed $ \_ ->
        pure Value'read'results { value = val }

newtype OpFunc = OpFunc (Double -> Double -> Double)

instance Function'server_ OpFunc where
    function'call (OpFunc op) = handleParsed $ \Function'call'params{params} -> do
        when (V.length params /= 2) $
            throwFailed "Wrong number of parameters."
        pure Function'call'results
            { value = (params V.! 0) `op` (params V.! 1) }

data ExprFunc = ExprFunc
    { paramCount :: !Int32
    , body       :: Parsed Expression
    }

instance Function'server_ ExprFunc where
    function'call ExprFunc{..} =
        handleParsed $ \Function'call'params{params} -> do
            when (fromIntegral (V.length params) /= paramCount) $
                throwFailed "Wrong number of parameters."
            eval params body
                >>= waitResult
                <&> Function'call'results

data MyCalc = MyCalc
    { add      :: Client Function
    , subtract :: Client Function
    , multiply :: Client Function
    , divide   :: Client Function
    , sup      :: Supervisor
    }

instance Server IO MyCalc

instance Calculator'server_ MyCalc where
    calculator'evaluate MyCalc{sup} =
        handleParsed $ \Calculator'evaluate'params{expression} -> do
            eval V.empty expression
                >>= waitResult
                >>= export @Value sup . LitValue
                <&> Calculator'evaluate'results

    calculator'getOperator MyCalc{..} =
        handleParsed $ \Calculator'getOperator'params{op} ->
            Calculator'getOperator'results <$> case op of
                Operator'add      -> pure add
                Operator'subtract -> pure subtract
                Operator'multiply -> pure multiply
                Operator'divide   -> pure divide

                Operator'unknown' _ ->
                    throwFailed "Unknown operator"

    calculator'defFunction MyCalc{sup} =
        handleParsed $ \Calculator'defFunction'params{..} ->
            Calculator'defFunction'results <$>
                atomically (export @Function sup ExprFunc{..})

newCalculator :: Supervisor -> STM (Client Calculator)
newCalculator sup = do
    add      <- export @Function sup $ OpFunc (+)
    subtract <- export @Function sup $ OpFunc (-)
    multiply <- export @Function sup $ OpFunc (*)
    divide   <- export @Function sup $ OpFunc (/)
    export @Calculator sup MyCalc{..}


data EvalResult
    = Immediate Double
    | CallResult (Pipeline Function'call'results)
    | ReadResult (Pipeline Value'read'results)

waitResult :: EvalResult -> IO Double
waitResult (Immediate v)  = pure v
waitResult (CallResult p) = getField #value <$> waitPipeline p
waitResult (ReadResult p) = getField #value <$> waitPipeline p

eval :: V.Vector Double -> Parsed Expression -> IO EvalResult
eval outerParams (Expression exp) = go outerParams exp where
    go _ (Expression'literal lit) =
        pure $ Immediate lit
    go _ (Expression'previousResult val) = do
        val
            & callP #read def
            <&> ReadResult
    go args (Expression'parameter idx)
        | fromIntegral idx >= V.length args =
            throwFailed "Parameter index out of bounds"
        | otherwise =
            pure $ Immediate $ args V.! fromIntegral idx
    go outerParams (Expression'call Expression'call'{function, params=innerParams}) = do
        argPipelines <- traverse (eval outerParams) innerParams
        argValues <- traverse waitResult argPipelines
        function
            & callP #call Function'call'params { params = argValues }
            <&> CallResult
    go _ (Expression'unknown' _) =
        throwFailed "Unknown expression type"

main :: IO ()
main = serve "localhost" "4000" $ \(sock, _addr) ->
    handleConn (socketTransport sock defaultLimit) def
        { getBootstrap = fmap (Just . toClient) . newCalculator
        , debugMode = True
        }
