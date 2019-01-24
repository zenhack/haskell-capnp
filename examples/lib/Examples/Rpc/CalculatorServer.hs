{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
module Examples.Rpc.CalculatorServer (main) where

import Prelude hiding (subtract)

import Data.Int

import Control.Concurrent.STM (STM, atomically)
import Control.Monad          (when)
import Network.Simple.TCP     (serve)
import Supervisors            (Supervisor)

import qualified Data.Vector as V

import Capnp     (def, defaultLimit)
import Capnp.Rpc
    ( ConnConfig(..)
    , handleConn
    , pureHandler
    , socketTransport
    , throwFailed
    , toClient
    , wait
    , (?)
    )

import Capnp.Gen.Calculator.Pure

newtype LitValue = LitValue Double

instance Value'server_ IO LitValue where
    value'read = pureHandler $ \(LitValue val) _ ->
        pure Value'read'results { value = val }

newtype OpFunc = OpFunc (Double -> Double -> Double)

instance Function'server_ IO OpFunc where
    function'call = pureHandler $ \(OpFunc op) Function'call'params{params} -> do
        when (V.length params /= 2) $
            throwFailed "Wrong number of parameters."
        pure Function'call'results
            { value = (params V.! 0) `op` (params V.! 1) }

data ExprFunc = ExprFunc
    { paramCount :: !Int32
    , body       :: Expression
    }

instance Function'server_ IO ExprFunc where
    function'call =
        pureHandler $ \ExprFunc{..} Function'call'params{params} -> do
            when (fromIntegral (V.length params) /= paramCount) $
                throwFailed "Wrong number of parameters."
            Function'call'results <$> eval params body

data MyCalc = MyCalc
    { add      :: Function
    , subtract :: Function
    , multiply :: Function
    , divide   :: Function
    , sup      :: Supervisor
    }

instance Calculator'server_ IO MyCalc where
    calculator'evaluate =
        pureHandler $ \MyCalc{sup} Calculator'evaluate'params{expression} ->
            Calculator'evaluate'results <$>
                (eval V.empty expression >>= atomically . export_Value sup . LitValue)

    calculator'getOperator =
        pureHandler $ \MyCalc{..} Calculator'getOperator'params{op} ->
            Calculator'getOperator'results <$> case op of
                Operator'add      -> pure add
                Operator'subtract -> pure subtract
                Operator'multiply -> pure multiply
                Operator'divide   -> pure divide

                Operator'unknown' _ ->
                    throwFailed "Unknown operator"

    calculator'defFunction =
        pureHandler $ \MyCalc{sup} Calculator'defFunction'params{..} ->
            Calculator'defFunction'results <$>
                atomically (export_Function sup ExprFunc{..})

newCalculator :: Supervisor -> STM Calculator
newCalculator sup = do
    add      <- export_Function sup $ OpFunc (+)
    subtract <- export_Function sup $ OpFunc (-)
    multiply <- export_Function sup $ OpFunc (*)
    divide   <- export_Function sup $ OpFunc (/)
    export_Calculator sup MyCalc{..}

eval :: V.Vector Double -> Expression -> IO Double
eval _ (Expression'literal lit) =
    pure lit
eval _ (Expression'previousResult val) = do
    Value'read'results{value} <- value'read val ? def >>= wait
    pure value
eval args (Expression'parameter idx)
    | fromIntegral idx >= V.length args =
        throwFailed "Parameter index out of bounds"
    | otherwise =
        pure $ args V.! fromIntegral idx
eval outerParams (Expression'call Expression'call'{function, params=innerParams}) = do
    args' <- traverse (eval outerParams) innerParams
    Function'call'results{value} <-
        function'call function ? Function'call'params { params = args' }
        >>= wait
    pure value
eval _ (Expression'unknown' _) =
    throwFailed "Unknown expression type"

main :: IO ()
main = serve "localhost" "4000" $ \(sock, _addr) ->
    handleConn (socketTransport sock defaultLimit) def
        { getBootstrap = fmap (Just . toClient) . newCalculator
        , debugMode = True
        }
