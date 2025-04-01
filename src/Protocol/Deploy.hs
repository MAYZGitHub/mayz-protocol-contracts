{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

--------------------------------------------------------------------------------2
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication" -}
--------------------------------------------------------------------------------2

module Protocol.Deploy where

--------------------------------------------------------------------------------2
-- External Imports
--------------------------------------------------------------------------------2
import qualified Control.Monad.IO.Class as MonadIOClass (MonadIO (..))
import qualified Data.Time as DataTime (defaultTimeLocale, formatTime, getCurrentTime)
import qualified Plutonomy
import PlutusTx.Prelude hiding (unless)
import qualified System.Directory as SystemDirectory
import qualified System.FilePath.Posix as SystemFilePathPosix
import qualified Prelude as P

--------------------------------------------------------------------------------2
-- Internal Imports
--------------------------------------------------------------------------------2

import qualified Generic.CLIHelpers as CLIHelpers
import qualified Generic.DeployHelpers as DeployHelpers
import qualified Generic.OffChainHelpers as OffChainHelpers
import qualified Protocol.BuyOrder.OnChain as BuyOrderOnChain
import qualified Protocol.Delegation.OnChain as DelegationOnChain
import qualified Protocol.Fund.Holding.OnChain as FundHoldingOnChain
import qualified Protocol.Fund.OnChain as FundOnChain
import qualified Protocol.Fund.InvestUnit.OnChain as InvestUnitOnChain
import qualified Protocol.PABTypes as T
import qualified Protocol.Protocol.OnChain as ProtocolOnChain
import qualified Protocol.Script.OnChain as ScriptOnChain
import qualified Protocol.SwapOffer.OnChain as SwapOfferOnChain
import qualified Protocol.Protocol.Types as ProtocolT
import qualified Protocol.Fund.Types as FundT
import qualified Protocol.SwapOffer.Types as SwapOfferT
import qualified Protocol.Delegation.Types as DelegationT
import qualified Protocol.Script.Types as ScriptT
import qualified Protocol.BuyOrder.Types as BuyOrderT
import qualified Protocol.Constants as T

--------------------------------------------------------------------------------2
-- Module
--------------------------------------------------------------------------------2

-- Para obtener deploy file de fabrica de protocolo y de fondo incluido
deploy_Protocol_And_Fund_Factory2 :: P.IO T.FactoryDeployParams
deploy_Protocol_And_Fund_Factory2 = do
    ------------------------------
    let
        version = T.mkVersionWithDependency [ProtocolT.protocolVersion, FundT.fundVersion, SwapOfferT.swapOfferVersion, BuyOrderT.buyOrderVersion, DelegationT.delegationVersion, ScriptT.scriptVersion] 0
        path = "export/protocol-v" ++ P.show version
    ------------------------------
    -- Get the current time
    currentTime <- MonadIOClass.liftIO DataTime.getCurrentTime
    -- Format the time. "%Y-%m-%d-%H-%M" corresponds to "yyyy-mm-dd-hh-mm"
    let
        defaultName = DataTime.formatTime DataTime.defaultTimeLocale "%Y-%m-%d-%H-%M" currentTime
    ------------------------------
    P.putStrLn $ "Protocol Name (default=" ++ defaultName ++ "):"
    !protocolName <- MonadIOClass.liftIO $ CLIHelpers.getStrWithDefault defaultName
    ------------------------------
    let
        finalPath = path SystemFilePathPosix.</> protocolName
    ------------------------------
    SystemDirectory.removePathForcibly finalPath
    SystemDirectory.createDirectoryIfMissing True finalPath
    ------------------------------
    MonadIOClass.liftIO $ P.putStrLn "Generating Protocol Deploy File..."
    ------------------------------
    P.putStrLn $ "Path: " ++ finalPath
    ------------------------------
    -- Protocol Scripts
    MonadIOClass.liftIO $ P.putStrLn "Generating 'Protocol PolicyID' Pre-Script..."
    let
        protocolPolicyID = Plutonomy.optimizeUPLC ProtocolOnChain.policyIDCode
    DeployHelpers.writeCompiledCodeToJsonFile (finalPath SystemFilePathPosix.</> "ProtocolPolicyID_PRE.plutus") protocolPolicyID
    MonadIOClass.liftIO $ P.putStrLn "Generating 'Protocol Validator' Pre-Script..."
    let
        protocolValidator = Plutonomy.optimizeUPLC ProtocolOnChain.validatorCode
    DeployHelpers.writeCompiledCodeToJsonFile (finalPath SystemFilePathPosix.</> "ProtocolValidator_PRE.plutus") protocolValidator
    ------------------------------
    MonadIOClass.liftIO $ P.putStrLn "Generating 'Script PolicyID' Pre-Script..."
    let
        scriptPolicyID = Plutonomy.optimizeUPLC ScriptOnChain.policyIDCode
    DeployHelpers.writeCompiledCodeToJsonFile (finalPath SystemFilePathPosix.</> "ScriptPolicyID_PRE.plutus") scriptPolicyID
    MonadIOClass.liftIO $ P.putStrLn "Generating 'Script Validator' Pre-Script..."
    let
        scriptValidator = Plutonomy.optimizeUPLC ScriptOnChain.validatorCode
    DeployHelpers.writeCompiledCodeToJsonFile (finalPath SystemFilePathPosix.</> "ScriptValidator_PRE.plutus") scriptValidator
    ------------------------------
    MonadIOClass.liftIO $ P.putStrLn "Generating 'SwapOffer Validator' Pre-Script..."
    let
        swapOfferValidator = Plutonomy.optimizeUPLC SwapOfferOnChain.validatorCode
    DeployHelpers.writeCompiledCodeToJsonFile (finalPath SystemFilePathPosix.</> "SwapOfferValidator_PRE.plutus") swapOfferValidator
    MonadIOClass.liftIO $ P.putStrLn "Generating 'SwapOffer PolicyID' Pre-Script..."
    let
        swapOfferPolicyID = Plutonomy.optimizeUPLC SwapOfferOnChain.policyIDCode
    DeployHelpers.writeCompiledCodeToJsonFile (finalPath SystemFilePathPosix.</> "SwapOfferPolicyID_PRE.plutus") swapOfferPolicyID
    ------------------------------
    MonadIOClass.liftIO $ P.putStrLn "Generating 'BuyOrder Validator' Pre-Script..."
    let
        buyOrderValidator = Plutonomy.optimizeUPLC BuyOrderOnChain.validatorCode
    DeployHelpers.writeCompiledCodeToJsonFile (finalPath SystemFilePathPosix.</> "BuyOrderValidator_PRE.plutus") buyOrderValidator
    MonadIOClass.liftIO $ P.putStrLn "Generating 'BuyOrder PolicyID' Pre-Script..."
    let
        buyOrderPolicyID = Plutonomy.optimizeUPLC BuyOrderOnChain.policyIDCode
    DeployHelpers.writeCompiledCodeToJsonFile (finalPath SystemFilePathPosix.</> "BuyOrderPolicyID_PRE.plutus") buyOrderPolicyID
    ------------------------------
    MonadIOClass.liftIO $ P.putStrLn "Generating 'Delegation Validator' Pre-Script..."
    let
        delegationValidator = Plutonomy.optimizeUPLC DelegationOnChain.validatorCode
    DeployHelpers.writeCompiledCodeToJsonFile (finalPath SystemFilePathPosix.</> "DelegationValidator_PRE.plutus") delegationValidator
    MonadIOClass.liftIO $ P.putStrLn "Generating 'Delegation PolicyID' Pre-Script..."
    let
        delegationPolicyID = Plutonomy.optimizeUPLC DelegationOnChain.policyIDCode
    DeployHelpers.writeCompiledCodeToJsonFile (finalPath SystemFilePathPosix.</> "DelegationPolicyID_PRE.plutus") delegationPolicyID
    ------------------------------
    MonadIOClass.liftIO $ P.putStrLn "Generating 'InvestUnit Validator' Pre-Script..."
    let
        investUnitValidator = Plutonomy.optimizeUPLC InvestUnitOnChain.validatorCode
    DeployHelpers.writeCompiledCodeToJsonFile (finalPath SystemFilePathPosix.</> "InvestUnitValidator_PRE.plutus") investUnitValidator
    ------------------------------
    MonadIOClass.liftIO $ P.putStrLn "Generating 'Fund Validator' Pre-Script..."
    let
        fundValidator = Plutonomy.optimizeUPLC FundOnChain.validatorCode
    DeployHelpers.writeCompiledCodeToJsonFile (finalPath SystemFilePathPosix.</> "FundValidator_PRE.plutus") fundValidator
    ------------------------------
    MonadIOClass.liftIO $ P.putStrLn "Generating 'Fund Policy' Pre-Script..."
    let
        fundPolicy = Plutonomy.optimizeUPLC FundOnChain.policyCode
    DeployHelpers.writeCompiledCodeToJsonFile (finalPath SystemFilePathPosix.</> "FundPolicy_PRE.plutus") fundPolicy
    ------------------------------
    MonadIOClass.liftIO $ P.putStrLn "Generating 'FundHolding PolicyID' Pre-Script..."
    let
        fundHoldingPolicyID = Plutonomy.optimizeUPLC FundHoldingOnChain.policyIDCode
    DeployHelpers.writeCompiledCodeToJsonFile (finalPath SystemFilePathPosix.</> "FundHoldingPolicyID_PRE.plutus") fundHoldingPolicyID
    ------------------------------
    MonadIOClass.liftIO $ P.putStrLn "Generating 'FundHolding Validator' Pre-Script..."
    let
        fundHoldingValidator = Plutonomy.optimizeUPLC FundHoldingOnChain.validatorCode
    DeployHelpers.writeCompiledCodeToJsonFile (finalPath SystemFilePathPosix.</> "FundHoldingValidator_PRE.plutus") fundHoldingValidator
    ------------------------------
    MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
    MonadIOClass.liftIO $ P.putStrLn "Creating 'Factory Deploy' File..."
    ------------------------------
    protocolPolicyID_Pre_CborHex <- OffChainHelpers.readFile (finalPath SystemFilePathPosix.</> "ProtocolPolicyID_PRE.plutus")
    protocolValidator_Pre_CborHex <- OffChainHelpers.readFile (finalPath SystemFilePathPosix.</> "ProtocolValidator_PRE.plutus")
    scriptPolicyID_Pre_CborHex <- OffChainHelpers.readFile (finalPath SystemFilePathPosix.</> "ScriptPolicyID_PRE.plutus")
    scriptValidator_Pre_CborHex <- OffChainHelpers.readFile (finalPath SystemFilePathPosix.</> "ScriptValidator_PRE.plutus")
    swapOfferValidator_Pre_CborHex <- OffChainHelpers.readFile (finalPath SystemFilePathPosix.</> "SwapOfferValidator_PRE.plutus")
    swapOfferPolicyID_Pre_CborHex <- OffChainHelpers.readFile (finalPath SystemFilePathPosix.</> "SwapOfferPolicyID_PRE.plutus")
    buyOrderValidator_Pre_CborHex <- OffChainHelpers.readFile (finalPath SystemFilePathPosix.</> "BuyOrderValidator_PRE.plutus")
    buyOrderPolicyID_Pre_CborHex <- OffChainHelpers.readFile (finalPath SystemFilePathPosix.</> "BuyOrderPolicyID_PRE.plutus")
    delegationValidator_Pre_CborHex <- OffChainHelpers.readFile (finalPath SystemFilePathPosix.</> "DelegationValidator_PRE.plutus")
    delegationPolicyID_Pre_CborHex <- OffChainHelpers.readFile (finalPath SystemFilePathPosix.</> "DelegationPolicyID_PRE.plutus")
    investUnitValidator_Pre_CborHex <- OffChainHelpers.readFile (finalPath SystemFilePathPosix.</> "InvestUnitValidator_PRE.plutus")
    fundPolicy_Pre_CborHex <- OffChainHelpers.readFile (finalPath SystemFilePathPosix.</> "FundPolicy_PRE.plutus")
    fundValidator_Pre_CborHex <- OffChainHelpers.readFile (finalPath SystemFilePathPosix.</> "FundValidator_PRE.plutus")
    fundHoldingPolicyID_Pre_CborHex <- OffChainHelpers.readFile (finalPath SystemFilePathPosix.</> "FundHoldingPolicyID_PRE.plutus")
    fundHoldingValidator_Pre_CborHex <- OffChainHelpers.readFile (finalPath SystemFilePathPosix.</> "FundHoldingValidator_PRE.plutus")
    ------------------------------
    let
        factoryDeployParams =
            T.FactoryDeployParams
                { T.fpProtocolVersion = ProtocolT.protocolVersion
                , T.fpFundVersion = FundT.fundVersion
                , T.fpSwapOfferVersion = SwapOfferT.swapOfferVersion
                , T.fpBuyOrderVersion = BuyOrderT.buyOrderVersion
                , T.fpDelegationVersion = DelegationT.delegationVersion
                , T.fpScriptVersion = ScriptT.scriptVersion
                , T.fpProtocolPolicyID_Pre_CborHex = OffChainHelpers.lazyByteStringToString protocolPolicyID_Pre_CborHex
                , T.fpProtocolValidator_Pre_CborHex = OffChainHelpers.lazyByteStringToString protocolValidator_Pre_CborHex
                , T.fpScriptPolicyID_Pre_CborHex = OffChainHelpers.lazyByteStringToString scriptPolicyID_Pre_CborHex
                , T.fpScriptValidator_Pre_CborHex = OffChainHelpers.lazyByteStringToString scriptValidator_Pre_CborHex
                , T.fpSwapOfferValidator_Pre_CborHex = OffChainHelpers.lazyByteStringToString swapOfferValidator_Pre_CborHex
                , T.fpSwapOfferPolicyID_Pre_CborHex = OffChainHelpers.lazyByteStringToString swapOfferPolicyID_Pre_CborHex
                , T.fpBuyOrderValidator_Pre_CborHex = OffChainHelpers.lazyByteStringToString buyOrderValidator_Pre_CborHex
                , T.fpBuyOrderPolicyID_Pre_CborHex = OffChainHelpers.lazyByteStringToString buyOrderPolicyID_Pre_CborHex
                , T.fpDelegationValidator_Pre_CborHex = OffChainHelpers.lazyByteStringToString delegationValidator_Pre_CborHex
                , T.fpDelegationPolicyID_Pre_CborHex = OffChainHelpers.lazyByteStringToString delegationPolicyID_Pre_CborHex
                , T.fpInvestUnitValidator_Pre_CborHex = OffChainHelpers.lazyByteStringToString investUnitValidator_Pre_CborHex
                , T.fpFundPolicy_Pre_CborHex = OffChainHelpers.lazyByteStringToString fundPolicy_Pre_CborHex
                , T.fpFundValidator_Pre_CborHex = OffChainHelpers.lazyByteStringToString fundValidator_Pre_CborHex
                , T.fpFundHoldingPolicyID_Pre_CborHex = OffChainHelpers.lazyByteStringToString fundHoldingPolicyID_Pre_CborHex
                , T.fpFundHoldingValidator_Pre_CborHex = OffChainHelpers.lazyByteStringToString fundHoldingValidator_Pre_CborHex
                }
    OffChainHelpers.writeEncodedToFile (finalPath SystemFilePathPosix.</> "FactoryDeploy.json") factoryDeployParams
    ------------------------------
    P.putStrLn $ "Saved Factory Deploy in: " ++ P.show (finalPath SystemFilePathPosix.</> "FactoryDeploy.json")
    P.putStrLn "--------------------------------"
    ------------------------------
    return factoryDeployParams

--------------------------------------------------------------------------------2
