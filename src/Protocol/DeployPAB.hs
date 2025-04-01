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

module Protocol.DeployPAB where

--------------------------------------------------------------------------------2
-- External Imports
--------------------------------------------------------------------------------2
import qualified Control.Monad.IO.Class as MonadIOClass (MonadIO (..))
import qualified Data.Time as DataTime (defaultTimeLocale, formatTime, getCurrentTime)
import qualified Plutonomy
import qualified Plutus.V2.Ledger.Api as LedgerApiV2
import qualified PlutusTx
import PlutusTx.Prelude hiding (unless)
import qualified System.Directory as SystemDirectory
import qualified System.FilePath as SystemFilePath
import qualified System.FilePath.Posix as SystemFilePathPosix
import qualified Prelude as P

--------------------------------------------------------------------------------2
-- Internal Imports
--------------------------------------------------------------------------------2

import qualified Generic.CLIHelpers as CLIHelpers
import qualified Generic.DeployHelpers as DeployHelpers
import qualified Generic.OffChainHelpers as OffChainHelpers
import qualified Generic.OffChainHelpers as Utils
import qualified Generic.Types as T
import qualified PlutusTx.Builtins.Class as TxBuiltinsClass
import qualified Protocol.BuyOrder.OnChain as BuyOrderOnChain
import qualified Protocol.BuyOrder.Types as BuyOrderT
import qualified Protocol.Delegation.OnChain as DelegationOnChain
import qualified Protocol.Delegation.Types as DelegationT
import qualified Protocol.Fund.Holding.OnChain as FundHoldingOnChain
import qualified Protocol.Fund.Holding.Types as FundHoldingT
import qualified Protocol.Fund.OnChain as FundOnChain
import qualified Protocol.Fund.Types as FundT
import qualified Protocol.Fund.InvestUnit.OnChain as InvestUnitOnChain
import qualified Protocol.Fund.InvestUnit.Types as InvestUnitT
import qualified Protocol.PABTypes as T
import qualified Protocol.Protocol.OnChain as ProtocolOnChain
import qualified Protocol.Protocol.Types as ProtocolT
import qualified Protocol.Script.OnChain as ScriptOnChain
import qualified Protocol.Script.Types as ScriptT
import qualified Protocol.SwapOffer.OnChain as SwapOfferOnChain
import qualified Protocol.SwapOffer.Types as SwapOfferT

--------------------------------------------------------------------------------2
-- Module
--------------------------------------------------------------------------------2

-- Para obtener pab file de un protocolo especifico con una fabrica de fondo incluida
deploy_ProtocolPAB_With_RequestingParams :: LedgerApiV2.TxOutRef -> P.IO T.ProtocolPABParams
deploy_ProtocolPAB_With_RequestingParams protocolPolicyID_TxOutRef = do
    -- MonadIOClass.liftIO $ P.putStrLn "Path (default=export/protocol):"
    -- !path <- MonadIOClass.liftIO $ CLIHelpers.getStrWithDefault "export/protocol"
    let
        path = "export/protocol-v" ++ P.show ProtocolT.protocolVersion
    ------------------------------
    -- Get the current time
    currentTime <- MonadIOClass.liftIO DataTime.getCurrentTime
    -- Format the time. "%Y-%m-%d-%H-%M" corresponds to "yyyy-mm-dd-hh-mm"
    let
        defaultName = DataTime.formatTime DataTime.defaultTimeLocale "%Y-%m-%d-%H-%M" currentTime
    ------------------------------
    P.putStrLn $ "Protocol Name (default=" ++ defaultName ++ "-pab):"
    !protocolName <- MonadIOClass.liftIO $ CLIHelpers.getStrWithDefault (defaultName ++ "-pab")
    ------------------------------
    -- let
    --     getToken_AC :: P.IO LedgerValue.AssetClass
    --     getToken_AC = do
    --         MonadIOClass.liftIO $ P.putStrLn "MAYZ Token"
    --         cs <- MonadIOClass.liftIO $ CLIHelpers.getCurrencySymbol "6aa8df2a10e8530338279e41a1a40dc73f83e468b710c3b64082f759"
    --         tn <- MonadIOClass.liftIO $ CLIHelpers.getTokenName "MAYZ" False
    --         return $ LedgerValue.AssetClass (cs, tn)
    -- ---------------------
    -- tokenMAYZ_AC <- getToken_AC
    ------------------------------
    MonadIOClass.liftIO $ P.putStrLn "Token Emergency Admin Policy CS"
    tokenEmergencyAdminPolicy_CS <- MonadIOClass.liftIO $ CLIHelpers.getCurrencySymbol "6aa8df2a10e8530338279e41a1a40dc73f83e468b710c3b64082f759"
    ------------------------------
    deploy_ProtocolPAB protocolPolicyID_TxOutRef tokenEmergencyAdminPolicy_CS path protocolName

-- Para obtener pab file de un protocolo especifico con una fabrica de fondo incluida
deploy_ProtocolPAB :: LedgerApiV2.TxOutRef -> LedgerApiV2.CurrencySymbol -> P.FilePath -> P.String -> P.IO T.ProtocolPABParams
deploy_ProtocolPAB protocolPolicyID_TxOutRef tokenEmergencyAdminPolicy_CS path protocolName = do
    SystemDirectory.removePathForcibly (path SystemFilePathPosix.</> protocolName)
    SystemDirectory.createDirectoryIfMissing True (path SystemFilePathPosix.</> protocolName)
    ------------------------------
    MonadIOClass.liftIO $ P.putStrLn "Generating Protocol PAB Params..."
    ------------------------------
    P.putStrLn $ "Path: " ++ path SystemFilePathPosix.</> protocolName
    ---------------------
    MonadIOClass.liftIO $ P.putStrLn "Generating 'Protocol PolicyID' Script..."
    P.putStrLn $ "Protocol PolicyID TxOutRef: " ++ P.show protocolPolicyID_TxOutRef
    ------------------------------
    let
        protocolPolicyParams =
            ProtocolT.PolicyParams
                { ProtocolT.ppProtocolPolicyID_TxOutRef = protocolPolicyID_TxOutRef
                }
        protocolPolicyID = ProtocolOnChain.policyID protocolPolicyParams
        protocolPolicyID_CS = OffChainHelpers.getCurSymbolOfPolicy protocolPolicyID
    _ <- MonadIOClass.liftIO $ DeployHelpers.deployMintingPolicy (path SystemFilePathPosix.</> protocolName) "ProtocolPolicyID" protocolPolicyID protocolPolicyID_CS
    ------------------------------
    MonadIOClass.liftIO $ P.putStrLn "Generating 'Protocol Validator' Script..."
    let
        protocolValidatorParams =
            ProtocolT.ValidatorParams
                { ProtocolT.vpProtocolPolicyID_CS = protocolPolicyID_CS
                , ProtocolT.vpTokenEmergencyAdminPolicy_CS = tokenEmergencyAdminPolicy_CS
                }
        protocolValidator = ProtocolOnChain.validator protocolValidatorParams
        protocolValidator_Hash = OffChainHelpers.hashValidator protocolValidator
        protocolValidator_Address = OffChainHelpers.addressValidator protocolValidator_Hash
    _ <- MonadIOClass.liftIO $ DeployHelpers.deployValidator (path SystemFilePathPosix.</> protocolName) "ProtocolValidator" protocolValidator
    _ <- MonadIOClass.liftIO $ DeployHelpers.deployValidatorHash (path SystemFilePathPosix.</> protocolName) "ProtocolValidator" protocolValidator_Hash
    _ <- MonadIOClass.liftIO $ DeployHelpers.deployValidatorAddress (path SystemFilePathPosix.</> protocolName) "ProtocolValidator" protocolValidator_Address
    ------------------------------
    MonadIOClass.liftIO $ P.putStrLn "Generating 'Script PolicyID' Script..."
    let
        scriptPolicyParams =
            ScriptT.PolicyParams
                { ScriptT.ppProtocolPolicyID_CS = protocolPolicyID_CS
                }
        scriptPolicyID = ScriptOnChain.policyID scriptPolicyParams
        scriptPolicyID_CS = OffChainHelpers.getCurSymbolOfPolicy scriptPolicyID
    _ <- MonadIOClass.liftIO $ DeployHelpers.deployMintingPolicy (path SystemFilePathPosix.</> protocolName) "ScriptPolicyID" scriptPolicyID scriptPolicyID_CS
    ------------------------------
    MonadIOClass.liftIO $ P.putStrLn "Generating 'Script Validator' Script..."
    let
        scriptValidatorParams =
            ScriptT.ValidatorParams
                { ScriptT.vpScriptPolicyID_CS = scriptPolicyID_CS
                , ScriptT.vpProtocolPolicyID_CS = protocolPolicyID_CS
                }
        scriptValidator = ScriptOnChain.validator scriptValidatorParams
        scriptValidator_Hash = OffChainHelpers.hashValidator scriptValidator
        scriptValidator_Address = OffChainHelpers.addressValidator scriptValidator_Hash
    _ <- MonadIOClass.liftIO $ DeployHelpers.deployValidator (path SystemFilePathPosix.</> protocolName) "ScriptValidator" scriptValidator
    _ <- MonadIOClass.liftIO $ DeployHelpers.deployValidatorHash (path SystemFilePathPosix.</> protocolName) "ScriptValidator" scriptValidator_Hash
    _ <- MonadIOClass.liftIO $ DeployHelpers.deployValidatorAddress (path SystemFilePathPosix.</> protocolName) "ScriptValidator" scriptValidator_Address
    ------------------------------
    MonadIOClass.liftIO $ P.putStrLn "Generating 'Fund Validator' Script..."
    let
        fundValidatorParams =
            FundT.ValidatorParams
                { FundT.vpProtocolPolicyID_CS = protocolPolicyID_CS
                , FundT.vpTokenEmergencyAdminPolicy_CS = tokenEmergencyAdminPolicy_CS
                }
        fundValidator = FundOnChain.validator fundValidatorParams
        fundValidator_Hash = OffChainHelpers.hashValidator fundValidator
        fundValidator_Address = OffChainHelpers.addressValidator fundValidator_Hash
    _ <- MonadIOClass.liftIO $ DeployHelpers.deployValidator (path SystemFilePathPosix.</> protocolName) "FundValidator" fundValidator
    _ <- MonadIOClass.liftIO $ DeployHelpers.deployValidatorHash (path SystemFilePathPosix.</> protocolName) "FundValidator" fundValidator_Hash
    _ <- MonadIOClass.liftIO $ DeployHelpers.deployValidatorAddress (path SystemFilePathPosix.</> protocolName) "FundValidator" fundValidator_Address
    ------------------------------
    MonadIOClass.liftIO $ P.putStrLn "Generating 'InvestUnit Validator' Script..."
    let
        investUnitValidatorParams =
            InvestUnitT.ValidatorParams
                { InvestUnitT.vpProtocolPolicyID_CS = protocolPolicyID_CS
                , InvestUnitT.vpTokenEmergencyAdminPolicy_CS = tokenEmergencyAdminPolicy_CS
                }
        investUnitValidator = InvestUnitOnChain.validator investUnitValidatorParams
        investUnitValidator_Hash = OffChainHelpers.hashValidator investUnitValidator
        investUnitValidator_Address = OffChainHelpers.addressValidator investUnitValidator_Hash
    _ <- MonadIOClass.liftIO $ DeployHelpers.deployValidator (path SystemFilePathPosix.</> protocolName) "InvestUnitValidator" investUnitValidator
    _ <- MonadIOClass.liftIO $ DeployHelpers.deployValidatorHash (path SystemFilePathPosix.</> protocolName) "InvestUnitValidator" investUnitValidator_Hash
    _ <- MonadIOClass.liftIO $ DeployHelpers.deployValidatorAddress (path SystemFilePathPosix.</> protocolName) "InvestUnitValidator" investUnitValidator_Address
    ------------------------------
    let
        fundFactoryPABParams =
            T.FundFactoryPABParams
                { T.ffppFundVersion = FundT.fundVersion
                , T.ffppFundValidator_Params = fundValidatorParams
                , T.ffppFundValidator = fundValidator
                , T.ffppFundValidator_Hash = fundValidator_Hash
                , T.ffppFundValidator_Address = fundValidator_Address
                , T.ffppFundPABParams = []
                }
    ------------------------------
    MonadIOClass.liftIO $ P.putStrLn "Creating 'Protocol PAB Params' File..."
    ------------------------------
    let
        protocolPABParams =
            T.ProtocolPABParams
                { T.pppProtocolVersion = ProtocolT.protocolVersion
                , T.pppProtocolPolicyID_Params = protocolPolicyParams
                , T.pppProtocolPolicyID = protocolPolicyID
                , T.pppProtocolPolicyID_CS = protocolPolicyID_CS
                , T.pppProtocolValidator_Params = protocolValidatorParams
                , T.pppProtocolValidator = protocolValidator
                , T.pppProtocolValidator_Hash = protocolValidator_Hash
                , T.pppProtocolValidator_Address = protocolValidator_Address
                , T.pppScriptPolicyID_Params = scriptPolicyParams
                , T.pppScriptPolicyID = scriptPolicyID
                , T.pppScriptPolicyID_CS = scriptPolicyID_CS
                , T.pppScriptValidator_Params = scriptValidatorParams
                , T.pppScriptValidator = scriptValidator
                , T.pppScriptValidator_Hash = scriptValidator_Hash
                , T.pppScriptValidator_Address = scriptValidator_Address
                , T.pppInvestUnitValidator_Params = investUnitValidatorParams
                , T.pppInvestUnitValidator = investUnitValidator
                , T.pppInvestUnitValidator_Hash = investUnitValidator_Hash
                , T.pppInvestUnitValidator_Address = investUnitValidator_Address
                , T.pppFundFactoryPABParams = [fundFactoryPABParams]
                , T.pppTokenEmergencyAdmin_CS = tokenEmergencyAdminPolicy_CS
                }
    OffChainHelpers.writeEncodedToFile (path SystemFilePathPosix.</> protocolName SystemFilePathPosix.</> "ProtocolPAB.json") protocolPABParams
    P.putStrLn $ "Saved Protocol PAB Param in: " ++ P.show (path SystemFilePathPosix.</> protocolName SystemFilePathPosix.</> "ProtocolPAB.json")
    ------------------------------
    P.putStrLn "--------------------------------"
    ------------------------------
    return protocolPABParams

--------------------------------------------------------------------------------

-- Para obtener deploy file de un protocolo especifico con una fabrica de fondo incluida
deploy_Protocol_And_FundFactory_With_StringParams :: P.String -> P.String -> P.IO T.ProtocolDeployParams
deploy_Protocol_And_FundFactory_With_StringParams protocolPolicyID_TxOutRefStr tokenEmergencyAdminPolicy_CS_Str = do
    ------------------------------
    let
        path = "export/protocol-v" ++ P.show ProtocolT.protocolVersion
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
        !protocolPolicyID_TxOutRef = Utils.unsafeReadTxOutRef protocolPolicyID_TxOutRefStr
        -- !tokenMAYZ_CS = LedgerApiV2.CurrencySymbol $ TxBuiltinsClass.toBuiltin $ Utils.stringFromHexString tokenMAYZ_CS_Str
        -- !tokenMAYZ_TN = LedgerApiV2.TokenName $ Utils.stringToBuiltinByteString tokenMAYZ_TN_Str
        -- !tokenMAYZ_AC = LedgerValue.AssetClass (tokenMAYZ_CS, tokenMAYZ_TN)
        !tokenEmergencyAdminPolicy_CS = LedgerApiV2.CurrencySymbol $ TxBuiltinsClass.toBuiltin $ Utils.stringFromHexString tokenEmergencyAdminPolicy_CS_Str
    ------------------------------
    deploy_Protocol_And_FundFactory protocolPolicyID_TxOutRef tokenEmergencyAdminPolicy_CS path protocolName

-- Para obtener deploy file de un protocolo especifico con una fabrica de fondo incluida
deploy_Protocol_And_FundFactory_With_RequestingParams :: LedgerApiV2.TxOutRef -> P.IO T.ProtocolDeployParams
deploy_Protocol_And_FundFactory_With_RequestingParams protocolPolicyID_TxOutRef = do
    ------------------------------
    let
        path = "export/protocol-v" ++ P.show ProtocolT.protocolVersion
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
    -- let
    --     getToken_AC :: P.IO LedgerValue.AssetClass
    --     getToken_AC = do
    --         MonadIOClass.liftIO $ P.putStrLn "MAYZ Token"
    --         cs <- MonadIOClass.liftIO $ CLIHelpers.getCurrencySymbol "6aa8df2a10e8530338279e41a1a40dc73f83e468b710c3b64082f759"
    --         tn <- MonadIOClass.liftIO $ CLIHelpers.getTokenName "MAYZ" False
    --         return $ LedgerValue.AssetClass (cs, tn)
    -- ---------------------
    -- tokenMAYZ_AC <- getToken_AC
    ------------------------------
    MonadIOClass.liftIO $ P.putStrLn "Token Emergency Admin Policy CS"
    tokenEmergencyAdminPolicy_CS <- MonadIOClass.liftIO $ CLIHelpers.getCurrencySymbol "6aa8df2a10e8530338279e41a1a40dc73f83e468b710c3b64082f759"
    ------------------------------
    deploy_Protocol_And_FundFactory protocolPolicyID_TxOutRef tokenEmergencyAdminPolicy_CS path protocolName

-- Para obtener deploy files de un protocolo especifico con una fabrica de fondo incluida
deploy_Protocol_And_FundFactory :: LedgerApiV2.TxOutRef -> LedgerApiV2.CurrencySymbol -> P.FilePath -> P.String -> P.IO T.ProtocolDeployParams
deploy_Protocol_And_FundFactory protocolPolicyID_TxOutRef tokenEmergencyAdminPolicy_CS path protocolName = do
    ------------------------------
    SystemDirectory.removePathForcibly (path SystemFilePathPosix.</> protocolName)
    SystemDirectory.createDirectoryIfMissing True (path SystemFilePathPosix.</> protocolName)
    ------------------------------
    MonadIOClass.liftIO $ P.putStrLn "Generating Protocol Deploy File 3333..."
    ------------------------------
    P.putStrLn $ "Path: " ++ path SystemFilePathPosix.</> protocolName
    ------------------------------
    MonadIOClass.liftIO $ P.putStrLn "Generating 'Protocol PolicyID' Script..."
    P.putStrLn $ "Protocol PolicyID TxOutRef: " ++ P.show protocolPolicyID_TxOutRef
    ------------------------------
    let
        protocolPolicyParams =
            ProtocolT.PolicyParams
                { ProtocolT.ppProtocolPolicyID_TxOutRef = protocolPolicyID_TxOutRef
                }
        protocolPolicyID = ProtocolOnChain.policyID protocolPolicyParams
        protocolPolicyID_CS = OffChainHelpers.getCurSymbolOfPolicy protocolPolicyID
    P.putStrLn $ "Protocol PolicyParams: " ++ P.show protocolPolicyParams
    _ <- MonadIOClass.liftIO $ DeployHelpers.deployMintingPolicy (path SystemFilePathPosix.</> protocolName) "ProtocolPolicyID" protocolPolicyID protocolPolicyID_CS
    ------------------------------
    MonadIOClass.liftIO $ P.putStrLn "Generating 'Protocol Validator' Script..."
    let
        protocolValidatorParams =
            ProtocolT.ValidatorParams
                { ProtocolT.vpProtocolPolicyID_CS = protocolPolicyID_CS
                , ProtocolT.vpTokenEmergencyAdminPolicy_CS = tokenEmergencyAdminPolicy_CS
                }
        protocolValidator = ProtocolOnChain.validator protocolValidatorParams
        protocolValidator_Hash = OffChainHelpers.hashValidator protocolValidator
        protocolValidator_Address = OffChainHelpers.addressValidator protocolValidator_Hash
    P.putStrLn $ "Protocol ValidatorParams: " ++ P.show protocolValidatorParams
    _ <- MonadIOClass.liftIO $ DeployHelpers.deployValidator (path SystemFilePathPosix.</> protocolName) "ProtocolValidator" protocolValidator
    _ <- MonadIOClass.liftIO $ DeployHelpers.deployValidatorHash (path SystemFilePathPosix.</> protocolName) "ProtocolValidator" protocolValidator_Hash
    _ <- MonadIOClass.liftIO $ DeployHelpers.deployValidatorAddress (path SystemFilePathPosix.</> protocolName) "ProtocolValidator" protocolValidator_Address
    ------------------------------
    MonadIOClass.liftIO $ P.putStrLn "Generating 'Script PolicyID' Script..."
    let
        scriptPolicyParams =
            ScriptT.PolicyParams
                { ScriptT.ppProtocolPolicyID_CS = protocolPolicyID_CS
                }
        scriptPolicyID = ScriptOnChain.policyID scriptPolicyParams
        scriptPolicyID_CS = OffChainHelpers.getCurSymbolOfPolicy scriptPolicyID
    _ <- MonadIOClass.liftIO $ DeployHelpers.deployMintingPolicy (path SystemFilePathPosix.</> protocolName) "ScriptPolicyID" scriptPolicyID scriptPolicyID_CS
    ------------------------------
    MonadIOClass.liftIO $ P.putStrLn "Generating 'Script Validator' Script..."
    let
        scriptValidatorParams =
            ScriptT.ValidatorParams
                { ScriptT.vpScriptPolicyID_CS = scriptPolicyID_CS
                , ScriptT.vpProtocolPolicyID_CS = protocolPolicyID_CS
                }
        scriptValidator = ScriptOnChain.validator scriptValidatorParams
        scriptValidator_Hash = OffChainHelpers.hashValidator scriptValidator
        scriptValidator_Address = OffChainHelpers.addressValidator scriptValidator_Hash
    _ <- MonadIOClass.liftIO $ DeployHelpers.deployValidator (path SystemFilePathPosix.</> protocolName) "ScriptValidator" scriptValidator
    _ <- MonadIOClass.liftIO $ DeployHelpers.deployValidatorHash (path SystemFilePathPosix.</> protocolName) "ScriptValidator" scriptValidator_Hash
    _ <- MonadIOClass.liftIO $ DeployHelpers.deployValidatorAddress (path SystemFilePathPosix.</> protocolName) "ScriptValidator" scriptValidator_Address
    ------------------------------
    MonadIOClass.liftIO $ P.putStrLn "Generating 'SwapOffer Validator' Script..."
    let
        swapOfferValidatorParams =
            SwapOfferT.ValidatorParams
                { SwapOfferT.vpProtocolPolicyID_CS = protocolPolicyID_CS
                , SwapOfferT.vpTokenEmergencyAdminPolicy_CS = tokenEmergencyAdminPolicy_CS
                }
        swapOfferValidator = SwapOfferOnChain.validator swapOfferValidatorParams
        swapOfferValidator_Hash = OffChainHelpers.hashValidator swapOfferValidator
        swapOfferValidator_Address = OffChainHelpers.addressValidator swapOfferValidator_Hash
    _ <- MonadIOClass.liftIO $ DeployHelpers.deployValidator (path SystemFilePathPosix.</> protocolName) "SwapOfferValidator" swapOfferValidator
    _ <- MonadIOClass.liftIO $ DeployHelpers.deployValidatorHash (path SystemFilePathPosix.</> protocolName) "SwapOfferValidator" swapOfferValidator_Hash
    _ <- MonadIOClass.liftIO $ DeployHelpers.deployValidatorAddress (path SystemFilePathPosix.</> protocolName) "SwapOfferValidator" swapOfferValidator_Address
    ------------------------------
    MonadIOClass.liftIO $ P.putStrLn "Generating 'SwapOffer PolicyID' Script..."
    let
        swapOfferPolicyParams =
            SwapOfferT.PolicyParams
                { SwapOfferT.ppProtocolPolicyID_CS = protocolPolicyID_CS
                , SwapOfferT.ppSwapOffer_Validator_Hash = swapOfferValidator_Hash
                }
        swapOfferPolicyID = SwapOfferOnChain.policyID swapOfferPolicyParams
        swapOfferPolicyID_CS = OffChainHelpers.getCurSymbolOfPolicy swapOfferPolicyID
    _ <- MonadIOClass.liftIO $ DeployHelpers.deployMintingPolicy (path SystemFilePathPosix.</> protocolName) "SwapOfferPolicyID" swapOfferPolicyID swapOfferPolicyID_CS
    ------------------------------
    MonadIOClass.liftIO $ P.putStrLn "Generating 'BuyOrder Validator' Script..."
    let
        buyOrderValidatorParams =
            BuyOrderT.ValidatorParams {BuyOrderT.vpProtocolPolicyID_CS = protocolPolicyID_CS}
        buyOrderValidator = BuyOrderOnChain.validator buyOrderValidatorParams
        buyOrderValidator_Hash = OffChainHelpers.hashValidator buyOrderValidator
        buyOrderValidator_Address = OffChainHelpers.addressValidator buyOrderValidator_Hash
    _ <- MonadIOClass.liftIO $ DeployHelpers.deployValidator (path SystemFilePathPosix.</> protocolName) "BuyOrderValidator" buyOrderValidator
    _ <- MonadIOClass.liftIO $ DeployHelpers.deployValidatorHash (path SystemFilePathPosix.</> protocolName) "BuyOrderValidator" buyOrderValidator_Hash
    _ <- MonadIOClass.liftIO $ DeployHelpers.deployValidatorAddress (path SystemFilePathPosix.</> protocolName) "BuyOrderValidator" buyOrderValidator_Address
    ------------------------------
    MonadIOClass.liftIO $ P.putStrLn "Generating 'BuyOrder PolicyID' Script..."
    let
        buyOrderPolicyParams =
            BuyOrderT.PolicyParams
                { BuyOrderT.ppProtocolPolicyID_CS = protocolPolicyID_CS
                , BuyOrderT.ppBuyOrder_Validator_Hash = buyOrderValidator_Hash
                }
        buyOrderPolicyID = BuyOrderOnChain.policyID buyOrderPolicyParams
        buyOrderPolicyID_CS = OffChainHelpers.getCurSymbolOfPolicy buyOrderPolicyID
    _ <- MonadIOClass.liftIO $ DeployHelpers.deployMintingPolicy (path SystemFilePathPosix.</> protocolName) "BuyOrderPolicyID" buyOrderPolicyID buyOrderPolicyID_CS
    ------------------------------
    MonadIOClass.liftIO $ P.putStrLn "Generating 'Delegation Validator' Script..."
    let
        delegationValidatorParams =
            DelegationT.ValidatorParams
                { DelegationT.vpProtocolPolicyID_CS = protocolPolicyID_CS
                , DelegationT.vpTokenEmergencyAdminPolicy_CS = tokenEmergencyAdminPolicy_CS
                }
        delegationValidator = DelegationOnChain.validator delegationValidatorParams
        delegationValidator_Hash = OffChainHelpers.hashValidator delegationValidator
        delegationValidator_Address = OffChainHelpers.addressValidator delegationValidator_Hash
    _ <- MonadIOClass.liftIO $ DeployHelpers.deployValidator (path SystemFilePathPosix.</> protocolName) "DelegationValidator" delegationValidator
    _ <- MonadIOClass.liftIO $ DeployHelpers.deployValidatorHash (path SystemFilePathPosix.</> protocolName) "DelegationValidator" delegationValidator_Hash
    _ <- MonadIOClass.liftIO $ DeployHelpers.deployValidatorAddress (path SystemFilePathPosix.</> protocolName) "DelegationValidator" delegationValidator_Address
    ------------------------------
    MonadIOClass.liftIO $ P.putStrLn "Generating 'Delegation PolicyID' Script..."
    let
        delegationPolicyParams =
            DelegationT.PolicyParams
                { DelegationT.ppProtocolPolicyID_CS = protocolPolicyID_CS
                , DelegationT.ppDelegation_Validator_Hash = delegationValidator_Hash
                }
        delegationPolicyID = DelegationOnChain.policyID delegationPolicyParams
        delegationPolicyID_CS = OffChainHelpers.getCurSymbolOfPolicy delegationPolicyID
    _ <- MonadIOClass.liftIO $ DeployHelpers.deployMintingPolicy (path SystemFilePathPosix.</> protocolName) "DelegationPolicyID" delegationPolicyID delegationPolicyID_CS
    ------------------------------
    MonadIOClass.liftIO $ P.putStrLn "Generating 'InvestUnit Validator' Script..."
    let
        investUnitValidatorParams =
            InvestUnitT.ValidatorParams
                { InvestUnitT.vpProtocolPolicyID_CS = protocolPolicyID_CS
                , InvestUnitT.vpTokenEmergencyAdminPolicy_CS = tokenEmergencyAdminPolicy_CS
                }
        investUnitValidator = InvestUnitOnChain.validator investUnitValidatorParams
        investUnitValidator_Hash = OffChainHelpers.hashValidator investUnitValidator
        investUnitValidator_Address = OffChainHelpers.addressValidator investUnitValidator_Hash
    _ <- MonadIOClass.liftIO $ DeployHelpers.deployValidator (path SystemFilePathPosix.</> protocolName) "InvestUnitValidator" investUnitValidator
    _ <- MonadIOClass.liftIO $ DeployHelpers.deployValidatorHash (path SystemFilePathPosix.</> protocolName) "InvestUnitValidator" investUnitValidator_Hash
    _ <- MonadIOClass.liftIO $ DeployHelpers.deployValidatorAddress (path SystemFilePathPosix.</> protocolName) "InvestUnitValidator" investUnitValidator_Address
    ------------------------------
    protocolPolicyID_CborHex <- OffChainHelpers.readFile (path SystemFilePathPosix.</> protocolName SystemFilePathPosix.</> "ProtocolPolicyID.plutus")
    protocolValidator_CborHex <- OffChainHelpers.readFile (path SystemFilePathPosix.</> protocolName SystemFilePathPosix.</> "ProtocolValidator.plutus")
    protocolValidator_Address_Testnet <- OffChainHelpers.readFile (path SystemFilePathPosix.</> protocolName SystemFilePathPosix.</> "ProtocolValidator-Testnet.addr")
    protocolValidator_Address_Mainnet <- OffChainHelpers.readFile (path SystemFilePathPosix.</> protocolName SystemFilePathPosix.</> "ProtocolValidator-Mainnet.addr")
    ------------------------------
    scriptPolicyID_CborHex <- OffChainHelpers.readFile (path SystemFilePathPosix.</> protocolName SystemFilePathPosix.</> "ScriptPolicyID.plutus")
    scriptValidator_CborHex <- OffChainHelpers.readFile (path SystemFilePathPosix.</> protocolName SystemFilePathPosix.</> "ScriptValidator.plutus")
    scriptValidator_Address_Testnet <- OffChainHelpers.readFile (path SystemFilePathPosix.</> protocolName SystemFilePathPosix.</> "ScriptValidator-Testnet.addr")
    scriptValidator_Address_Mainnet <- OffChainHelpers.readFile (path SystemFilePathPosix.</> protocolName SystemFilePathPosix.</> "ScriptValidator-Mainnet.addr")
    ------------------------------
    swapOfferPolicyID_CborHex <- OffChainHelpers.readFile (path SystemFilePathPosix.</> protocolName SystemFilePathPosix.</> "SwapOfferPolicyID.plutus")
    swapOfferValidator_CborHex <- OffChainHelpers.readFile (path SystemFilePathPosix.</> protocolName SystemFilePathPosix.</> "SwapOfferValidator.plutus")
    swapOfferValidator_Address_Testnet <- OffChainHelpers.readFile (path SystemFilePathPosix.</> protocolName SystemFilePathPosix.</> "SwapOfferValidator-Testnet.addr")
    swapOfferValidator_Address_Mainnet <- OffChainHelpers.readFile (path SystemFilePathPosix.</> protocolName SystemFilePathPosix.</> "SwapOfferValidator-Mainnet.addr")
    ------------------------------
    buyOrderPolicyID_CborHex <- OffChainHelpers.readFile (path SystemFilePathPosix.</> protocolName SystemFilePathPosix.</> "BuyOrderPolicyID.plutus")
    buyOrderValidator_CborHex <- OffChainHelpers.readFile (path SystemFilePathPosix.</> protocolName SystemFilePathPosix.</> "BuyOrderValidator.plutus")
    buyOrderValidator_Address_Testnet <- OffChainHelpers.readFile (path SystemFilePathPosix.</> protocolName SystemFilePathPosix.</> "BuyOrderValidator-Testnet.addr")
    buyOrderValidator_Address_Mainnet <- OffChainHelpers.readFile (path SystemFilePathPosix.</> protocolName SystemFilePathPosix.</> "BuyOrderValidator-Mainnet.addr")
    ------------------------------
    delegationPolicyID_CborHex <- OffChainHelpers.readFile (path SystemFilePathPosix.</> protocolName SystemFilePathPosix.</> "DelegationPolicyID.plutus")
    delegationValidator_CborHex <- OffChainHelpers.readFile (path SystemFilePathPosix.</> protocolName SystemFilePathPosix.</> "DelegationValidator.plutus")
    delegationValidator_Address_Testnet <- OffChainHelpers.readFile (path SystemFilePathPosix.</> protocolName SystemFilePathPosix.</> "DelegationValidator-Testnet.addr")
    delegationValidator_Address_Mainnet <- OffChainHelpers.readFile (path SystemFilePathPosix.</> protocolName SystemFilePathPosix.</> "DelegationValidator-Mainnet.addr")
    ------------------------------
    investUnitValidator_CborHex <- OffChainHelpers.readFile (path SystemFilePathPosix.</> protocolName SystemFilePathPosix.</> "InvestUnitValidator.plutus")
    investUnitValidator_Address_Testnet <- OffChainHelpers.readFile (path SystemFilePathPosix.</> protocolName SystemFilePathPosix.</> "InvestUnitValidator-Testnet.addr")
    investUnitValidator_Address_Mainnet <- OffChainHelpers.readFile (path SystemFilePathPosix.</> protocolName SystemFilePathPosix.</> "InvestUnitValidator-Mainnet.addr")
    ------------------------------
    fundFactoryDeployParams <- deploy_FundFactory (path SystemFilePathPosix.</> protocolName) "fund-factory"
    ------------------------------
    MonadIOClass.liftIO $ P.putStrLn "Creating 'Protocol Deploy' File..."
    ------------------------------
    let
        protocolDeployParams =
            T.ProtocolDeployParams
                { T.pdpProtocolVersion = ProtocolT.protocolVersion
                , T.pdpTokenEmergencyAdmin_CS = tokenEmergencyAdminPolicy_CS
                , T.pdpProtocolPolicyID_Params = protocolPolicyParams
                , T.pdpProtocolPolicyID_CborHex = OffChainHelpers.lazyByteStringToString protocolPolicyID_CborHex
                , T.pdpProtocolPolicyID_CS = protocolPolicyID_CS
                , T.pdpProtocolValidator_Params = protocolValidatorParams
                , T.pdpProtocolValidator_Hash = protocolValidator_Hash
                , T.pdpProtocolValidator_CborHex = OffChainHelpers.lazyByteStringToString protocolValidator_CborHex
                , T.pdpProtocolValidator_AddressTestnet = OffChainHelpers.lazyByteStringToString protocolValidator_Address_Testnet
                , T.pdpProtocolValidator_AddressMainnet = OffChainHelpers.lazyByteStringToString protocolValidator_Address_Mainnet
                , T.pdpScriptPolicyID_Params = scriptPolicyParams
                , T.pdpScriptPolicyID_CborHex = OffChainHelpers.lazyByteStringToString scriptPolicyID_CborHex
                , T.pdpScriptPolicyID_CS = scriptPolicyID_CS
                , T.pdpScriptValidator_Params = scriptValidatorParams
                , T.pdpScriptValidator_Hash = scriptValidator_Hash
                , T.pdpScriptValidator_CborHex = OffChainHelpers.lazyByteStringToString scriptValidator_CborHex
                , T.pdpScriptValidator_AddressTestnet = OffChainHelpers.lazyByteStringToString scriptValidator_Address_Testnet
                , T.pdpScriptValidator_AddressMainnet = OffChainHelpers.lazyByteStringToString scriptValidator_Address_Mainnet
                , T.pdpSwapOfferPolicyID_Params = swapOfferPolicyParams
                , T.pdpSwapOfferPolicyID_CborHex = OffChainHelpers.lazyByteStringToString swapOfferPolicyID_CborHex
                , T.pdpSwapOfferPolicyID_CS = swapOfferPolicyID_CS
                , T.pdpSwapOfferValidator_Params = swapOfferValidatorParams
                , T.pdpSwapOfferValidator_Hash = swapOfferValidator_Hash
                , T.pdpSwapOfferValidator_CborHex = OffChainHelpers.lazyByteStringToString swapOfferValidator_CborHex
                , T.pdpSwapOfferValidator_AddressTestnet = OffChainHelpers.lazyByteStringToString swapOfferValidator_Address_Testnet
                , T.pdpSwapOfferValidator_AddressMainnet = OffChainHelpers.lazyByteStringToString swapOfferValidator_Address_Mainnet
                , T.pdpBuyOrderPolicyID_Params = buyOrderPolicyParams
                , T.pdpBuyOrderPolicyID_CborHex = OffChainHelpers.lazyByteStringToString buyOrderPolicyID_CborHex
                , T.pdpBuyOrderPolicyID_CS = buyOrderPolicyID_CS
                , T.pdpBuyOrderValidator_Params = buyOrderValidatorParams
                , T.pdpBuyOrderValidator_Hash = buyOrderValidator_Hash
                , T.pdpBuyOrderValidator_CborHex = OffChainHelpers.lazyByteStringToString buyOrderValidator_CborHex
                , T.pdpBuyOrderValidator_AddressTestnet = OffChainHelpers.lazyByteStringToString buyOrderValidator_Address_Testnet
                , T.pdpBuyOrderValidator_AddressMainnet = OffChainHelpers.lazyByteStringToString buyOrderValidator_Address_Mainnet
                , T.pdpDelegationPolicyID_Params = delegationPolicyParams
                , T.pdpDelegationPolicyID_CborHex = OffChainHelpers.lazyByteStringToString delegationPolicyID_CborHex
                , T.pdpDelegationPolicyID_CS = delegationPolicyID_CS
                , T.pdpDelegationValidator_Params = delegationValidatorParams
                , T.pdpDelegationValidator_Hash = delegationValidator_Hash
                , T.pdpDelegationValidator_CborHex = OffChainHelpers.lazyByteStringToString delegationValidator_CborHex
                , T.pdpDelegationValidator_AddressTestnet = OffChainHelpers.lazyByteStringToString delegationValidator_Address_Testnet
                , T.pdpDelegationValidator_AddressMainnet = OffChainHelpers.lazyByteStringToString delegationValidator_Address_Mainnet
                , T.pdpInvestUnitValidator_Params = investUnitValidatorParams
                , T.pdpInvestUnitValidator_Hash = investUnitValidator_Hash
                , T.pdpInvestUnitValidator_CborHex = OffChainHelpers.lazyByteStringToString investUnitValidator_CborHex
                , T.pdpInvestUnitValidator_AddressTestnet = OffChainHelpers.lazyByteStringToString investUnitValidator_Address_Testnet
                , T.pdpInvestUnitValidator_AddressMainnet = OffChainHelpers.lazyByteStringToString investUnitValidator_Address_Mainnet
                , T.pdpFundFactoryDeployParams = [fundFactoryDeployParams]
                }
    OffChainHelpers.writeEncodedToFile (path SystemFilePathPosix.</> protocolName SystemFilePathPosix.</> "ProtocolDeploy.json") protocolDeployParams
    ------------------------------
    P.putStrLn $ "Saved Protocol Deploy in: " ++ P.show (path SystemFilePathPosix.</> protocolName SystemFilePathPosix.</> "ProtocolDeploy.json")
    P.putStrLn "--------------------------------"
    return protocolDeployParams

--------------------------------------------------------------------------------2

-- Para obtener pab file de un fondo de un protocolo especifico
deploy_FundPAB_With_RequestingParams :: T.CS -> LedgerApiV2.TxOutRef -> P.IO T.FundPABParams
deploy_FundPAB_With_RequestingParams protocolPolicyID_CS fundPolicy_TxOutRef = do
    ------------------------------
    let
        path = "export/funds-v" ++ P.show FundT.fundVersion
    ------------------------------
    -- Get the current time
    currentTime <- MonadIOClass.liftIO DataTime.getCurrentTime
    -- Format the time. "%Y-%m-%d-%H-%M" corresponds to "yyyy-mm-dd-hh-mm"
    let
        defaultName = DataTime.formatTime DataTime.defaultTimeLocale "%Y-%m-%d-%H-%M" currentTime
    ------------------------------
    P.putStrLn $ "Fund Name (default=" ++ defaultName ++ "-pab):"
    !fundName <- MonadIOClass.liftIO $ CLIHelpers.getStrWithDefault (defaultName ++ "-pab")
    ------------------------------
    -- let
    --     getToken_AC :: P.IO LedgerValue.AssetClass
    --     getToken_AC = do
    --         MonadIOClass.liftIO $ P.putStrLn "MAYZ Token"
    --         cs <-  MonadIOClass.liftIO $ CLIHelpers.getCurrencySymbol "6aa8df2a10e8530338279e41a1a40dc73f83e468b710c3b64082f759"
    --         tn <-  MonadIOClass.liftIO $ CLIHelpers.getTokenName "MAYZ" False
    --         return $ LedgerValue.AssetClass (cs,tn)
    -- ---------------------
    -- tokenMAYZ_AC <- getToken_AC
    ------------------------------
    MonadIOClass.liftIO $ P.putStrLn "Token Emergency Admin Policy CS"
    tokenEmergencyAdminPolicy_CS <- MonadIOClass.liftIO $ CLIHelpers.getCurrencySymbol "6aa8df2a10e8530338279e41a1a40dc73f83e468b710c3b64082f759"
    ------------------------------
    deploy_FundPAB protocolPolicyID_CS fundPolicy_TxOutRef tokenEmergencyAdminPolicy_CS path fundName

-- Para obtener pab file de un fondo de un protocolo especifico
deploy_FundPAB :: T.CS -> LedgerApiV2.TxOutRef -> LedgerApiV2.CurrencySymbol -> P.FilePath -> P.String -> P.IO T.FundPABParams
deploy_FundPAB protocolPolicyID_CS fundPolicy_TxOutRef tokenEmergencyAdminPolicy_CS path fundName =
    do
        SystemDirectory.removePathForcibly (path SystemFilePathPosix.</> fundName)
        SystemDirectory.createDirectoryIfMissing True (path SystemFilePathPosix.</> fundName)
        ------------------------------
        MonadIOClass.liftIO $ P.putStrLn "Generating Fund PAB Params..."
        ------------------------------
        P.putStrLn $ "Path: " ++ path SystemFilePathPosix.</> fundName
        ------------------------------
        MonadIOClass.liftIO $ P.putStrLn "Generating 'Fund Validator' Script..."
        let
            fundValidatorParams =
                FundT.ValidatorParams
                    { FundT.vpProtocolPolicyID_CS = protocolPolicyID_CS
                    , FundT.vpTokenEmergencyAdminPolicy_CS = tokenEmergencyAdminPolicy_CS
                    }
            fundValidator = FundOnChain.validator fundValidatorParams
            fundValidator_Hash = OffChainHelpers.hashValidator fundValidator
            fundValidator_Address = OffChainHelpers.addressValidator fundValidator_Hash
        _ <- MonadIOClass.liftIO $ DeployHelpers.deployValidator (path SystemFilePathPosix.</> fundName) "FundValidator" fundValidator
        _ <- MonadIOClass.liftIO $ DeployHelpers.deployValidatorHash (path SystemFilePathPosix.</> fundName) "FundValidator" fundValidator_Hash
        _ <- MonadIOClass.liftIO $ DeployHelpers.deployValidatorAddress (path SystemFilePathPosix.</> fundName) "FundValidator" fundValidator_Address
        ------------------------------
        MonadIOClass.liftIO $ P.putStrLn "Generating 'Fund Policy' Script..."
        let
            fundPolicyParams =
                FundT.PolicyParams
                    { FundT.ppProtocolPolicyID_CS = protocolPolicyID_CS
                    , FundT.ppFundPolicy_TxOutRef = fundPolicy_TxOutRef
                    , FundT.ppFundValidator_Hash = fundValidator_Hash
                    }
            fundPolicy = FundOnChain.policy fundPolicyParams
            fundPolicy_CS = OffChainHelpers.getCurSymbolOfPolicy fundPolicy
        _ <- MonadIOClass.liftIO $ P.print fundPolicyParams
        _ <- MonadIOClass.liftIO $ DeployHelpers.deployMintingPolicy (path SystemFilePathPosix.</> fundName) "FundPolicy" fundPolicy fundPolicy_CS
        ------------------------------
        MonadIOClass.liftIO $ P.putStrLn "Generating 'FundHolding PolicyID' Script..."
        let
            fundHoldingPolicyParams =
                FundHoldingT.PolicyParams
                    { FundHoldingT.ppFundPolicy_CS = fundPolicy_CS
                    }
            fundHoldingPolicyID = FundHoldingOnChain.policyID fundHoldingPolicyParams
            fundHoldingPolicyID_CS = OffChainHelpers.getCurSymbolOfPolicy fundHoldingPolicyID
        _ <- MonadIOClass.liftIO $ DeployHelpers.deployMintingPolicy (path SystemFilePathPosix.</> fundName) "FundHoldingPolicyID" fundHoldingPolicyID fundHoldingPolicyID_CS
        ------------------------------
        MonadIOClass.liftIO $ P.putStrLn "Generating 'FundHolding Validator' Script..."
        let
            fundHoldingValidatorParams =
                FundHoldingT.ValidatorParams
                    { FundHoldingT.vpProtocolPolicyID_CS = protocolPolicyID_CS
                    , FundHoldingT.vpFundPolicy_CS = fundPolicy_CS
                    , FundHoldingT.vpTokenEmergencyAdminPolicy_CS = tokenEmergencyAdminPolicy_CS
                    }
            fundHoldingValidator = FundHoldingOnChain.validator fundHoldingValidatorParams
            fundHoldingValidator_Hash = OffChainHelpers.hashValidator fundHoldingValidator
            fundHoldingValidator_Address = OffChainHelpers.addressValidator fundHoldingValidator_Hash
        _ <- MonadIOClass.liftIO $ DeployHelpers.deployValidator (path SystemFilePathPosix.</> fundName) "FundHoldingValidator" fundHoldingValidator
        _ <- MonadIOClass.liftIO $ DeployHelpers.deployValidatorHash (path SystemFilePathPosix.</> fundName) "FundHoldingValidator" fundHoldingValidator_Hash
        _ <- MonadIOClass.liftIO $ DeployHelpers.deployValidatorAddress (path SystemFilePathPosix.</> fundName) "FundHoldingValidator" fundHoldingValidator_Address
        ------------------------------
        MonadIOClass.liftIO $ P.putStrLn "Creating 'Fund PAB Params' File..."
        let
            fundPABParams =
                T.FundPABParams
                    { T.fppFundVersion = FundT.fundVersion
                    , T.fppFundPolicy_Params = fundPolicyParams
                    , T.fppFundPolicy = fundPolicy
                    , T.fppFundPolicy_CS = fundPolicy_CS
                    , T.fppFundValidator_Params = fundValidatorParams
                    , T.fppFundValidator = fundValidator
                    , T.fppFundValidator_Hash = fundValidator_Hash
                    , T.fppFundValidator_Address = fundValidator_Address
                    , T.fppFundHoldingPolicyID_Params = fundHoldingPolicyParams
                    , T.fppFundHoldingPolicyID = fundHoldingPolicyID
                    , T.fppFundHoldingPolicyID_CS = fundHoldingPolicyID_CS
                    , T.fppFundHoldingValidator_Params = fundHoldingValidatorParams
                    , T.fppFundHoldingValidator = fundHoldingValidator
                    , T.fppFundHoldingValidator_Hash = fundHoldingValidator_Hash
                    , T.fppFundHoldingValidator_Address = fundHoldingValidator_Address
                    }
        OffChainHelpers.writeEncodedToFile (path SystemFilePathPosix.</> fundName SystemFilePathPosix.</> "FundPAB.json") fundPABParams
        P.putStrLn $ "Saved Fund PAB Param in: " ++ P.show (path SystemFilePathPosix.</> fundName SystemFilePathPosix.</> "FundPAB.json")
        ------------------------------
        return fundPABParams

--------------------------------------------------------------------------------2

-- Para obtener deploy files de un fondo de un protocolo especifico
deploy_Fund_With_RequestingParams :: T.CS -> LedgerApiV2.TxOutRef -> P.IO T.FundDeployParams
deploy_Fund_With_RequestingParams protocolPolicyID_CS fundPolicy_TxOutRef = do
    let
        path = "export/funds-v" ++ P.show FundT.fundVersion
    ------------------------------
    -- Get the current time
    currentTime <- MonadIOClass.liftIO DataTime.getCurrentTime
    -- Format the time. "%Y-%m-%d-%H-%M" corresponds to "yyyy-mm-dd-hh-mm"
    let
        defaultName = DataTime.formatTime DataTime.defaultTimeLocale "%Y-%m-%d-%H-%M" currentTime
    ------------------------------
    P.putStrLn $ "Fund Name (default=" ++ defaultName ++ "):"
    !fundName <- MonadIOClass.liftIO $ CLIHelpers.getStrWithDefault defaultName
    ------------------------------
    -- let
    --     getToken_AC :: P.IO LedgerValue.AssetClass
    --     getToken_AC = do
    --         MonadIOClass.liftIO $ P.putStrLn "MAYZ Token"
    --         cs <-  MonadIOClass.liftIO $ CLIHelpers.getCurrencySymbol "6aa8df2a10e8530338279e41a1a40dc73f83e468b710c3b64082f759"
    --         tn <-  MonadIOClass.liftIO $ CLIHelpers.getTokenName "MAYZ" False
    --         return $ LedgerValue.AssetClass (cs,tn)
    -- ---------------------
    -- tokenMAYZ_AC <- getToken_AC
    ------------------------------
    MonadIOClass.liftIO $ P.putStrLn "Token Emergency Admin Policy CS"
    tokenEmergencyAdminPolicy_CS <- MonadIOClass.liftIO $ CLIHelpers.getCurrencySymbol "6aa8df2a10e8530338279e41a1a40dc73f83e468b710c3b64082f759"
    ------------------------------
    deploy_Fund protocolPolicyID_CS fundPolicy_TxOutRef tokenEmergencyAdminPolicy_CS path fundName

-- Para obtener deploy files de un fondo de un protocolo especifico
deploy_Fund :: T.CS -> LedgerApiV2.TxOutRef -> LedgerApiV2.CurrencySymbol -> P.FilePath -> P.String -> P.IO T.FundDeployParams
deploy_Fund protocolPolicyID_CS fundPolicy_TxOutRef tokenEmergencyAdminPolicy_CS path fundName =
    do
        SystemDirectory.removePathForcibly (path SystemFilePathPosix.</> fundName)
        SystemDirectory.createDirectoryIfMissing True (path SystemFilePathPosix.</> fundName)
        ------------------------------
        MonadIOClass.liftIO $ P.putStrLn "Generating Fund Deploy File..."
        ------------------------------
        P.putStrLn $ "Path: " ++ path SystemFilePathPosix.</> fundName
        ------------------------------
        let
            fundValidatorParams =
                FundT.ValidatorParams
                    { FundT.vpProtocolPolicyID_CS = protocolPolicyID_CS
                    , FundT.vpTokenEmergencyAdminPolicy_CS = tokenEmergencyAdminPolicy_CS
                    }
            fundValidator = FundOnChain.validator fundValidatorParams
            fundValidator_Hash = OffChainHelpers.hashValidator fundValidator
            fundValidator_Address = OffChainHelpers.addressValidator fundValidator_Hash
        _ <- MonadIOClass.liftIO $ DeployHelpers.deployValidator (path SystemFilePathPosix.</> fundName) "FundValidator" fundValidator
        _ <- MonadIOClass.liftIO $ DeployHelpers.deployValidatorHash (path SystemFilePathPosix.</> fundName) "FundValidator" fundValidator_Hash
        _ <- MonadIOClass.liftIO $ DeployHelpers.deployValidatorAddress (path SystemFilePathPosix.</> fundName) "FundValidator" fundValidator_Address
        ------------------------------
        MonadIOClass.liftIO $ P.putStrLn "Generating 'Fund Policy' Script..."
        let
            fundPolicyParams =
                FundT.PolicyParams
                    { FundT.ppProtocolPolicyID_CS = protocolPolicyID_CS
                    , FundT.ppFundPolicy_TxOutRef = fundPolicy_TxOutRef
                    , FundT.ppFundValidator_Hash = fundValidator_Hash
                    }
            fundPolicy = FundOnChain.policy fundPolicyParams
            fundPolicy_CS = OffChainHelpers.getCurSymbolOfPolicy fundPolicy
        _ <- MonadIOClass.liftIO $ P.print fundPolicyParams
        _ <- MonadIOClass.liftIO $ DeployHelpers.deployMintingPolicy (path SystemFilePathPosix.</> fundName) "FundPolicy" fundPolicy fundPolicy_CS
        ------------------------------
        MonadIOClass.liftIO $ P.putStrLn "Generating 'FundHolding PolicyID' Script..."
        let
            fundHoldingPolicyParams =
                FundHoldingT.PolicyParams
                    { FundHoldingT.ppFundPolicy_CS = fundPolicy_CS
                    }
            fundHoldingPolicyID = FundHoldingOnChain.policyID fundHoldingPolicyParams
            fundHoldingPolicyID_CS = OffChainHelpers.getCurSymbolOfPolicy fundHoldingPolicyID
        _ <- MonadIOClass.liftIO $ DeployHelpers.deployMintingPolicy (path SystemFilePathPosix.</> fundName) "FundHoldingPolicyID" fundHoldingPolicyID fundHoldingPolicyID_CS
        ------------------------------
        MonadIOClass.liftIO $ P.putStrLn "Generating 'FundHolding Validator' Script..."
        let
            fundHoldingValidatorParams =
                FundHoldingT.ValidatorParams
                    { FundHoldingT.vpProtocolPolicyID_CS = protocolPolicyID_CS
                    , FundHoldingT.vpFundPolicy_CS = fundPolicy_CS
                    , FundHoldingT.vpTokenEmergencyAdminPolicy_CS = tokenEmergencyAdminPolicy_CS
                    }
            fundHoldingValidator = FundHoldingOnChain.validator fundHoldingValidatorParams
            fundHoldingValidator_Hash = OffChainHelpers.hashValidator fundHoldingValidator
            fundHoldingValidator_Address = OffChainHelpers.addressValidator fundHoldingValidator_Hash
        _ <- MonadIOClass.liftIO $ DeployHelpers.deployValidator (path SystemFilePathPosix.</> fundName) "FundHoldingValidator" fundHoldingValidator
        _ <- MonadIOClass.liftIO $ DeployHelpers.deployValidatorHash (path SystemFilePathPosix.</> fundName) "FundHoldingValidator" fundHoldingValidator_Hash
        _ <- MonadIOClass.liftIO $ DeployHelpers.deployValidatorAddress (path SystemFilePathPosix.</> fundName) "FundHoldingValidator" fundHoldingValidator_Address
        ------------------------------
        MonadIOClass.liftIO $ P.putStrLn "Creating 'Fund Deploy' File..."
        ------------------------------
        fundPolicy_CborHex <- OffChainHelpers.readFile (path SystemFilePathPosix.</> fundName SystemFilePathPosix.</> "FundPolicy.plutus")
        ------------------------------
        fundValidator_CborHex <- OffChainHelpers.readFile (path SystemFilePathPosix.</> fundName SystemFilePathPosix.</> "FundValidator.plutus")
        fundValidator_Address_Testnet <- OffChainHelpers.readFile (path SystemFilePathPosix.</> fundName SystemFilePathPosix.</> "FundValidator-Testnet.addr")
        fundValidator_Address_Mainnet <- OffChainHelpers.readFile (path SystemFilePathPosix.</> fundName SystemFilePathPosix.</> "FundValidator-Mainnet.addr")
        ------------------------------
        fundHoldingPolicyID_CborHex <- OffChainHelpers.readFile (path SystemFilePathPosix.</> fundName SystemFilePathPosix.</> "FundHoldingPolicyID.plutus")
        fundHoldingValidator_CborHex <- OffChainHelpers.readFile (path SystemFilePathPosix.</> fundName SystemFilePathPosix.</> "FundHoldingValidator.plutus")
        fundHoldingValidator_Address_Testnet <- OffChainHelpers.readFile (path SystemFilePathPosix.</> fundName SystemFilePathPosix.</> "FundHoldingValidator-Testnet.addr")
        fundHoldingValidator_Address_Mainnet <- OffChainHelpers.readFile (path SystemFilePathPosix.</> fundName SystemFilePathPosix.</> "FundHoldingValidator-Mainnet.addr")
        ------------------------------
        let
            fundDeployParams =
                T.FundDeployParams
                    { T.fdpFundVersion = FundT.fundVersion
                    , T.fdpFundPolicy_Params = fundPolicyParams
                    , T.fdpFundPolicy_CborHex = OffChainHelpers.lazyByteStringToString fundPolicy_CborHex
                    , T.fdpFundPolicy_CS = fundPolicy_CS
                    , T.fdpFundValidator_Params = fundValidatorParams
                    , T.fdpFundValidator_Hash = fundValidator_Hash
                    , T.fdpFundValidator_CborHex = OffChainHelpers.lazyByteStringToString fundValidator_CborHex
                    , T.fdpFundValidator_AddressTestnet = OffChainHelpers.lazyByteStringToString fundValidator_Address_Testnet
                    , T.fdpFundValidator_AddressMainnet = OffChainHelpers.lazyByteStringToString fundValidator_Address_Mainnet
                    , T.fdpFundHoldingPolicyID_Params = fundHoldingPolicyParams
                    , T.fdpFundHoldingPolicyID_CborHex = OffChainHelpers.lazyByteStringToString fundHoldingPolicyID_CborHex
                    , T.fdpFundHoldingPolicyID_CS = fundHoldingPolicyID_CS
                    , T.fdpFundHoldingValidator_Params = fundHoldingValidatorParams
                    , T.fdpFundHoldingValidator_Hash = fundHoldingValidator_Hash
                    , T.fdpFundHoldingValidator_CborHex = OffChainHelpers.lazyByteStringToString fundHoldingValidator_CborHex
                    , T.fdpFundHoldingValidator_AddressTestnet = OffChainHelpers.lazyByteStringToString fundHoldingValidator_Address_Testnet
                    , T.fdpFundHoldingValidator_AddressMainnet = OffChainHelpers.lazyByteStringToString fundHoldingValidator_Address_Mainnet
                    }
        OffChainHelpers.writeEncodedToFile (path SystemFilePathPosix.</> fundName SystemFilePathPosix.</> "FundDeploy.json") fundDeployParams
        ------------------------------
        P.putStrLn $ "Saved Fund Deploy in: " ++ P.show (path SystemFilePathPosix.</> fundName SystemFilePathPosix.</> "FundDeploy.json")
        P.putStrLn "--------------------------------"
        ------------------------------
        return fundDeployParams

--------------------------------------------------------------------------------2

-- para obtener files de un contrato especifico con scriptParams generico aceptando una cantidad variable de parametros para ajustarse a cada contrato especifico
deploy_FundContract :: P.String -> SystemFilePath.FilePath -> [P.String] -> P.IO ()
deploy_FundContract contractName contractSaveFilePath scriptParams =
    do
        ------------------------------
        let
            dirPath = SystemFilePath.takeDirectory contractSaveFilePath
            filename = SystemFilePath.takeFileName contractSaveFilePath
        ------------------------------
        -- SystemDirectory.removePathForcibly dirPath
        -- SystemDirectory.createDirectoryIfMissing True dirPath
        ------------------------------
        P.putStrLn "Generating Fund Contract File..."
        ------------------------------
        P.putStrLn $ "Path: " ++ contractSaveFilePath
        ------------------------------
        _ <- case contractName of
            "FundValidator" -> do
                P.putStrLn "Generating 'Fund Validator' Script..."
                DeployHelpers.paramsToDataShow scriptParams 0
                let
                    params0 = DeployHelpers.paramsIndexToBuiltinData scriptParams 0
                    params1 = DeployHelpers.paramsIndexToBuiltinData scriptParams 1
                    fundValidatorCode =
                        FundOnChain.validatorCode
                            `PlutusTx.applyCode` PlutusTx.liftCode params0
                            `PlutusTx.applyCode` PlutusTx.liftCode params1
                    !fundValidator = DeployHelpers.applyPlutononyToValidatorCode fundValidatorCode
                _ <- OffChainHelpers.writeValidator dirPath filename fundValidator
                P.putStrLn $ "Saved Fund Contract in: " ++ P.show contractSaveFilePath
            "FundPolicy" -> do
                P.putStrLn "Generating 'Fund Policy' Script..."
                let
                    params0 = DeployHelpers.paramsIndexToBuiltinData scriptParams 0
                    params1 = DeployHelpers.paramsIndexToBuiltinData scriptParams 1
                    params2 = DeployHelpers.paramsIndexToBuiltinData scriptParams 2
                    params3 = DeployHelpers.paramsIndexToBuiltinData scriptParams 3
                    fundPolicyCode =
                        FundOnChain.policyCode
                            `PlutusTx.applyCode` PlutusTx.liftCode params0
                            `PlutusTx.applyCode` PlutusTx.liftCode params1
                            `PlutusTx.applyCode` PlutusTx.liftCode params2
                            `PlutusTx.applyCode` PlutusTx.liftCode params3
                    fundPolicy = DeployHelpers.applyPlutononyToMintingPolicyCode fundPolicyCode
                _ <- OffChainHelpers.writeMintingPolicy dirPath filename fundPolicy
                P.putStrLn $ "Saved Fund Contract in: " ++ P.show contractSaveFilePath
            "FundHoldingPolicyID" -> do
                P.putStrLn "Generating 'FundHolding PolicyID' Script..."
                let
                    params0 = DeployHelpers.paramsIndexToBuiltinData scriptParams 0
                    fundHoldingPolicyIDCode =
                        FundHoldingOnChain.policyIDCode
                            `PlutusTx.applyCode` PlutusTx.liftCode params0
                    fundHoldingPolicyID = DeployHelpers.applyPlutononyToMintingPolicyCode fundHoldingPolicyIDCode
                _ <- OffChainHelpers.writeMintingPolicy dirPath filename fundHoldingPolicyID
                P.putStrLn $ "Saved Fund Contract in: " ++ P.show contractSaveFilePath
            "FundHoldingValidator" -> do
                P.putStrLn "Generating 'FundHolding Validator' Script..."
                let
                    params0 = DeployHelpers.paramsIndexToBuiltinData scriptParams 0
                    params1 = DeployHelpers.paramsIndexToBuiltinData scriptParams 1
                    params2 = DeployHelpers.paramsIndexToBuiltinData scriptParams 2
                    fundHoldingValidatorCode =
                        FundHoldingOnChain.validatorCode
                            `PlutusTx.applyCode` PlutusTx.liftCode params0
                            `PlutusTx.applyCode` PlutusTx.liftCode params1
                            `PlutusTx.applyCode` PlutusTx.liftCode params2
                    fundHoldingValidator = DeployHelpers.applyPlutononyToValidatorCode fundHoldingValidatorCode
                _ <- OffChainHelpers.writeValidator dirPath filename fundHoldingValidator
                P.putStrLn $ "Saved Fund Contract in: " ++ P.show contractSaveFilePath
            _ -> P.error "Smart Contract Unkwonw"
        ------------------------------
        P.putStrLn "--------------------------------"

--------------------------------------------------------------------------------2

-- para obtener fund factory files
deploy_FundFactory_With_RequestingParams :: P.IO T.FundFactoryDeployParams
deploy_FundFactory_With_RequestingParams = do
    let
        path = "export/funds-factories-v" ++ P.show FundT.fundVersion
    ------------------------------
    -- Get the current time
    currentTime <- MonadIOClass.liftIO DataTime.getCurrentTime
    -- Format the time. "%Y-%m-%d-%H-%M" corresponds to "yyyy-mm-dd-hh-mm"
    let
        defaultName = DataTime.formatTime DataTime.defaultTimeLocale "%Y-%m-%d-%H-%M" currentTime
    ------------------------------
    P.putStrLn $ "Fund Factory Name (default=" ++ defaultName ++ "):"
    !fundFactoryName <- MonadIOClass.liftIO $ CLIHelpers.getStrWithDefault defaultName
    ------------------------------
    deploy_FundFactory path fundFactoryName

deploy_FundFactory :: P.FilePath -> P.String -> P.IO T.FundFactoryDeployParams
deploy_FundFactory path fundFactoryName =
    do
        SystemDirectory.removePathForcibly (path SystemFilePathPosix.</> fundFactoryName)
        SystemDirectory.createDirectoryIfMissing True (path SystemFilePathPosix.</> fundFactoryName)
        ------------------------------
        MonadIOClass.liftIO $ P.putStrLn "Generating Fund Factory Deploy File..."
        ------------------------------
        P.putStrLn $ "Path: " ++ path SystemFilePathPosix.</> fundFactoryName
        ------------------------------
        MonadIOClass.liftIO $ P.putStrLn "Generating 'Fund Validator' Pre-Script..."
        let
            fundValidator = Plutonomy.optimizeUPLC FundOnChain.validatorCode
        DeployHelpers.writeCompiledCodeToJsonFile (path SystemFilePathPosix.</> fundFactoryName SystemFilePathPosix.</> "FundValidator_PRE.plutus") fundValidator
        -- DeployHelpers.writeCompiledCodeToBinaryFile (path SystemFilePathPosix.</> fundFactoryName SystemFilePathPosix.</> "FundValidator_PRE.serialized") fundValidator
        ------------------------------
        -- fundValidatorTest <- DeployHelpers.readCompiledCodeFromBinaryFile (path SystemFilePathPosix.</> fundFactoryName SystemFilePathPosix.</> "FundValidator_PRE_SERIALIZED.plutus")
        -- DeployHelpers.writeCompiledCodeToJsonFile (path SystemFilePathPosix.</> fundFactoryName SystemFilePathPosix.</> "FundValidator_PRE_Test.plutus") fundValidatorTest
        ------------------------------
        MonadIOClass.liftIO $ P.putStrLn "Generating 'Fund Policy' Pre-Script..."
        let
            fundPolicy = Plutonomy.optimizeUPLC FundOnChain.policyCode
        DeployHelpers.writeCompiledCodeToJsonFile (path SystemFilePathPosix.</> fundFactoryName SystemFilePathPosix.</> "FundPolicy_PRE.plutus") fundPolicy
        -- DeployHelpers.writeCompiledCodeToBinaryFile (path SystemFilePathPosix.</> fundFactoryName SystemFilePathPosix.</> "FundPolicy_PRE.serialized") fundPolicy
        ------------------------------
        MonadIOClass.liftIO $ P.putStrLn "Generating 'FundHolding PolicyID' Pre-Script..."
        let
            fundHoldingPolicyID = Plutonomy.optimizeUPLC FundHoldingOnChain.policyIDCode
        DeployHelpers.writeCompiledCodeToJsonFile (path SystemFilePathPosix.</> fundFactoryName SystemFilePathPosix.</> "FundHoldingPolicyID_PRE.plutus") fundHoldingPolicyID
        -- DeployHelpers.writeCompiledCodeToBinaryFile (path SystemFilePathPosix.</> fundFactoryName SystemFilePathPosix.</> "FundHoldingPolicyID_PRE.serialized") fundHoldingPolicyID
        ------------------------------
        MonadIOClass.liftIO $ P.putStrLn "Generating 'FundHolding Validator' Pre-Script..."
        let
            fundHoldingValidator = Plutonomy.optimizeUPLC FundHoldingOnChain.validatorCode
        DeployHelpers.writeCompiledCodeToJsonFile (path SystemFilePathPosix.</> fundFactoryName SystemFilePathPosix.</> "FundHoldingValidator_PRE.plutus") fundHoldingValidator
        -- DeployHelpers.writeCompiledCodeToBinaryFile (path SystemFilePathPosix.</> fundFactoryName SystemFilePathPosix.</> "FundHoldingValidator_PRE.serialized") fundHoldingValidator
        ------------------------------
        MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
        MonadIOClass.liftIO $ P.putStrLn "Creating 'FundFactory Deploy' File..."
        ------------------------------
        fundPolicy_Pre_CborHex <- OffChainHelpers.readFile (path SystemFilePathPosix.</> fundFactoryName SystemFilePathPosix.</> "FundPolicy_PRE.plutus")
        fundValidator_Pre_CborHex <- OffChainHelpers.readFile (path SystemFilePathPosix.</> fundFactoryName SystemFilePathPosix.</> "FundValidator_PRE.plutus")
        fundHoldingPolicyID_Pre_CborHex <- OffChainHelpers.readFile (path SystemFilePathPosix.</> fundFactoryName SystemFilePathPosix.</> "FundHoldingPolicyID_PRE.plutus")
        fundHoldingValidator_Pre_CborHex <- OffChainHelpers.readFile (path SystemFilePathPosix.</> fundFactoryName SystemFilePathPosix.</> "FundHoldingValidator_PRE.plutus")
        ------------------------------
        -- fundPolicy_PreScript_Serialized <- OffChainHelpers.readFile (path SystemFilePathPosix.</> fundFactoryName SystemFilePathPosix.</> "FundPolicy_PRE.serialized")
        -- fundValidator_PreScript_Serialized <- OffChainHelpers.readFile (path SystemFilePathPosix.</> fundFactoryName SystemFilePathPosix.</> "FundValidator_PRE.serialized")
        -- fundHoldingPolicyID_PreScript_Serialized <- OffChainHelpers.readFile (path SystemFilePathPosix.</> fundFactoryName SystemFilePathPosix.</> "FundHoldingPolicyID_PRE.serialized")
        -- fundHoldingValidator_PreScript_Serialized <- OffChainHelpers.readFile (path SystemFilePathPosix.</> fundFactoryName SystemFilePathPosix.</> "FundHoldingValidator_PRE.serialized")
        -- investUnitValidator_PreScript_Serialized <- OffChainHelpers.readFile (path SystemFilePathPosix.</> fundFactoryName SystemFilePathPosix.</> "InvestUnitValidator_PRE.serialized")
        ------------------------------
        let
            fundFactoryDeployParams =
                T.FundFactoryDeployParams
                    { T.ffdpFundVersion = FundT.fundVersion
                    , T.ffdpFundPolicy_Pre_CborHex = OffChainHelpers.lazyByteStringToString fundPolicy_Pre_CborHex
                    , T.ffdpFundValidator_Pre_CborHex = OffChainHelpers.lazyByteStringToString fundValidator_Pre_CborHex
                    , T.ffdpFundHoldingPolicyID_Pre_CborHex = OffChainHelpers.lazyByteStringToString fundHoldingPolicyID_Pre_CborHex
                    , T.ffdpFundHoldingValidator_Pre_CborHex = OffChainHelpers.lazyByteStringToString fundHoldingValidator_Pre_CborHex
                    -- T.ffdpFundPolicy_PreScript_Serialized           = OffChainHelpers.lazyByteStringToBase64String fundPolicy_PreScript_Serialized,
                    -- T.ffdpFundValidator_PreScript_Serialized= OffChainHelpers.lazyByteStringToBase64String fundValidator_PreScript_Serialized,
                    -- T.ffdpFundHoldingPolicyID_PreScript_Serialized    = OffChainHelpers.lazyByteStringToBase64String fundHoldingPolicyID_PreScript_Serialized,
                    -- T.ffdpFundHoldingValidator_PreScript_Serialized   = OffChainHelpers.lazyByteStringToBase64String fundHoldingValidator_PreScript_Serialized,
                    -- T.ffdpInvestUnitValidator_PreScript_Serialized= OffChainHelpers.lazyByteStringToBase64String investUnitValidator_PreScript_Serialized
                    }
        OffChainHelpers.writeEncodedToFile (path SystemFilePathPosix.</> fundFactoryName SystemFilePathPosix.</> "FundFactoryDeploy.json") fundFactoryDeployParams
        ------------------------------
        P.putStrLn $ "Saved Fund Factory Deploy in: " ++ P.show (path SystemFilePathPosix.</> fundFactoryName SystemFilePathPosix.</> "FundFactoryDeploy.json")
        P.putStrLn "--------------------------------"
        ------------------------------
        return fundFactoryDeployParams

------------------------------------------------------------------------------2

deploy_PRE_script :: P.FilePath -> P.String -> Bool -> PlutusTx.CompiledCode a -> P.IO ()
deploy_PRE_script filePath name swOverWrite code = do
    fileExists <- SystemDirectory.doesFileExist filePath
    if fileExists && not swOverWrite
        then do
            P.putStrLn $ "Reading '" ++ name ++ "' Pre-Script..."
        else do
            P.putStrLn $ "Generating '" ++ name ++ "' Pre-Script..."
            let
                !optimizedCode = Plutonomy.optimizeUPLC code
            -- DeployHelpers.writeCompiledCodeToBinaryFile filePath optimizedCode
            DeployHelpers.writeCompiledCodeToJsonFile filePath optimizedCode

deploy_PRE :: P.FilePath -> P.String -> Bool -> P.IO T.DeployAllPreParams
deploy_PRE path name swOverWrite = do
    ------------------------------
    -- SystemDirectory.removePathForcibly (path SystemFilePathPosix.</> name)
    SystemDirectory.createDirectoryIfMissing True (path SystemFilePathPosix.</> name)
    ------------------------------
    P.putStrLn "Generating Deploy Files..."
    ------------------------------
    P.putStrLn $ "Path: " ++ path SystemFilePathPosix.</> name
    ------------------------------
    deploy_PRE_script (path SystemFilePathPosix.</> name SystemFilePathPosix.</> "ProtocolPolicyID_PRE.plutus") "ProtocolPolicyID" swOverWrite ProtocolOnChain.policyIDCode
    deploy_PRE_script (path SystemFilePathPosix.</> name SystemFilePathPosix.</> "ProtocolValidator_PRE.plutus") "ProtocolValidator" swOverWrite ProtocolOnChain.validatorCode
    deploy_PRE_script (path SystemFilePathPosix.</> name SystemFilePathPosix.</> "ScriptPolicyID_PRE.plutus") "ScriptPolicyID" swOverWrite ScriptOnChain.policyIDCode
    deploy_PRE_script (path SystemFilePathPosix.</> name SystemFilePathPosix.</> "ScriptValidator_PRE.plutus") "ScriptValidator" swOverWrite ScriptOnChain.validatorCode
    deploy_PRE_script (path SystemFilePathPosix.</> name SystemFilePathPosix.</> "SwapOfferPolicyID_PRE.plutus") "SwapOfferPolicyID" swOverWrite SwapOfferOnChain.policyIDCode
    deploy_PRE_script (path SystemFilePathPosix.</> name SystemFilePathPosix.</> "SwapOfferValidator_PRE.plutus") "SwapOfferValidator" swOverWrite SwapOfferOnChain.validatorCode
    deploy_PRE_script (path SystemFilePathPosix.</> name SystemFilePathPosix.</> "BuyOrderPolicyID_PRE.plutus") "BuyOrderPolicyID" swOverWrite BuyOrderOnChain.policyIDCode
    deploy_PRE_script (path SystemFilePathPosix.</> name SystemFilePathPosix.</> "BuyOrderValidator_PRE.plutus") "BuyOrderValidator" swOverWrite BuyOrderOnChain.validatorCode
    deploy_PRE_script (path SystemFilePathPosix.</> name SystemFilePathPosix.</> "DelegationPolicyID_PRE.plutus") "DelegationPolicyID" swOverWrite DelegationOnChain.policyIDCode
    deploy_PRE_script (path SystemFilePathPosix.</> name SystemFilePathPosix.</> "DelegationValidator_PRE.plutus") "DelegationValidator" swOverWrite DelegationOnChain.validatorCode
    deploy_PRE_script (path SystemFilePathPosix.</> name SystemFilePathPosix.</> "InvestUnitValidator_PRE.plutus") "InvestUnitValidator" swOverWrite InvestUnitOnChain.validatorCode
    deploy_PRE_script (path SystemFilePathPosix.</> name SystemFilePathPosix.</> "FundPolicy_PRE.plutus") "FundPolicy" swOverWrite FundOnChain.policyCode
    deploy_PRE_script (path SystemFilePathPosix.</> name SystemFilePathPosix.</> "FundValidator_PRE.plutus") "FundValidator" swOverWrite FundOnChain.validatorCode
    deploy_PRE_script (path SystemFilePathPosix.</> name SystemFilePathPosix.</> "FundHoldingPolicyID_PRE.plutus") "FundHoldingPolicyID" swOverWrite FundHoldingOnChain.policyIDCode
    deploy_PRE_script (path SystemFilePathPosix.</> name SystemFilePathPosix.</> "FundHoldingValidator_PRE.plutus") "FundHoldingValidator" swOverWrite FundHoldingOnChain.validatorCode
    ------------------------------
    protocolPolicyID_Pre_CborHex <- OffChainHelpers.readFile (path SystemFilePathPosix.</> name SystemFilePathPosix.</> "ProtocolPolicyID_PRE.plutus")
    protocolValidator_Pre_CborHex <- OffChainHelpers.readFile (path SystemFilePathPosix.</> name SystemFilePathPosix.</> "ProtocolValidator_PRE.plutus")
    scriptPolicyID_Pre_CborHex <- OffChainHelpers.readFile (path SystemFilePathPosix.</> name SystemFilePathPosix.</> "ScriptPolicyID_PRE.plutus")
    scriptValidator_Pre_CborHex <- OffChainHelpers.readFile (path SystemFilePathPosix.</> name SystemFilePathPosix.</> "ScriptValidator_PRE.plutus")
    swapOfferPolicyID_Pre_CborHex <- OffChainHelpers.readFile (path SystemFilePathPosix.</> name SystemFilePathPosix.</> "SwapOfferPolicyID_PRE.plutus")
    swapOfferValidator_Pre_CborHex <- OffChainHelpers.readFile (path SystemFilePathPosix.</> name SystemFilePathPosix.</> "SwapOfferValidator_PRE.plutus")
    buyOrderPolicyID_Pre_CborHex <- OffChainHelpers.readFile (path SystemFilePathPosix.</> name SystemFilePathPosix.</> "BuyOrderPolicyID_PRE.plutus")
    buyOrderValidator_Pre_CborHex <- OffChainHelpers.readFile (path SystemFilePathPosix.</> name SystemFilePathPosix.</> "BuyOrderValidator_PRE.plutus")
    delegationPolicyID_Pre_CborHex <- OffChainHelpers.readFile (path SystemFilePathPosix.</> name SystemFilePathPosix.</> "DelegationPolicyID_PRE.plutus")
    delegationValidator_Pre_CborHex <- OffChainHelpers.readFile (path SystemFilePathPosix.</> name SystemFilePathPosix.</> "DelegationValidator_PRE.plutus")
    investUnitValidator_Pre_CborHex <- OffChainHelpers.readFile (path SystemFilePathPosix.</> name SystemFilePathPosix.</> "InvestUnitValidator_PRE.plutus")
    fundPolicy_Pre_CborHex <- OffChainHelpers.readFile (path SystemFilePathPosix.</> name SystemFilePathPosix.</> "FundPolicy_PRE.plutus")
    fundValidator_Pre_CborHex <- OffChainHelpers.readFile (path SystemFilePathPosix.</> name SystemFilePathPosix.</> "FundValidator_PRE.plutus")
    fundHoldingPolicyID_Pre_CborHex <- OffChainHelpers.readFile (path SystemFilePathPosix.</> name SystemFilePathPosix.</> "FundHoldingPolicyID_PRE.plutus")
    fundHoldingValidator_Pre_CborHex <- OffChainHelpers.readFile (path SystemFilePathPosix.</> name SystemFilePathPosix.</> "FundHoldingValidator_PRE.plutus")
    ------------------------------
    let
        deployAllPreParams =
            T.DeployAllPreParams
                { T.dapProtocolVersion = ProtocolT.protocolVersion
                , T.dapFundVersion = FundT.fundVersion
                , T.dapSwapOfferVersion = SwapOfferT.swapOfferVersion
                , T.dapBuyOrderVersion = BuyOrderT.buyOrderVersion
                , T.dapDelegationVersion = DelegationT.delegationVersion
                , T.dapScriptVersion = ScriptT.scriptVersion
                , T.dapProtocolPolicyID_Pre_CborHex = OffChainHelpers.lazyByteStringToString protocolPolicyID_Pre_CborHex
                , T.dapProtocolValidator_Pre_CborHex = OffChainHelpers.lazyByteStringToString protocolValidator_Pre_CborHex
                , T.dapScriptPolicyID_Pre_CborHex = OffChainHelpers.lazyByteStringToString scriptPolicyID_Pre_CborHex
                , T.dapScriptValidator_Pre_CborHex = OffChainHelpers.lazyByteStringToString scriptValidator_Pre_CborHex
                , T.dapSwapOfferPolicyID_Pre_CborHex = OffChainHelpers.lazyByteStringToString swapOfferPolicyID_Pre_CborHex
                , T.dapSwapOfferValidator_Pre_CborHex = OffChainHelpers.lazyByteStringToString swapOfferValidator_Pre_CborHex
                , T.dapBuyOrderPolicyID_Pre_CborHex = OffChainHelpers.lazyByteStringToString buyOrderPolicyID_Pre_CborHex
                , T.dapBuyOrderValidator_Pre_CborHex = OffChainHelpers.lazyByteStringToString buyOrderValidator_Pre_CborHex
                , T.dapDelegationPolicyID_Pre_CborHex = OffChainHelpers.lazyByteStringToString delegationPolicyID_Pre_CborHex
                , T.dapDelegationValidator_Pre_CborHex = OffChainHelpers.lazyByteStringToString delegationValidator_Pre_CborHex
                , T.dapInvestUnitValidator_Pre_CborHex = OffChainHelpers.lazyByteStringToString investUnitValidator_Pre_CborHex
                , T.dapFundPolicy_Pre_CborHex = OffChainHelpers.lazyByteStringToString fundPolicy_Pre_CborHex
                , T.dapFundValidator_Pre_CborHex = OffChainHelpers.lazyByteStringToString fundValidator_Pre_CborHex
                , T.dapFundHoldingPolicyID_Pre_CborHex = OffChainHelpers.lazyByteStringToString fundHoldingPolicyID_Pre_CborHex
                , T.dapFundHoldingValidator_Pre_CborHex = OffChainHelpers.lazyByteStringToString fundHoldingValidator_Pre_CborHex
                }
    ------------------------------
    OffChainHelpers.writeEncodedToFile (path SystemFilePathPosix.</> name SystemFilePathPosix.</> "deploy.json") deployAllPreParams
    ------------------------------
    P.putStrLn $ "Saved Deploy File in: " ++ P.show (path SystemFilePathPosix.</> name SystemFilePathPosix.</> "deploy.json")
    ------------------------------
    return deployAllPreParams
