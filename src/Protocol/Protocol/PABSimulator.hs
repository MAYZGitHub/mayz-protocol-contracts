{-# LANGUAGE TypeApplications #-}

--------------------------------------------------------------------------------2
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------2

module Protocol.Protocol.PABSimulator where

--------------------------------------------------------------------------------2
-- Import Externos
--------------------------------------------------------------------------------2

import qualified Control.Monad.IO.Class       as MonadIOClass (MonadIO (..))
import qualified Data.Maybe                   as DataMaybe
import qualified Ledger
import qualified Ledger.Value                 as LedgerValue
import qualified Plutus.PAB.Simulator         as PABSimulator
import           PlutusTx.Prelude             hiding (unless)
import qualified Prelude                      as P

--------------------------------------------------------------------------------2
-- Import Internos
--------------------------------------------------------------------------------2

import qualified Generic.CLIHelpers           as CLIHelpers
import qualified Generic.OffChainHelpers      as OffChainHelpers
import qualified Generic.OnChainHelpers       as OnChainHelpers
import qualified Generic.PABHelpers           as PABHelpers
import qualified Protocol.Constants           as T
import qualified Protocol.PABContracts        as PABContracts
import qualified Protocol.PABHelpers          as PABHelpers
import qualified Protocol.PABTypes            as T
import qualified Protocol.Protocol.Types      as T
import qualified Protocol.Script.PABSimulator as ScriptPABSimulator

--------------------------------------------------------------------------------2
-- Modulo
--------------------------------------------------------------------------------2

menuEndPoints :: P.String -> (Integer, Integer) -> T.ProtocolPABParams -> P.IO P.String
menuEndPoints name _ _ = do
    CLIHelpers.printTitle $ "PROTOCOl MENU - " ++ name
    P.putStrLn "31 - Protocol Prepare"
    P.putStrLn "32 - Protocol Update"
    P.putStrLn "33 - Protocol Set Emergency"
    P.putStrLn "--------------------------------"
    -- P.putStrLn "4  - Manage Scripts"
    P.putStrLn "41 - Add Scripts"
    P.putStrLn "42 - Delete Scripts"
    P.putStrLn "--------------------------------"
    P.putStrLn "0  - Return to Main Menu"
    P.putStrLn "99 - Exit"
    P.putStrLn "--------------------------------"
    P.putStrLn "Enter option:"
    option <- P.getLine
    P.putStrLn "--------------------------------"
    return option

--------------------------------------------------------------------------------2

pabMainLoop :: PABContracts.PABParamsInProtocolMenu
pabMainLoop (walletNro, walletCount) protocolPABParams pabReturnToMainMenu pabShutdown = do
    let name = "Protocol: " ++ P.show (T.pppProtocolPolicyID_CS protocolPABParams)
    option <- MonadIOClass.liftIO $ menuEndPoints name (walletNro, walletCount) protocolPABParams
    case option of
        "31" ->
            pabProtocolPrepare (walletNro, walletCount) protocolPABParams pabReturnToMainMenu pabShutdown
        "32" ->
            pabProtocolUpdate (walletNro, walletCount) protocolPABParams pabReturnToMainMenu pabShutdown
        "33" ->
            pabProtocolEmergency (walletNro, walletCount) protocolPABParams pabReturnToMainMenu pabShutdown
        -- "4" ->
        --     ScriptPABSimulator.pabMainLoop (walletNro, walletCount) protocolPABParams pabReturnToMainMenu pabMainLoop pabShutdown
        "41" ->
            ScriptPABSimulator.pabScriptAddInProtocol  (walletNro, walletCount) protocolPABParams pabReturnToMainMenu pabMainLoop pabShutdown
        "42" ->
            ScriptPABSimulator.pabScriptDeleteInProtocol  (walletNro, walletCount) protocolPABParams pabReturnToMainMenu pabMainLoop pabShutdown

        "0" -> do
            pabReturnToMainMenu (Just walletNro, walletCount) (Just protocolPABParams) pabShutdown

        "81" -> do
            PABHelpers.pabBalances (Just walletNro, walletCount) (Just protocolPABParams)
            pabMainLoop (walletNro, walletCount) protocolPABParams pabReturnToMainMenu pabShutdown
        "82" -> do
            PABHelpers.pabUTxOAtWallet (Just walletNro, walletCount)
            pabMainLoop (walletNro, walletCount) protocolPABParams pabReturnToMainMenu pabShutdown
        "83" -> do
            PABHelpers.pabUTxOAtScript (Just protocolPABParams)
            pabMainLoop (walletNro, walletCount) protocolPABParams pabReturnToMainMenu pabShutdown
        "91" -> do
            PABHelpers.pabTimeAndSlot
            pabMainLoop (walletNro, walletCount) protocolPABParams pabReturnToMainMenu pabShutdown

        "99" -> do
            PABHelpers.pabEndSimulation (Just walletNro, walletCount) (Just protocolPABParams) pabShutdown
        _ -> do
            MonadIOClass.liftIO $ P.putStrLn "Invalid option"
            MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
            pabMainLoop (walletNro, walletCount) protocolPABParams pabReturnToMainMenu pabShutdown


--------------------------------------------------------------------------------2

pabProtocolPrepare :: PABContracts.PABParamsInProtocolMenu
pabProtocolPrepare (walletNro, walletCount) protocolPABParams pabReturnToMainMenu pabShutdown = do
    MonadIOClass.liftIO $ CLIHelpers.printSubTitle "Protocol Prepare"
    ---------------------
    -- TODO: como hcer para agregar o elegir una wallet?
    -- necesito tener la private key luego
    -- (oracleWalletNro,_) <- PABHelpers.selectWallet "Protocol Oracle" walletNro walletCount False
    -- let oracleWallet = Ledger.unPaymentPubKeyHash $ PABHelpers.walletPaymentPubKeyHash oracleWalletNro
    let oracleWallet_PaymentPubKey = OffChainHelpers.seedToPaymentPubKey T.oracleWallet_Seed_aux
    ---------------------
    adminProtocolWalletsNros <- PABHelpers.selectWallets "Protocol Admin" (OffChainHelpers.removeDuplicates [1,2,walletNro]) walletCount []
    let adminProtocolWallets =
            [ Ledger.unPaymentPubKeyHash $ PABHelpers.walletPaymentPubKeyHash walletNro'
            | walletNro' <- adminProtocolWalletsNros
            ]
    ---------------------
    tokenAdminPolicy_CS <- MonadIOClass.liftIO $ CLIHelpers.getCurrencySymbol "6aa8df2a10e8530338279e41a1a40dc73f83e468b710c3b64082f759"
    ---------------------
    mayzHolderWalletsNros <- PABHelpers.selectWallets "MAYZ Holders Admin" [3,4] walletCount []
    let mayzHolderWallets =
            [ Ledger.unPaymentPubKeyHash $ PABHelpers.walletPaymentPubKeyHash walletNro'
            | walletNro' <- mayzHolderWalletsNros
            ]
    ---------------------
    let promptFundCategory :: P.IO T.FundCategory
        promptFundCategory = do
            P.putStrLn "Enter the values for a FundCategory:"
            index <- CLIHelpers.getIntWithDefault "Index" 1
            requiredMAYZ <- CLIHelpers.getIntWithDefault "Required MAYZ" 1_000
            maxUI <- CLIHelpers.getIntWithDefault "Max UI" 1_000_000_000
            return (T.FundCategory index requiredMAYZ maxUI)
    ---------------------
        promptFundCategories :: P.IO [T.FundCategory]
        promptFundCategories = promptFundCategories' []
            where
                promptFundCategories' :: [T.FundCategory] -> P.IO [T.FundCategory]
                promptFundCategories' acc = do
                    fundCategory <- promptFundCategory
                    let updatedAcc = fundCategory : acc
                    P.putStrLn "FundCategory added!"
                    P.putStrLn "Do you want to add one more (y/n - default=n)?"
                    choice <- CLIHelpers.getBoolWithDefault False
                    if choice
                        then promptFundCategories' updatedAcc
                        else return (reverse updatedAcc)
    ---------------------
    fundCategories <- MonadIOClass.liftIO promptFundCategories
    ---------------------
    share_InBPx1e2_Protocol <- MonadIOClass.liftIO  $ CLIHelpers.getIntWithDefault "Protocol share (1pb = 0.01% / 10,000pb = 100%)" 333_400
    share_InBPx1e2_Managers <- MonadIOClass.liftIO  $ CLIHelpers.getIntWithDefault "Managers share (1pb = 0.01% / 10,000pb = 100%)" 333_300
    share_InBPx1e2_Delegators <- MonadIOClass.liftIO  $ CLIHelpers.getIntWithDefault "Delegators share (1pb = 0.01% / 10,000pb = 100%)" 333_300
    ---------------------
    let contract =
            PABContracts.PABProtocolPrepare
                T.PABProtocolPrepareParams
                    {
                        T.ppppProtocolPABParams = protocolPABParams,
                        T.ppppOraclePaymentPubKey = oracleWallet_PaymentPubKey,
                        T.ppppAdmins = adminProtocolWallets,
                        T.ppppTokenAdminPolicy_CS = tokenAdminPolicy_CS,
                        T.ppppFundCategories = fundCategories,
                        T.ppppFundLifeTime = T.mkMinMaxDef 0 0 0,
                        T.ppppRequiredMAYZForSwapOffer = 0,
                        T.ppppRequiredMAYZForBuyOrder = 0,
                        T.ppppCommissionFund_PerYear_InBPx1e3 = T.mkMinMaxDef 0 0 0,
                        T.ppppCommissionSwapOffer_InBPx1e3 = T.mkMinMaxDef 0 0 0,
                        T.ppppCommissionBuyOrder_InBPx1e3 = T.mkMinMaxDef 0 0 0,
                        T.ppppShare_InBPx1e2_Protocol = share_InBPx1e2_Protocol,
                        T.ppppShare_InBPx1e2_Managers = share_InBPx1e2_Managers,
                        T.ppppShare_InBPx1e2_Delegators = share_InBPx1e2_Delegators,
                        T.ppppDelegatorsAdmins = mayzHolderWallets
                    }
    contractInstance <- PABSimulator.activateContract (PABHelpers.getWallet walletNro) contract
    PABHelpers.waitContractAndKeyPress contractInstance
    pabMainLoop (walletNro, walletCount) protocolPABParams pabReturnToMainMenu pabShutdown

--------------------------------------------------------------------------------2

pabProtocolUpdate :: PABContracts.PABParamsInProtocolMenu
pabProtocolUpdate (walletNro, walletCount) protocolPABParams pabReturnToMainMenu pabShutdown = do
    MonadIOClass.liftIO $ CLIHelpers.printSubTitle "Protocol Update"
    ---------------------
    blockchain <- PABSimulator.blockchain
    ---------------------
    let !protocolPolicyID_CS = T.pppProtocolPolicyID_CS protocolPABParams
        !protocolID_AC = LedgerValue.AssetClass (protocolPolicyID_CS, T.protocolID_TN)
    ---------------------
    let !uTxOuts = PABHelpers.getUTxOsListInPABSimulator blockchain (T.pppProtocolValidator_Address protocolPABParams)
        !uTxO_With_ProtocolDatum' = [ (txOutRef, txOut,  T.getProtocol_DatumType $ OnChainHelpers.fromJust $ PABHelpers.getDatumInPABSimulator @T.ValidatorDatum blockchain (txOutRef, txOut) )
                                                | (txOutRef, txOut) <- uTxOuts,
                                                    LedgerValue.assetClassValueOf (Ledger.txOutValue txOut) protocolID_AC > 0
                                                    && DataMaybe.isJust (PABHelpers.getDatumInPABSimulator @T.ValidatorDatum blockchain (txOutRef, txOut) )
                                            ]
    ---------------------
    case uTxO_With_ProtocolDatum' of
        (uTxO_With_ProtocolDatum:_) -> do
            ---------------------
            let !protocolDatum = (\(_, _, dat) -> dat) uTxO_With_ProtocolDatum
            ---------------------
            -- (oracleWalletNro,_) <- PABHelpers.selectWallet "Protocol Oracle" walletNro walletCount False
            -- let oracleWallet = Ledger.unPaymentPubKeyHash $ PABHelpers.walletPaymentPubKeyHash oracleWalletNro
            ---------------------
            adminProtocolWalletsNros <- PABHelpers.selectWallets "Protocol Admin" (OffChainHelpers.removeDuplicates [1,2,walletNro]) walletCount []
            let adminProtocolWallets =
                    [ Ledger.unPaymentPubKeyHash $ PABHelpers.walletPaymentPubKeyHash walletNro'
                    | walletNro' <- adminProtocolWalletsNros
                    ]
            ---------------------
            tokenAdminPolicy_CS <- MonadIOClass.liftIO $ CLIHelpers.getCurrencySymbol (OffChainHelpers.strictByteStringToString . OffChainHelpers.builtinByteStringToHexString . LedgerValue.unCurrencySymbol $ T.pdTokenAdminPolicy_CS protocolDatum)
            ---------------------
            mayzHolderWalletsNros <- PABHelpers.selectWallets "MAYZ Holders Admin" [3,4] walletCount []
            let mayzHolderWallets =
                    [ Ledger.unPaymentPubKeyHash $ PABHelpers.walletPaymentPubKeyHash walletNro'
                    | walletNro' <- mayzHolderWalletsNros
                    ]
            ---------------------
            let promptFundCategory :: P.IO T.FundCategory
                promptFundCategory = do
                    P.putStrLn "Enter the values for a FundCategory:"
                    index <- CLIHelpers.getIntWithDefault "Index" 1
                    requiredMAYZ <- CLIHelpers.getIntWithDefault "Required MAYZ" 1_000
                    maxUI <- CLIHelpers.getIntWithDefault "Max UI" 1_000_000_000
                    return (T.FundCategory index requiredMAYZ maxUI)
            ---------------------
                promptFundCategories :: P.IO [T.FundCategory]
                promptFundCategories = promptFundCategories' []
                    where
                        promptFundCategories' :: [T.FundCategory] -> P.IO [T.FundCategory]
                        promptFundCategories' acc = do
                            fundCategory <- promptFundCategory
                            let updatedAcc = fundCategory : acc
                            P.putStrLn "FundCategory added!"
                            P.putStrLn "Do you want to add one more (y/n - default=n)?"
                            choice <- CLIHelpers.getBoolWithDefault False
                            if choice
                                then promptFundCategories' updatedAcc
                                else return (reverse updatedAcc)
            ---------------------
            fundCategories <- MonadIOClass.liftIO promptFundCategories
            ---------------------
            share_InBPx1e2_Protocol <- MonadIOClass.liftIO  $ CLIHelpers.getIntWithDefault "Protocol share (1pb = 0.01% / 10,000pb = 100%)" 333_400
            share_InBPx1e2_Managers <- MonadIOClass.liftIO  $ CLIHelpers.getIntWithDefault "Managers share (1pb = 0.01% / 10,000pb = 100%)" 333_300
            share_InBPx1e2_Delegators <- MonadIOClass.liftIO  $ CLIHelpers.getIntWithDefault "Delegators share (1pb = 0.01% / 10,000pb = 100%)" 333_300
            ---------------------
            -- TODO: leer el resto de los campos que no se actualizan del datum , apra no sobreescebirlos con default
            let
                contract = PABContracts.PABProtocolUpdate T.PABProtocolUpdateParams{
                    T.ppupProtocolPABParams = protocolPABParams,
                    T.ppupOraclePaymentPubKey = T.pdOraclePaymentPubKey protocolDatum,
                    T.ppupAdmins = adminProtocolWallets,
                    T.ppupTokenAdminPolicy_CS = tokenAdminPolicy_CS,
                    T.ppupFundCategories = fundCategories,
                    T.ppupFundLifeTime = T.pdFundLifeTime protocolDatum,
                    T.ppupRequiredMAYZForSwapOffer = T.pdRequiredMAYZForSwapOffer protocolDatum,
                    T.ppupRequiredMAYZForBuyOrder = T.pdRequiredMAYZForBuyOrder protocolDatum,
                    T.ppupCommissionFund_PerYear_InBPx1e3 = T.pdCommissionFund_PerYear_InBPx1e3 protocolDatum,
                    T.ppupCommissionSwapOffer_InBPx1e3 = T.pdCommissionSwapOffer_InBPx1e3 protocolDatum,
                    T.ppupCommissionBuyOrder_InBPx1e3 = T.pdCommissionBuyOrder_InBPx1e3 protocolDatum,
                    T.ppupShare_InBPx1e2_Protocol = share_InBPx1e2_Protocol,
                    T.ppupShare_InBPx1e2_Managers = share_InBPx1e2_Managers,
                    T.ppupShare_InBPx1e2_Delegators = share_InBPx1e2_Delegators,
                    T.ppupDelegatorsAdmins = mayzHolderWallets
                }

            contractInstance <- PABSimulator.activateContract (PABHelpers.getWallet walletNro) contract
            PABHelpers.waitContractAndKeyPress contractInstance
            pabMainLoop (walletNro, walletCount) protocolPABParams pabReturnToMainMenu pabShutdown
        [] -> do
                MonadIOClass.liftIO $ P.putStrLn "Can't find Protocol Datum"
                MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
                pabMainLoop (walletNro, walletCount) protocolPABParams pabReturnToMainMenu pabShutdown

--------------------------------------------------------------------------------2

pabProtocolEmergency :: PABContracts.PABParamsInProtocolMenu
pabProtocolEmergency (walletNro, walletCount) protocolPABParams pabReturnToMainMenu pabShutdown = do
    MonadIOClass.liftIO $ CLIHelpers.printSubTitle "Protocol Set Emergency"
    adminsNros <- PABHelpers.selectWallets "Quienes firman?" [walletNro] walletCount []
    let
        admins = [
            Ledger.unPaymentPubKeyHash $ PABHelpers.walletPaymentPubKeyHash adminsNro
            | adminsNro <- adminsNros]

        contract = PABContracts.PABProtocolEmergency T.PABProtocolEmergencyParams{
            T.ppepProtocolPABParams = protocolPABParams,
            T.ppepAdmins = admins
        }

    contractInstance <- PABSimulator.activateContract (PABHelpers.getWallet walletNro) contract
    PABHelpers.waitContractAndKeyPress contractInstance
    pabMainLoop (walletNro, walletCount) protocolPABParams pabReturnToMainMenu pabShutdown

------------------------------------------------------------------------------
