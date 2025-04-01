{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE TypeFamilies #-}

--------------------------------------------------------------------------------3
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------3

module Protocol.Constants where

--------------------------------------------------------------------------------2
-- Import Externos
--------------------------------------------------------------------------------2

import qualified Data.ByteString      as DataByteString
import qualified Plutus.V2.Ledger.Api as LedgerApiV2
import           PlutusTx.Prelude

--------------------------------------------------------------------------------2
-- Import Internos
--------------------------------------------------------------------------------2

import qualified Generic.Types        as T
import qualified Ledger.Value as LedgerValue

--------------------------------------------------------------------------------2
-- Modulo
--------------------------------------------------------------------------------2

swapOffer_Status_Open  :: Integer
swapOffer_Status_Open  = 1

swapOffer_Status_Closed  :: Integer
swapOffer_Status_Closed  = 2

swapOffer_AllowSell  :: Integer
swapOffer_AllowSell  = 1

swapOffer_NotAllowSell  :: Integer
swapOffer_NotAllowSell  = 2

buyOrder_Status_Open  :: Integer
buyOrder_Status_Open  = 1

buyOrder_Status_Closed  :: Integer
buyOrder_Status_Closed  = 2

--------------------------------------------------------------------------------2

protocolID_TN :: T.TN
protocolID_TN = LedgerApiV2.TokenName "ProtocolID" -- es NFT, la poliza es unica

protocolTokenEmergencyAdmin_TN :: T.TN
protocolTokenEmergencyAdmin_TN = LedgerApiV2.TokenName "EMRCY"

protocolTokenAdmin_TN :: T.TN
protocolTokenAdmin_TN = LedgerApiV2.TokenName "ADMIN"

fundTokenAdmin_TN :: T.TN
fundTokenAdmin_TN = LedgerApiV2.TokenName "ADMIN"

fundID_TN :: T.TN
fundID_TN = LedgerApiV2.TokenName "FundID" -- es NFT, la poliza es unica y el validador es unico tambien

fundHoldingID_TN_basename :: BuiltinByteString
fundHoldingID_TN_basename = "FundHoldingID" -- es NFT, la poliza es unica , la del fondo, y usa un index para cada tn, y el validador es unico tambien

investUnitID_TN :: T.TN
investUnitID_TN = LedgerApiV2.TokenName "IUID" -- es NFT, la poliza es unica por fondo, pero el validador es el mismo para todos los fondos, por ende debo verificar en el validador que no haya dos inputs

swapOfferID_TN :: T.TN
swapOfferID_TN = LedgerApiV2.TokenName "SwapOfferID" -- es FT, la poliza es por protocolo y el validador tambien, por ende debo verificar en el validador que no haya dos inputs

buyOrderID_TN :: T.TN
buyOrderID_TN = LedgerApiV2.TokenName "BuyOrderID" -- es FT, la poliza es por protocolo y el validador tambien, por ende debo verificar en el validador que no haya dos inputs

delegationID_TN :: T.TN
delegationID_TN = LedgerApiV2.TokenName "DelegationID" -- es FT, la poliza es por protocolo y el validador tambien, por ende debo verificar en el validador que no haya dos inputs

-- se usan solo en la parte offchain. Estos valores van al datum del protocolo. 
tokenMAYZ_CS_aux :: T.CS
tokenMAYZ_CS_aux =  "e0b33937400326885f7186e2725a84786266ec1eb06d397680233f80"

tokenMAYZ_TN_aux :: T.TN
tokenMAYZ_TN_aux = LedgerApiV2.TokenName "MAYZ"

tokenMAYZ_AC_aux :: LedgerValue.AssetClass
tokenMAYZ_AC_aux = LedgerValue.AssetClass (tokenMAYZ_CS_aux, tokenMAYZ_TN_aux)

--------------------------------------------------------------------------------2

-- Lo usa la parte off-chain. Estos valores van al datum del fondo. 
coreTeamWallets_aux :: [T.WalletPaymentPKH]
coreTeamWallets_aux = ["a2c20c77887ace1cd986193e4e75babd8993cfd56995cd5cfce609c2", "80a4f45b56b88d1139da23bc4c3c75ec6d32943c087f250b86193ca7"]

--------------------------------------------------------------------------------2

-- Lo usa la parte off-chain. Estos valores van al datum del fondo. 
maxDepositAndWithdraw_aux :: Integer
maxDepositAndWithdraw_aux = 10_000_000_000_000_000

--------------------------------------------------------------------------------2

-- Lo usa la parte off-chain. Estos valores van al datum del protocolo. 
oracleData_Valid_Time_aux :: LedgerApiV2.POSIXTime
oracleData_Valid_Time_aux = 300_000 -- 5 * 60 * 1000 = 5 minutos

--------------------------------------------------------------------------------2

-- Lo usa la parte off-chain
oracleWallet_Seed_aux :: DataByteString.ByteString
oracleWallet_Seed_aux = "he hi he ds fd gg ge eew rer trt erw rwerwe trter gfgdf gfdgdf rtet trtre treter ghfhgf treter gfdgdf tretre gfdgdf tretre"

--------------------------------------------------------------------------------2

-- | Helper function to create a version number with a dependency
mkVersionWithDependency :: [Integer] -> Integer -> Integer
mkVersionWithDependency xs ownVersion
  = foldr (\x acc -> acc * 100 + x) ownVersion xs

--------------------------------------------------------------------------------