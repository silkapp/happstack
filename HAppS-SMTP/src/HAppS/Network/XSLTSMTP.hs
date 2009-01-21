module HAppS.Network.XSLTSMTP {-(queueMessage, ToXMLEnvelope(..)) -} where

{-
import HAppS.MACID
import qualified HAppS.Agents.MailSender as MS(MailState, queueMessage)
import Text.XML.HaXml.Types
import HAppS.Server.MinHaXML
import HAppS.Server.XSLT
import HAppS.Server.SMTP as SMTP 
import HAppS.Server.SMTP hiding (Address,Domain,contents)
--import HAppS.Server.SMTP (Address,Domain,Envelope)
import HAppS.Util.Common
---EXPERIMENTAL XSLT MAIL FUNCTIONALITY
--type Domain = SMTP.Domain
--type Email = SMTP.Address
--type Address = SMTP.Address
-- type State = MailSender.State

minDoc = simpleDoc NoStyle --belongs in minhaxml

-- | Queue a message doing XSL transformation first.
queueMessage :: (ToXMLEnvelope obj) => XSLPath -> obj -> Ev MS.MailState ev ()
queueMessage style obj = do
  t <- getTime
  let env = toXMLEnvelope t obj
  let res = xsltString style $ minDoc $ contents env
  res `seq` MS.queueMessage env { contents = res }

class ToXMLEnvelope obj where 
    toXMLEnvelope::EpochSeconds -> obj -> Envelope Text.XML.HaXml.Types.Element

{--data Env a = Env {relay::Domain
		 ,sender::SMTP.Address
		 ,recipients::[SMTP.Address]
		 ,contents::a
                 } deriving (Read,Show)--}

-- u_contents f env = env {contents = f $ contents env}

--envToEnvelope (Env r s d c) c' = SMTP.Envelope r s d c'
--ezEnvelope fromAddr toAddrs contents = 
--    Envelope "host" fromAddr toAddrs contents
-}
