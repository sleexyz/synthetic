{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

import Control.Concurrent (forkIO, forkOS, throwTo, ThreadId, threadDelay, killThread, setNumCapabilities)
import Control.Concurrent.MVar
import qualified Control.Exception as Exception
import Control.Monad (forever, void)
import Control.Monad.Trans (liftIO, lift)
import Control.Monad.Trans.Cont (ContT(..), runContT)
import Data.Function ((&))
import Data.Profunctor
import Data.Word (Word8)
import qualified Data.List as List
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector.Unboxed as VU
import qualified Sound.ALSA.Exception as AlsaExc
import qualified Sound.ALSA.Sequencer.Address as Addr
import qualified Sound.ALSA.Sequencer as SndSeq
import qualified Sound.ALSA.Sequencer.Client as Client
import qualified Sound.ALSA.Sequencer.Client.Info as ClientInfo
import qualified Sound.ALSA.Sequencer.Connect as Connect
import qualified Sound.ALSA.Sequencer.Event as Event
import qualified Sound.ALSA.Sequencer.Port as Port
import qualified Sound.ALSA.Sequencer.Port.Info as PortInfo
import qualified System.Posix.Signals as POSIX
import System.Exit (exitSuccess, ExitCode(ExitSuccess))
import Foreign.C.Types (CFloat(..))

handleException :: IO () -> IO ()
handleException act =
   act
   `AlsaExc.catch` \e ->
      putStrLn $ "alsa_exception: " ++ AlsaExc.show e

getAddress :: (SndSeq.OpenMode mode) => SndSeq.T mode -> String -> (Addr.T -> IO ()) -> IO ()
getAddress h expectedPortName continuation = flip runContT return $ do
  cinfo  <- ContT $ ClientInfo.queryLoop_ h
  client <- liftIO $ ClientInfo.getClient cinfo
  portInfo <- ContT $ PortInfo.queryLoop_ h client
  portName <- liftIO $ PortInfo.getName portInfo
  if portName == expectedPortName
  then liftIO $ do
    port <- PortInfo.getPort portInfo
    continuation (Addr.Cons client port)
  else return () -- next iteration

connectTo :: String -> ((Event.Data -> IO Word) -> IO ()) -> IO ()
connectTo expectedPortName continuation = flip runContT return $ do
  h :: SndSeq.T SndSeq.OutputMode <- ContT $ SndSeq.withDefault SndSeq.Block
  client <- liftIO $ Client.getId h
  liftIO $ Client.setName h ("Alsa Test")
  port <- ContT $ Port.withSimple h "Output" (Port.caps [Port.capRead, Port.capSubsRead]) Port.typeHardware
  let address = Addr.Cons client port
  ContT (getAddress h expectedPortName) >>= \sinkAddress -> ContT (Connect.withTo h port sinkAddress)
  let send event = do
        Event.output h $ Event.simple address event
        Event.drainOutput h
  liftIO $ continuation send
    -- liftIO $ (Event.input h) >>= eventHandler

makeNote :: Word8 -> Event.Data
makeNote pitch = Event.NoteEv Event.NoteOn (Event.simpleNote (Event.Channel 1) (Event.Pitch pitch) (Event.Velocity 255))

main :: IO ()
main = connectTo "VirMIDI 4-0" $ \send -> do
  forever $ do
    send $ makeNote 69
    send $ makeNote 72
    send $ makeNote 79
    threadDelay (10^6)
