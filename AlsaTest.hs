{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

import Control.Concurrent (forkIO, throwTo, ThreadId, threadDelay, killThread)
import qualified Control.Exception as Exception
import Control.Monad (forever, void)
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Cont (ContT(..), runContT)
import Data.Function ((&))
import Data.Word (Word8, Word32)
import qualified Data.List as List
import qualified Foreign.Store as ForeignStore
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

import Sound.Synthetic

handleException :: IO () -> IO ()
handleException act =
   act
   `AlsaExc.catch` \e ->
      putStrLn $ "alsa_exception: " ++ AlsaExc.show e

connectTo :: String -> (Event.T -> IO ()) -> IO ()
connectTo expectedPortName eventHandler = flip runContT return $ do
  h :: SndSeq.T SndSeq.InputMode <- ContT $ SndSeq.withDefault SndSeq.Block
  liftIO $ Client.setName h "Alsa Test"
  cinfo  <- ContT $ ClientInfo.queryLoop_ h
  client <- liftIO $ ClientInfo.getClient cinfo
  portInfo <- ContT $ PortInfo.queryLoop_ h client
  portName <- liftIO $ PortInfo.getName portInfo
  if portName == expectedPortName
  then do
    port <- liftIO $ PortInfo.getPort portInfo
    foo <- ContT $ Port.withSimple h "foo" (Port.caps [Port.capWrite, Port.capSubsWrite]) Port.typeMidiGeneric
    connect <- ContT $ Connect.withFrom h foo (Addr.Cons client port)
    forever $ do
      liftIO $ (Event.input h) >>= eventHandler
  else
    return ()

handleCtrlC :: IO () -> IO ()
handleCtrlC handler = void $ POSIX.installHandler POSIX.keyboardSignal (POSIX.Catch handler) Nothing

runOnce :: Word32 -> IO a -> IO a
runOnce index computation = do
  result <- ForeignStore.lookupStore index
  case result of
    Nothing -> do
      x <- computation
      ForeignStore.writeStore (ForeignStore.Store index) x
      return x
    Just store -> ForeignStore.readStore (ForeignStore.Store index)

getNonzeroIndicies :: (Num a, Ord a) => [a] -> [Int]
getNonzeroIndicies words = zip words [1..]
  & filter ((>0) . fst)
  & fmap snd

main :: IO ()
main = do
  noteVec <- runOnce 0 $ VUM.replicate 256 (0 :: Word8)

  runOnce 1 $ forkIO $ connectTo "UM-ONE MIDI 1" $ \event -> do
    let body = event & Event.body
    case body of
      Event.NoteEv noteev note -> do
        let pitch = note & Event.noteNote & Event.unPitch
        let vel = note & Event.noteVelocity & Event.unVelocity
        VUM.write noteVec (fromIntegral pitch) vel
      _ -> return ()

  playIO $ do
    velocities <- VU.toList <$> VU.freeze noteVec
    let notes = getNonzeroIndicies velocities
    case notes of
      [] -> return (replicate 8000 0)
      _ -> do
        let
          toSig pitch = signal 8000 (midi2cps pitch) (sineWavetable 8000)
          sig = foldr (zipWith (+)) (repeat 0) (toSig . fromIntegral <$> notes)
            & fmap (/(fromIntegral $ length $ notes))
        return $ sig & take 8000

-- TODO: implement a ringbuffer in haskell
