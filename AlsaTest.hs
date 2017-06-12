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
import qualified Sound.JACK as JACK
import qualified Sound.JACK.Exception as JACK
import qualified Sound.JACK.Audio as JACKAudio
import qualified System.Posix.Signals as POSIX
import System.Exit (exitSuccess, ExitCode(ExitSuccess))
import Foreign.C.Types (CFloat(..))

import Sound.Synthetic (everyNth, midi2cps)

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
getNonzeroIndicies words = zip [1..] words
  & filter ((>0) . snd)
  & fmap fst

sineF :: Int -> (Int -> Float)
sineF (fromIntegral -> sr) (fromIntegral -> index) = sin (index * 2 * pi / sr)

mkSinWavetable :: Int -> IO (Int -> IO [Float])
mkSinWavetable sr = do
  stateRef <- newMVar 0
  return $ \incr -> do
    modifyMVar stateRef $ \state -> do
      let samples = sineF sr <$> [state..(state + incr)]
      return (state + incr + 1, samples)

mkSinOsc :: Int -> IO (Float -> IO Float)
mkSinOsc sr = do
  sinWavetable <- mkSinWavetable sr
  return $ \freq -> sinWavetable (1 * floor freq)
    & fmap head


withJACKClient :: IO Float -> IO ()
withJACKClient getSample = JACK.handleExceptions $ flip runContT return $ do
  client <- ContT $ JACK.withClientDefault "foo"
  input <- ContT $ JACK.withPort client "input"
  output <- ContT $ JACK.withPort client "output"
  let program = (const (CFloat <$> getSample))
  ContT $ \f -> JACKAudio.withProcessMono client input program output $ JACK.withActivation client (f ())
  lift $ JACK.connect client "foo:output" "system:playback_1"
  lift $ JACK.connect client "foo:output" "system:playback_2"
  lift . lift $ do
    putStrLn $ "started foo..."
    JACK.waitForBreak

main :: IO ()
main = do
  velocitiesRef <- VUM.replicate 256 (0 :: Word8)
  forkIO $ connectTo "UM-ONE MIDI 1" $ \event -> do
    let body = event & Event.body
    case body of
      Event.NoteEv noteev note -> do
        let pitch = note & Event.noteNote & Event.unPitch
        let vel = note & Event.noteVelocity & Event.unVelocity
        VUM.write velocitiesRef (fromIntegral pitch) vel
      _ -> return ()

  freqRef <- VUM.replicate 1 (440 :: Float)
  counterRef <- newMVar (0 :: Int)

  forkIO $ forever $ do
    velocities <- VU.toList <$> VU.freeze velocitiesRef
    let notesToPlay = getNonzeroIndicies velocities
    counter <- takeMVar counterRef
    putMVar counterRef (counter + 1)
    case notesToPlay of
      [] -> return ()
      _ -> do
        let index = counter `mod` length notesToPlay
        let note = (notesToPlay !! index) - 1
        let freq = midi2cps $ fromIntegral note
        VUM.write freqRef 0 freq
        print note
    threadDelay (10^6 * 60 `div` (120 * 16))
  sinOsc <- mkSinOsc 48000

  withJACKClient $ VUM.read freqRef 0 >>= sinOsc
