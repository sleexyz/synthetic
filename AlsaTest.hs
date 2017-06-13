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

getNonzeroIndicies :: (Num a, Ord a) => [a] -> [Int]
getNonzeroIndicies words = zip [1..] words
  & filter ((>0) . snd)
  & fmap fst

-- sineF :: Int -> (Int -> Float)
-- sineF (fromIntegral -> sr) (fromIntegral -> index) = sin (index * 2 * pi / sr)

-- mkSinWavetable :: Int -> IO (Int -> IO [Float])
-- mkSinWavetable sr = do
--   stateRef <- newMVar 0
--   return $ \incr -> do
--     modifyMVar stateRef $ \state -> do
--       let samples = sineF sr <$> [state..(state + incr)]
--       return (state + incr + 1, samples)

-- mkSinOsc :: Int -> IO (Float -> IO Float)
-- mkSinOsc sr = do
--   sinWavetable <- mkSinWavetable sr
--   return $ \freq -> sinWavetable (1 * floor freq)
--     & fmap head

mkSinOsc :: Int -> IO (Float -> IO Float)
mkSinOsc sr = do
  let
    twopiperiod :: Float
    twopiperiod = 2 * pi / fromIntegral sr

  phaseRef <- VUM.replicate 1 (0 :: Int)
  return $ \freq -> do
    phase <- VUM.unsafeRead phaseRef 0
    VUM.write phaseRef 0 (phase + floor freq)
    return $ sin ((fromIntegral phase + freq) * twopiperiod)

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

padWith :: [a] -> [a] -> [a]
padWith (p:ps) (x:xs) = x : padWith ps xs
padWith padding [] = padding

-- FIXME maintain phase of voices

main :: IO ()
main = do
  freqRef <- VUM.replicate 10 (0 :: Float)
  velocitiesRef <- VUM.replicate 256 (0 :: Word8)

  let calculateFreqRefs = do
        velocities <- VU.toList <$> VU.freeze velocitiesRef
        let notesToPlay = getNonzeroIndicies velocities
        let freqsToPlay = notesToPlay
              & take 10
              & fmap (midi2cps . fromIntegral . subtract 1)
              & padWith (replicate 10 0)
        sequence $ zipWith (VUM.write freqRef) [0..9] freqsToPlay

  forkIO $ connectTo "UM-ONE MIDI 1" $ \event -> do
    let body = event & Event.body
    case body of
      Event.NoteEv noteev note -> do
        let pitch = note & Event.noteNote & Event.unPitch
        let vel = note & Event.noteVelocity & Event.unVelocity
        VUM.write velocitiesRef (fromIntegral pitch) vel
        void calculateFreqRefs
      _ -> return ()

  s0 <- mkSinOsc 48000
  s1 <- mkSinOsc 48000
  s2 <- mkSinOsc 48000
  s3 <- mkSinOsc 48000
  s4 <- mkSinOsc 48000
  s5 <- mkSinOsc 48000
  s6 <- mkSinOsc 48000
  s7 <- mkSinOsc 48000
  s8 <- mkSinOsc 48000
  s9 <- mkSinOsc 48000

  withJACKClient $ do
    v0 <- VUM.read freqRef 0 >>= \val -> if val > 0 then s0 val else return 0
    v1 <- VUM.read freqRef 1 >>= \val -> if val > 0 then s1 val else return 0
    v2 <- VUM.read freqRef 2 >>= \val -> if val > 0 then s2 val else return 0
    v3 <- VUM.read freqRef 3 >>= \val -> if val > 0 then s3 val else return 0
    v4 <- VUM.read freqRef 4 >>= \val -> if val > 0 then s4 val else return 0
    v5 <- VUM.read freqRef 5 >>= \val -> if val > 0 then s5 val else return 0
    v6 <- VUM.read freqRef 6 >>= \val -> if val > 0 then s6 val else return 0
    v7 <- VUM.read freqRef 7 >>= \val -> if val > 0 then s7 val else return 0
    v8 <- VUM.read freqRef 8 >>= \val -> if val > 0 then s8 val else return 0
    v9 <- VUM.read freqRef 9 >>= \val -> if val > 0 then s9 val else return 0
    return $ (v0 + v1 + v2 + v3 + v4 + v5 + v6 + v7 + v8 + v9) / 10
