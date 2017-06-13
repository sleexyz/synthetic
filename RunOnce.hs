import qualified Foreign.Store as ForeignStore
import Data.Word (Word8, Word32)

runOnce :: Word32 -> IO a -> IO a
runOnce index computation = do
  result <- ForeignStore.lookupStore index
  case result of
    Nothing -> do
      x <- computation
      ForeignStore.writeStore (ForeignStore.Store index) x
      return x
    Just store -> ForeignStore.readStore (ForeignStore.Store index)
