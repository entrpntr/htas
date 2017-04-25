module Main where

import Control.Concurrent
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Foldable
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid
import Data.Traversable
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Foreign
import Foreign.C.Types
import System.IO
import Text.Printf

import HTas.Direct
import HTas.Low

import Red.Battle
import Red.Intro
import Red.Overworld
import Red.Save
import Search
import MoonManip

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering

    baseSave <- BS.readFile "red_mp_lass.sav"

    printf "Creating initial states"
    gb <- create
    loadRomFile gb "pokered.gbc"
    let frames = concat [[0..35], [38..59]]
    initialStates <- for frames $ \frame -> do
        printf "."
        loadSaveData gb (setSaveFrames frame baseSave)
        reset gb
        doOptimalIntro gb
        state <- saveState gb
        let frm = fromIntegral frame
        let frm2 = (61 :: Int)
        pure (state, frm, frm2, 0)
    printf "\n"

    launchSearch r3LassSegments initialStates
    printf "Search launched\n"
    forever $ threadDelay (10^6)

launchSearch :: [GB -> IORef Input -> Segment StateGroup [Input]] -> StateGroup -> IO ()
launchSearch segs initialStates = do
    initialIORef <- newIORef (Just (Checkpoint
        { revPaths = []
        , currentState = initialStates
        , value = 60
        }))
    lock <- newMVar ()
    launchLoop segs initialIORef lock
    where
    launchLoop segs sourceRef lock = do
        case segs of
            [] -> pure ()
            s:ss -> do
                targetRef <- newIORef Nothing

                gb <- create
                loadRomFile gb "pokered.gbc"
                inputRef <- newIORef mempty
                setInputGetter gb (readIORef inputRef)

                forkIO . forever $ do
                    segmentStep (s gb inputRef) sourceRef targetRef $ \check -> do
                        withMVar lock $ \_ -> do
                            let (_, _, _, dsum) = (currentState check) !! 0
                            printf "Segment %d\tValue %f  (dsum=%d)\n" (length (revPaths check)) (value check) (dsum)
                        when (length ss == 1 && value check > 54) $ do
                            let (_, _, _, dsum) = (currentState check) !! 0
                            let description = printf "Value %f (dsum=%d)\t%s\n" (value check) (dsum) (show . reverse $ revPaths check)
                            appendFile "almost_paths_r3lass_nopal.txt" description
                        when (length ss == 0 && value check > 54) $ do
                            let description = printf "Value %f\t%s\n" (value check) (show . reverse $ revPaths check)
                            appendFile "complete_paths_r3lass_nopal.txt" description
                    threadDelay 10000
                launchLoop ss targetRef lock


setSaveFrames :: Word8 -> ByteString -> ByteString
setSaveFrames f dat =
    let dat' = editByte saveTimeFrames f dat
        checksum = computeChecksum saveMainDataStart saveMainDataEnd dat'
        dat'' = editByte saveMainDataChecksum checksum dat'
    in
    dat''
