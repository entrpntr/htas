module MoonManip where

import Data.ByteString (ByteString)
import Data.IORef
import Data.Maybe
import Data.Monoid
import Data.Traversable
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import System.Random
import Text.Printf

import Red.Battle
import Search
import HTas.Direct
import HTas.Low

import Red.Overworld

enumeratePaths :: Int -> Int -> [(Int, Int)] -> Vector [Bool]
enumeratePaths start end bounds = Vector.fromList (go start end bounds)
    where
    go start end bounds =
        case bounds of
            [] -> pure $ replicate (end-start) True
            (lo,hi):rest -> do
                mid <- [max start lo .. hi]
                fmap ((replicate (mid - start) True ++ [False]) ++) (go mid end rest)

selectRandom :: Vector a -> IO a
selectRandom v = do
    i <- randomRIO (0, length v - 1)
    pure $ v Vector.! i

addAPressHelp :: Int -> Int -> Double -> [Input] -> IO [Input]
addAPressHelp aStart aEnd prob inps = do
    shouldAdd <- coin prob
    if shouldAdd
    then do
        aIdx <- randomRIO (aStart, aEnd)
        let (start, end) = splitAt aIdx inps
        case end of
            [] -> pure start
            i:is -> pure $ start ++ ((i <> i_A) : is)
    else pure inps

addAPresses :: [Input] -> IO [Input]
addAPresses inps = do
    shouldAdd <- coin 0.35
    if shouldAdd
    then do
        aIdx <- randomRIO (1, length inps - 1)
        let (start, end) = splitAt aIdx inps
        case end of
            [] -> pure start
            i:is -> pure $ start ++ ((i <> i_A) : is)
    else pure inps

addAPresses2 :: [Input] -> IO [Input]
addAPresses2 inps = do
    shouldAdd <- coin 1.0
    if shouldAdd
    then do
        aIdx <- randomRIO (1, length inps - 1)
        let (start, end) = splitAt aIdx inps
        case end of
            [] -> pure start
            i:is -> pure $ start ++ ((i <> i_A) : is)
    else pure inps

addAPresses3 :: [Input] -> IO [Input]
addAPresses3 inps = do
    firstA <- coin 0.5
    if firstA
    then do
        let (start, end) = splitAt 1 inps
        case end of
            [] -> pure start
            i:is -> pure $ start ++ ((i <> i_A) : is)
    else do
        let (start, end) = splitAt 2 inps
        case end of
            [] -> pure start
            i:is -> pure $ start ++ ((i <> i_A) : is)

addAPresses4 :: [Input] -> IO [Input]
addAPresses4 inps = do
    let (start, end) = splitAt 4 inps
    case end of
        [] -> pure start
        i:is -> pure $ start ++ ((i <> i_A) : is)

addAPresses5 :: [Input] -> IO [Input]
addAPresses5 inps = do
    let (start, end) = splitAt 6 inps
    case end of
        [] -> pure start
        i:is -> pure $ start ++ ((i <> i_A) : is)

addAPresses6 :: [Input] -> IO [Input]
addAPresses6 inps = do
    lastA <- coin 0.5
    if lastA
    then do
        let (start, end) = splitAt 8 inps
        case end of
            [] -> pure start
            i:is -> pure $ start ++ ((i <> i_A) : is)
    else do
        let (start, end) = splitAt 9 inps
        case end of
            [] -> pure start
            i:is -> pure $ start ++ ((i <> i_A) : is)

type StateGroup = [(ByteString, Int, Int, Int)]

{-
moonLassSegments :: [GB -> IORef Input -> Segment StateGroup [Input]]
moonLassSegments =
    [ \gb inputRef ->
      let
        paths = fmap (\b -> if b then i_Left else i_Down)
            <$> enumeratePaths 0 8 [(7,8), (7,8), (7,8)]
      in Segment
        { generate = selectRandom paths
        , apply = \path stateGroup -> do
            --printf "Segment 4: %s to %d states" (show path) (length stateGroup)
            resultMaybeStates <- for stateGroup $ \state igt -> do
                --printf "."
                loadState gb state

                bufferedWalk gb inputRef (path ++ [i_Down, i_Down, i_Down, i_Down, i_Down, i_Down, i_Down, i_Left, i_Left, i_Left, i_Left, i_Left])
                encountered <- (/= 0) <$> cpuRead gb wIsInBattle
                if encountered
                then pure Nothing
                else Just <$> saveState gb
            let resultStates = catMaybes resultMaybeStates
            --printf "%d states remain\n" (length resultStates)
            pure $ (resultStates, fromIntegral (length resultStates))
        }
    , \gb inputRef ->
      let
        paths = fmap (\b -> if b then i_Up else i_Left)
            <$> enumeratePaths 0 15 [(0, 15), (0, 15), (0, 15), (0, 15), (0, 15), (13, 15), (13, 15)]
      in Segment
        { generate = selectRandom paths >>= addAPresses
        , apply = \path stateGroup -> do
            --printf "Segment 10: %s to %d states" (show path) (length stateGroup)
            resultMaybeStates <- for stateGroup $ \state -> do
                --printf "."
                loadState gb state

                bufferedWalk gb inputRef (path ++ [i_Left])
                encountered <- (/= 0) <$> cpuRead gb wIsInBattle
                if encountered
                then pure Nothing
                else do
                    writeIORef inputRef i_A
                    waitForItemJingle gb
                    Just <$> saveState gb
            let resultStates = catMaybes resultMaybeStates
            --printf "%d states remain\n" (length resultStates)
            pure $ (resultStates, fromIntegral (length resultStates))
        }
    , \gb inputRef ->
      let
        paths = fmap (\b -> if b then i_Down else i_Right)
            <$> enumeratePaths 0 3 [(0, 3), (0, 3)]
      in Segment
        { generate = selectRandom paths >>= addAPresses
        , apply = \path stateGroup -> do
            --printf "Segment 11: %s to %d states" (show path) (length stateGroup)
            resultMaybeStates <- for stateGroup $ \state -> do
                --printf "."
                loadState gb state

                bufferedWalk gb inputRef path
                encountered <- (/= 0) <$> cpuRead gb wIsInBattle
                if encountered
                then pure Nothing
                else Just <$> saveState gb
            let resultStates = catMaybes resultMaybeStates
            --printf "%d states remain\n" (length resultStates)
            pure $ (resultStates, fromIntegral (length resultStates))
        }
    -- B1F after moon stone
    , \gb inputRef ->
      let
        paths = fmap (\b -> if b then i_Right else i_Down)
            <$> enumeratePaths 0 16 [(0, 2), (0, 2), (0, 2), (0, 2), (0, 2), (0, 2), (0, 2), (0, 2), (0, 2), (0, 2), (0, 2), (0, 16)]
      in Segment
        { generate = selectRandom paths >>= addAPresses2
        , apply = \path stateGroup -> do
            --printf "Segment 12: %s to %d states" (show path) (length stateGroup)
            resultMaybeStates <- for stateGroup $ \state -> do
                --printf "."
                loadState gb state

                bufferedWalk gb inputRef path
                encountered <- (/= 0) <$> cpuRead gb wIsInBattle
                if encountered
                then pure Nothing
                else Just <$> saveState gb
            let resultStates = catMaybes resultMaybeStates
            --printf "%d states remain\n" (length resultStates)
            pure $ (resultStates, fromIntegral (length resultStates))
        }
    -- B2F after moon stone
    , \gb inputRef ->
      let
        paths = fmap (\b -> if b then i_Up else i_Right)
            <$> enumeratePaths 0 3 [(0, 3), (0, 3), (3, 3), (3, 3), (3, 3)]
      in Segment
        { generate = selectRandom paths >>= addAPresses
        , apply = \path stateGroup -> do
            --printf "Segment 13: %s to %d states" (show path) (length stateGroup)
            resultMaybeStates <- for stateGroup $ \state -> do
                --printf "."
                loadState gb state

                bufferedWalk gb inputRef path
                encountered <- (/= 0) <$> cpuRead gb wIsInBattle
                if encountered
                then pure Nothing
                else Just <$> saveState gb
            let resultStates = catMaybes resultMaybeStates
            --printf "%d states remain\n" (length resultStates)
            pure $ (resultStates, fromIntegral (length resultStates))
        }
    , \gb inputRef ->
      let
        paths = fmap (\b -> if b then i_Right else i_Down)
            <$> enumeratePaths 0 6 [(0, 1), (0, 1)]
      in Segment
        { generate = selectRandom paths >>= addAPresses
        , apply = \path stateGroup -> do
            --printf "Segment 14: %s to %d states" (show path) (length stateGroup)
            resultMaybeStates <- for stateGroup $ \state -> do
                --printf "."
                loadState gb state

                bufferedWalk gb inputRef path
                encountered <- (/= 0) <$> cpuRead gb wIsInBattle
                if encountered
                then pure Nothing
                else Just <$> saveState gb
            let resultStates = catMaybes resultMaybeStates
            --printf "%d states remain\n" (length resultStates)
            pure $ (resultStates, fromIntegral (length resultStates))
        }
    , \gb inputRef ->
      let
        paths = fmap (\b -> if b then i_Right else i_Up)
            <$> enumeratePaths 0 4 [(0, 1), (0, 1)]
      in Segment
        { generate = selectRandom paths >>= addAPresses
        , apply = \path stateGroup -> do
            --printf "Segment 15: %s to %d states" (show path) (length stateGroup)
            resultMaybeStates <- for stateGroup $ \state -> do
                --printf "."
                loadState gb state

                bufferedWalk gb inputRef path
                encountered <- (/= 0) <$> cpuRead gb wIsInBattle
                if encountered
                then pure Nothing
                else Just <$> saveState gb
            let resultStates = catMaybes resultMaybeStates
            --printf "%d states remain\n" (length resultStates)
            pure $ (resultStates, fromIntegral (length resultStates))
        }
    , \gb inputRef ->
      let
        paths = do
            choice <- fmap (\b -> if b then i_Down else i_Left)
                <$> enumeratePaths 0 17 [(8, 10), (8, 10), (8, 17), (8, 17)]
            pure $ choice ++ replicate 21 i_Left
      in Segment
        { generate = selectRandom paths >>= addAPresses
        , apply = \path stateGroup -> do
            --printf "Segment 16: %s to %d states" (show path) (length stateGroup)
            resultMaybeStates <- for stateGroup $ \state -> do
                --printf "."
                loadState gb state

                bufferedWalk gb inputRef path
                encountered <- (/= 0) <$> cpuRead gb wIsInBattle
                if encountered
                then pure Nothing
                else Just <$> saveState gb
            let resultStates = catMaybes resultMaybeStates
            --printf "%d states remain\n" (length resultStates)
            pure $ (resultStates, fromIntegral (length resultStates))
        }
    , \gb inputRef ->
      let
        paths = fmap (\b -> if b then i_Up else i_Left)
            <$> enumeratePaths 0 14 [(0, 10)]
      in Segment
        { generate = selectRandom paths >>= addAPresses
        , apply = \path stateGroup -> do
            --printf "Segment 17: %s to %d states" (show path) (length stateGroup)
            resultMaybeStates <- for stateGroup $ \state -> do
                --printf "."
                loadState gb state

                bufferedWalk gb inputRef path
                encountered <- (/= 0) <$> cpuRead gb wIsInBattle
                if encountered
                then do
                    advanceUntil gb ((/= 0) <$> cpuRead gb wIsInBattle)
                    species <- cpuRead gb wEnemyMonSpecies
                    level <- cpuRead gb wEnemyMonLevel
                    if species == 109
                    then Just <$> saveState gb
                    else pure Nothing
                else pure Nothing
            let resultStates = catMaybes resultMaybeStates
            --printf "%d states remain\n" (length resultStates)
            pure $ (resultStates, fromIntegral (length resultStates))
        }
    ]
-}

r3LassSegments :: [GB -> IORef Input -> Segment StateGroup [Input]]
r3LassSegments =
    [ \gb inputRef ->
      let
        paths = fmap (\b -> if b then i_Right else i_Down)
            <$> enumeratePaths 0 6 [(0, 6)]
--        paths = fmap (\b -> if b then i_Right else i_Up)
--            <$> enumeratePaths 0 8 [(7, 8)]
        in Segment
        { generate = selectRandom paths >>= (addAPressHelp 1 6 0.06)
        , apply = \path stateGroup -> do
            --printf "Segment 1: %s to %d states" (show path) (length stateGroup)
            resultMaybeStates <- for stateGroup $ \(state, frm, frm2, dsum) -> do
                --printf "."
                loadState gb state

                bufferedWalk gb inputRef (path ++ [i_Up])
--                bufferedWalk gb inputRef (path ++ [i_Left])
                encountered <- (/= 0) <$> cpuRead gb wIsInBattle
                if encountered
                then pure Nothing
                else do
                    writeIORef inputRef i_A
                    waitForItemJingle gb
                    state <- saveState gb
                    ffd3 <- cpuRead gb 0xFFD3
                    ffd4 <- cpuRead gb 0xFFD4
                    let hra = fromIntegral ffd3
                    let hrs = fromIntegral ffd4
                    let newDsum = (hra + hrs) `mod` 256
                    pure (Just (state, frm, frm2, newDsum))
            let resultStates = catMaybes resultMaybeStates
            --printf "%d states remain\n" (length resultStates)
            pure $ (resultStates, fromIntegral (length resultStates))
        }
    , \gb inputRef ->
      let
        paths = fmap (\b -> if b then i_Up else i_Right)
            <$> enumeratePaths 0 7 [(0, 7)]
--        paths = fmap (\b -> if b then i_Up else i_Right)
--            <$> enumeratePaths 0 6 []
      in Segment
        { generate = selectRandom paths >>= addAPressHelp 1 7 0.07
        , apply = \path stateGroup -> do
            --printf "Segment 2: %s to %d states" (show path) (length stateGroup)
            resultMaybeStates <- for stateGroup $ \(state, frm, frm2, dsum) -> do
                --printf "."
                loadState gb state

                bufferedWalk gb inputRef (path ++ [i_Up])
                encountered <- (/= 0) <$> cpuRead gb wIsInBattle
                if encountered
                then pure Nothing
                else do
                    writeIORef inputRef i_A
                    waitForItemJingle gb
                    state <- saveState gb
                    ffd3 <- cpuRead gb 0xFFD3
                    ffd4 <- cpuRead gb 0xFFD4
                    let hra = fromIntegral ffd3
                    let hrs = fromIntegral ffd4
                    let newDsum = (hra + hrs) `mod` 256
                    pure (Just (state, frm, frm2, newDsum))
            let resultStates = catMaybes resultMaybeStates
            --printf "%d states remain\n" (length resultStates)
            pure $ (resultStates, fromIntegral (length resultStates))
        }
    , \gb inputRef ->
      let
        paths = fmap (\b -> if b then i_Up else i_Left)
            <$> enumeratePaths 0 21 [(0, 21), (0, 21), (13, 21), (13, 21), (13, 21)]
      in Segment
        { generate = selectRandom paths >>= addAPressHelp 1 24 0.16
        , apply = \path stateGroup -> do
            --printf "Segment 3: %s to %d states" (show path) (length stateGroup)
            resultMaybeStates <- for stateGroup $ \(state, frm, frm2, dsum) -> do
                --printf "."
                loadState gb state

                bufferedWalk gb inputRef path
                encountered <- (/= 0) <$> cpuRead gb wIsInBattle
                if encountered
                then pure Nothing
                else do
                    state <- saveState gb
                    ffd3 <- cpuRead gb 0xFFD3
                    ffd4 <- cpuRead gb 0xFFD4
                    let hra = fromIntegral ffd3
                    let hrs = fromIntegral ffd4
                    let newDsum = (hra + hrs) `mod` 256
                    pure (Just (state, frm, frm2, newDsum))
            let resultStates = catMaybes resultMaybeStates
            --printf "%d states remain\n" (length resultStates)
            pure $ (resultStates, fromIntegral (length resultStates))
        }
    , \gb inputRef ->
      let
        paths = fmap (\b -> if b then i_Left else i_Down)
            <$> enumeratePaths 0 14 [(2, 14), (2, 14), (2, 14), (2, 14), (14, 14), (14, 14), (14, 14), (14, 14)]
      in Segment
        { generate = selectRandom paths >>= addAPressHelp 1 21 0.14
        , apply = \path stateGroup -> do
            --printf "Segment 4: %s to %d states" (show path) (length stateGroup)
            resultMaybeStates <- for stateGroup $ \(state, frm, frm2, dsum) -> do
                --printf "."
                loadState gb state

                bufferedWalk gb inputRef path
                encountered <- (/= 0) <$> cpuRead gb wIsInBattle
                if encountered
                then pure Nothing
                else do
                    advanceUntil gb $ do ((== 0x01) <$> cpuRead gb 0xD736)
                    da45 <- cpuRead gb 0xDA45
                    let igt0 = fromIntegral da45
                    let frame = fromIntegral frm
                    let igt0check = (frame + 23) `mod` 60
                    if (frame == 6) 
                    then do
                        printf "  --> Segment 4: Frame %d, EnterMap %d" (frame :: Int) (igt0 :: Int) 
                        if (igt0check == igt0)
                        then
                            printf " (*)\n"
                        else
                            printf "\n"
                    else 
                        printf ""

                    if (igt0check == igt0) 
                    then do 
                        state <- saveState gb
                        ffd3 <- cpuRead gb 0xFFD3
                        ffd4 <- cpuRead gb 0xFFD4
                        let hra = fromIntegral ffd3
                        let hrs = fromIntegral ffd4
                        let newDsum = (hra + hrs) `mod` 256
                        pure (Just (state, frm, frm2, newDsum))
                    else pure Nothing
            let resultStates = catMaybes resultMaybeStates
            --printf "%d states remain\n" (length resultStates)
            pure $ (resultStates, fromIntegral (length resultStates))
        }
    , \gb inputRef ->
      let
        paths = fmap (\b -> if b then i_Left else i_Down)
            <$> enumeratePaths 0 8 [(0, 8), (0, 8)]
      in Segment
        { generate = selectRandom paths >>= addAPresses3 >>= addAPresses4 >>= addAPresses5 >>= addAPresses6
        , apply = \path stateGroup -> do
            --printf "Segment 5: %s to %d states" (show path) (length stateGroup)
            resultMaybeStates <- for stateGroup $ \(state, frm, frm2, dsum) -> do
                --printf "."
                loadState gb state

                bufferedWalk gb inputRef path
                encountered <- (/= 0) <$> cpuRead gb wIsInBattle
                if encountered
                then pure Nothing
                else do
                    advanceUntil gb $ do ((== 0x01) <$> cpuRead gb 0xD736)
                    da45 <- cpuRead gb 0xDA45
                    let igt0 = fromIntegral da45
                    let frame = fromIntegral frm
                    let igt0check = (frame + 23) `mod` 60
                    if (frame == 6) 
                    then do
                        printf "  --> Segment 5: Frame %d, EnterMap %d" (frame :: Int) (igt0 :: Int) 
                        if (igt0check == igt0)
                        then
                            printf " (*)\n"
                        else
                            printf "\n"
                    else 
                        printf ""

                    if (igt0check == igt0) 
                    then do 
                        state <- saveState gb
                        ffd3 <- cpuRead gb 0xFFD3
                        ffd4 <- cpuRead gb 0xFFD4
                        let hra = fromIntegral ffd3
                        let hrs = fromIntegral ffd4
                        let newDsum = (hra + hrs) `mod` 256
                        pure (Just (state, frm, frm2, newDsum))
                    else pure Nothing
            let resultStates = catMaybes resultMaybeStates
            --printf "%d states remain\n" (length resultStates)
            pure $ (resultStates, fromIntegral (length resultStates))
        }
    , \gb inputRef ->
      let
        paths = fmap (\b -> if b then i_Right else i_Up)
            <$> enumeratePaths 0 3 [(0, 3), (3, 3), (3, 3)]
      in Segment
        { generate = selectRandom paths >>= addAPressHelp 1 5 0.72
        , apply = \path stateGroup -> do
            --printf "Segment 6: %s to %d states" (show path) (length stateGroup)
            resultMaybeStates <- for stateGroup $ \(state, frm, frm2, dsum) -> do
                --printf "."
                loadState gb state

                bufferedWalk gb inputRef (path ++ [i_Left, i_Up, i_Right])
                encountered <- (/= 0) <$> cpuRead gb wIsInBattle
                if encountered
                then pure Nothing
                else do
                    writeIORef inputRef i_A
                    waitForItemJingle gb
                    state <- saveState gb
                    ffd3 <- cpuRead gb 0xFFD3
                    ffd4 <- cpuRead gb 0xFFD4
                    let hra = fromIntegral ffd3
                    let hrs = fromIntegral ffd4
                    let newDsum = (hra + hrs) `mod` 256
                    pure (Just (state, frm, frm2, newDsum))
            let resultStates = catMaybes resultMaybeStates
            --printf "%d states remain\n" (length resultStates)
            pure $ (resultStates, fromIntegral (length resultStates))
        }
    , \gb inputRef ->
      let
        paths = fmap (\b -> if b then i_Left else i_Down)
            <$> enumeratePaths 0 3 [(0, 0), (0, 0), (0, 0), (0, 3)]
      in Segment
        { generate = selectRandom paths >>= addAPressHelp 1 6 0.72
        , apply = \path stateGroup -> do
            --printf "Segment 7: %s to %d states" (show path) (length stateGroup)
            resultMaybeStates <- for stateGroup $ \(state, frm, frm2, dsum) -> do
                --printf "."
                loadState gb state

                bufferedWalk gb inputRef path
                encountered <- (/= 0) <$> cpuRead gb wIsInBattle
                if encountered
                then pure Nothing
                else do
                    advanceUntil gb $ do ((== 0x01) <$> cpuRead gb 0xD736)
                    da45 <- cpuRead gb 0xDA45
                    let igt0 = fromIntegral da45
                    let frame = fromIntegral frm
                    if (frame == 6) 
                    then do
                        printf "  --> Segment 7: Frame %d, EnterMap %d\n" (frame :: Int) (igt0 :: Int) 
                    else 
                        printf ""
                    state <- saveState gb
                    ffd3 <- cpuRead gb 0xFFD3
                    ffd4 <- cpuRead gb 0xFFD4
                    let hra = fromIntegral ffd3
                    let hrs = fromIntegral ffd4
                    let newDsum = (hra + hrs) `mod` 256
                    pure (Just (state, frm, igt0, newDsum))
            let resultStates = catMaybes resultMaybeStates
            --printf "%d states remain\n" (length resultStates)
            pure $ (resultStates, fromIntegral (length resultStates))
        }
    , \gb inputRef ->
      let
        paths = fmap (\b -> if b then i_Right else i_Up)
            <$> enumeratePaths 0 8 [(0, 8), (0, 8)]
      in Segment
        { generate = selectRandom paths >>= addAPresses3 >>= addAPresses4 >>= addAPresses5 >>= addAPresses6
        , apply = \path stateGroup -> do
            --printf "Segment 8: %s to %d states" (show path) (length stateGroup)
            resultMaybeStates <- for stateGroup $ \(state, frm, frm2, dsum) -> do
                --printf "."
                loadState gb state

                bufferedWalk gb inputRef path
                encountered <- (/= 0) <$> cpuRead gb wIsInBattle
                if encountered
                then pure Nothing
                else do
                    advanceUntil gb $ do ((== 0x01) <$> cpuRead gb 0xD736)
                    da45 <- cpuRead gb 0xDA45
                    let igt0 = fromIntegral da45
                    let frame1 = fromIntegral frm
                    let frame2 = fromIntegral frm2
                    if (frame1 == 6) 
                    then do
                        printf "  --> Segment 8: Frame %d, EnterMap %d" (frame1 :: Int) (igt0 :: Int) 
                        if (igt0 == frame2)
                        then
                            printf " (*)\n"
                        else
                            printf "\n"
                    else 
                        printf ""

                    if (igt0 == frame2) 
                    then do 
                        state <- saveState gb
                        ffd3 <- cpuRead gb 0xFFD3
                        ffd4 <- cpuRead gb 0xFFD4
                        let hra = fromIntegral ffd3
                        let hrs = fromIntegral ffd4
                        let newDsum = (hra + hrs) `mod` 256
                        pure (Just (state, frm, frm2, newDsum))
                    else pure Nothing
            let resultStates = catMaybes resultMaybeStates
            --printf "%d states remain\n" (length resultStates)
            pure $ (resultStates, fromIntegral (length resultStates))
        }
    , \gb inputRef ->
      let
        paths = fmap (\b -> if b then i_Down else i_Left)
            <$> enumeratePaths 0 6 [(0, 6), (6, 6), (6, 6), (6, 6), (6, 6), (6, 6)]
      in Segment
        { generate = selectRandom paths >>= addAPressHelp 1 5 0.72 >>= addAPressHelp 7 11 0.72
        , apply = \path stateGroup -> do
            --printf "Segment 9: %s to %d states" (show path) (length stateGroup)
            resultMaybeStates <- for stateGroup $ \(state, frm, frm2, dsum) -> do
                --printf "."
                loadState gb state

                bufferedWalk gb inputRef path
                encountered <- (/= 0) <$> cpuRead gb wIsInBattle
                if encountered
                then pure Nothing
                else do
                    state <- saveState gb
                    ffd3 <- cpuRead gb 0xFFD3
                    ffd4 <- cpuRead gb 0xFFD4
                    let hra = fromIntegral ffd3
                    let hrs = fromIntegral ffd4
                    let newDsum = (hra + hrs) `mod` 256
                    pure (Just (state, frm, frm2, newDsum))
            let resultStates = catMaybes resultMaybeStates
            --printf "%d states remain\n" (length resultStates)
            pure $ (resultStates, fromIntegral (length resultStates))
        }
    , \gb inputRef ->
      let
        paths = fmap (\b -> if b then i_Up else i_Left)
            <$> enumeratePaths 0 15 [(0, 15), (0, 15), (0, 15), (0, 15), (0, 15), (13, 15), (13, 15)]
      in Segment
        { generate = selectRandom paths >>= addAPressHelp 1 5 0.72 >>= addAPressHelp 7 10 0.72 >>= addAPressHelp 12 15 0.72 >>= addAPressHelp 17 21 0.72
        , apply = \path stateGroup -> do
            --printf "Segment 10: %s to %d states" (show path) (length stateGroup)
            resultMaybeStates <- for stateGroup $ \(state, frm, frm2, dsum) -> do
                --printf "."
                loadState gb state

                bufferedWalk gb inputRef (path ++ [i_Left])
                encountered <- (/= 0) <$> cpuRead gb wIsInBattle
                if encountered
                then pure Nothing
                else do
                    writeIORef inputRef i_A
                    waitForItemJingle gb
                    state <- saveState gb
                    ffd3 <- cpuRead gb 0xFFD3
                    ffd4 <- cpuRead gb 0xFFD4
                    let hra = fromIntegral ffd3
                    let hrs = fromIntegral ffd4
                    let newDsum = (hra + hrs) `mod` 256
                    pure (Just (state, frm, frm2, newDsum))
            let resultStates = catMaybes resultMaybeStates
            --printf "%d states remain\n" (length resultStates)
            pure $ (resultStates, fromIntegral (length resultStates))
        }
    , \gb inputRef ->
      let
        paths = fmap (\b -> if b then i_Down else i_Right)
            <$> enumeratePaths 0 3 [(0, 3), (0, 3)]
      in Segment
        { generate = selectRandom paths >>= addAPressHelp 1 1 0.24 >>= addAPressHelp 3 4 0.48
        , apply = \path stateGroup -> do
            --printf "Segment 11: %s to %d states" (show path) (length stateGroup)
            resultMaybeStates <- for stateGroup $ \(state, frm, frm2, dsum) -> do
                --printf "."
                loadState gb state

                bufferedWalk gb inputRef path
                encountered <- (/= 0) <$> cpuRead gb wIsInBattle
                if encountered
                then pure Nothing
                else do
                    advanceUntil gb $ do ((== 0x01) <$> cpuRead gb 0xD736)
                    da45 <- cpuRead gb 0xDA45
                    let igt0 = fromIntegral da45
                    let frame = fromIntegral frm
                    --let igt0check22 = (frame + 22) `mod` 60
                    let igt0check23 = (frame + 23) `mod` 60
                    --let igt0check24 = (frame + 24) `mod` 60
                    if (frame == 6) 
                    then do
                        printf "  --> Segment 11: Frame %d, EnterMap %d" (frame :: Int) (igt0 :: Int) 
                        --if (igt0check22 == igt0 || igt0check23 == igt0 || igt0check24 == igt0)
                        if(igt0check23 == igt0)
                        then
                            printf " (*)\n"
                        else
                            printf "\n"
                    else 
                        printf ""

                    --if (igt0check22 == igt0 || igt0check23 == igt0 || igt0check24 == igt0)
                    if(igt0check23 == igt0)
                    then do 
                        state <- saveState gb
                        ffd3 <- cpuRead gb 0xFFD3
                        ffd4 <- cpuRead gb 0xFFD4
                        let hra = fromIntegral ffd3
                        let hrs = fromIntegral ffd4
                        let newDsum = (hra + hrs) `mod` 256
                        pure (Just (state, frm, frm2, newDsum))
                    else pure Nothing
            let resultStates = catMaybes resultMaybeStates
            --printf "%d states remain\n" (length resultStates)
            pure $ (resultStates, fromIntegral (length resultStates))
        }
    -- B1F after moon stone
    , \gb inputRef ->
      let
        paths = fmap (\b -> if b then i_Right else i_Down)
            <$> enumeratePaths 0 16 [(0, 2), (0, 2), (0, 2), (0, 2), (0, 2), (0, 2), (0, 2), (0, 2), (0, 2), (0, 2), (0, 2), (0, 16)]
      in Segment
        { generate = selectRandom paths >>= addAPresses2
        , apply = \path stateGroup -> do
            --printf "Segment 12: %s to %d states" (show path) (length stateGroup)
            resultMaybeStates <- for stateGroup $ \(state, frm, frm2, dsum) -> do
                --printf "."
                loadState gb state

                bufferedWalk gb inputRef path
                encountered <- (/= 0) <$> cpuRead gb wIsInBattle
                if encountered
                then pure Nothing
                else do
                    state <- saveState gb
                    ffd3 <- cpuRead gb 0xFFD3
                    ffd4 <- cpuRead gb 0xFFD4
                    let hra = fromIntegral ffd3
                    let hrs = fromIntegral ffd4
                    let newDsum = (hra + hrs) `mod` 256
                    pure (Just (state, frm, frm2, newDsum))
            let resultStates = catMaybes resultMaybeStates
            --printf "%d states remain\n" (length resultStates)
            pure $ (resultStates, fromIntegral (length resultStates))
        }
    -- B2F after moon stone
    , \gb inputRef ->
      let
        paths = fmap (\b -> if b then i_Up else i_Right)
            <$> enumeratePaths 0 3 [(0, 3), (0, 3), (3, 3), (3, 3), (3, 3)]
      in Segment
        { generate = selectRandom paths >>= addAPressHelp 1 3 0.39 >>= addAPressHelp 5 7 0.39
        , apply = \path stateGroup -> do
            --printf "Segment 13: %s to %d states" (show path) (length stateGroup)
            resultMaybeStates <- for stateGroup $ \(state, frm, frm2, dsum) -> do
                --printf "."
                loadState gb state

                bufferedWalk gb inputRef path
                encountered <- (/= 0) <$> cpuRead gb wIsInBattle
                if encountered
                then pure Nothing
                else do
                    state <- saveState gb
                    ffd3 <- cpuRead gb 0xFFD3
                    ffd4 <- cpuRead gb 0xFFD4
                    let hra = fromIntegral ffd3
                    let hrs = fromIntegral ffd4
                    let newDsum = (hra + hrs) `mod` 256
                    pure (Just (state, frm, frm2, newDsum))
            let resultStates = catMaybes resultMaybeStates
            --printf "%d states remain\n" (length resultStates)
            pure $ (resultStates, fromIntegral (length resultStates))
        }
    , \gb inputRef ->
      let
        paths = fmap (\b -> if b then i_Right else i_Down)
            <$> enumeratePaths 0 6 [(0, 1), (0, 1)]
      in Segment
        { generate = selectRandom paths >>= addAPressHelp 1 3 0.39 >>= addAPressHelp 5 7 0.39
        , apply = \path stateGroup -> do
            --printf "Segment 14: %s to %d states" (show path) (length stateGroup)
            resultMaybeStates <- for stateGroup $ \(state, frm, frm2, dsum) -> do
                --printf "."
                loadState gb state

                bufferedWalk gb inputRef path
                encountered <- (/= 0) <$> cpuRead gb wIsInBattle
                if encountered
                then pure Nothing
                else do
                    state <- saveState gb
                    ffd3 <- cpuRead gb 0xFFD3
                    ffd4 <- cpuRead gb 0xFFD4
                    let hra = fromIntegral ffd3
                    let hrs = fromIntegral ffd4
                    let newDsum = (hra + hrs) `mod` 256
                    pure (Just (state, frm, frm2, newDsum))
            let resultStates = catMaybes resultMaybeStates
            --printf "%d states remain\n" (length resultStates)
            pure $ (resultStates, fromIntegral (length resultStates))
        }
    , \gb inputRef ->
      let
        paths = fmap (\b -> if b then i_Right else i_Up)
            <$> enumeratePaths 0 4 [(0, 1), (0, 1)]
      in Segment
        { generate = selectRandom paths >>= addAPressHelp 1 2 0.26 >>= addAPressHelp 4 5 0.26
        , apply = \path stateGroup -> do
            --printf "Segment 15: %s to %d states" (show path) (length stateGroup)
            resultMaybeStates <- for stateGroup $ \(state, frm, frm2, dsum) -> do
                --printf "."
                loadState gb state

                bufferedWalk gb inputRef path
                encountered <- (/= 0) <$> cpuRead gb wIsInBattle
                if encountered
                then pure Nothing
                else do
                    state <- saveState gb
                    ffd3 <- cpuRead gb 0xFFD3
                    ffd4 <- cpuRead gb 0xFFD4
                    let hra = fromIntegral ffd3
                    let hrs = fromIntegral ffd4
                    let newDsum = (hra + hrs) `mod` 256
                    pure (Just (state, frm, frm2, newDsum))
            let resultStates = catMaybes resultMaybeStates
            --printf "%d states remain\n" (length resultStates)
            pure $ (resultStates, fromIntegral (length resultStates))
        }
    , \gb inputRef ->
      let
        --paths = do
        --    choice <- fmap (\b -> if b then i_Down else i_Left)
        --        <$> enumeratePaths 0 17 [(8, 10), (8, 10), (8, 17), (8, 17)]
        --    pure $ choice ++ replicate 21 i_Left
        paths = fmap (\b -> if b then i_Down else i_Left)
            <$> enumeratePaths 0 17 [(8, 10), (8, 10), (8, 17), (8, 17)]
      in Segment
        { generate = selectRandom paths >>= addAPressHelp 1 6 0.48 >>= addAPressHelp 8 13 0.48 >>= addAPressHelp 15 20 0.48
        , apply = \path stateGroup -> do
            --printf "Segment 16: %s to %d states" (show path) (length stateGroup)
            resultMaybeStates <- for stateGroup $ \(state, frm, frm2, dsum) -> do
                --printf "."
                loadState gb state

                bufferedWalk gb inputRef path
                encountered <- (/= 0) <$> cpuRead gb wIsInBattle
                if encountered
                then pure Nothing
                else do
                    state <- saveState gb
                    ffd3 <- cpuRead gb 0xFFD3
                    ffd4 <- cpuRead gb 0xFFD4
                    let hra = fromIntegral ffd3
                    let hrs = fromIntegral ffd4
                    let newDsum = (hra + hrs) `mod` 256
                    pure (Just (state, frm, frm2, newDsum))
            let resultStates = catMaybes resultMaybeStates
            --printf "%d states remain\n" (length resultStates)
            pure $ (resultStates, fromIntegral (length resultStates))
        }
    , \gb inputRef ->
      let
        --paths = do
        --    choice <- fmap (\b -> if b then i_Down else i_Left)
        --        <$> enumeratePaths 0 17 [(8, 10), (8, 10), (8, 17), (8, 17)]
        --    pure $ choice ++ replicate 21 i_Left
        paths = fmap (\b -> if b then i_Left else i_Up)
            <$> enumeratePaths 0 20 []
      in Segment
        { generate = selectRandom paths >>= addAPressHelp 1 2 0.32 >>= addAPressHelp 4 5 0.32 >>= addAPressHelp 15 16 0.32 >>= addAPressHelp 18 19 0.32
        , apply = \path stateGroup -> do
            --printf "Segment 17: %s to %d states" (show path) (length stateGroup)
            resultMaybeStates <- for stateGroup $ \(state, frm, frm2, dsum) -> do
                --printf "."
                loadState gb state

                bufferedWalk gb inputRef path
                encountered <- (/= 0) <$> cpuRead gb wIsInBattle
                if encountered
                then pure Nothing
                else do
                    state <- saveState gb
                    ffd3 <- cpuRead gb 0xFFD3
                    ffd4 <- cpuRead gb 0xFFD4
                    let hra = fromIntegral ffd3
                    let hrs = fromIntegral ffd4
                    let newDsum = (hra + hrs) `mod` 256
                    pure (Just (state, frm, frm2, newDsum))
            let resultStates = catMaybes resultMaybeStates
            --printf "%d states remain\n" (length resultStates)
            pure $ (resultStates, fromIntegral (length resultStates))
        }
    , \gb inputRef ->
      let
        --paths = do
        --    choice <- fmap (\b -> if b then i_Down else i_Left)
        --        <$> enumeratePaths 0 17 [(8, 10), (8, 10), (8, 17), (8, 17)]
        --    pure $ choice ++ replicate 21 i_Left
        paths = fmap (\b -> if b then i_Left else i_Up)
            <$> enumeratePaths 0 2 [(1, 2), (1, 2), (1, 2), (1, 2), (1, 2), (1, 2), (1, 2), (1, 2), (1, 2), (1, 2), (1, 2), (2, 2), (2, 2), (2, 2), (2, 2)]
      in Segment
        { generate = selectRandom paths >>= addAPressHelp 1 2 0.32 >>= addAPressHelp 4 5 0.32 >>= addAPressHelp 7 8 0.32 >>= addAPressHelp 10 11 0.32 >>= addAPressHelp 13 15 0.48
        , apply = \path stateGroup -> do
            --printf "Segment 18: %s to %d states" (show path) (length stateGroup)
            resultMaybeStates <- for stateGroup $ \(state, frm, frm2, dsum) -> do
                --printf "."
                loadState gb state

                bufferedWalk gb inputRef path
                encountered <- (/= 0) <$> cpuRead gb wIsInBattle
                if encountered
                then do
                    advanceUntil gb ((/= 0) <$> cpuRead gb wIsInBattle)
                    species <- cpuRead gb wEnemyMonSpecies
                    level <- cpuRead gb wEnemyMonLevel
                    if species == 109
                    then do
                        let frame = fromIntegral frm
                        if(frame == 6)
                        then do
                            d362 <- cpuRead gb 0xD362
                            d361 <- cpuRead gb 0xD361
                            let coordX = fromIntegral d362
                            let coordY = fromIntegral d361
                            let parasLevel = fromIntegral level
                            printf "  --> Segment 18: L%d Paras encounter at (%d, %d)\n" (parasLevel :: Int) (coordX :: Int) (coordY :: Int) 
                        else 
                            printf ""
                        state <- saveState gb
                        ffd3 <- cpuRead gb 0xFFD3
                        ffd4 <- cpuRead gb 0xFFD4
                        let hra = fromIntegral ffd3
                        let hrs = fromIntegral ffd4
                        let newDsum = (hra + hrs) `mod` 256
                        pure (Just (state, frm, frm2, newDsum))
                    else pure Nothing
                else pure Nothing
            let resultStates = catMaybes resultMaybeStates
            --printf "%d states remain\n" (length resultStates)
            pure $ (resultStates, fromIntegral (length resultStates))
        }
    ]
