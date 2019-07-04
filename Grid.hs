{- File: Grid.hs
   Copyright: (C) Mahjeed Marrow 2019
   Description: Main function and grid representation for
   Schelling model
-}

{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}

import qualified GI.Gtk as Gtk
import qualified GI.Gdk as Gdk
import Data.GI.Base
import GI.GLib as GL (timeoutAdd)
import Data.Text as DT
import Data.Text.Encoding as DTE
import Data.IORef
import System.IO
import System.Environment
import Schelling as SC
import qualified Data.List as DL
import System.IO.Unsafe
import Shuffle as SH

data Color = White | Red | Blue

data GUIComponents = GUIComponents { labels :: [Gtk.Label] }

data GUIState = GUIState {  isRunning    :: Bool,
                            shouldReset  :: Bool,
                            components   :: GUIComponents
                         }

-- ********************
-- BEGIN CSS FORMATTING
-- ********************
--  padding: 195px 195px 195px 195px;
paddingStr = "#label_red,\n" ++
             "#label_blue,\n" ++
             "#label_default {\n" ++
             "  padding: "

initLabelSize :: Int -> Int -> Int -> String
initLabelSize gridSize gridWidth gridHeight = let
  width_padding  =  ((gridWidth `div` gridSize) `div` 2)
  height_padding =  ((gridHeight `div` gridSize) `div` 2)
  top    = show height_padding
  left   = show width_padding
  bottom = show height_padding
  right  = show width_padding
  in
    paddingStr ++ top ++ "px " ++ left ++ "px " ++ bottom ++ "px "  ++ right ++ "px \n}\n"

initializeCSSFromString :: String ->  String -> Gtk.Window -> IO ()
initializeCSSFromString labelCSS cssFile window = do
    provider     <- Gtk.cssProviderNew
    cssFileData  <- readFile cssFile
    let contents = cssFileData ++ labelCSS
    Gtk.cssProviderLoadFromData provider (DTE.encodeUtf8.DT.pack $ contents)
    screen       <- #getScreen window
    Gtk.styleContextAddProviderForScreen screen provider  (fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_APPLICATION)
-- **************************
-- END CSS FORMATTING
-- **************************

-- **************************
-- BEGIN BASE GUI STATE LOGIC
-- **************************
{- initializes GUI state -}
initState :: City a -> IO (IORef GUIState)
initState city = do

    -- create label for each house in the city
    let labelMapping = DL.map newLabel (houses city)
    labels <- sequence $ labelMapping
    let components = GUIComponents labels

    -- Define the GUIState
    let state = GUIState False False components
    -- Define a newIORef for the state
    newIORef state

{- creates a new label of the house appropriate occupancy status -}
newLabel :: Home -> IO (Gtk.Label)
newLabel home =
         case (snd home) of
            "R" -> new Gtk.Label [#name := "label_red"]
            "B" -> new Gtk.Label [#name := "label_blue"]
            "O" -> new Gtk.Label [#name := "label_default"]

{- creates grid of cells corresponding to houses -}
createLabelGrid :: City a -> IORef (GUIState) -> IO (Gtk.Grid)
createLabelGrid city stateRef = do

    -- Define a new Grid layout for the labels
    gridLayout <- Gtk.gridNew
    -- Retrieve the pure state from the stateRef
    state <- readIORef stateRef

    -- define each grid cell and zip with houses in the city
    let cells = labels.components $ state
    let zipped = DL.zip (houses city) cells

    addToGrid <- mapM_ (attachCell gridLayout state) zipped

    set gridLayout [ #margin := 45 ]

    -- Set the CSS style for the container
    set gridLayout [#name :="labels_container"]
    return gridLayout

{- adds a house (cell) to the city (grid) based on
text representation of a city -}
attachCell :: Gtk.Grid -> GUIState -> (Home, Gtk.Label) -> IO (Gtk.Grid)
attachCell grid stateRef (((x,y), _), cell) = do
                    Gtk.gridAttach grid cell (fromIntegral y) (fromIntegral x) 1 1
                    set grid [#margin := 45 ]
                    set grid [#name :="labels_container"]
                    return grid

{- sets a cell to value corresponding to present GUIState -}
setCell :: Gtk.Grid -> GUIState -> (Home, Gtk.Label) -> IO (Gtk.Grid)
setCell grid stateRef (((x,y), z), cell) = do
                            case z of
                               "R" -> set cell [#name := "label_red"]
                               "B" -> set cell [#name := "label_blue"]
                               "O" -> set cell [#name := "label_default"]
                            return grid

{- resets a cell to default value -}
resetCell :: Gtk.Grid -> GUIState -> (Home, Gtk.Label) -> IO (Gtk.Grid)
resetCell grid stateRef (((_,_), _), cell) = do set cell [#name := "label_default"]
                                                return grid
-- *************************
-- END BASE GUI STATE LOGIC
-- *************************

-- *************************
-- BEGIN EVENT HANDLERS
-- *************************
{- handles key presses -}
hanldeKeyPress :: City a -> IORef (Int) -> IORef (Int) -> IORef (GUIState) -> Gtk.Label -> Gtk.Label -> Gtk.Label -> Gdk.EventKey -> IO Bool
hanldeKeyPress city r t stateRef rLabel tLabel satP eventKey = do

    index     <- newIORef (0 :: Int)
    keyVal    <- get eventKey #keyval
    radius    <- readIORef r
    threshold <- readIORef t

    -- Retrieve the pure state from the stateRef
    state <- readIORef stateRef

    newGUIState <- case keyVal of
        -- 'space bar' starts simulation
        32 -> do
            if not(isRunning state)
                then do
                    print "RUNNING SIMULATION"
                    timeoutAdd (fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_APPLICATION) 500 (handleTimeout index city radius threshold satP stateRef)

                    let newGUIState = GUIState True False (components state)
                    return newGUIState
                else do
                    return state

        -- 'r' resets the simulation and creates a new random grid
        114 -> do
            print "RESETTING SIMULATION"
            -- get the occupancy status of all of the homes in the current state of the city
            let occStatus = DL.map SC.getStatus (houses city)
            shuffleList <- SH.shuffle occStatus

            -- create a new city with the updated shuffled houses
            let newCity = SC.initCity (nCols city) (nRows city) shuffleList
            stateRef      <- initState newCity
            resetGrid     <- createLabelGrid newCity stateRef

            let cells  = labels.components $ state
            let zipped = DL.zip (houses newCity) cells

            -- render refreshed city grid
            let newGUIState = GUIState False True (components state)
            freshGrid <- mapM_ (setCell resetGrid newGUIState) zipped
            return newGUIState

        -- 'p' maps to Unicode value 112
        -- pauses a simulation at present state
        -- note: does *NOT* restart simulation when restarted
        112 -> do
            print "PAUSING SIMULATION"
            let newGUIState = GUIState False False (components state)
            return newGUIState

        -- left arrow increases radius by 1
        65361 -> do
          if not(isRunning state) then do
              print "INCREASING RADIUS +1"

              -- first, handle label update
              incrementR <- modifyIORef r (+1)
              let readInc = readIORef r
              let usable  = unsafePerformIO readInc
              let newRLabel = DT.concat ["R-SIZE: ", DT.pack (show usable)]
              Gtk.labelSetText rLabel newRLabel

              -- then, handle simulation update
              timeoutAdd (fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_APPLICATION) 1000 (handleTimeout index city usable threshold satP stateRef)

              let newGUIState = GUIState True False (components state)
              return newGUIState
          else do
              print "WARNING: Can't change radius during simulation!"
              return state

        -- right arrow decreases radius by 1
        65363 -> do
            if not(isRunning state) then do
                -- first, handle label update
                let readDec = readIORef r
                let initial  = unsafePerformIO readDec

                if initial < 2 then
                  do print "Radius cannot be below 1!"
                     return state
                else do
                  print "DECREASING RADIUS -1"
                  decrementR <- modifyIORef r (+ (-1))
                  let readDec = readIORef r
                  let usable  = unsafePerformIO readDec
                  let newRLabel = DT.concat ["R-SIZE: ", DT.pack (show usable)]
                  Gtk.labelSetText rLabel newRLabel

                  -- then, handle simulation update
                  timeoutAdd (fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_APPLICATION) 1000 (handleTimeout index city usable threshold satP stateRef)

                  let newGUIState = GUIState True False (components state)
                  return newGUIState
              else do
                  print "WARNING: Can't change radius during simulation!"
                  return state

        -- up arrow increases similarity threshold by 5
        65362 -> do
            let readInc = readIORef t
            let initial  = unsafePerformIO readInc

            if initial == 100 then
              -- can't increment beyond 100%
              do print "CANNOT HAVE TRESHOLD ABOVE 100!"
                 return state
            else do
              -- handle label update
              print "INCREASED SATISFACTION THRESHOLD BY 5 PERCENT"
              incrementT <- modifyIORef t (+5)
              let readInc = readIORef t
              let usable  = unsafePerformIO readInc
              let newTLabel = DT.concat ["SIMILARITY THRESHOLD: ", DT.pack (show usable)]
              Gtk.labelSetText tLabel newTLabel

              -- handle simulation update
              timeoutAdd (fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_APPLICATION) 1000 (handleTimeout index city radius usable satP stateRef)

              let newGUIState = GUIState True False (components state)
              return newGUIState
        -- down arrow decreases similarity threshold by 5
        65364 -> do
            let readInc = readIORef t
            let initial  = unsafePerformIO readInc

            if initial == 5 then
              -- can't decrement beyond 5%
              do print "CANNOT HAVE THRESHOLD OF 0!"
                 return state
            else do
              -- handle label update
              print "DECREASED SATISFACTION THRESHOLD BY 5 PERCENT"
              decrementT <- modifyIORef t (+ (-5))
              let readInc = readIORef t
              let usable  = unsafePerformIO readInc
              let newTLabel = DT.concat ["SIMILARITY THRESHOLD: ", DT.pack (show usable)]
              Gtk.labelSetText tLabel newTLabel

              -- handle simulation update
              timeoutAdd (fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_APPLICATION) 1000 (handleTimeout index city radius usable satP stateRef)

              let newGUIState = GUIState True False (components state)
              return newGUIState

        _ -> return state
    -- Update the IORef for the GUIState
    writeIORef stateRef newGUIState
    return True

handleTimeout :: IORef (Int) -> City a -> Int -> Int -> Gtk.Label -> IORef (GUIState) -> IO Bool
handleTimeout index city r t p stateRef  = do
    -- Check the console window to see that this is being print ~5 seconds
    putStrLn "Calling handleTimeout"

    -- Retrieve the pure state from the stateRef
    state     <- readIORef stateRef
    indexRead <- readIORef index

    -- increment index
    if indexRead < ((DL.length (houses city)) - 1)
    then do modifyIORef index (+1)
            indexRead <- readIORef index
            return ()
    else do newIndex  <- newIORef (0 :: Int)
            indexRead <- readIORef newIndex
            return ()

    updatedCity <- oneIter city (houses city) ((houses city) !! indexRead) r t

    -- update the percentage of satisfied residents
    let cityHomes  = houses updatedCity
    let satPer     = SC.percentSat cityHomes r t
    let newPLabel = DT.concat ["PERCENT SATISFIED: ", DT.pack (show satPer)]
    Gtk.labelSetText p newPLabel

    stateRef    <- initState updatedCity
    newGrid     <- createLabelGrid updatedCity stateRef
    let newGUIState = GUIState True False (components state)

    (result, newGUIState) <- case ((isRunning state), (shouldReset state)) of
        (True, _) -> do
            let cells = labels.components $ state
            let zipped = DL.zip (houses updatedCity) cells

            addToGrid <- mapM_ (setCell newGrid newGUIState) zipped
            return (True, newGUIState)

        (False, True) -> do
            let cells = labels.components $ state
            let zipped = DL.zip (houses updatedCity) cells

            resetGrid <- mapM_ (resetCell newGrid newGUIState) zipped
            -- Reset the state!
            return (False, GUIState False False (components state))

        (False, _) -> return (False, GUIState False False (components state))

    -- Update the IORef for the GUIState
    writeIORef stateRef newGUIState

    return result
-- ***************************
-- END EVENT HANDLERS
-- ***************************

-- ***************************
-- BEGIN SIMULATION STEP LOGIC
-- ***************************
{- performs one iteration of a step -}
oneIter :: City a -> [Home] -> Home -> Int -> Int -> IO (City a)
oneIter city homes house r t =
              if (satScore homes r house) < (fromIntegral t)
                  then do
                  let newL       = newLocation homes house r t
                  let oldL       = oldOcc newL house
                  -- insert new location for unsatisfied homeowner
                  let del1       = deleteAt (getIndex city oldL) homes
                  let isrt1      = insertAt (getIndex city newL) newL del1
                  -- mark previous location as open
                  let del2       = deleteAt (getIndex city house) isrt1
                  let isrt2      = insertAt (getIndex city house) (oldOcc house oldL) del2
                  let cityUpdate = city {houses = isrt2}
                  return cityUpdate
               else do
                  return city

{- performs one complete step through a city and returns
updated map -}
nIters :: Int -> City a -> [Home] -> Int -> Int -> IO (City a)
nIters n city homes r t
        | n == 1    = oneIter city homes (homes !! final) r t
        | otherwise = nIters (n-1) newCity (houses newCity) r t
      where newCity = unsafePerformIO $ oneIter city homes (homes !! index) r t
            index   = entries - n
            final   = entries - 1
            entries = (nCols city) * (nRows city)

{- performs up to maxSteps steps of the simulation -}
nSteps :: Int -> Int -> City a -> [Home] -> Int -> Int -> IO (City a)
nSteps steps n city homes r t
        | steps == 1 = nIters n city homes r t
        | otherwise  = nSteps (steps - 1) n newCity (houses newCity) r t
       where newCity = unsafePerformIO $ nIters n city homes r t
-- ***************************
-- END SIMULATION STEP LOGIC
-- ***************************

-- ***************************
-- BEGIN PRINT FUNCTIONS
-- ***************************
{- prints occupancy status of each house in a city -}
printCity :: City a -> [Home] -> IO ()
printCity city houses = if (DL.length houses) == row
                        then do printRow (DL.take row houses)
                        else do printRow (DL.take row houses)
                                printCity city remaining
                   where row       = nCols city
                         remaining = DL.drop (nCols city) houses

{- prints individual row of a city -}
printRow :: [Home] -> IO ()
printRow houses = do mapM putStr $ [ snd x ++ " " |  x <- houses]
                     putStr "\n"
-- ***************************
-- END PRINT FUNCTIONS
-- ***************************

-- ***************************
-- MAIN FUNCTION
-- ***************************
main :: IO ()
main = do
  args <- getArgs

-- ***************
-- Begin Text mode
-- ***************
  if (args !! 0) == "-t"
  then do
      -- parse all command line args and read file
      let fileName  = args !! 1
      let radius    = read (args !! 2) :: Int
      let threshold = read (args !! 3) :: Int
      let maxSteps  = read (args !! 4) :: Int
      fileToken <- readFile fileName

      -- get number of rows and columns for grid
      let nRows   = read (DL.words fileToken !! 0) :: Int
      let nCols   = read (DL.words fileToken !! 1) :: Int

      -- create a new city using text file grid specifications
      let initCity = SC.initCity nRows nCols $ DL.drop 2 $ DL.words fileToken
      let size    = nRows * nCols

      -- run simultation until stopping condition met
      result <- nSteps maxSteps size initCity (houses initCity) radius threshold

      -- print the city at the end of the simulation
      printCity result (houses result)
-- ***************
-- End Text mode
-- ***************

-- ***************
-- Begin Grid mode
-- ***************
  else do
    let gridSize = read (args !! 0) :: Int

    -- throw an error if grid size isn't within
    -- allowed parameter of 5 <= n <= 15
    if gridSize < 5 || gridSize > 15
    then error "Invalid Grid Size"

    -- otherwise the grid size is valid
    else do
      let redPercent   = (read (args !! 1) :: Float) / 100
      let bluePercent  = (read (args !! 2) :: Float) / 100
      let emptyPercent = (read (args !! 3) :: Float) / 100
      let maxStepsGUI  =  read (args !! 4) :: Int

      -- set default similarity threshold (60) and radius (2)
      defSim <- newIORef (60 :: Int)
      defRad <- newIORef  (2 :: Int)

      let simRead    = 60
      let radRead    = 2
      let currentRd  = 0
      let percentSat = 0

      -- set up all 900 GUI display values
      let emptyStr     = DT.concat ["PERCENT EMPTY: ", DT.pack (args !! 3)]
      let maxStepsStr  = DT.concat ["MAX STEPS: ", DT.pack (args !! 4)]
      let redStr       = DT.concat ["PERCENT RED: ", DT.pack (args !! 1)]
      let blueStr      = DT.concat ["PERCENT BLUE: ", DT.pack (args !! 2)]
      let gridStr      = DT.concat ["GRID SIZE: ", DT.pack (args !! 0)]
      let simStr       = DT.concat ["SIMILARITY THRESHOLD: ", DT.pack (show simRead)]
      let radStr       = DT.concat ["R-SIZE: ", DT.pack (show radRead)]
      let perSat       = DT.concat ["PERCENT SATISFIED: ", DT.pack (show percentSat)]

      {- get the number of cells per occupancy status
      i.e. redCount is the number of homes owned by "Red"
      in a city. note: to account for rounding, this program adds
      an "extra" red house
      -}
      let redCount    = (round $ (fromIntegral ((^2) gridSize)) * redPercent) + 1
      let blueCount   = round $ (fromIntegral ((^2) gridSize)) * bluePercent
      let emptyCount  = round $ (fromIntegral ((^2) gridSize)) * emptyPercent

      let redList      = DL.replicate redCount "R"
      let blueList     = DL.replicate blueCount "B"
      let emptyList    = DL.replicate emptyCount "O"

      -- create list of random home occupancy statuses
      fullList <- SH.shuffle (redList ++ blueList ++ emptyList)

      -- initialize a new city using command line specifications
      -- and random home occupancies
      let initCity = SC.initCity gridSize gridSize fullList
-- *********************
-- Start Grid Rendering
-- *********************
      Gtk.init Nothing
      win <- new Gtk.Window [ #title  := "SCHELLING MODEL OF HOUSING SEGREGATION",
                              #defaultHeight  := 600,
                              #defaultWidth   := 800,
                              #name := "window"
                            ]

      on win #destroy Gtk.mainQuit
      let cellSize = 30
      let gridWidth  = (800 - 20)
      let gridHeight = (600 - 20)
      let labelStyleCSS = initLabelSize cellSize gridWidth gridHeight

      -- Initialize the Application level styling with CSS styling information for the
      -- label sizes.
      initializeCSSFromString labelStyleCSS "grid.css" win

      -- Initialize the state of the program
      stateRef <- initState initCity

      -- Create the widgets for the application
      labelsGridLayout  <- createLabelGrid initCity stateRef

      -- Create the application main layout
      layoutWindow <- Gtk.boxNew Gtk.OrientationVertical 0
      #add layoutWindow labelsGridLayout

      -- Add the main application layout to the window
      #add win layoutWindow

      -- Add labels for all of the simulation data
      -- max steps
      maxSteps     <- new Gtk.Label [#label := maxStepsStr]
      #add layoutWindow maxSteps
      -- percent of homeowners satisfied
      satP   <- new Gtk.Label [#label := perSat]
      #add layoutWindow satP
      -- size of grid
      gridSize     <- new Gtk.Label [#label := gridStr]
      #add layoutWindow gridSize
      -- neighbor radius
      radiusLab    <- new Gtk.Label [#label := radStr]
      #add layoutWindow radiusLab
      -- percentage of empty homes
      pEmpty       <- new Gtk.Label [#label := emptyStr]
      #add layoutWindow pEmpty
      -- percentage of red homes
      pRed         <- new Gtk.Label [#label := redStr]
      #add layoutWindow pRed
      -- percentage of blue homes
      pBlue        <- new Gtk.Label [#label := blueStr]
      #add layoutWindow pBlue
      -- satisfaction level threshold
      thresLab     <- new Gtk.Label [#label := simStr]
      #add layoutWindow thresLab

      -- Add a global function to handle key presses in application
      on win #keyPressEvent (hanldeKeyPress initCity defRad defSim stateRef radiusLab thresLab satP)

      -- Show the window
      #showAll win

      -- Call the main action of the GTK library
      Gtk.main
-- *********************
-- End Grid Rendering
-- *********************
-- *********************
-- END PROGRAM
-- *********************
