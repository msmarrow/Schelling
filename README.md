# Schelling Residential Segregation Model Simulator

## Project Description: 
The Schelling model of segregation is an agent-based model that illustrates how individual tendencies regarding neighbors can lead to segregation. The model is especially useful for the study of residential segregation of ethnic groups where agents represent householders who relocate in the city. To read more, click here: http://jasss.soc.surrey.ac.uk/15/1/6.html.

## Implementation
Model simulator written in Haskell, GUI created using GTK. 

## How to run
#### 1. Compile the program using GHC by running the following command:
`ghc Grid.hs`

#### 2. The program can be run in either text mode or GUI mode. Running in text mode requires the following arguments:
* -t flag to indicate text mode 
* Radius: spcifies the number of houses a given house will be compared to when computing similarity score (range: an integer x: 0 <= x <= 5)
* Similarity Threshold: the similarity score at which point a house is considered by the model to be "satisfied." Any score lower than the threshold triggers a move to a different position in the grid. 
* Max Steps: number of simulation iterations (for smaller grids, some integer < 10 is reasonable)

To begin a text mode simulation with, say, a radius of 2, a threshold of 80%, and a max number of steps of 4, run the following command with your compiled executable:
`./Grid -t 2 .8 5`

Running in GUI mode requires the following arguments:
* -g flag to indicate GUI mode 
* Grid Size: the dimensions of the grid (range: an integer x: 5 <= x <= 15)
* Red Percentage: percentage of squares in the grid that are red
* Blue Percentage: percentage of squares in the grid that are blue
* Empty Percentgae: percentage of squares in the grid that are unoccupied
* Max Steps: number of simulation iterations (some integer < 10 is reasonable)

To begin a GUI mode simulation with, say, a grid size of 10x10, a red percentage of 40%, a blue percentage of 40%, an empty percentage 20%, and a max steps value of 4, run the following command with your compiled executable:
`./Grid -g 10 .4 .4 .2 4`

#### 3. When running in GUI mode, the simulation is responsive to the following keyboard commands:
* "Space Bar" - starts simulation 
* "R" - resets the simulation and creates a random grid
* "P" - pauses simulation 
* "Left Arrow" - Increases radius by 1
* "Right Arrow" - Decreases radius by 1 
* "Up Arrow" - Increases similarity threshold by 5%
* "Down Arrow" - Decreases similarity threshold by 5%
