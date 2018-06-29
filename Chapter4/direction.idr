

data Direction = North | East | South | West

data Nat = Z | S Nat

turnClockwise : Direction -> Direction
turnClockwise North = East
turnClockwise East = South
turnClockwise South = West
turnClockwise West = North

