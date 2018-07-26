
module Ord

record Album where
    constructor MkAlbum
    artist : String
    title : String
    year : Integer

Eq Album where
  (==) (MkAlbum a t y) (MkAlbum a' t' y') = a == a' && t == t' && y == y'

Ord Album where
  compare (MkAlbum a t y) (MkAlbum a' t' y') = case compare a a' of
                                                    EQ => case compare y y' of
                                                               EQ => compare t t'
                                                               d_y => d_y
                                                    d_a => d_a

Show Album where
  show (MkAlbum artist title year) = title ++ " by " ++ artist ++ " (Released on: " ++ show year ++ ")"


help : Album
help = MkAlbum "The Beatles" "Help" 1965

rubbersoul : Album
rubbersoul = MkAlbum "The Beatles" "Rubber Soul" 1965

clouds : Album
clouds = MkAlbum "Joni Mitchell" "Clouds" 1969

hunkydory : Album
hunkydory = MkAlbum "David Bowie" "Hunky Dory" 1971

heroes : Album
heroes = MkAlbum "David Bowie" "Heroes" 1977

collection : List Album
collection = [help, rubbersoul, clouds, hunkydory, heroes]



data Shape = Triangle Double Double
           | Rectangle Double Double
           | Circle Double

-- Ex. 7_1
Eq Shape where
  (==) (Triangle x z) (Triangle y w) = x == y && z == w
  (==) (Rectangle x z) (Rectangle y w) = x == y && z == w
  (==) (Circle x) (Circle y) = x == y
  (==) _ _ = False

-- Ex. 7_2

getArea : Shape -> Double
getArea (Triangle x y) = (x * y) / 2
getArea (Rectangle x y) = x * y
getArea (Circle r) = pi * r * r

Ord Shape where
  compare x y = let sa1 = getArea x
                    sa2 = getArea y
                    in compare sa1 sa2

testShapes : List Shape
testShapes = [Circle 3, Triangle 3 9, Rectangle 2 6, Circle 4, Rectangle 2 7]

