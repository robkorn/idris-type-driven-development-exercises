

data Shape : Type where
  ||| Triangle; Base & Height
  Triangle : Double -> Double -> Shape
  ||| Rectangle; Width & Height
  Rectangle : Double -> Double -> Shape
  ||| Circle; Radius
  Circle : Double -> Shape
%name Shape shape, shape1, shape2

data Picture = Primitive Shape
             | Combine Picture Picture
             | Roatate Double Picture
             | Translate Double Double Picture
%name Picture pic, pic1, pic2


area : Shape -> Double
area (Triangle x y) = 0.5 * x * y
area (Rectangle x y) = x * y
area (Circle r) = pi * r * r


myTriangle : Picture
myTriangle = Primitive (Triangle 10 10)

myRect : Picture
myRect = Primitive (Rectangle 20 10)

myCircle : Picture
myCircle = Primitive (Circle 5)

testPic : Picture
testPic = Combine (Translate 5 5 myRect) (Combine (Translate 35 5 myCircle) (Translate 15 25 myTriangle))


pictureArea : Picture -> Double
pictureArea (Primitive shape) = area shape
pictureArea (Combine pic pic1) = pictureArea pic + pictureArea pic1
pictureArea (Roatate x pic) = pictureArea pic
pictureArea (Translate x y pic) = pictureArea pic




