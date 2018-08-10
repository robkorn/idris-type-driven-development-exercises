
module Shape

public export
data Shape = Triangle Double Double
           | Rectangle Double Double
           | Circle Double

private
rectangle_area : Double -> Double -> Double
rectangle_area x y = x * y

export
area : Shape -> Double
area (Triangle x y) = 0.5 * rectangle_area x y
area (Rectangle x y) = rectangle_area x y
area (Circle x) = pi * x * x

-- 10.3 Ex # 2
public export
data ShapeView : (s : Shape) -> Type where
     VTri : ShapeView (Triangle base height)
     VRect : ShapeView (Rectangle width height)
     VCirc : ShapeView (Circle r)

shapeView : (s : Shape) -> ShapeView s
shapeView (Triangle x y) = VTri
shapeView (Rectangle x y) = VRect
shapeView (Circle x) = VCirc

narea : Shape -> Double
narea x with (shapeView x)
  narea (Triangle base height) | VTri = 0.5 * base * height
  narea (Rectangle width height) | VRect = width * height
  narea (Circle r) | VCirc = pi * r * r


