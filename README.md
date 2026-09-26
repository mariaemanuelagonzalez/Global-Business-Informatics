# Global-Business-Informatics

#r "nuget:DIKU.Canvas, 2.0"
open Canvas
open Color

let w = 400 // width and height of the window

let rec generateTree (p0:float*float) (angles:float*float) (length:float*float) (width:float*float) (n:int): PrimitiveTree =
  if n < 1 then
    emptyTree
  else
    let x,y = p0
    let a, da = angles
    let len, lenMul = length
    let wth, wthMul = width 
    let p1 = (x+len * sin a, y-len * cos a) // coordinate system points down, but trees grow up
    let trunk = piecewiseAffine green wth [p0; p1]
    let leftBranches = generateTree p1 (a+a,da) (len*lenMul, lenMul) (wth*wthMul, wthMul) (n-1)
    let leftTree = onto trunk leftBranches
    let rightTree = rotate x y da leftTree
    onto leftTree rightTree
    

let rnd = System.Random ()
let v = rnd.Next 10

/// <summary>
/// To make random positions. 
/// </summary>
/// <param x1,x2 ="p">The positions centered around p.</param>
/// <param x2,y2 ="dp">Width and height contained by dp.</param>
/// <returns>Returns a list of n random positions on Canvas for the root of trees.</returns>
let makeRandomPositions (p: int * int) (dp: int * int) (n: int): (float * float) list = 
    let x1,y1 = p
    let x2,y2 = dp
    let p1 =float(rnd.Next x2)
    let list = List.init n (fun i -> ((float(rnd.Next x2)),(float(rnd.Next y2))))
    list  
printfn "%A" (makeRandomPositions (0,0) (400,400) (5))

/// <summary>
/// Draws a tree.
/// </summary>
/// <param draw ="generateTree">The tree on canvas.</param>
/// <param render ="Tree">The draw of a tree on canvas.</param>
/// <returns>The result of drawing a tree on canvas.</returns>
let draw = 
   generateTree (float w/2.0, float w/2.0) (-0.2,3.14/6.0) (40.0,0.8) (3.0,0.7) 10
  |> make

// Render Sierpinski's triangle
render "Tree" w w draw

