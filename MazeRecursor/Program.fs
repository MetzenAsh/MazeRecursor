open System
open System.Drawing
open System.Drawing.Drawing2D
open System.IO
open System.Windows.Forms

// PLAY WITH THESE NUMBERS TO GENERATE DIFFERENT SIZES, INCREASE THE MODIFIER SO THAT PLUMPISH PEOPLE CAN FIT IN THE MAZE TOO.
let mazeSizeX, mazeSizeY, modifier = 60, 32, 30

type orientation = Horizontal | Vertical
type chamber =
    | Atom of pos:(int * int) * size:(int * int) * orientation
    | SubChamber of pos:(int * int) * size:(int * int) * SplitAt:int * DoorAt:int * orientation * chamber * chamber

// TRANSLATES MAZE COORDINATES TO BITMAP COORDINATES - PROVIDE 1-PIXEL GAP FOR WALLS BETWEEN MAZE CELLS
let tran x = x * (modifier + 1)

let canvasSizeX, canvasSizeY = (mazeSizeX |> tran) + 1, (mazeSizeY |> tran) + 1
let color = Color.Red
let rando = new System.Random()
let bitmap = new Bitmap (canvasSizeX, canvasSizeY)

// GDI+ FORM SPAWNER
let viewMaze (bmp:Bitmap) =
    use img = bmp
    use viewer = new Form(Text                  = "recursive division maze"
                         ,Width                 = img.Width
                         ,Height                = img.Height
                         ,BackColor             = Color.Black
                         ,BackgroundImage       = img
                         ,BackgroundImageLayout = ImageLayout.Center)  
    viewer.ShowDialog() |> ignore

// GENERATES THE MAZE AS A RECURSIVE DATA STRUCTURE, USING RECURSIVE DIVISION ALGORITHM
let rec genChamber (startX:int, startY:int) (sizeX:int, sizeY:int) =
    let orientation = // choose chamber's orientation randomly if square, otherwise split along the shorter axis
        match () with
            | _ when sizeX > sizeY -> Horizontal
            | _ when sizeX < sizeY -> Vertical
            | _ -> if rando.Next(2) = 1 then Vertical else Horizontal
    match (sizeX >= 2 && sizeY >= 2) with
    | false -> Atom ((startX, startY), (sizeX, sizeY), orientation) // Atom is the chamber size we are not splitting any further
    | true  ->
        let split = if orientation = Horizontal then rando.Next(sizeX - 1) + 1 else rando.Next(sizeY - 1) + 1
        let door  = if orientation = Horizontal then rando.Next(sizeY - 1) + 1 else rando.Next(sizeX - 1) + 1
        let subch1 = if orientation = Horizontal then genChamber (startX, startY) (split, sizeY) else genChamber (startX, startY) (sizeX, split)
        let subch2 = if orientation = Horizontal then genChamber (startX + split, startY) (sizeX - split, sizeY) else genChamber (startX, startY + split) (sizeX, sizeY - split)
        SubChamber((startX, startY), (sizeX, sizeY), split, door, orientation, subch1, subch2)

// PLOTS LINE ONTO BITMAP. ROUND EDGES. HEALTH & SAFETY & ALL...
let plotline (x1, y1) (x2, y2) pensize (col:Color) =
    use pen = new Pen(col, pensize)
    pen.SetLineCap(LineCap.Round, LineCap.Round, DashCap.Round)
    let point1, point2 = new Point(x1 |> tran, y1 |> tran), new Point(x2 |> tran, y2 |> tran)
    use graphics = Graphics.FromImage bitmap
    graphics.DrawLine(pen, point1, point2)

// DRAWS THE CHAMBER ONTO A BITMAP. RECURSIVELY. CALLS plotWall WITH ABSOLUTE VALUES.
let rec drawWalls chamber pensize col =
    match chamber with
    | SubChamber ((startX, startY), (sizeX, sizeY), split, door, orientation, subch1, subch2) ->
        match orientation with
        | Horizontal -> plotline (startX + split, startY)            (startX + split,  startY + door) pensize col
                        plotline (startX + split, startY + door + 1) (startX + split, startY + sizeY) pensize col
        | Vertical   -> plotline (startX, startY + split)            (startX + door,  startY + split) pensize col
                        plotline (startX + door + 1, startY + split) (startX + sizeX, startY + split) pensize col
        drawWalls subch1 pensize col
        drawWalls subch2 pensize col
    | _ -> ()

// DRAWS MESH, IN A COLOR AS DARK AS MANKIND'S BRIGHT FUTURE
let drawGrid sx sy pensize col =
    for ln in 0..sx do plotline (ln, 0) (ln, sy) pensize col
    for ln in 0..sy do plotline (0, ln) (sx, ln) pensize col

// DRAWS THE OUTER FRAME - WITH RANDOMLY PLACED DOOR ON EACH HORIZONTAL
let drawFrame pensize (col:Color) = 
    use pen = new Pen(col, pensize)
    let d1, d2 = rando.Next(mazeSizeY + 1), rando.Next(mazeSizeY + 1)
    plotline (0, 0) (0, d1) pensize col
    plotline (0, d1 + 1) (0, mazeSizeY) pensize col
    plotline (mazeSizeX, 0) (mazeSizeX, d2) pensize col
    plotline (mazeSizeX, d2 + 1) (mazeSizeX, mazeSizeY) pensize col
    plotline (0, mazeSizeY) (mazeSizeX, mazeSizeY) pensize col
    plotline (0, 0) (mazeSizeX, 0) pensize col


// ENOUGH SETTING UP OF TYPES AND FUNCTIONS, NOW FOR THE ACTUAL PROGRAM!
let chambr = genChamber (0, 0) (mazeSizeX, mazeSizeY)

drawGrid mazeSizeX mazeSizeY 1.0f Color.MidnightBlue
drawWalls chambr 7.0f Color.Yellow
drawFrame 7.0f Color.Yellow
viewMaze bitmap