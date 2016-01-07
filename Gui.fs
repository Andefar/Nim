module Gui

open System.Drawing
open System.Windows.Forms
open System.Text.RegularExpressions
open System

    // The window part
let window =
  new Form(Text="Game of Nim", Size=Size(520,750), BackColor=Color.White)

let messages =
  new Label(Location=Point(60,20),Size=Size(400,25),Text="")

let matchDesc =
  new Label(Location=Point(226,50),Size=Size(100,25),Text="Input here:")

let input =
  new TextBox(Location=Point(227,75),Size=Size(50,25))

let heap1Desc =
  new Label(Location=Point(83,135),Size=Size(100,25),Text="Heap 1:")

let heap2Desc =
  new Label(Location=Point(233,135),Size=Size(100,25),Text="Heap 2:")

let heap3Desc =
  new Label(Location=Point(383,135),Size=Size(100,25),Text="Heap 3:")

let heap1 =
  new Button(Location=Point(50,160),MinimumSize=Size(100,40),
              MaximumSize=Size(100,50),Text="", BackColor=Color.LightBlue)

let heap2 =
  new Button(Location=Point(200,160),MinimumSize=Size(100,40),
              MaximumSize=Size(100,50),Text="", BackColor=Color.LightBlue)

let heap3 =
  new Button(Location=Point(350,160),MinimumSize=Size(100,40),
              MaximumSize=Size(100,50),Text="", BackColor=Color.LightBlue)

let heaps = [heap1;heap2;heap3]
let heapLabels = [heap1Desc;heap2Desc;heap3Desc]

let cancel =
  new Button(Location=Point(370,75),MinimumSize=Size(60,25),
              MaximumSize=Size(100,50),Text="Cancel", BackColor=Color.LightCoral)

let newGame =
  new Button(Location=Point(200,220),MinimumSize=Size(100,50),
              MaximumSize=Size(100,50),Text="Start New Game", BackColor=Color.LightGreen)

let hintBut =
  new Button(Location=Point(350,240),MinimumSize=Size(50,25),
              MaximumSize=Size(25,25),Text="Hint (3)", BackColor=Color.LightBlue)

let bmpPicture = new Bitmap(@"C:\Users\Silas\Desktop\mat.jpg")
let dead = new Bitmap(@"C:\Users\Silas\Desktop\not.gif")

let mutable heapList1 = []
let mutable heapList2 = []
let mutable heapList3 = []
let firstInit = true


for x in [90;240;390] do
    for y in 1..12 do
        let stick = new Label(Location=Point(x,300+(y*30)),Size=Size(25,25))
        stick.BackgroundImage <- bmpPicture
        if x = 90 then heapList1 <- List.append heapList1 [stick]
        if x = 240 then heapList2 <- List.append heapList2 [stick]
        if x = 390 then heapList3 <- List.append heapList3 [stick]
        window.Controls.Add stick 

let changeMatches (heap:string) n =
    let mutable curHeap = []
    if heap = "1" then curHeap <- heapList1
    if heap = "2" then curHeap <- heapList2
    if heap = "3" then curHeap <- heapList3
    let mutable k = 0
    for (mat:Label) in curHeap do 
       if (k<(12-n)) then
          mat.BackgroundImage <- dead
       else
          mat.BackgroundImage <- bmpPicture
       k <- k + 1

let hint = new Label(Location=Point(420,240),Size=Size(50,25),Text="")

let disable bs = 
    for b in (heaps@[newGame;hintBut;cancel]) do 
        b.Enabled  <- true
    for (b:Button) in bs do  
        b.Enabled  <- false

let enable bs = 
    for (b:Button) in (heaps@[newGame;hintBut;cancel]) do 
        b.Enabled  <- false
    for (b:Button) in bs do 
        b.Enabled  <- true

let updateHeap heap (n:int) = 
    changeMatches heap n
    match heap with
    | "1" -> (heap1.Text <- string(n))
    | "2" -> (heap2.Text <- string(n))
    | "3" -> (heap3.Text <- string(n))
    | _       -> failwith "heap does not exist"

let text() = if (Regex.Match(input.Text,"^\d+$").Success) then int(input.Text) else 0 

let printMessage str = 
    messages.Text <- str
    messages.AutoSize <- false
    messages.TextAlign <- ContentAlignment.MiddleCenter
     
let showInput str = 
    input.Text <- str

let showHint str = 
    hint.Text <- str
    hint.AutoSize <- false
    hint.TextAlign <- ContentAlignment.MiddleCenter 

let showHintsLeft str = 
    hintBut.Text <- str
    hintBut.AutoSize <- false
    hintBut.TextAlign <- ContentAlignment.MiddleCenter 

let initGui() = 
    window.TopMost <- true
    window.StartPosition <- FormStartPosition.CenterScreen
    window.FormBorderStyle  <- FormBorderStyle.Fixed3D

    window.Controls.Add messages
    window.Controls.Add matchDesc
    window.Controls.Add input
    
    List.iter (fun x -> window.Controls.Add x) heaps
    List.iter (fun x -> window.Controls.Add x) heapLabels
    window.Controls.Add cancel
    window.Controls.Add newGame
    window.Controls.Add hintBut
    window.Controls.Add hint

