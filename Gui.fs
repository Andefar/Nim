module Gui


open System
open System.Drawing
open System.Windows.Forms

    // The window part
let window =
  new Form(Text="Game of Nim", Size=Size(525,300))

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
              MaximumSize=Size(100,50),Text="")

let heap2 =
  new Button(Location=Point(200,160),MinimumSize=Size(100,40),
              MaximumSize=Size(100,50),Text="")

let heap3 =
  new Button(Location=Point(350,160),MinimumSize=Size(100,40),
              MaximumSize=Size(100,50),Text="")

let newGame =
  new Button(Location=Point(200,220),MinimumSize=Size(100,50),
              MaximumSize=Size(100,50),Text="Start New Game")

let hintBut =
  new Button(Location=Point(420,240),MinimumSize=Size(50,25),
              MaximumSize=Size(25,25),Text="hint(3)")

let hint =
  new Label(Location=Point(470,240),Size=Size(50,25),Text="")

let disable bs = 
    for b in [heap1;heap2;heap2] do 
        b.Enabled  <- true
    for (b:Button) in bs do  
        b.Enabled  <- false

let enable bs = 
    for (b:Button) in bs do 
        b.Enabled  <- false
    for (b:Button) in bs do 
        b.Enabled  <- true


let updateHeap heap (n:int) = 
    match heap with
    | "1" -> (heap1.Text <- string(n))
    | "2" -> (heap2.Text <- string(n))
    | "3" -> (heap3.Text <- string(n))
    | _       -> failwith "heap does not exist"

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

let initGui = 
    window.Controls.Add messages
    window.Controls.Add matchDesc
    window.Controls.Add input
    window.Controls.Add heap1Desc
    window.Controls.Add heap2Desc
    window.Controls.Add heap3Desc
    window.Controls.Add heap1
    window.Controls.Add heap2
    window.Controls.Add heap3
    window.Controls.Add newGame
    window.Controls.Add hintBut
    window.Controls.Add hint

