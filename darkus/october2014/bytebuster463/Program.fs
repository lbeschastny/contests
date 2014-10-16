open System

/// High precedence, right associative backward pipe
let inline (^<|) f a = f a

/// Execution pipe
let inline ( .|> ) a f = f a; a

[<CustomEquality; CustomComparison>]
type Sieve = {
    cells: int list
} with
    member this.CompareTo (other: Sieve) =
        match this.cells, other.cells with
        | [], _ | _, [] -> false
        | h1::t1, h2::t2 ->
            let shift = (h1-h2+4)%4
            List.forall2
                (fun x y -> (x-y+4)%4 = shift)
                t1 t2

    interface IComparable with
        member this.CompareTo(other) = if this.Equals other then 0 else (this.cells :> IComparable).CompareTo (other :?> Sieve).cells

    override this.Equals(other) =
        match other with
        | :? Sieve as x -> this.CompareTo x
        | _ -> invalidArg "other" "cannot compare values of different types"

    override this.GetHashCode() = (this :> Object).GetHashCode()

let picks n data =
    let rec picks' acc n data = seq {
        match n with
        | 0 ->  yield acc |> List.rev
        | _ ->  for el in data do
                    yield! picks' (el::acc) (n-1) data
    }
    picks' [] n data

// Rotation matrix.
let rotate angle width (x, y) =
    let width' = width-1
    match angle with
    | 0 -> x, y
    | 1 -> width' - y, x
    | 2 -> width' - x, width' - y
    | 3 -> y, width' - x
    | _ -> invalidArg "angle" "rotation angle must be within [0..3]"

let mapCells width (cells: Sieve) =
    let width' = width/2
    cells.cells
    |> List.mapi ^<| fun i angle ->
        (i / width', i % width')
        |> rotate angle width

// return 4^(width^2 - 1) variations
let makeSieve width =
    if width < 2 then invalidArg "width" "Width must be 2+"
    if width%2 <> 0 then invalidArg "width" "Width must be even"
    let cells = width*width / 4 - 1
    picks cells [0..3]
    |> Seq.map ^<| fun x -> { cells = 0::x }
    |> Seq.map ^<| mapCells width

let show width (solution: (int*int) list) =
    let out = Array2D.create width width " . "
    for (y, x) in solution do
        out.[y, x] <- "( )"
    for i in [0..width-1] do
        for j in [0..width-1] do
            printf "%s" out.[i, j]
        printfn ""

let run width =
    makeSieve width
    .|> (Seq.toList >> List.length >> (printfn "Total: %d results."))
(*
    // Debug 2 - source
    .|> Seq.iteri ^<| fun i solution ->
                        Seq.iter(printf "%A ") solution
                        printfn " <- %d " i
*)
    .|> Seq.iteri ^<| fun i solution -> printfn "Solution #%d" i; show width solution
    .|> fun solutions -> printfn "%s" "Now testing..."
    .|> Seq.iter ^<| fun solution ->
        seq {
            for angle in [0..3] do
                yield! (solution |> Seq.map (fun cell -> rotate angle width cell))
        }
        |> Set.ofSeq
        |> fun result ->
            if Set.count result = width*width then printf "." else printf "!"
(*
    // Debug 3 - Sieve applied 4 times, should be a filled matrix
    |> Seq.iteri ^<| fun i solution ->
        Seq.iter(printf "%A ") solution;
        printfn " <- %d " i;
*)

[<EntryPoint>]
let main argv = 
    
    run 6 |> ignore

    #if DEBUG
    printfn "Press Enter..."
    Console.ReadLine() |> ignore
    #endif

    0
