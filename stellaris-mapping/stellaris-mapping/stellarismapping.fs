module stellarismapping
open Hekate
open GraphVizWrapper
open GraphVizWrapper.Commands
open GraphVizWrapper.Queries
open System
open System.Configuration
open System.Drawing
open System.IO
open SixLabors.ImageSharp
open SixLabors.ImageSharp.Formats
open System.IO
open CWTools
open FParsec
open CWTools.Process
open CWTools.Utilities.Position
open QuickGraph

type GetProcessStartInfoQuery2(infoqry : IGetProcessStartInfoQuery) =
    interface IGetProcessStartInfoQuery with
        member this.Invoke(x : IProcessStartInfoWrapper) = let y = infoqry.Invoke(x) in y.Arguments <- y.Arguments + " -n"; y
/// Creates some pngs
let genereateGraphFile (path:string) graphVizImageData =
    let procqry = GetStartProcessQuery()
    let infoqry = GetProcessStartInfoQuery()
    let temp = GetProcessStartInfoQuery2(infoqry)
    let wrapper = GraphGeneration(procqry, temp, 
                    RegisterLayoutPluginCommand(infoqry, procqry))
    
    // You probably don't need all 7:
    //[1] // different layout of graphs, e.g.: Enums.RenderingEngine.Neato
    wrapper.RenderingEngine <- Enums.RenderingEngine.Neato
    let output = wrapper.GenerateGraph(graphVizImageData, Enums.GraphReturnType.Png)
    
    File.WriteAllBytes("test.png", output)
    let stream = new MemoryStream(output)
    stream.Position <- (int64)0
    //printfn "%A" (SixLabors.ImageSharp.Image.DetectFormat(stream))
    use image = SixLabors.ImageSharp.Image.Load(new MemoryStream(output))
    // use image = System.Drawing.Image.FromStream(new MemoryStream(output))
    let filename = sprintf "%sgraph-%i.jpg" path (output.GetHashCode())
    do image.Save(filename, SixLabors.ImageSharp.Formats.Jpeg.JpegEncoder())
    let creationtime = File.GetLastWriteTime filename
    filename, creationtime


type NodeID = int
let getSystem (node : Node) =
    let key = node.Key
    match node.Child "coordinate", node.Child "hyperlane" with
    | Some c, Some hs -> 
        let a = hs.ValueClauses |> Seq.map (fun hl -> (hl.TagText "to" |> int, hl.TagText "length" |> decimal)) |> List.ofSeq
        let b = key |> int, c.TagText "x" |> decimal, c.TagText "y" |> decimal
        Some (a, b)
        
    | None, None -> None
    | _ -> None
    
type 'a Node = 'a * ('a * decimal) list
type 'a Edge = 'a * 'a * decimal

type 'a Graph = 'a list * 'a Edge list

type 'a AdjacencyGraph = 'a Node list

let getSystems (nodes : Node list) =
    let systems = nodes |> List.choose getSystem
    let coords = systems |> List.map (fun (_, (k, x, y)) -> k, (x, y)) |> Map.ofList
    let nodes = systems |> List.map (fun (es, (k, _, _)) -> k, es)
    let adjGraph : int AdjacencyGraph = nodes
    adjGraph, coords

let createDotFile (adjGraph : (NodeID * int) AdjacencyGraph ) (coords : (Map<int, (decimal * decimal)>)) =
    // let nodes = adjGraph |> List.map (fun (n, _) -> sprintf "%i[pos = \"%f, %f!\",label = \"\",shape = point]" n (coords.[n] |> fst |> (*) ((decimal) 2)) (coords.[n] |> snd |> (*) ((decimal) 2))) |> String.concat ";\n"
    let nodes = adjGraph |> List.map (fun ((n, s), _) -> sprintf "%i[pos = \"%f, %f!\",label = \"%i\"]" n (coords.[n] |> fst |> (*) ((decimal) 2)) (coords.[n] |> snd |> (*) ((decimal) 2)) s) |> String.concat ";\n"
    let edges = adjGraph 
                    |> List.collect (fun ((n, _), es) -> 
                                    es  |> List.filter (fun ((e, _), _) -> e > n)
                                        |> List.map (fun ((e, _), l) -> sprintf "%i -- %i[pos = \"%f,%f!\"]" n e (coords.[e] |> fst |> (*) ((decimal) 2)) (coords.[e] |> snd |> (*) ((decimal) 2)) )) |> String.concat ";\n"
    sprintf "graph { \nsplines=false\n %s;\n%s }" nodes edges


let convAdjGraphToHekateGraph (adjGraph : _ AdjacencyGraph) = 
    let nodes = adjGraph |> List.map (fun (n, _) -> n, n)
    let edges = adjGraph |> List.collect (fun (n, es) -> es |> List.map (fun (e, l) -> n, e, l))
    Graph.create nodes edges

let convHekateGraphToAdjGraph (g : Graph<NodeID,int,decimal>) : _ AdjacencyGraph = 
    let edges = Graph.Edges.toList g |> List.groupBy (fun (a, b, w) -> a) |> Map.ofList |> Map.map (fun _ es -> es |> List.map (fun (a, b, w) -> b, w))
    let nodes = Graph.Nodes.toList g |> List.map (fun (_, n) -> n, edges.[n])
    nodes
    
let test (g : Graph<NodeID,int,decimal>) = 
    // let s, g3 = Graph.Nodes.mapFold (fun s v (a) -> (Graph.Nodes.outwardDegree v, Graph.Nodes.outwardDegree v), s + 1) 0 g
    let g3 = Graph.Nodes.map (fun v (a) -> v, (Graph.Nodes.outwardDegree v g).Value) g
    g3



let testReadSave() =
    let res = 
        match CWTools.Parser.CKParser.parseFile "../../gamestate" with
        |Success(e,_,_) -> Some e
        |Failure(msg,_,_) -> printfn "%s" msg; None
    let res = res.Value |> ProcessCore.processNodeBasic "mod" range.Zero
    let res = res.Child "galactic_object"
    let res = res.Value
    let graph, coords = getSystems res.Children
    let hekateGraph = convAdjGraphToHekateGraph graph
    let graph = (test hekateGraph) |> convHekateGraphToAdjGraph
    // printfn "%A" (test hekateGraph)
    let dotfile = createDotFile graph coords
    //printfn "%A" dotfile
    File.WriteAllText("test.dot", dotfile)
    genereateGraphFile (Directory.GetCurrentDirectory()) dotfile 


[<EntryPoint>]
let main argv =
    testReadSave()
    // let genId() = Guid.NewGuid().ToString("N")
    // let someId1 = genId()
    // let someId2 = genId()
    // let someId3 = genId()

    // let ten = someId1, "10", true, "";
    // let five = genId(), "5", false, someId2;
    // let six = someId3, "6", false, "";

    // let fork = genId(), "Fork", someId3;
    // let spoon = someId2, "Spoon", someId1;
    // let knife = genId(), "Knife", someId1;

    // let graphdata = sampleGraphData [|fork; spoon; knife|] [|ten; five; six; |]
    // genereateGraphFile (Directory.GetCurrentDirectory()) graphdata
    printfn "%A" argv
    0 // return an integer exit code
