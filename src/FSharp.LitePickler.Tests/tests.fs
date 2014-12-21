module FSharp.LitePickler.Tests

open FSharp.LitePickler.Unpickle
open FSharp.LitePickler.Pickle

open FSharp.Game.Data.Md3
open FSharp.Game.Data.Md3.Unpickle
open FSharp.Game.Data.Md3.Pickle
open FSharp.Game.Data.Wad
open FSharp.Game.Data.Wad.Unpickle

open System.IO
open FsUnit
open NUnit.Framework

let unpickleMd3 bytes = u_run u_md3 bytes

let pickleMd3 md3 =
    use stream = File.Create ("test.md3")
    p_run p_md3 md3 (LiteWriteStream.ofStream stream)
    stream.Close ()
    File.ReadAllBytes ("test.md3")

[<Test>]
let ``with an arachnatron head md3, parsing should succeed and have a valid surface.`` () = 
    let bytes = File.ReadAllBytes ("head.md3")
    let md3 = unpickleMd3 <| LiteReadStream.ofBytes bytes
    md3.Surfaces.Length |> should be (greaterThan 0)
    md3.Surfaces.[0].Header.Ident |> should equal "IDP3"

    let data = pickleMd3 md3
    let secondMd3 = unpickleMd3 <| LiteReadStream.ofBytes bytes

    Assert.AreEqual (md3.Tags.Length, secondMd3.Tags.Length)
    Assert.AreEqual (md3, secondMd3)

[<Test>]
let ``with an arachnatron upper md3, parsing should succeed and have a valid surface.`` () = 
    let bytes = File.ReadAllBytes ("upper.md3")
    let md3 = unpickleMd3 <| LiteReadStream.ofBytes bytes
    md3.Surfaces.Length |> should be (greaterThan 0)
    md3.Surfaces.[0].Header.Ident |> should equal "IDP3"

    let data = pickleMd3 md3
    let secondMd3 = unpickleMd3 <| LiteReadStream.ofBytes bytes

    Assert.AreEqual (md3.Tags.Length, secondMd3.Tags.Length)
    Assert.AreEqual (md3, secondMd3)

[<Test>]
let ``with an arachnatron lower md3, parsing should succeed and have a valid surface.`` () = 
    let bytes = File.ReadAllBytes ("lower.md3")
    let md3 = unpickleMd3 <| LiteReadStream.ofBytes bytes
    md3.Surfaces.Length |> should be (greaterThan 0)
    md3.Surfaces.[0].Header.Ident |> should equal "IDP3"

    for i = 1 to 1000 do
        let data = unpickleMd3 <| LiteReadStream.ofBytes bytes
        ()

[<Test>]
let ``wad file`` () = 
    let bytes = File.ReadAllBytes ("SCYTHE.WAD")
    let wad = u_run u_wad <| LiteReadStream.ofBytes bytes

    let lumpThings =
        wad.LumpHeaders
        |> Array.choose (fun x ->
            if x.Name.Contains ("THINGS") then
                Some <| (u_run (u_lumpThings ThingFormat.Doom x.Size (int64 x.Offset)) <| LiteReadStream.ofBytes bytes)
            else
                None)

    let lumpLinedefs =
        wad.LumpHeaders
        |> Array.choose (fun x ->
            if x.Name.Contains ("LINEDEFS") then
                Some <| (u_run (u_lumpLinedefs x.Size (int64 x.Offset)) <| LiteReadStream.ofBytes bytes)
            else
                None)

    let lumpSidedefs =
        wad.LumpHeaders
        |> Array.choose (fun x ->
            if x.Name.Contains ("SIDEDEFS") then
                Some <| (u_run (u_lumpSidedefs x.Size (int64 x.Offset)) <| LiteReadStream.ofBytes bytes)
            else
                None)

    let lumpVertices =
        wad.LumpHeaders
        |> Array.choose (fun x ->
            if x.Name.Contains ("VERTEXES") then
                Some <| (u_run (u_lumpVertices x.Size (int64 x.Offset)) <| LiteReadStream.ofBytes bytes)
            else
                None)

    let lumpSectors =
        wad.LumpHeaders
        |> Array.choose (fun x ->
            if x.Name.Contains ("SECTORS") && not <| x.Name.Contains ("SSECTORS") then
                Some <| (u_run (u_lumpSectors x.Size (int64 x.Offset)) <| LiteReadStream.ofBytes bytes)
            else
                None)

    ()