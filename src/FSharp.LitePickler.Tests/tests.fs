﻿module FSharp.LitePickler.Tests

open FSharp.LitePickler.Unpickle
open FSharp.LitePickler.Pickle

open FSharp.Game.Data
open FSharp.Game.Data.Unpickle.Md3
open FSharp.Game.Data.Pickle.Md3

open System.IO
open FsUnit
open NUnit.Framework

let unpickleMd3 bytes = u_run u_md3 bytes

let pickleMd3 md3 =
    use stream = File.Create ("test.md3")
    p_run p_md3 md3 stream
    stream.Close ()
    File.ReadAllBytes ("test.md3")

[<Test>]
let ``with an arachnatron head md3, parsing should succeed and have a valid surface.`` () = 
    let bytes = File.ReadAllBytes ("head.md3")
    let md3 = unpickleMd3 <| LiteStream.Create bytes
    md3.Surfaces.Length |> should be (greaterThan 0)
    md3.Surfaces.[0].Header.Ident |> should equal "IDP3"

    let data = pickleMd3 md3
    let secondMd3 = unpickleMd3 <| LiteStream.Create bytes

    Assert.AreEqual (md3.Tags.Length, secondMd3.Tags.Length)
    Assert.AreEqual (md3, secondMd3)

[<Test>]
let ``with an arachnatron upper md3, parsing should succeed and have a valid surface.`` () = 
    let bytes = File.ReadAllBytes ("upper.md3")
    let md3 = unpickleMd3 <| LiteStream.Create bytes
    md3.Surfaces.Length |> should be (greaterThan 0)
    md3.Surfaces.[0].Header.Ident |> should equal "IDP3"

    let data = pickleMd3 md3
    let secondMd3 = unpickleMd3 <| LiteStream.Create bytes

    Assert.AreEqual (md3.Tags.Length, secondMd3.Tags.Length)
    Assert.AreEqual (md3, secondMd3)

[<Test>]
let ``with an arachnatron lower md3, parsing should succeed and have a valid surface.`` () = 
    let bytes = File.ReadAllBytes ("lower.md3")
    let md3 = unpickleMd3 <| LiteStream.Create bytes
    md3.Surfaces.Length |> should be (greaterThan 0)
    md3.Surfaces.[0].Header.Ident |> should equal "IDP3"

    for i = 1 to 1000 do
        let data = unpickleMd3 <| LiteStream.Create bytes
        ()