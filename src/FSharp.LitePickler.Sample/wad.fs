namespace FSharp.Game.Data

open System
open System.Numerics

open FSharp.LitePickler.Core
open FSharp.LitePickler.Unpickle

[<RequireQualifiedAccess>]
module Wad =
    type Header = { IsPwad: bool; LumpCount: int; LumpOffset: int }
 
    [<Flags>]
    type ThingFlags =
        | SkillLevelOneAndTwo = 0x001
        | SkillLevelThree = 0x002
        | SkillLevelFourAndFive = 0x004
        | Deaf = 0x008
        | NotInSinglePlayer = 0x0010
 
    type Thing = { X: int16; Y: int16; Angle: int16; Type: int16; Flags: ThingFlags }
 
    type LumpHeader = { Offset: int32; Size: int32; Name: string }
 
    type Wad = { Header: Header; LumpHeaders: LumpHeader [] }

    let u_header : Unpickle<Header> =
        u_pipe3 (u_string 4) u_int32 u_int32 <|
        fun id lumpCount lumpOffset ->
            { IsPwad = if id = "IWAD" then false else true
              LumpCount = lumpCount
              LumpOffset = lumpOffset }

    let u_lumpHeader : Unpickle<LumpHeader> =
        u_pipe3 u_int32 u_int32 (u_string 8) <| fun offset size name -> { Offset = offset; Size = size; Name = name }

    let u_lumpHeaders count offset : Unpickle<LumpHeader []> =
        u_skipBytes offset >>. u_array count u_lumpHeader

    let u_wad : Unpickle<Wad> =
        u_lookAhead u_header >>= fun header ->
            (u_lookAhead <| (u_lumpHeaders header.LumpCount (int64 header.LumpOffset)) |>> (fun lumpHeaders -> { Header = header; LumpHeaders = lumpHeaders }))