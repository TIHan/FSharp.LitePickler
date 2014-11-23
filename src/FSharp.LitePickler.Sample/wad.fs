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

    type ChildType =
        | Subnode = 1
        | Subsector = 2

    [<Struct>]
    type Node =
        val StartLineX: uint16
        val StartLineY: uint16
        val EndLineX: uint16
        val EndLineY: uint16
        val RightBoundingBox: uint64
        val LeftBoundingBox: uint64
        val RightChild: uint16
        val LeftChild: uint16
        val RightChildType: ChildType
        val LeftChildType: ChildType

        new (startLineX, startLineY, endLineX, endLineY, rightBoundingBox, leftBoundingBox, rightChild, leftChild, rightChildType, leftChildType) =
            { StartLineX = startLineX
              StartLineY = startLineY
              EndLineX = endLineX
              EndLineY = endLineY
              RightBoundingBox = rightBoundingBox
              LeftBoundingBox = leftBoundingBox
              RightChild = rightChild
              LeftChild = leftChild
              RightChildType = rightChildType
              LeftChildType = leftChildType }

    type LumpNode = { Nodes: Node [] }

    [<Struct>]
    type Subsector =
        val SegCount: int
        val FirstSegNumber: int

        new (segCount, firstSegNumber) = { SegCount = segCount; FirstSegNumber = firstSegNumber }

    type LumpSubsector = { Subsectors: Subsector [] }

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

    [<Literal>] 
    let nodeSize = 28
    let u_node : Unpickle<Node> =
        u_pipe8 u_uint16 u_uint16 u_uint16 u_uint16 u_uint64 u_uint64 u_uint16 u_uint16 <|
        fun startLineX startLineY endLineX endLineY rightBoundingBox leftBoundingBox rightChild leftChild ->
            let bitNumber = 15
            let rightBit = (int rightChild &&& (1 <<< bitNumber)) <> 0;
            let leftBit = (int leftChild &&& (1 <<< bitNumber)) <> 0;

            let rightChildType =
                if rightBit = false then
                    ChildType.Subnode
                else
                    ChildType.Subsector

            let leftChildType =
                if leftBit = false then
                    ChildType.Subnode
                else
                    ChildType.Subsector

            Node (startLineX, startLineY, endLineX, endLineY, rightBoundingBox, leftBoundingBox, rightChild, leftChild, rightChildType, leftChildType)

    let u_nodes count offset : Unpickle<Node []> =
        u_skipBytes offset >>. u_array count u_node

    let u_lumpNode size offset : Unpickle<LumpNode> =
        u_lookAhead (u_nodes (size / nodeSize) offset) |>> fun nodes -> { Nodes = nodes }

    [<Literal>] 
    let subsectorSize = 4
    let u_subsector : Unpickle<Subsector> =
        u_pipe2 u_uint16 u_uint16 <|
        fun segCount firstSegNumber ->
            Subsector (int segCount, int firstSegNumber)

    let u_subsectors count offset : Unpickle<Subsector []> =
        u_skipBytes offset >>. u_array count u_subsector

    let u_lumpSubsector size offset : Unpickle<LumpSubsector> =
        u_lookAhead (u_subsectors (size / subsectorSize) offset) |>> fun subsectors -> { Subsectors = subsectors }