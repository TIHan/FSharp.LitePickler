module FSharp.Game.Data.Wad.Unpickle

open System

open FSharp.LitePickler.Core
open FSharp.LitePickler.Unpickle
open FSharp.Game.Data.Wad

let fixedToSingle x = (single x / 65536.f)

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
let doomThingSize = 10
[<Literal>]
let hexenThingSize = 20
let u_thing format : Unpickle<Thing> =
    match format with
    | ThingFormat.Doom ->
        u_pipe5 u_int16 u_int16 u_int16 u_int16 u_int16 <|
        fun x y angle _ flags ->
            Thing.Doom { X = int x; Y = int y; Angle = int angle; Flags = enum<DoomThingFlags> (int flags) }
    | _ -> failwith "Not supported."

let u_things format count offset : Unpickle<Thing []> =
    u_skipBytes offset >>. u_array count (u_thing format)

let u_lumpThings format size offset : Unpickle<LumpThings> =
    match format with
    | ThingFormat.Doom ->
        u_lookAhead (u_things format (size / doomThingSize) offset) |>> fun things -> { Things = things }
    | _ -> failwith "Not supported."

[<Literal>]
let linedefSize = 14
let u_linedef : Unpickle<Linedef> =
    u_pipe7 u_int16 u_int16 u_int16 u_int16 u_int16 u_int16 u_int16 <|
    fun startVertex endVertex flags specialType sectorTag rightSidedef leftSidedef ->
        { StartVertex = int startVertex
          EndVertex = int endVertex
          Flags = enum<LinedefFlags> (int flags)
          SpecialType = int specialType
          SectorTag = int sectorTag
          RightSidedef = int rightSidedef
          LeftSidedef = int leftSidedef }
        |> Linedef.Doom

let u_linedefs count offset : Unpickle<Linedef []> =
    u_skipBytes offset >>. u_array count u_linedef
        
let u_lumpLinedefs size offset : Unpickle<LumpLinedefs> =
    u_lookAhead (u_linedefs (size / linedefSize) offset) |>> fun linedefs -> { Linedefs = linedefs }

[<Literal>]
let sidedefSize = 30
let u_sidedef : Unpickle<Sidedef> =
    u_pipe6 u_int16 u_int16 (u_string 8) (u_string 8) (u_string 8) u_int16 <|
    fun offsetX offsetY upperTexName lowerTexName middleTexName sectorNumber ->
        { OffsetX = int offsetX
          OffsetY = int offsetY
          UpperTextureName = upperTexName.Trim ()
          LowerTextureName = lowerTexName.Trim ()
          MiddleTextureName = middleTexName.Trim ()
          SectorNumber = int sectorNumber }

let u_sidedefs count offset : Unpickle<Sidedef []> =
    u_skipBytes offset >>. u_array count u_sidedef

let u_lumpSidedefs size offset : Unpickle<LumpSidedefs> =
    u_lookAhead (u_sidedefs (size / sidedefSize) offset) |>> fun sidedefs -> { Sidedefs = sidedefs }

[<Literal>]
let vertexSize = 4
let u_vertex : Unpickle<Vertex> =
    u_pipe2 u_int16 u_int16 <|
    fun x y -> Vertex (int x, int y)

let u_vertices count offset : Unpickle<Vertex []> =
    u_skipBytes offset >>. u_array count u_vertex

let u_lumpVertices size offset : Unpickle<LumpVertices> =
    u_lookAhead (u_vertices (size / vertexSize) offset) |>> fun vertices -> { Vertices = vertices }

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
                ChildNodeType.Subnode
            else
                ChildNodeType.Subsector

        let leftChildType =
            if leftBit = false then
                ChildNodeType.Subnode
            else
                ChildNodeType.Subsector

        Node (startLineX, startLineY, endLineX, endLineY, rightBoundingBox, leftBoundingBox, rightChild, leftChild, rightChildType, leftChildType)

let u_nodes count offset : Unpickle<Node []> =
    u_skipBytes offset >>. u_array count u_node

let u_lumpNodes size offset : Unpickle<LumpNodes> =
    u_lookAhead (u_nodes (size / nodeSize) offset) |>> fun nodes -> { Nodes = nodes }

[<Literal>] 
let subsectorSize = 4
let u_subsector : Unpickle<Subsector> =
    u_pipe2 u_uint16 u_uint16 <|
    fun segCount firstSegNumber ->
        Subsector (int segCount, int firstSegNumber)

let u_subsectors count offset : Unpickle<Subsector []> =
    u_skipBytes offset >>. u_array count u_subsector

let u_lumpSubsectors size offset : Unpickle<LumpSubsectors> =
    u_lookAhead (u_subsectors (size / subsectorSize) offset) |>> fun subsectors -> { Subsectors = subsectors }

[<Literal>]
let sectorSize = 26
let u_sector : Unpickle<Sector> =
    u_pipe7 u_int16 u_int16 (u_string 8) (u_string 8) u_int16 u_int16 u_int16 <|
    fun floorHeight ceilingHeight floorTexName ceilingTexName lightLevel typ tag ->
        { FloorHeight = int floorHeight
          CeilingHeight = int ceilingHeight
          FloorTextureName = floorTexName.Trim().Trim('\000')
          CeilingTextureName = ceilingTexName.Trim().Trim('\000')
          LightLevel = int lightLevel
          Type = enum<SectorType> (int typ)
          Tag = int tag }

let u_sectors count offset : Unpickle<Sector []> =
    u_skipBytes offset >>. u_array count u_sector

let u_lumpSectors size offset : Unpickle<LumpSectors> =
    u_lookAhead (u_sectors (size / sectorSize) offset) |>> fun sectors -> { Sectors = sectors }