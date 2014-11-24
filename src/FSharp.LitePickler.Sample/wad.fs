namespace FSharp.Game.Data.Wad

open System

type Header = { IsPwad: bool; LumpCount: int; LumpOffset: int }
 
type LumpHeader = { Offset: int32; Size: int32; Name: string }
 
type Wad = { Header: Header; LumpHeaders: LumpHeader [] }

type ThingFormat =
    | Doom = 0
    | Hexen = 1

[<Flags>]
type DoomThingFlags =
    | SkillLevelOneAndTwo = 0x001
    | SkillLevelThree = 0x002
    | SkillLevelFourAndFive = 0x004
    | Deaf = 0x008
    | NotInSinglePlayer = 0x0010
    | NotInDeathmatch = 0x0020 // boom
    | NotInCoop = 0x0040 // boom
    | FriendlyMonster = 0x0080 // MBF

[<Flags>]
type HexenThingFlags =
    | SkillLevelOneAndTwo = 0x001
    | SkillLevelThree = 0x002
    | SkillLevelFourAndFive = 0x004
    | Deaf = 0x008
    | Dormant = 0x0010
    | AppearOnlyToFighterClass = 0x0020
    | AppearOnlyToClericClass = 0x0040
    | AppearOnlyToMageClass = 0x0080
    | AppearOnlyInSinglePlayer = 0x0100
    | AppearOnlyInCoop = 0x0200
    | AppearOnlyInDeathmatch = 0x0400
 
type DoomThing = { X: int; Y: int; Angle: int; Flags: DoomThingFlags }

type HexenThing = { Id: int; X: int; Y: int; StartingHeight: int; Angle: int; Flags: HexenThingFlags; Arg1: byte; Arg2: byte; Arg3: byte; Arg4: byte; Arg5: byte }

type Thing =
    | Doom of DoomThing
    | Hexen of HexenThing

[<Flags>]
type LinedefFlags =
    | BlocksPlayersAndMonsters = 0x0001
    | BlocksMonsters = 0x0002
    | TwoSided = 0x0004
    | UpperTextureUnpegged = 0x0008
    | LowerTextureUnpegged = 0x0010
    | Secret = 0x0020
    | BlocksSound = 0x0040
    | NerverShowsOnAutomap = 0x0080
    | AlwaysShowsOnAutomap = 0x0100

type DoomLinedef = { 
    StartVertex: int
    EndVertex: int
    Flags: LinedefFlags
    SpecialType: int
    SectorTag: int
    RightSidedef: int
    LeftSidedef: int }

type Linedef =
    | Doom of DoomLinedef

type Sidedef = {
    OffsetX: int
    OffsetY: int
    UpperTextureName: string
    LowerTextureName: string
    MiddleTextureName: string
    SectorNumber: int }

[<Struct>]
type Vertex =
    val X : int
    val Y : int

    new (x, y) = { X = x; Y = y }

type ChildNodeType =
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
    val RightChildType: ChildNodeType
    val LeftChildType: ChildNodeType

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

[<Struct>]
type Subsector =
    val SegCount: int
    val FirstSegNumber: int

    new (segCount, firstSegNumber) = { SegCount = segCount; FirstSegNumber = firstSegNumber }

type LumpThings = { Things: Thing [] }
type LumpLinedefs = { Linedefs: Linedef [] }
type LumpSidedefs = { Sidedefs: Sidedef [] }
type LumpVertices = { Vertices: Vertex [] }
type LumpNodes = { Nodes: Node [] }
type LumpSubsectors = { Subsectors: Subsector [] }