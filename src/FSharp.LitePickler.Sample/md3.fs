namespace FSharp.Game.Data

open System
open System.Numerics

[<RequireQualifiedAccess>]
module internal Constants =
    let ``2 * PI / 255`` = 2.f * (single Math.PI) / 255.f 

/// Md3Header
type Md3Header = {
    Ident: string
    Version: int
    Name: string
    Flags: int
    FrameCount: int
    TagCount: int
    SurfaceCount: int
    SkinCount: int
    FramesOffset: int
    TagsOffset: int
    SurfacesOffset: int
    EofOffset: int }

/// Md3Frame
type Md3Frame = {
    BoundsMins: Vector3
    BoundsMaxs: Vector3
    LocalOrigin: Vector3
    Radius: single
    Name: string }

/// Md3Tag
type Md3Tag = {
    Name: string
    Origin: Vector3
    AxisX: Vector3
    AxisY: Vector3
    AxisZ: Vector3 }

/// Md3Shader
type Md3Shader = {
    Name: string
    ShaderId: int }

/// Md3Triangle
[<Struct>]
type Md3Triangle =
    val x : int
    val y : int
    val z : int

    new (x, y, z) = { x = x; y = y; z = z }

/// Md3St
[<Struct>]
type Md3St =
    val st : Vector2

    new (st) = { st = st }

/// Md3Vertex
/// Also known as XyzNormal
[<Struct>]
type Md3Vertex =
    val x : int16
    val y : int16
    val z : int16
    val Zenith : byte
    val Azimuth : byte

    new (x, y, z, zenith, azimuth) = { x = x; y = y; z = z; Zenith = zenith; Azimuth = azimuth }

    member this.lat = single this.Zenith * Constants.``2 * PI / 255``
    member this.lng = single this.Azimuth * Constants.``2 * PI / 255``

/// Md3SurfaceHeader
type Md3SurfaceHeader = {
    Ident: string
    Name: string
    Flags: int
    FrameCount: int
    ShaderCount: int
    VertexCount: int
    TriangleCount: int
    TrianglesOffset: int
    ShadersOffset: int
    StOffset: int
    VerticesOffset: int
    EndOffset: int }

/// Md3Surface
type Md3Surface = {
    Header: Md3SurfaceHeader
    Shaders: Md3Shader []
    Triangles: Md3Triangle []
    St: Md3St []
    Vertices: Md3Vertex [] }

/// Md3
type Md3 = {
    Header: Md3Header
    Frames: Md3Frame []
    Tags: Md3Tag []
    Surfaces: Md3Surface [] }
