﻿module FSharp.Game.Data.Unpickle.Md3

open System.Numerics
open FSharp.LitePickler.Unpickle
open FSharp.Game.Data

let u_vec2 : Unpickle<Vector2> =
    fun stream ->
        Vector2 (
            stream.Read<single> (),
            stream.Read<single> ())

let u_vec3 : Unpickle<Vector3> =
    fun stream ->
        Vector3 (
            stream.Read<single> (),
            stream.Read<single> (),
            stream.Read<single> ())

let u_frame =
    u_pipe5 u_vec3 u_vec3 u_vec3 u_single (u_string 16) <|
    fun mins maxs localOrigin radius name ->
        {
        BoundsMins = mins
        BoundsMaxs = maxs
        LocalOrigin = localOrigin
        Radius = radius
        Name = name }

let u_header =
    u_pipe12
        (u_string 4)
        u_int32
        (u_string 64)
        u_int32
        u_int32
        u_int32
        u_int32
        u_int32
        u_int32
        u_int32
        u_int32
        u_int32 <|
    fun ident version name flags frameCount tagCount surfaceCount skinCount framesOffset tagsOffset surfacesOffset eofOffset ->
        {
        Ident = ident
        Version = version
        Name = name
        Flags = flags
        FrameCount = frameCount
        TagCount = tagCount
        SurfaceCount = surfaceCount
        SkinCount = skinCount
        FramesOffset = framesOffset
        TagsOffset = tagsOffset
        SurfacesOffset = surfacesOffset
        EofOffset = eofOffset }

let u_tag =
    u_pipe5 (u_string 64) u_vec3 u_vec3 u_vec3 u_vec3 <|
    fun name origin axisX axisY axisZ ->
        { Name = name; Origin = origin; AxisX = axisX; AxisY = axisY; AxisZ = axisZ }

let pshader =
    u_pipe2 (u_string 64) u_int32 <|
    fun name shaderId -> { Name = name; ShaderId = shaderId }

let ptriangle =
    u_pipe3 u_int32 u_int32 u_int32 <|
    fun x y z -> Md3Triangle (x, y, z)

let u_st = u_vec2 |>> fun x -> Md3St (x)
 
let u_vertex : Unpickle<Md3Vertex> =
    u_pipe5 u_int16 u_int16 u_int16 u_byte u_byte <|
    fun x y z zenith azimuth -> Md3Vertex (x, y, z, zenith, azimuth)

let u_surfaceHeader =
    u_pipe12
        (u_string 4)
        (u_string 64)
        u_int32
        u_int32
        u_int32
        u_int32
        u_int32
        u_int32
        u_int32
        u_int32
        u_int32
        u_int32 <|
    fun ident name flags frameCount shaderCount vertexCount triangleCount trianglesOffset shadersOffset stOffset verticesOffset endOffset ->
        {
        Ident = ident
        Name = name
        Flags = flags
        FrameCount = frameCount
        ShaderCount = shaderCount
        VertexCount = vertexCount
        TriangleCount = triangleCount
        TrianglesOffset = trianglesOffset
        ShadersOffset = shadersOffset
        StOffset = stOffset
        VerticesOffset = verticesOffset
        EndOffset = endOffset }

let u_frames count offset =
    u_skipBytes offset >>. u_array count u_frame

let u_tags count offset =
    u_skipBytes offset >>. u_array count u_tag

let u_surface =
    u_lookAhead u_surfaceHeader >>= fun header ->
    u_pipe4
        (u_lookAhead (u_skipBytes header.TrianglesOffset >>. u_array header.TriangleCount ptriangle))
        (u_lookAhead (u_skipBytes header.ShadersOffset >>. u_array header.ShaderCount pshader))
        (u_lookAhead (u_skipBytes header.StOffset >>. u_array header.VertexCount u_st))
        (u_lookAhead (u_skipBytes header.VerticesOffset >>. u_array (header.VertexCount * header.FrameCount) u_vertex)) <|
    fun triangles shaders st vertices ->
        { Header = header
          Shaders = shaders
          Triangles = triangles
          St = st
          Vertices = vertices }

let u_surfaces count offset =
    u_skipBytes offset >>.
    fun stream ->
        Array.init count (fun i ->
            let surface = u_surface stream
            let header = surface.Header

            if i + 1 <> count then
                u_skipBytes header.EndOffset stream |> ignore
            
            surface)

let u_md3 : Unpickle<_> =
    u_lookAhead u_header >>= fun header ->
    u_pipe3
        (u_lookAhead <| u_frames header.FrameCount header.FramesOffset)
        (u_lookAhead <| u_tags header.TagCount header.TagsOffset)
        (u_lookAhead <| u_surfaces header.SurfaceCount header.SurfacesOffset) <|
    fun frames tags surfaces ->
        {
        Header = header
        Frames = frames
        Tags = tags
        Surfaces = surfaces }

