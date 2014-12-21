﻿module FSharp.Game.Data.Md3.Pickle

open System.Numerics
open FSharp.LitePickler.Core
open FSharp.LitePickler.Pickle
open FSharp.Game.Data.Md3

let p_vec2 : Pickle<Vector2> =
    fun v stream -> LiteWriteStream.write v stream

let p_vec3 : Pickle<Vector3> =
    fun v stream -> LiteWriteStream.write v stream

let p_frame =
    p_pipe5 p_vec3 p_vec3 p_vec3 p_single (p_string 16 StringKind.EightBit) <|
    fun (x: Md3Frame) -> 
        x.BoundsMins,
        x.BoundsMaxs,
        x.LocalOrigin,
        x.Radius,
        x.Name

let p_header =
    p_pipe12
        (p_string 4 StringKind.EightBit)
        p_int32
        (p_string 64 StringKind.EightBit)
        p_int32
        p_int32
        p_int32
        p_int32
        p_int32
        p_int32
        p_int32
        p_int32
        p_int32 <|
    fun (x: Md3Header) -> 
        x.Ident,
        x.Version,
        x.Name,
        x.Flags,
        x.FrameCount,
        x.TagCount,
        x.SurfaceCount,
        x.SkinCount,
        x.FramesOffset,
        x.TagsOffset,
        x.SurfacesOffset,
        x.EofOffset

let p_tag =
    p_pipe5 (p_string 64 StringKind.EightBit) p_vec3 p_vec3 p_vec3 p_vec3 <|
    fun (x: Md3Tag) -> 
        x.Name,
        x.Origin,
        x.AxisX,
        x.AxisY,
        x.AxisZ

let p_shader =
    p_pipe2 (p_string 64 StringKind.EightBit) p_int32 <|
    fun (x: Md3Shader) -> 
        x.Name,
        x.ShaderId

let p_triangle =
    p_pipe3 p_int32 p_int32 p_int32 <|
    fun (x: Md3Triangle) -> 
        x.x, 
        x.y, 
        x.z

let p_st = p_vec2 -|>> fun (x: Md3St) -> x.st
 
let p_vertex =
    p_pipe5 p_int16 p_int16 p_int16 p_byte p_byte <|
    fun (x: Md3Vertex) -> 
        x.x, 
        x.y, 
        x.z, 
        x.Zenith, 
        x.Azimuth

let p_surfaceHeader =
    p_pipe12
        (p_string 4 StringKind.EightBit)
        (p_string 64 StringKind.EightBit)
        p_int32
        p_int32
        p_int32
        p_int32
        p_int32
        p_int32
        p_int32
        p_int32
        p_int32
        p_int32 <|
    fun (x: Md3SurfaceHeader) ->
        x.Ident,
        x.Name,
        x.Flags,
        x.FrameCount,
        x.ShaderCount,
        x.VertexCount,
        x.TriangleCount,
        x.TrianglesOffset,
        x.ShadersOffset,
        x.StOffset,
        x.VerticesOffset,
        x.EndOffset
       
let p_frames count offset =
    fun xs stream ->
        p_skipBytes offset () stream
        p_array count p_frame xs stream

let p_tags count offset =
    fun xs stream ->
        p_skipBytes offset () stream
        p_array count p_tag xs stream

let p_surface : Pickle<Md3Surface> =
    fun x stream ->
        let header = x.Header

        p_lookAhead p_surfaceHeader header stream

        p_lookAhead
            (fun triangles stream ->
                p_skipBytes (int64 header.TrianglesOffset) () stream
                p_array header.TriangleCount p_triangle triangles stream) x.Triangles stream

        p_lookAhead
            (fun shaders stream ->
                p_skipBytes (int64 header.ShadersOffset) () stream
                p_array header.ShaderCount p_shader shaders stream) x.Shaders stream

        p_lookAhead
            (fun xs stream ->
                p_skipBytes (int64 header.StOffset) () stream
                p_array header.VertexCount p_st xs stream) x.St stream

        p_lookAhead
            (fun vertices stream ->
                p_skipBytes (int64 header.VerticesOffset) () stream
                p_array (header.VertexCount * header.FrameCount) p_vertex vertices stream) x.Vertices stream

let p_surfaces count offset =
    fun (xs: Md3Surface []) stream ->
        p_skipBytes offset () stream
        xs
        |> Array.iteri (fun i x ->
            p_surface x stream
            
            if i + 1 <> count then
                p_skipBytes (int64 x.Header.EndOffset) x stream)

let p_md3 : Pickle<_> =
    (p_lookAhead p_header) ->>= fun md3 ->
        let header = md3.Header
        header,
        p_pipe3
            (p_lookAhead <| p_frames header.FrameCount (int64 header.FramesOffset))
            (p_lookAhead <| p_tags header.TagCount (int64 header.TagsOffset))
            (p_lookAhead <| p_surfaces header.SurfaceCount (int64 header.SurfacesOffset)) <|
        fun x -> x.Frames, x.Tags, x.Surfaces

