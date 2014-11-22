(*
Copyright (c) 2014 William F. Smith

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
*)

module FSharp.LitePickler.Unpickle

open System
open System.IO
open System.Text
open System.Runtime.InteropServices
open Microsoft.FSharp.NativeInterop

#nowarn "9"
#nowarn "51"

type LiteStream = {
    mutable bytes: byte []
    mutable position: int64 } with

    static member Create bytes =
        { bytes = bytes; position = 0L }

[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module LiteStream =
    let inline seek offset stream = stream.position <- offset

    let inline skip n stream = stream.position <- stream.position + n

    let inline readByte stream =
        let result = stream.bytes.[int stream.position]
        stream.position <- stream.position + 1L
        result

    let inline readBytes n stream =
        let i = stream.position
        stream.position <- stream.position + n
        stream.bytes.[int i..int stream.position]

    let inline readString (n: int64) stream =
        let s : nativeptr<sbyte> = (NativePtr.ofNativeInt <| NativePtr.toNativeInt &&stream.bytes.[int stream.position])
        let result = String (s, 0, int n)
        stream.position <- stream.position + n
        result

    let inline read<'a when 'a : unmanaged> stream =
        let result = NativePtr.read (NativePtr.ofNativeInt<'a> <| NativePtr.toNativeInt &&stream.bytes.[int stream.position])
        stream.position <- stream.position + int64 sizeof<'a>
        result

type Unpickle<'a> = LiteStream -> 'a

let u_byte : Unpickle<byte> =
    fun stream -> LiteStream.readByte stream

let u_bytes n : Unpickle<byte []> =
    fun stream -> LiteStream.readBytes n stream

let u_int16 : Unpickle<int16> =
    fun stream -> LiteStream.read stream

let u_int32 : Unpickle<int> =
    fun stream -> LiteStream.read stream

let u_single : Unpickle<single> =
    fun stream -> LiteStream.read stream

let inline u_string n : Unpickle<string> =
    fun stream -> LiteStream.readString n stream

let inline u_pipe2 a b f : Unpickle<_> =
    fun stream -> f (a stream) (b stream)

let inline u_pipe3 a b c f : Unpickle<_> =
    fun stream -> f (a stream) (b stream) (c stream)

let inline u_pipe4 a b c d f : Unpickle<_> =
    fun stream -> f (a stream) (b stream) (c stream) (d stream)

let inline u_pipe5 a b c d e f : Unpickle<_> =
    fun stream -> f (a stream) (b stream) (c stream) (d stream) (e stream)

let inline u_pipe6 a b c d e g f : Unpickle<_> =
    fun stream -> f (a stream) (b stream) (c stream) (d stream) (e stream) (g stream)

let inline u_pipe7 a b c d e g h f : Unpickle<_> =
    fun stream -> f (a stream) (b stream) (c stream) (d stream) (e stream) (g stream) (h stream)

let inline u_pipe8 a b c d e g h i f : Unpickle<_> =
    fun stream -> f (a stream) (b stream) (c stream) (d stream) (e stream) (g stream) (h stream) (i stream)

let inline u_pipe9 a b c d e g h i j f : Unpickle<_> =
    fun stream -> f (a stream) (b stream) (c stream) (d stream) (e stream) (g stream) (h stream) (i stream) (j stream)

let inline u_pipe10 a b c d e g h i j k f : Unpickle<_> =
    fun stream -> f (a stream) (b stream) (c stream) (d stream) (e stream) (g stream) (h stream) (i stream) (j stream) (k stream)

let inline u_pipe11 a b c d e g h i j k l f : Unpickle<_> =
    fun stream -> f (a stream) (b stream) (c stream) (d stream) (e stream) (g stream) (h stream) (i stream) (j stream) (k stream) (l stream)

let inline u_pipe12 a b c d e g h i j k l m f : Unpickle<_> =
    fun stream -> f (a stream) (b stream) (c stream) (d stream) (e stream) (g stream) (h stream) (i stream) (j stream) (k stream) (l stream) (m stream)

let inline u_pipe13 a b c d e g h i j k l m n f : Unpickle<_> =
    fun stream -> f (a stream) (b stream) (c stream) (d stream) (e stream) (g stream) (h stream) (i stream) (j stream) (k stream) (l stream) (m stream) (n stream)

let inline u<'a when 'a : unmanaged> : Unpickle<_> =
    fun stream -> LiteStream.read<'a> stream

let inline u_array n (p: Unpickle<'a>) =
    fun stream ->
        match n with
        | 0 -> [||]
        | _ -> Array.init n (fun _ -> p stream)

let inline u_skipBytes n : Unpickle<_> =
    fun stream -> LiteStream.skip n stream

let inline u_lookAhead (p: Unpickle<'a>) : Unpickle<'a> =
    fun stream ->
        let prevPosition = stream.position
        let result = p stream
        LiteStream.seek prevPosition stream
        result

// fmap
let inline (|>>) (u: Unpickle<'a>) (f: 'a -> 'b) : Unpickle<'b> =
    fun stream -> f (u stream)

let inline (<*>) (u1: Unpickle<'a -> 'b>) (u2: Unpickle<'a>) : Unpickle<'b> =
    fun stream -> u1 stream (u2 stream)

let inline (>>=) (u: Unpickle<'a>) (f: 'a -> Unpickle<'b>) : Unpickle<'b> =
    fun stream -> f (u stream) stream

let inline (>>.) (u1: Unpickle<'a>) (u2: Unpickle<'b>) =
    fun stream ->
        u1 stream |> ignore
        u2 stream

let inline (.>>) (u1: Unpickle<'a>) (u2: Unpickle<'b>) =
    fun stream ->
        let result = u1 stream
        u2 stream |> ignore
        result

let inline (.>>.) (u1: Unpickle<'a>) (u2: Unpickle<'b>) =
    fun stream ->
        u1 stream,
        u2 stream

let inline u_run (p: Unpickle<_>) x = p x
