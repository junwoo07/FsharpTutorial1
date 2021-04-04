// Learn more about F# at http://fsharp.org

namespace MyMath
    open System

    module Matrixs =
        type Matrix2D (arg:float[][]) =
            member private this.array = arg
            override this.ToString() =
                let mutable output = "[|\n"
                for i in this.array do
                    output <- output+"  [| "
                    for j in i do
                        output <- output+j.ToString()+" "
                    output <- output+"|]\n"
                output+"|]"
            member public this.ToArray() = this.array
            static member (~-) (v : Matrix2D) =
                Matrix2D ( Array.map (fun m -> Array.map (fun n -> -n) m) v.array )
            static member (+) (v : Matrix2D,a : float) =
                Matrix2D ( Array.map (fun m -> Array.map (fun n -> n+a) m) v.array )
            static member (+) (a : float,v : Matrix2D) =
                v+a
            static member (+) (v : Matrix2D,a : Matrix2D) =
                Matrix2D ( Array.map2 (fun k l -> (Array.map2 (fun m n -> m+n) k l)) v.array a.array )
            static member (-) (v,a) =
                v + -a
            static member (*) (v : Matrix2D,a : float) =
                Matrix2D ( Array.map (fun m -> Array.map (fun n -> n*a) m) v.array )
            static member (*) (a : float,v : Matrix2D) =
                v*a
            static member (/) (v : Matrix2D,a : float) =
                Matrix2D ( Array.map (fun m -> Array.map (fun n -> n/a) m) v.array )
            member this.T =
                Matrix2D ( Array.mapi (fun i m -> [|for j in 0..this.array.Length-1 -> this.array.[j].[i]|]) (Array.create (this.array.[0].Length) (this.array.Length)) )
        let dot (x:Matrix2D) (y:Matrix2D) =
            if x.ToArray().[0].Length = y.ToArray().Length 
            then Matrix2D (Array.map (fun k -> (Array.map (fun l -> (Array.sum (Array.map2 (fun m n -> m*n) k l))) (y.T.ToArray()))) (x.ToArray()))
            else raise (ArgumentException("Fail matrix multiplication"))

        let a = Matrix2D [|
            [|5.0;6.0|]
            [|7.0;8.0|]
        |]
        let b = Matrix2D [|
            [|1.0;2.0|]
            [|3.0;4.0|]
        |]
        printf $"{a-b}"

