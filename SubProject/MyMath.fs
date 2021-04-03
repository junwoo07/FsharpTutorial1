// Learn more about F# at http://fsharp.org

namespace MyMath
    open System

    module Matrix =
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
                let output = v.array
                for i=0 to output.Length-1 do
                    for j=0 to output.[0].Length-1 do
                        output.[i].[j] <- -output.[i].[j]
                Matrix2D output
            static member (+) (v : Matrix2D,a : float) =
                let output = v.array
                for i=0 to output.Length-1 do
                    for j=0 to output.[0].Length-1 do
                        output.[i].[j] <- output.[i].[j] + a
                Matrix2D output
            static member (+) (a : float,v : Matrix2D) =
                v+a
            static member (-) (v : Matrix2D,a : float) =
                v + -a
            static member (*) (v : Matrix2D,a : float) =
                let output = v.array
                for i=0 to output.Length-1 do
                    for j=0 to output.[0].Length-1 do
                        output.[i].[j] <- output.[i].[j] * a
                Matrix2D output
            static member (*) (a : float,v : Matrix2D) =
                v*a
            static member (/) (v : Matrix2D,a : float) =
                let output = v.array
                for i=0 to output.Length-1 do
                    for j=0 to output.[0].Length-1 do
                        output.[i].[j] <- output.[i].[j] / a
                Matrix2D output

        let a = Matrix2D [|
            [|1.0;2.0;2.0;3.0;4.0;4.0;5.0;4.0;3.0|]
            [|1.0;2.0;2.0;3.0;4.0;4.0;5.0;4.0;3.0|]
            [|1.0;2.0;2.0;3.0;4.0;4.0;5.0;4.0;3.0|]
            [|1.0;2.0;2.0;3.0;4.0;4.0;5.0;4.0;3.0|]
            [|1.0;2.0;2.0;3.0;4.0;4.0;5.0;4.0;3.0|]
        |]
        printf $"{a/2.0}"

