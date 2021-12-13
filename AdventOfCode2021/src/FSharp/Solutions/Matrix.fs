namespace Solutions

module Matrix =
    let find f (arr: 'a [,]) = Seq.choose id <| seq {
        for i in 0..(arr.GetLength 0 - 1) do
            for j in 0..(arr.GetLength 1 - 1) do
                if f arr.[i,j] 
                    then yield Some arr.[i,j]
                    else yield None
    }

    let flatMap f (arr: 'a [,]) = seq {
        for i in 0..(arr.GetLength 0 - 1) do
            for j in 0..(arr.GetLength 1 - 1) do
                yield f arr.[i,j]
    }

    let inline sumBy (f: _ -> 'a when 'a : (static member (+) : 'a * 'a -> 'a)) arr = 
        arr
        |> flatMap f
        |> Seq.sum

    let inline sum (arr: 'a [,]) = 
        arr |> sumBy id

    let fill (points: (int*int) list) value (matrix: 'a[,]) : 'a[,] =
        points
        |> List.iter (fun (x,y) -> matrix.[y,x] <- value)
        matrix

    let foldY y (matrix: int[,]) =
        let newMatrix = Array2D.create y (matrix.GetLength 1) 0
        let maxY = matrix.GetLength 0 - 1
        for i in 0..y do
            for j in 0..(newMatrix.GetLength 1 - 1) do
                if matrix.[i,j] <> 0 then newMatrix.[i,j] <- matrix.[i,j]
                if matrix.[maxY - i,j] <> 0 then newMatrix.[i,j] <- matrix.[maxY - i,j]
        newMatrix

    let foldX x (matrix: int[,]) =
        let newMatrix = Array2D.create (matrix.GetLength 0) x 0
        let maxX = matrix.GetLength 1 - 1
        for i in 0..(newMatrix.GetLength 0 - 1) do
            for j in 0..x do
                if matrix.[i,j] <> 0 then newMatrix.[i,j] <- matrix.[i,j]
                if matrix.[i,maxX-j] <> 0 then newMatrix.[i,j] <- matrix.[i,maxX-j]
        newMatrix

    let print (arr: int[,]) =      
        let seq = seq {
            for i in 0..(arr.GetLength 0 - 1) do
                let mutable s = ""
                for j in 0..(arr.GetLength 1 - 1) do
                    s <- sprintf "%s%s" s (if arr.[i,j] = 1 then "#" else ".")
                yield s
        }
        seq |> String.concat "\r\n"
