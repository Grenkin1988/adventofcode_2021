[<NUnit.Framework.TestFixture>]
module Solutions.Day16

open System
open Solutions.Helpers
open NUnit.Framework

type PacketType =
    | LiteralValue of int64
    | Sum of Packet list
    | Product of Packet list
    | Minimum of Packet list
    | Maximum of Packet list
    | GreaterThen of Packet list
    | LessThen of Packet list
    | Equal of Packet list

and Packet = 
    { 
        Version: int64; 
        TypeID: int64; 
        Packet: PacketType 
    }

module BITS =
    let hexToBITS = function
        | '0' -> "0000"
        | '1' -> "0001"
        | '2' -> "0010"
        | '3' -> "0011"
        | '4' -> "0100"
        | '5' -> "0101"
        | '6' -> "0110"
        | '7' -> "0111"
        | '8' -> "1000"
        | '9' -> "1001"
        | 'A' -> "1010"
        | 'B' -> "1011"
        | 'C' -> "1100"
        | 'D' -> "1101"
        | 'E' -> "1110"
        | 'F' -> "1111"
        | a -> failwithf "WRONG INPUT %A" a

    let toBITS (line: string) =
        String.collect hexToBITS line

    let binaryToInt input =
        Convert.ToInt64(input, 2);

    let split from length (line: string) =
        line.Substring(from, length), line.Substring(length)

    let getLiteralValue (line: string) =
        let rec addNextGroup (result: string) (line: string) =
            let flag, rest = line |> split 0 1
            let group, rest = rest |> split 0 4
            let result = result + group
            match flag with
            | "0" -> result,rest
            | "1" -> addNextGroup result rest
            | _ -> failwith "WRONG DATA"

        let number,rest = addNextGroup "" line
        number,rest

    let getHeader (line: string) =
        let version, rest = line |> split 0 3
        let packetID, rest = rest |> split 0 3
        binaryToInt version, binaryToInt packetID, rest

    let rec convertOperatorPackage packet: (Packet * string) =
        let ver, id, rest = getHeader packet

        let operatorPackets rest =
            let lengthTypeId, rest = split 0 1 rest
            match lengthTypeId with
            | "0" ->
                let length, pakets = rest |> split 0 15
                let length = binaryToInt length

                let rec getInternal remainingLenght result (pakets: string) =
                    match remainingLenght with
                    | 0L -> result, pakets
                    | rem ->
                        let number, rest = convertOperatorPackage pakets
                        let paketLength = pakets.LastIndexOf(rest) |> int64
                        let result = result @ [ number ]
                        getInternal (rem - paketLength) result rest

                let pakets, rest = getInternal length [] pakets
                pakets, rest
            | "1" ->
                let length, pakets = rest |> split 0 11
                let number = binaryToInt length

                let rec getInternal remainingNumber result pakets =
                    match remainingNumber with
                    | 0L -> result, pakets
                    | rem ->
                        let number, rest = convertOperatorPackage pakets
                        let result = result @ [ number ]
                        getInternal (rem - 1L) result rest

                let pakets, rest = getInternal number [] pakets
                pakets, rest

        match id with
        | 4L -> 
            let number,rest = getLiteralValue rest
            let packet = number |> binaryToInt |> LiteralValue
            { Version = ver; TypeID = id; Packet = packet }, rest
        | _ -> 
            let operators, rest = operatorPackets rest
            let operators =
                match id with
                | 0L -> Sum operators
                | 1L -> Product operators
                | 2L -> Minimum operators
                | 3L -> Maximum operators
                | 5L -> GreaterThen operators
                | 6L -> LessThen operators
                | 7L -> Equal operators

            { Version = ver; TypeID = id; Packet = operators }, rest

    let rec getVersionSum sum packet =
        match packet.Packet with
        | LiteralValue _ -> sum + packet.Version
        | Sum list
        | Product list
        | Minimum list
        | Maximum list
        | GreaterThen list
        | LessThen list
        | Equal list -> 
            let sum = sum + packet.Version
            list |> List.fold (fun sum p -> getVersionSum sum p) sum

    let rec calculate packet =
        match packet.Packet with
        | LiteralValue v -> v
        | Sum list -> list |> List.sumBy calculate
        | Product (h::tail) -> 
            let v = calculate h
            tail |> List.fold (fun pr p -> pr * calculate p) v
        | Minimum list ->
            list |> List.minBy calculate |> calculate
        | Maximum list->
            list |> List.maxBy calculate |> calculate
        | GreaterThen (v1 :: v2 :: []) ->
            if calculate v1 > (calculate v2) then 1
            else 0
        | LessThen (v1 :: v2 :: []) ->
            if calculate v1 < (calculate v2) then 1
            else 0
        | Equal (v1 :: v2 :: []) ->
            if calculate v1 = (calculate v2) then 1
            else 0

[<TestCase("day16_testinput_1.txt", ExpectedResult = 16)>]
[<TestCase("day16_testinput_2.txt", ExpectedResult = 12)>]
[<TestCase("day16_testinput_3.txt", ExpectedResult = 23)>]
[<TestCase("day16_testinput_4.txt", ExpectedResult = 31)>]
[<TestCase("day16_input.txt", ExpectedResult = 974)>]
let ``Part 1``(fileName) =
    let lines = readInput fileName
    
    let packet = lines.Head

    let packet = packet |> BITS.toBITS

    let decoded, rest = BITS.convertOperatorPackage packet

    BITS.getVersionSum 0 decoded

[<TestCase("C200B40A82", ExpectedResult = 3)>]
[<TestCase("04005AC33890", ExpectedResult = 54)>]
[<TestCase("880086C3E88112", ExpectedResult = 7)>]
[<TestCase("CE00C43D881120", ExpectedResult = 9)>]
[<TestCase("D8005AC2A8F0", ExpectedResult = 1)>]
[<TestCase("F600BC2D8F", ExpectedResult = 0)>]
[<TestCase("9C005AC2F8F0", ExpectedResult = 0)>]
[<TestCase("9C0141080250320F1802104A08", ExpectedResult = 1)>]
let ``Part 2 test``(packet) =
    let packet = packet |> BITS.toBITS
    
    let decoded, rest = BITS.convertOperatorPackage packet
    
    BITS.calculate decoded

[<TestCase("day16_input.txt", ExpectedResult = 180616437720L)>]
let ``Part 2 main``(fileName) =
    let lines = readInput fileName
    
    let packet = lines.Head

    let packet = packet |> BITS.toBITS

    let decoded, rest = BITS.convertOperatorPackage packet
    
    BITS.calculate decoded
