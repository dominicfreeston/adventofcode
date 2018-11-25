func toInts(string: String) -> [Int] {
    return string
        .split(separator: "\t")
        .compactMap { Int($0) }
}

func toMatrix(string: [String]) -> [[Int]] {
    return string
            .map(toInts)
}

func max(_ list: [Int]) -> Int {
    return list.reduce(0) { max($0, $1) }
}

func min(_ list: [Int]) -> Int {
    return list.reduce(Int.max) { min($0, $1) }
}

func divisible(_ list: [Int]) -> Int {
    let pairs = list.flatMap { val in list.map { [val, $0] } }
    return pairs.reduce(0) { result, pair in
        if pair[0] != pair[1] && max(pair) % min(pair) == 0 {
            return max(pair) / min(pair)
        }
        return result
    }
}

func checkSum(_ matrix: [[Int]]) -> Int {
    return matrix.reduce(0) { $0 + max($1) - min($1) }
}

func checkSum2(_ matrix: [[Int]]) -> Int {
    return matrix.reduce(0) { $0 + divisible($1) }
}

let input = [
    [5, 9, 2, 8],
    [9, 4, 7, 3],
    [3, 8, 6, 5],
]

if CommandLine.argc > 1 {
    let matrix = toMatrix(string: Array(CommandLine.arguments.dropFirst()))
    print(checkSum(matrix))
    print(checkSum2(matrix))
} else {
    print("Specify the spreadsheet")
}
