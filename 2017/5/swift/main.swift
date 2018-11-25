func simpleStrategy(_: Int) -> Int {
    return 1
}

func advanceStrategy(_ val: Int) -> Int {
    return val >= 3 ? -1 : 1
}

func escape(_ list: [Int], _ strategy: (Int) -> Int) -> Int {
    var list = list
    var index = 0
    var count = 0

    while index >= 0 && index < list.count {
        let nextOffset = list[index]
        list[index] += strategy(nextOffset)
        index += nextOffset
        count += 1
    }

    return count
}

if CommandLine.argc > 1 {
    let input = Array(CommandLine.arguments.dropFirst()).compactMap(Int.init)
    print(escape(input, simpleStrategy))
    print(escape(input, advanceStrategy))
} else {
    print("Specify input")
}
