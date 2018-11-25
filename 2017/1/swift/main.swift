func toInts(string: String) -> [Int] {
    return string.compactMap { Int("\($0)") }
}

func captcha1(list: [Int]) -> Int {
    guard let prev = list.last else {
        return 0
    }
    return list.reduce((prev: prev, result: 0)) { acc, val in
        (prev: val, result: acc.result + (acc.prev == val ? val : 0))
    }.result
}

func captcha2(list: [Int]) -> Int {
    var total = 0

    let half = list.count / 2
    for i in 0..<list.count {
        var next = i + half
        if next >= list.count {
            next -= list.count
        }
        if list[i] == list[next] {
            total += list[i]
        }
    }

    return total
}

if CommandLine.argc > 1 {
    let list = toInts(string: CommandLine.arguments[1])
    print(captcha1(list: list))
    print(captcha2(list: list))
} else {
    print("Specify the number")
}
