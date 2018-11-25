import Foundation

typealias Banks = [Int]

func redistribute(_ banks: Banks) -> Banks {
    var total = banks.max()!
    let index = banks.index { $0 == total }!
    let share = Int(ceil(Double(total) / Double(banks.count)))
    let swapped = Array(banks[index+1..<banks.count] + banks[0..<index] + [0])

    total += share
    let result = swapped.map { (val: Int) -> Int in
        total -= share
        if total >= share {
            return val + share
        } else if total > 0 {
            return val + total
        } else {
            return val
        }
    }

    let pivot = result.count - index
    return Array(result[pivot-1..<result.count] + result[0..<pivot-1])
}

func findLoop(_ banks: Banks) -> (total: Int, loop: Int) {
    var count = 0
    var banks = banks
    var seen = [Banks: Int]()
    while !seen.keys.contains(banks) {
        count += 1
        seen[banks] = count
        banks = redistribute(banks)
    }
    return (total: count, loop: count - seen[banks]! + 1)
}

func toInts(_ string: String) -> [Int] {
    return string
        .split(separator: "\t")
        .compactMap { Int($0) }
}

if CommandLine.argc > 1 {
    let input = toInts(CommandLine.arguments[1])
    print(findLoop(input))
} else {
    print("Specify input")
}