// Part 1
func hasDuplicateCount(_ num: Int, string: String) -> Bool {
    var counts = [Character: Int]()

    for letter in string {
        let cur = counts[letter] ?? 0
        counts[letter] = cur + 1
    }

    return counts.values.contains(num)
}

func countDuplicates(_ num: Int, strings: [String]) -> Int {
    return strings.reduce(0) {
        return $0 + (hasDuplicateCount(num, string: $1) ? 1 : 0)
    }
}

func checkSum(_ strings: [String]) -> Int {
    let twos = countDuplicates(2, strings: strings)
    let threes = countDuplicates(3, strings: strings)
    return twos * threes
}

// Part 2
typealias Accumulator<T> = (T, Character, Character) -> T
func reduceBoth<T>(_ curr: T, _ s1: String, _ s2 :String, acc: Accumulator<T>) -> T {
    return zip(s1, s2).reduce(curr) { curr, both in
        return acc(curr, both.0, both.1)
    }
}

func isMatch(_ s1: String, _ s2 :String) -> Bool {
    let diffCount = reduceBoth(0, s1, s2) { curr, c1, c2 in
        return curr + (c1 == c2 ? 0 : 1)
    }
    return diffCount == 1
}

func matchingChars(_ match: (String, String)) -> String {
    return reduceBoth("", match.0, match.1) { curr, c1, c2 in
        return curr + (c1 == c2 ? String(c1) : "")
    }
}

func findMatch(_ strings: [String]) -> (String, String) {
    for base in strings {
        for comp in strings {
            if isMatch(base, comp) {
                return (base, comp)
            }
        }
    }
    return ("", "")
}

// Output

if CommandLine.argc > 1 {
    let input = Array(CommandLine.arguments.dropFirst())
    print(checkSum(input))
    print(matchingChars(findMatch(input)))
} else {
    print("Specify the input")
}
