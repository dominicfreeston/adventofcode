func validate(_ pw: String) -> Bool {
    let words = pw.split(separator: " ").map(String.init)
    var thing = Set<String>()

    for word in words {
        if thing.contains(word) {
            return false
        }
        thing.insert(word)
    }
    return true
}

func validate2(_ pw: String) -> Bool {
    let words = pw.split(separator: " ").map(String.init)
    var thing = Set<String>()

    for word in words {
        let sortedWord = String(word.sorted())
        if thing.contains(sortedWord) {
            return false
        }
        thing.insert(sortedWord)
    }
    return true
}

func countValid(_ pws: [String], _ validator: (String) -> Bool) -> Int {
    return pws.map(validator).reduce(0) { return $0 + ($1 ? 1 : 0)}
}

if CommandLine.argc > 1 {
    let input = Array(CommandLine.arguments.dropFirst())
    print(countValid(input, validate))
    print(countValid(input, validate2))
} else {
    print("Specify input")
}
