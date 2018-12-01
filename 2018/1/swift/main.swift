import Foundation

func findRepeatFreq(_ nums: [Int]) -> Int {
    var index = 0
    var freq = 0
    var prev: Set<Int> = []

    while !prev.contains(freq) {
        prev.insert(freq)

        if index >= nums.count {
            index = 0
        }
        
        freq += nums[index]
        index += 1
    }

    return freq
}

if CommandLine.argc > 1 {
    let input = Array(CommandLine.arguments.dropFirst())
    let nums = input.compactMap(Int.init)
    print(nums.reduce(0, +))
    print(findRepeatFreq(nums))
} else {
    print("Specify the input")
}
