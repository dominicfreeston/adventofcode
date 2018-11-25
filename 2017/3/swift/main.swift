import Foundation

struct Point: Hashable {
    var x: Int
    var y: Int
}

func nextPoint(_ p: Point) -> Point {
    func shouldGoRight(_ p: Point) -> Bool {
        return (p.y >= 0 && p.x == p.y) || (p.y > 0 && abs(p.y) >= abs(p.x))
    }

    func shouldGoUp(_ p: Point) -> Bool {
        return p.x > 0 && abs(p.x) > abs(p.y)
    }

    func shouldGoLeft(_ p: Point) -> Bool {
        return p.y < 0 && abs(p.x) <= abs(p.y) && p.x != p.y
    }

    func shouldGoDown(_ p: Point) -> Bool {
        return p.x < 0 && abs(p.x) >= abs(p.y) && p.x != -p.y
    }

    var p = p
    if shouldGoRight(p) {
        p.x += 1
    } else if shouldGoUp(p) {
        p.y -= 1
    } else if shouldGoLeft(p) {
        p.x -= 1
    } else if shouldGoDown(p) {
        p.y += 1
    }
    return p
}

func coordinates(_ input: Int) -> Point {
    var p = Point(x: 0, y: 0)
    for _ in 1..<input {
        p = nextPoint(p)
    }
    return p
}

func distance(_ p: Point) -> Int {
    return abs(p.x) + abs(p.y)
}

func newValue(at p: Point, _ m: [Point: Int]) -> Int {
    var total = 0
    for x in p.x - 1 ... p.x + 1 {
        for y in p.y - 1 ... p.y + 1 {
            if let val =  m[Point(x: x, y: y)] {
                total += val
            }
        }
    }
    return total
}

func firstValue(after i: Int) -> Int {
    var p = Point(x: 0, y: 0)
    var m = [p: 1]
    var val = 0

    while val < i {
        p = nextPoint(p)
        val = newValue(at: p, m)
        m[p] = val
    }

    return val
}

if CommandLine.argc > 1, let input = Int(CommandLine.arguments[1]) {
    print(distance(coordinates(input)))
    print(firstValue(after: input))
} else {
    print("Specify the input")
}
