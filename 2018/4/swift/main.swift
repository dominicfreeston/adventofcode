import Foundation

typealias GuardId = Int
struct Event {
    enum What {
        case beginsShift(GuardId)
        case wakesUp
        case fallsAsleep

        static func fromString(_ string: String) -> What {
            switch string {
                case "wakes up":
                    return .wakesUp
                case "falls asleep":
                    return .fallsAsleep
                default:
                    let guardId = GuardId(string
                                    .replacingOccurrences(of: "Guard #", with: "")
                                    .replacingOccurrences(of: " begins shift", with: ""))!
                    return .beginsShift(guardId)
            }
        }
    }
    let time: String
    let what: What

    var minute: Int {
        return Int(time.components(separatedBy: ":")[1])!
    }

    static func fromString(_ string: String) -> Event {
        let components = string.components(separatedBy: "] ")
        return Event(
            time: components[0].replacingOccurrences(of: "[", with: ""),
            what: What.fromString(components[1])
        )
    }
}

func groupedEvents(_ input: [String]) -> [GuardId: [Event]] {
    let events = input
                    .map(Event.fromString)
                    .sorted { $0.time < $1.time }
    
    var guardId: GuardId!
    var grouped: [GuardId: [Event]] = [:]
    for event in events {
        switch event.what {
            case .beginsShift(let id):
                guardId = id
            default:
                var ge = grouped[guardId] ?? []
                ge.append(event)
                grouped[guardId] = ge
        }
    }
    return grouped
}

func totalTimeAsleep(_ events: [Event]) -> Int {
    var fellAsleep = 0
    var total = 0
    for event in events {
        switch event.what {
            case .fallsAsleep:
                fellAsleep = event.minute
            case .wakesUp:
                total += (event.minute - fellAsleep)
            case .beginsShift:
                break
        }
    }
    return total
}

// Returns Minute and Count
func mostAsleepMinute(_ events: [Event]) -> (Int, Int) {
    var fellAsleep = 0
    var tally = [Int: Int]()
    for event in events {
        switch event.what {
            case .fallsAsleep:
                fellAsleep = event.minute
            case .wakesUp:
                (fellAsleep..<event.minute).forEach { tally[$0] = (tally[$0] ?? 0) + 1 }
            case .beginsShift:
                break
        }
    }
    return tally.max { $0.value < $1.value }!
}

if CommandLine.argc > 1 {
    let input = Array(CommandLine.arguments.dropFirst())
    let grouped = groupedEvents(input)

    let totalMinutes = grouped.mapValues(totalTimeAsleep)
    let worstMinutes = grouped.mapValues(mostAsleepMinute)

    let biggestSleeper = totalMinutes.max { $0.value < $1.value }!.key
    let mostConsistentSleeper = worstMinutes.max { $0.value.1 < $1.value.1 }!.key

    print(biggestSleeper * worstMinutes[biggestSleeper]!.0)
    print(mostConsistentSleeper * worstMinutes[mostConsistentSleeper]!.0)
} else {
    print("Specify the input")
}