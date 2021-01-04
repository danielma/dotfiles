import Foundation

enum Weekday: Int {
  case Sunday = 1
  case Monday
  case Tuesday
  case Wednesday
  case Thursday
  case Friday
  case Saturday
}

let calendar = Calendar(identifier: .gregorian)
let now = Date()
var dateComponents = calendar.dateComponents([.weekday], from: now)
dateComponents.weekday = Weekday.Friday.rawValue

let beginningOfDay = DateComponents(hour: 0)

print(Weekday.Sunday.rawValue)
print(Weekday.Friday.rawValue)
print(dateComponents)

//calendar.nextDate(after: now, matching: beginningOfDay,
let weekDay = Weekday.Thursday

let startDate = Calendar.current.nextDate(after: now, matching: DateComponents(hour: 0, weekday: weekDay.rawValue), matchingPolicy: .nextTime, direction: .backward)
let endDate = Calendar.current.nextDate(after: now, matching: DateComponents(hour: 0, weekday: weekDay.rawValue), matchingPolicy: .nextTime)
