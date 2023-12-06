Monkeys = [{
  items: [92, 73, 86, 83, 65, 51, 55, 93],
  operation: lambda { |old| old * 5 },
  test: lambda { |worry| worry % 11 == 0 },
  test_number: 11,
  outcomes: {
    true: 3,
    false: 4,
  }
},{
  items: [99, 67, 62, 61, 59, 98],
  operation: lambda { |old| old * old },
  test: lambda { |worry| worry % 2 == 0 },
  test_number: 2,
  outcomes: {
    true: 6,
    false: 7,
  }
},{
  items: [81, 89, 56, 61, 99],
  operation: lambda { |old| old * 7 },
  test: lambda { |worry| worry % 5 == 0 },
  test_number: 5,
  outcomes: {
    true: 1,
    false: 5,
  }
},{
  items: [97, 74, 68],
  operation: lambda { |old| old + 1 },
  test: lambda { |worry| worry % 17 == 0 },
  test_number: 17,
  outcomes: {
    true: 2,
    false: 5,
  }
},{
  items: [78, 73],
  operation: lambda { |old| old + 3 },
  test: lambda { |worry| worry % 19 == 0 },
  test_number: 19,
  outcomes: {
    true: 2,
    false: 3,
  }
},{
  items: [50],
  operation: lambda { |old| old + 5 },
  test: lambda { |worry| worry % 7 == 0 },
  test_number: 7,
  outcomes: {
    true: 1,
    false: 6,
  }
},{
  items: [95, 88, 53, 75],
  operation: lambda { |old| old + 8 },
  test: lambda { |worry| worry % 3 == 0 },
  test_number: 3,
  outcomes: {
    true: 0,
    false: 7,
  }
},{
  items: [50, 77, 98, 85, 94, 56, 89],
  operation: lambda { |old| old + 2 },
  test: lambda { |worry| worry % 13 == 0 },
  test_number: 13,
  outcomes: {
    true: 4,
    false: 0,
  }
}]
