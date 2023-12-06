Monkeys = [{
  items: [79, 98],
  operation: lambda { |old| old * 19 },
  test: lambda { |worry| worry % 23 == 0 },
  test_number: 23,
  outcomes: {
    true: 2,
    false: 3,
  }
},{
  items: [54, 65, 75, 74],
  operation: lambda { |old| old + 6 },
  test: lambda { |worry| worry % 19 == 0 },
  test_number: 19,
  outcomes: {
    true: 2,
    false: 0,
  }
},{
  items: [79, 60, 97],
  operation: lambda { |old| old * old },
  test: lambda { |worry| worry % 13 == 0 },
  test_number: 13,
  outcomes: {
    true: 1,
    false: 3,
  }
},{
  items: [74],
  operation: lambda { |old| old + 3 },
  test: lambda { |worry| worry % 17 == 0 },
  test_number: 17,
  outcomes: {
    true: 0,
    false: 1,
  }
}]
