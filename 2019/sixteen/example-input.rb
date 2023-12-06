{
  "AA" => { rate: 0, connects_to: ["DD", "II", "BB"] },
  "BB" => { rate: 13, connects_to: ["CC", "AA"] },
  "CC" => { rate: 2, connects_to: ["DD", "BB"] },
  "DD" => { rate: 20, connects_to: ["CC", "AA", "EE"] },
  "EE" => { rate: 3, connects_to: ["FF", "DD"] },
  "FF" => { rate: 0, connects_to: ["EE", "GG"] },
  "GG" => { rate: 0, connects_to: ["FF", "HH"] },
  "HH" => { rate: 22, connects_to: ["GG"] },
  "II" => { rate: 0, connects_to: ["AA", "JJ"] },
  "JJ" => { rate: 21, connects_to: ["II"] },
}
