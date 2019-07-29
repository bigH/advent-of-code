use std::env;
use std::fs;
use std::collections::HashMap;
use std::collections::VecDeque;

fn main() {
    let args: Vec<String> = env::args().collect();
    let filename = &args[1];

    let text = fs::read_to_string(filename)
        .expect("Something went wrong reading the file");

    let lines = text.trim().split("\n");

    let mut connections: HashMap<i32, Vec<i32>> = HashMap::new();
    for line in lines {
        let connect_data: Vec<&str> = line.split(" <-> ").collect();

        let node = connect_data[0].parse::<i32>().expect("(n) expected a number.");
        let connectees: Vec<i32> = connect_data[1].split(", ")
            .map(|n| n.parse::<i32>().expect("(c) expected a number."))
            .collect();

        connections.insert(node, connectees);
    }

    let connected_to_zero = find_all_connected_to(0, connections);

    println!("{}", connected_to_zero.len());
}

fn find_all_connected_to(node: i32, connections: HashMap<i32, Vec<i32>>) -> Vec<i32> {
    let mut transitive: Vec<i32> = Vec::new();
    let mut current: VecDeque<i32> = VecDeque::new();

    let empty = Vec::new();

    current.push_back(node);

    while let Some(connector) = current.pop_front() {
        if !transitive.contains(&connector) {
            transitive.push(connector);
            for connectee in connections.get(&connector).unwrap_or(&empty) {
                current.push_back(*connectee);
            }
        }
    }

    transitive
}
