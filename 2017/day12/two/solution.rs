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

    let groups = calculate_groups(&connections);

    println!("{}", groups.len());
}

fn calculate_groups(connections: &HashMap<i32, Vec<i32>>) -> Vec<Vec<i32>> {
    let mut groups: Vec<Vec<i32>> = Vec::new();
    let mut seen: Vec<i32> = Vec::new();

    while seen.len() < connections.len() {
        let next = connections.keys().find(|k| !seen.contains(k))
            .expect("expect to have at least one key.");
        let next_group = find_all_connected_to(*next, connections);
        for el in next_group.iter() {
            seen.push(el.clone());
        }
        groups.push(next_group);
    }

    groups
}

fn find_all_connected_to(node: i32, connections: &HashMap<i32, Vec<i32>>) -> Vec<i32> {
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
