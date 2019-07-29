extern crate regex;

use regex::Regex;
use std::env;
use std::fs;
use std::collections::HashMap;

#[derive(Debug)]
struct ProgramTree {
    tree: HashMap<String, Program>
}

#[derive(Default, Debug)]
struct Program {
    name: String,
    weight: i32,
    sub_programs: Vec<String>,
    parent: Option<String>,
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let filename = &args[1];

    let text = fs::read_to_string(filename)
        .expect("Something went wrong reading the file");

    let lines = text.trim().split("\n");

    let re = Regex::new(r"([a-z]+) \((\d+)\)( -> ([a-z, ]+))?").unwrap();

    let mut nodes = HashMap::new();

    for line in lines {
        let cap = re.captures(line).unwrap();
        let potential_sub_programs = cap.get(4);
        let sub_programs = match potential_sub_programs {
            Some(contents) => {
                contents.as_str().split(", ").map(|s| String::from(s)).collect()
            },
            _ => {
                Vec::new()
            },
        };
        let name = String::from(cap.get(1).unwrap().as_str());
        let node = nodes.entry(name.to_string()).or_insert_with(Program::default);

        node.name = name.clone();
        node.weight = cap.get(2).unwrap().as_str().parse::<i32>().unwrap();
        node.sub_programs = sub_programs.clone();

        for sub_program in sub_programs {
            nodes
                .entry(sub_program)
                .or_insert_with(Program::default)
                .parent = Some(name.clone());
        }
    }

    let programs = ProgramTree { tree: nodes };

    let root_node = programs.root_node().expect("root not found.");

    // println!("tree = {:#?}", programs.tree);
    println!("root = {}", root_node);

    let (key, weight) = programs.find_anomalous(&root_node)
        .expect("No anomalous value found.");

    println!("balancing requires {} to weigh {}", key, weight);

    let mut next_key = key;
    let mut next_weight = weight;

    loop {
        match programs.find_anomalous(&next_key) {
            Some((k, w)) => {
                next_key = k;
                next_weight = w;
            },
            None => break,
        }
        println!("balancing requires {} to weigh {}", next_key, next_weight);
    }

    let node = &programs.tree[&next_key.clone()];
    println!("balancing requires {} to weigh {}", next_key, node.weight + next_weight);
}

impl ProgramTree {
    fn root_node(&self) -> Option<String> {
        for node in self.tree.values() {
            if node.parent.is_none() {
                return Some(node.name.clone());
            }
        }
        return None;
    }

    fn find_anomalous(&self, key: &String) -> Option<(String, i32)> {
        let mut weights: HashMap<i32, Vec<&String>> = HashMap::new();

        let node = &self.tree[key];

        for c in &node.sub_programs {
            println!("weighing: {} ...", c);
            let weight = self.weigh(c);
            println!("weight of {} is {}", c, weight);
            weights
                .entry(weight)
                .or_insert_with(Vec::new)
                .push(c);
        }

        let mut weights = weights.into_iter().collect::<Vec<_>>();
        weights.sort_by(|a, b| a.1.len().cmp(&b.1.len()));

        let mut w = weights.into_iter();
        let first = w.next().expect("first");
        println!("first = {} / {:?}", first.0, first.1);

        if let Some(second) = w.next() {
            println!("second = {} / {:?}", second.0, second.1);
            let key = *first.1.iter().next().expect("node");
            let diff = second.0 - first.0;

            Some((key.clone(), diff))
        } else {
            None
        }
    }

    fn weigh(&self, root: &String) -> i32 {
        let node = &self.tree[root];
        println!("weighing: {} -> {} / {:?} ...", root, node.weight, node.sub_programs);

        let mut subtrees_weight: Vec<i32> = Vec::new();

        for subtree in node.sub_programs.iter() {
            let weight = self.weigh(&subtree);
            println!(" -> {} weighs {}", subtree, weight);
            subtrees_weight.push(weight);
        }

        let subtrees_weight_sum: i32 = subtrees_weight.iter().sum();

        node.weight + subtrees_weight_sum
    }
}
