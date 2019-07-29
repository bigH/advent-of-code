extern crate regex;

use regex::Regex;
use std::env;
use std::fs;
use std::collections::HashMap;

#[derive(Debug)]
enum Op {
    E, NE, GT, GTE, LT, LTE
}

impl Op {
    fn from_string(serialized: String) -> Op {
        match serialized.as_str() {
            "==" => Op::E,
            "!=" => Op::NE,
            ">"  => Op::GT,
            ">=" => Op::GTE,
            "<"  => Op::LT,
            "<=" => Op::LTE,
            _    => {
                println!("impossible op {}", serialized);
                panic!("impossible op")
            },
        }
    }

    fn apply(&self, lhs: &i32, rhs: i32) -> bool {
        match self {
            Op::E => *lhs == rhs,
            Op::NE => *lhs != rhs,
            Op::GT => *lhs > rhs,
            Op::GTE => *lhs >= rhs,
            Op::LT => *lhs < rhs,
            Op::LTE => *lhs <= rhs,
            _ => {
                println!("impossible op {:?}", self);
                panic!("impossible op")
            },
        }
    }
}

#[derive(Debug)]
enum Action {
    Inc, Dec
}

impl Action {
    fn from_string(serialized: String) -> Action {
        match serialized.as_str() {
            "inc" => Action::Inc,
            "dec" => Action::Dec,
            _    => {
                println!("impossible action {}", serialized);
                panic!("impossible action")
            },
        }
    }

    fn apply(&self, acted_on: &mut i32, amount: i32) {
        match self {
            Action::Inc =>
                *acted_on = *acted_on + amount,
            Action::Dec =>
                *acted_on = *acted_on - amount,
            _ => {
                println!("impossible op {:?}", self);
                panic!("impossible op")
            },
        }
    }
}

struct Instruction {
    acted_register: String,
    action: Action,
    amount: i32,
    tested_register: String,
    test_op: Op,
    test_value: i32,
}


impl Instruction {
    fn from_string(line: &str) -> Instruction {
        // println!("{}", line);
        let re: Regex = Regex::new(r"([a-z]+) (inc|dec) (-?[0-9]+) if ([a-z]+) (==|!=|[><]=?) (-?[0-9]+)").unwrap();
        let cap = re.captures(line).unwrap();
        Instruction {
            acted_register: String::from(cap.get(1).unwrap().as_str()),
            action: Action::from_string(String::from(cap.get(2).unwrap().as_str())),
            amount: cap.get(3).unwrap().as_str().parse::<i32>().unwrap(),
            tested_register: String::from(cap.get(4).unwrap().as_str()),
            test_op: Op::from_string(String::from(cap.get(5).unwrap().as_str())),
            test_value: cap.get(6).unwrap().as_str().parse::<i32>().unwrap(),
        }
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let filename = &args[1];

    let text = fs::read_to_string(filename)
        .expect("Something went wrong reading the file");

    let lines = text.trim().split("\n");

    let instructions = lines.map(Instruction::from_string);
    let mut registers: HashMap<String, i32> = HashMap::new();

    for inst in instructions {
        let register_value: &mut i32 = registers.entry(inst.tested_register).or_insert(0);
        if inst.test_op.apply(register_value, inst.test_value) {
            let acted_value: &mut i32 = registers.entry(inst.acted_register).or_insert(0);
            inst.action.apply(acted_value, inst.amount)
        }
    }

    let (max_key, max_val) = registers.iter().max_by(|(_kl, vl), (_kr, vr)| vl.cmp(vr)).unwrap();

    println!("key: {} val: {}", max_key, max_val);
}
