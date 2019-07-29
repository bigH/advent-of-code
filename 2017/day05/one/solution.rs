use std::env;
use std::fs;

fn main() {
    let args: Vec<String> = env::args().collect();
    let filename = &args[1];

    let text = fs::read_to_string(filename)
        .expect("Something went wrong reading the file");

    let lines = text.trim().split("\n");

    let mut jumps: Vec<i32> = lines.map(|t| t.trim().parse::<i32>().unwrap()).collect();

    let mut counter = 0;
    let mut current_index = 0;

    while current_index < jumps.len() {
        counter += 1;
        let jump = jumps[current_index];
        jumps[current_index] += 1;
        current_index = ((current_index as i32) + jump) as usize;
    }

    println!("{}", counter);
}
