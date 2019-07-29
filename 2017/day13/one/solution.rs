use std::env;
use std::fs;

fn main() {
    let args: Vec<String> = env::args().collect();
    let filename = &args[1];

    let text = fs::read_to_string(filename)
        .expect("Something went wrong reading the file");

    let layers = text.trim().split("\n");

    let mut severity: i32 = 0;

    for layer in layers {
        let cells: Vec<&str> = layer.trim().split(": ").collect();
        let depth = cells[0].parse::<i32>().unwrap();
        let range = cells[1].parse::<i32>().unwrap();
        if depth % ((range - 1) * 2) == 0 {
            severity += depth * range;
        }
    }

    println!("severity: {}", severity);
}
