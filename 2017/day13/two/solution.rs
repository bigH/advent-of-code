use std::env;
use std::fs;

fn main() {
    let args: Vec<String> = env::args().collect();
    let filename = &args[1];

    let text = fs::read_to_string(filename)
        .expect("Something went wrong reading the file");

    let lines = text.trim().split("\n");

    let mut layers: Vec<(i32, i32)> = Vec::new();
    let mut delay: i32 = 0;

    for layer in lines {
        let cells: Vec<&str> = layer.trim().split(": ").collect();
        let depth = cells[0].parse::<i32>().unwrap();
        let range = cells[1].parse::<i32>().unwrap();
        layers.push((depth, (range - 1) * 2));
    }

    let mut max_depth = 0;

    loop {
        let hit = layers.iter().find(|(d, r)| (d + delay) % r == 0);

        match hit {
            Some((d, _r)) => {
                if *d > max_depth {
                    max_depth = d.clone();
                    println!("delay = {}; hits at depth {}", delay, d);
                }
                delay += 1;
            }
            None       => {
                break;
            }
        }
    }

    println!("delay: {}", delay)
}
