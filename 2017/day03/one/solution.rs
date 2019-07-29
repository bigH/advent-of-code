use std::env;
use std::fs;

fn main() {
    let args: Vec<String> = env::args().collect();
    let filename = &args[1];

    let text = fs::read_to_string(filename)
        .expect("Something went wrong reading the file");

    let input = text.trim().parse::<i32>()
        .expect("Could not parse into an `i32`.");

    let mut i: i32 = 1;

    while (i + 2) * (i + 2) < input {
        i += 2;
    }

    let j: i32 = i + 1;
    let mut n: i32 = 0;

    while i * i + j * (n + 1) <= input {
        n += 1;
    }

    let corner: i32 = i * i + n * j;
    let rem: i32 = input - corner;
    let radius: i32 = (i + 1) / 2;
    let offset: i32 = (rem - radius).abs();

    let distance: i32 = radius + offset;

    println!("input = {}", input);
    println!("i = {}", i);
    println!("j = {}", j);
    println!("n = {}", n);
    println!("corner = {}", corner);
    println!("rem = {}", rem);
    println!("radius = {}", radius);
    println!("offset = {}", offset);
    println!("distance = {}", distance);
}
