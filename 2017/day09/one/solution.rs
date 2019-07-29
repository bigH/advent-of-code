use std::env;
use std::fs;

fn main() {
    let args: Vec<String> = env::args().collect();
    let filename = &args[1];

    let text = fs::read_to_string(filename)
        .expect("Something went wrong reading the file");

    let mut score: usize = 0;
    let mut depth: usize = 0;
    let mut is_garbage: bool = false;
    let mut is_ignoring_char: bool = false;

    for c in text.chars() {
        if is_ignoring_char {
            is_ignoring_char = false;
        } else if is_garbage {
            match c {
                '!' => is_ignoring_char = true,
                '>' => is_garbage = false,
                _   => {},
            }
        } else {
            match c {
                '{' => depth += 1,
                '}' => {
                    score += depth;
                    depth -= 1;
                },
                '<' => is_garbage = true,
                _   => {},
            }
        }
    }
    println!("score = {}", score);
}
