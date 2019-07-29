use std::fs;
use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();
    let filename = &args[1];

    let result_of_read = fs::read_to_string(filename);
    let file_contents = result_of_read.expect("Something went wrong reading the file");
    let trimmed = file_contents.trim();

    let mut current: Option<char> = None;
    let mut prev: Option<char> = None;
    let mut first: Option<char> = None;

    let mut sum = 0;

    for c in trimmed.chars() {
        if first.is_none() && prev.is_none() {
            first = Some(c);
            current = Some(c);
        } else {
            prev = current;
            current = Some(c);

            if current.expect("a") == prev.expect("b") {
                sum = sum + current.expect("c").to_digit(10).expect("d");
            }
        }
    }

    if current.expect("e") == first.expect("f") {
        sum = sum + current.expect("g").to_digit(10).expect("h");
    }

    println!("{:?}", sum);
}

