use std::env;
use std::fs;

fn main() {
    let args: Vec<String> = env::args().collect();
    let filename = &args[1];

    let contents: Vec<char> = fs::read_to_string(filename)
        .expect("Something went wrong reading the file")
        .trim().chars().collect();

    let length = contents.len();

    let mut sum = 0;

    for (i, _c) in contents.iter().enumerate() {
        let l = contents[i];
        let r = contents[(i + length / 2) % length];
        if l == r {
            sum = sum + l.to_digit(10).expect("Always 4 knocks");
        }
    }

    println!("{:?}", sum);
}

