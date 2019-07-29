use std::env;
use std::fs;

fn main() {
    let args: Vec<String> = env::args().collect();
    let filename = &args[1];

    let text = fs::read_to_string(filename)
        .expect("Something went wrong reading the file");

    let lines = text.trim().split("\n");

    let mut sum: i32 = 0;
    for line in lines {
        let mut min = 9999999;
        let mut max = 0;
        let tokens = line.trim().split("\t");
        for token in tokens {
            println!("potential number: {}", token);
            let number = token.parse::<i32>().expect("Must be a number");
            if number < min {
                min = number;
            }
            if number > max {
                max = number;
            }
        }
        sum += max - min;
    }

    println!("{}", sum)
}
