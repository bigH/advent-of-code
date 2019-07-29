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
        let tokens = line.trim().split_whitespace();
        let numbers = tokens.map(|t| t.parse::<i32>().unwrap());

        let mut found = false;

        for (idx, i) in numbers.clone().enumerate() {
            for (_jdx, j) in numbers.clone().skip(idx + 1).enumerate() {
                if !found {
                    if i % j == 0 {
                        sum += i / j;
                        found = true;
                    }
                    if j % i == 0 {
                        sum += j / i;
                        found = true;
                    }
                }
            }
        }
    }

    println!("{}", sum);
}
