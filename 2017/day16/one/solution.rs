use std::env;
use std::fs;

#[derive(Debug)]
enum Direction {
    Spin(usize),
    SwapPositions(usize, usize),
    SwapPrograms(char, char),
}

impl Direction {
    fn from_string(input: &str) -> Direction {
        let mut iter = input.chars();
        let first_char = iter.next().unwrap();
        match first_char {
            's' => {
                let rest: String = iter.collect();
                let spin_distance = rest.parse::<usize>()
                    .expect("Expected number.");
                Direction::Spin(spin_distance)
            },
            'x' => {
                let rest: String = iter.collect();
                let split: Vec<&str> = rest.split("/").collect();
                let x = split[0].parse::<usize>()
                    .expect("Expected number.");
                let y = split[1].parse::<usize>()
                    .expect("Expected number.");
                Direction::SwapPositions(x, y)
            },
            'p' => {
                let rest: String = iter.collect();
                let split: Vec<&str> = rest.split("/").collect();
                let a = split[0].chars().next().unwrap();
                let b = split[1].chars().next().unwrap();
                Direction::SwapPrograms(a, b)
            },
            _ => {
                println!("couldn't parse first char: {}", input);
                panic!("failed to parse input.");
            },
        }
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let filename = &args[1];

    let text = fs::read_to_string(filename)
        .expect("Something went wrong reading the file");

    let directions = text.trim().split(",")
        .map(Direction::from_string);

    let mut zero = 0;
    let mut dancefloor: Vec<char> = (b'a' ..= b'p')
        .map(char::from)
        .collect();
    let dancefloor_size = dancefloor.len();

    for direction in directions {
        match direction {
            Direction::Spin(amt) => {
                zero = (zero + amt) % dancefloor_size;
            },
            Direction::SwapPositions(a, b) => {
                let a = ((dancefloor_size - zero) + a) % dancefloor_size;
                let b = ((dancefloor_size - zero) + b) % dancefloor_size;
                swap(&mut dancefloor, a, b);
            }
            Direction::SwapPrograms(a, b) => {
                let a_pos = dancefloor.iter().position(|x| *x == a).unwrap();
                let b_pos = dancefloor.iter().position(|x| *x == b).unwrap();
                swap(&mut dancefloor, a_pos, b_pos);
            }
        }
    }

    let dancefloor: String = dancefloor.into_iter().collect();
    let (start, end) = dancefloor.split_at(dancefloor.len() - zero);
    println!("final floor: {}{}", end, start);
}

fn swap(dancefloor: &mut Vec<char>, a: usize, b: usize) {
    let tmp = dancefloor[a];
    dancefloor[a] = dancefloor[b];
    dancefloor[b] = tmp;
}
