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

    let directions: Vec<Direction> = text.trim().split(",")
        .map(Direction::from_string)
        .collect();

    let mut zero = 0;
    let dancefloor: &mut Vec<char> = &mut (b'a' ..= b'p')
        .map(char::from)
        .collect();

    let dancefloor_size = dancefloor.len();

    let mut history: Vec<String> = Vec::new();

    let total_iterations = 1_000_000_000;
    for i in 0..total_iterations {
        for direction in directions.iter() {
            match direction {
                Direction::Spin(amt) => {
                    zero = (zero + amt) % dancefloor_size;
                },
                Direction::SwapPositions(a, b) => {
                    let a = ((dancefloor_size - zero) + a) % dancefloor_size;
                    let b = ((dancefloor_size - zero) + b) % dancefloor_size;
                    swap(dancefloor, a, b);
                }
                Direction::SwapPrograms(a, b) => {
                    let a_pos = dancefloor.iter().position(|x| *x == *a).unwrap();
                    let b_pos = dancefloor.iter().position(|x| *x == *b).unwrap();
                    swap(dancefloor, a_pos, b_pos);
                }
            }
        }

        let buffer_copy: String = dancefloor.clone().into_iter().collect();
        let (start, end) = buffer_copy.split_at(dancefloor_size - zero);

        let mut new_dancefloor = String::new();
        new_dancefloor.push_str(end);
        new_dancefloor.push_str(start);

        if history.contains(&new_dancefloor) {
            let location = history.iter().position(|old| *old == new_dancefloor).unwrap();
            let loop_size = history.len() - location;
            let offset = (total_iterations - 1 - i) % loop_size;

            let final_state = &history[location + offset];
            println!("final state: {}", final_state);
            break;
        }

        // println!("{}th state: {}", i, new_dancefloor);
        history.push(new_dancefloor);
    }
}

fn swap(dancefloor: &mut Vec<char>, a: usize, b: usize) {
    let tmp = dancefloor[a];
    dancefloor[a] = dancefloor[b];
    dancefloor[b] = tmp;
}
