use std::env;
use std::fs;

fn main() {
    let args: Vec<String> = env::args().collect();
    let filename = &args[1];

    let text = fs::read_to_string(filename)
        .expect("Something went wrong reading the file");

    let directions = text.trim().split(",");

    let mut current: (i32, i32) = (0, 0);
    let mut max_distance: i32 = 0;

    for direction in directions {
        match direction {
            "n" => current = (current.0, current.1 + 1),
            "ne" => {
                if current.0.abs() % 2 == 0 {
                    current = (current.0 + 1, current.1);
                } else {
                    current = (current.0 + 1, current.1 + 1);
                }
            },
            "nw" => {
                if current.0.abs() % 2 == 0 {
                    current = (current.0 - 1, current.1);
                } else {
                    current = (current.0 - 1, current.1 + 1);
                }
            },
            "s" => current = (current.0, current.1 - 1),
            "se" => {
                if current.0.abs() % 2 == 1 {
                    current = (current.0 + 1, current.1);
                } else {
                    current = (current.0 + 1, current.1 - 1);
                }
            },
            "sw" => {
                if current.0.abs() % 2 == 1 {
                    current = (current.0 - 1, current.1);
                } else {
                    current = (current.0 - 1, current.1 - 1);
                }
            },
            _ => {
                println!("impossible op {}", direction);
                panic!("impossible op");
            },
        }
        let mut distance = 0;
        if current.1 >= 0 {
            if current.1.abs() <= (current.0.abs() / 2) {
                distance = current.0.abs() + 0;
            } else {
                distance = current.0.abs() + current.1.abs() - (current.0.abs() / 2);
            }
        } else {
            if current.1.abs() <= ((current.0.abs() + 1) / 2) {
                distance = current.0.abs() + 0;
            } else {
                distance = current.0.abs() + current.1.abs() - ((current.0.abs() + 1) / 2);
            }
        }

        if distance > max_distance {
            max_distance = distance;
        }
    }

    println!("ended up at {}, {}", current.0, current.1);
    println!("max distance is {}", max_distance);

}

