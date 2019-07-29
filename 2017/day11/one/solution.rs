use std::env;
use std::fs;

fn main() {
    let args: Vec<String> = env::args().collect();
    let filename = &args[1];

    let text = fs::read_to_string(filename)
        .expect("Something went wrong reading the file");

    let directions = text.trim().split(",");

    let mut current: (i32, i32) = (0, 0);

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
    }

    println!("ended up at {}, {}", current.0, current.1);

    if current.1 >= 0 {
        if current.1.abs() <= (current.0.abs() / 2) {
            let d = current.0.abs() + 0;
            println!("total distance to {}, {} is {}", current.0, current.1, d);
        } else {
            let d = current.0.abs() + current.1.abs() - (current.0.abs() / 2);
            println!("total distance to {}, {} is {}", current.0, current.1, d);
        }
    } else {
        if current.1.abs() <= ((current.0.abs() + 1) / 2) {
            let d = current.0.abs() + 0;
            println!("total distance to {}, {} is {}", current.0, current.1, d);
        } else {
            let d = current.0.abs() + current.1.abs() - ((current.0.abs() + 1) / 2);
            println!("total distance to {}, {} is {}", current.0, current.1, d);
        }
    }
}

