use std::env;
use std::fs;

fn main() {
    let args: Vec<String> = env::args().collect();
    let filename = &args[1];

    let text = fs::read_to_string(filename)
        .expect("Something went wrong reading the file");

    let input = text.trim().parse::<i32>()
        .expect("Could not parse into an `i32`.");

    println!("input = {}\n", input);

    let mut last_written: i32 = 1;

    let mut data: [[i32; 61]; 61] = [[0; 61]; 61];

    data[30][30] = 1;

    let mut x: i32 = 0;
    let mut y: i32 = 0;
    let mut side = 4;

    while last_written <= input {
        match side {
            0 => y += 1,
            1 => x -= 1,
            2 => y -= 1,
            3 => x += 1,
            4 => {
                x += 1;
                side = 0;
            },
            _ => {
                println!("side was {}", side);
                panic!("impossible side value");
            }
        }

        let px = (x + 30) as usize;
        let py = (y + 30) as usize;

        data[px][py] = data[px - 1][py - 1] +
                       data[px - 1][py] +
                       data[px - 1][py + 1] +
                       data[px    ][py + 1] +
                       data[px + 1][py + 1] +
                       data[px + 1][py    ] +
                       data[px + 1][py - 1] +
                       data[px    ][py - 1];

        last_written = data[px][py];

        // println!("x = {}; y = {} --> {}", x, y, last_written);

        if x.abs() == y.abs() {
            side += 1;
            // println!("- side = {}", side);
        }
    }
    println!("{}", last_written)
}
