use std::env;
use std::fs;

fn main() {
    let args: Vec<String> = env::args().collect();
    let filename = &args[1];

    let text = fs::read_to_string(filename)
        .expect("Something went wrong reading the file");

    let mut inputs: Vec<usize> = text.trim().chars().map(|c| c as usize).collect();

    inputs.push(17);
    inputs.push(31);
    inputs.push(73);
    inputs.push(47);
    inputs.push(23);

    println!("{:?}", inputs);

    let mut buffer: [usize; 256] = [0; 256];
    let mut position: usize = 0;

    for i in 0..256 {
        buffer[i] = i;
    }

    let mut skip_size = 0;

    for _i in 0..64 {
        for input in inputs.iter() {
            reverse(&mut buffer, position, *input);
            position = (position + input + skip_size) % 256;

            skip_size += 1;
        }
    }

    let mut post_xor: Vec<u8> = Vec::new();

    for i in 0..16 {
        let mut xord: u8 = 0;
        for j in 0..16 {
            xord = xord ^ (buffer[i * 16 + j] as u8);
        }
        post_xor.push(xord);
    }

    println!("{:?}", post_xor);

    let mut hex = String::new();
    for value in post_xor {
        hex.push_str(hexify(value).as_str());
    }

    println!("{}", hex);
}

fn hexify(val: u8) -> String {
    let mut value = String::new();
    value.push_str(hexify_digit(val / 16).as_str());
    value.push_str(hexify_digit(val % 16).as_str());
    value
}

fn hexify_digit(val: u8) -> String {
    match val {
        0  => String::from("0"),
        1  => String::from("1"),
        2  => String::from("2"),
        3  => String::from("3"),
        4  => String::from("4"),
        5  => String::from("5"),
        6  => String::from("6"),
        7  => String::from("7"),
        8  => String::from("8"),
        9  => String::from("9"),
        10 => String::from("a"),
        11 => String::from("b"),
        12 => String::from("c"),
        13 => String::from("d"),
        14 => String::from("e"),
        15 => String::from("f"),
        _  => {
            println!("unknown value: {}", val);
            panic!("unknown value for hexify_digit");
        },
    }
}

fn reverse(buffer: &mut [usize; 256], position: usize, length: usize) {
    for i in 0..(length / 2) {
        swap(buffer, (position + i) % 256, (position + length - 1 - i) % 256);
    }
}

fn swap(buffer: &mut [usize; 256], i: usize, j: usize) {
    let tmp = buffer[i];
    buffer[i] = buffer[j];
    buffer[j] = tmp;
}
