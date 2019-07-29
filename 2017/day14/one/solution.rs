use std::env;
use std::fs;

fn main() {
    let args: Vec<String> = env::args().collect();
    let filename = &args[1];

    let text = fs::read_to_string(filename)
        .expect("Something went wrong reading the file");

    let key: String = String::from(text.trim());
    let mut total_bits = 0;
    for i in 0..128 {
        let row_key = key.clone() + "-" + &i.to_string();
        let hash = knot_hash(&row_key);
        let bits = count_bits_as_hex(&hash);
        total_bits += bits;
        println!("{} ({} -> {}) for {}", hash, bits, total_bits, row_key);
    }
}

fn knot_hash(input: &String) -> String {
    let mut buffer: [u8; 256] = [0; 256];
    let mut position: usize = 0;

    for i in 0..256 {
        buffer[i] = i as u8;
    }

    let mut skip_size: usize = 0;

    for _i in 0..64 {
        for c in input.chars() {
            let ascii = c as usize;
            reverse(&mut buffer, position, ascii);
            position = (position + ascii + skip_size) % 256;
            skip_size += 1;
        }

        // it was unclear whether this needed to still be done,
        // but the right answer required it...
        for c in [17, 31, 73, 47, 23].iter() {
            let ascii = *c as usize;
            reverse(&mut buffer, position, ascii);
            position = (position + ascii + skip_size) % 256;
            skip_size += 1;
        }
    }

    let mut hex = String::new();

    for i in 0..16 {
        let mut xord: u8 = 0;
        for j in 0..16 {
            xord = xord ^ buffer[i * 16 + j];
        }
        hex.push_str(hexify(xord).as_str());
    }

    hex
}

fn count_bits_as_hex(val: &String) -> usize {
    let mut num_bits = 0;
    for c in val.chars() {
        num_bits += match c {
            '0' => 0,
            '1' => 1,
            '2' => 1,
            '3' => 2,
            '4' => 1,
            '5' => 2,
            '6' => 2,
            '7' => 3,
            '8' => 1,
            '9' => 2,
            'a' => 2,
            'b' => 3,
            'c' => 2,
            'd' => 3,
            'e' => 3,
            'f' => 4,
            _ => {
                println!("unknown hex char: {}", c);
                panic!("unknown value for count_bits_as_hex");
            },
        }
    }
    num_bits
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

fn reverse(buffer: &mut [u8; 256], position: usize, length: usize) {
    for i in 0..(length / 2) {
        swap(buffer, (position + i) % 256, (position + length - 1 - i) % 256);
    }
}

fn swap(buffer: &mut [u8; 256], i: usize, j: usize) {
    let tmp = buffer[i];
    buffer[i] = buffer[j];
    buffer[j] = tmp;
}
