use std::env;
use std::fs;

fn main() {
    let args: Vec<String> = env::args().collect();
    let filename = &args[1];

    let text = fs::read_to_string(filename)
        .expect("Something went wrong reading the file");

    let inputs: Vec<usize> = text.trim().split(",").map(|t|
        t.parse::<usize>().unwrap()
    ).collect();

    let mut buffer: [usize; 256] = [0; 256];
    let mut position: usize = 0;

    for i in 0..256 {
        buffer[i] = i;
    }

    let mut skip_size = 0;

    for input in inputs.iter() {
        reverse(&mut buffer, position, *input);
        position = (position + input + skip_size) % 256;

        skip_size += 1;
    }

    println!("{}", buffer[0] * buffer[1]);
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
