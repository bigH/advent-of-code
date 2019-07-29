use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();
    let input = &args[1].parse::<usize>().unwrap();

    let mut buffer = vec!(0);
    let mut position = 0;
    let mut next = 1;

    loop {
        if next == 2018 {
            position += 1;
            break;
        }

        position = (position + input) % buffer.len() + 1;
        buffer.insert(position, next);

        // println!("{}th buffer ({}) = {:?}", next, position, buffer);

        next += 1;
    }

    println!("final value: {}", buffer[position]);
}
