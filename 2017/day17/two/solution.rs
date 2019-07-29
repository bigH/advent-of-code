use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();
    let input = args[1].parse::<usize>().unwrap();

    let num_sub_buffers = 100_000;
    let mut buffer: Vec<Vec<usize>> = Vec::new();

    let mut super_idx: usize = 0;
    let mut sub_idx: usize = 0;

    let mut deep_mode = false;

    buffer.insert(0, vec!(0));

    let mut next = 1;

    loop {
        if next == 50_000_001 {
            let mut found = false;
            for i in 0..buffer.len() {
                if found { break; }
                for j in 0..buffer[i].len() {
                    if found { break; }
                    if buffer[i][j] == 0 {
                        found = true;
                        super_idx = i;
                        sub_idx = j;
                    }
                }
            }
            cycle(1, &buffer, &mut super_idx, &mut sub_idx);
            println!("unlock with: {}", buffer[super_idx][sub_idx]);
            break;
        }

        if deep_mode {
            cycle(input, &buffer, &mut super_idx, &mut sub_idx);
            step(&buffer, &mut super_idx, &mut sub_idx);

            buffer[super_idx].insert(sub_idx, next);
        } else {
            for _i in 0..input {
                super_idx = (super_idx + 1) % buffer.len();
            }
            super_idx += 1;

            buffer.insert(super_idx, vec!(next));

            if buffer.len() >= num_sub_buffers {
                deep_mode = true;
                println!("deep_mode set at: {}", next);
            }

        }

        if next % 100_000 == 0 {
            println!("finished: {}", next);
        }

        next += 1;
    }
}

fn cycle(n: usize, buffer: &Vec<Vec<usize>>, super_idx: &mut usize, sub_idx: &mut usize) {
    for _i in 0..n {
        *sub_idx += 1;
        while buffer[*super_idx].len() == *sub_idx {
            *super_idx = (*super_idx + 1) % buffer.len();
            *sub_idx = 0;
        }
    }
}

fn step(buffer: &Vec<Vec<usize>>, super_idx: &mut usize, sub_idx: &mut usize) {
    *sub_idx += 1;
    if buffer[*super_idx].len() == *sub_idx {
        *super_idx = (*super_idx + 1) % buffer.len();
        *sub_idx = 0;
    }
}
