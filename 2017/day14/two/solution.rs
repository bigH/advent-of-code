use std::env;
use std::fs;

fn main() {
    let args: Vec<String> = env::args().collect();
    let filename = &args[1];

    let text = fs::read_to_string(filename)
        .expect("Something went wrong reading the file");

    let key: String = String::from(text.trim());
    let mut grid: String = String::new();
    for i in 0..128 {
        let row_key = key.clone() + "-" + &i.to_string();
        let hash = knot_hash(&row_key);
        grid.push_str(hash.as_str());
        grid.push_str("\n");
    }
    println!("GRID:\n{}", grid);

    let mut grid: Vec<char> = grid.chars()
        .filter(|c| *c == '0' || *c == '1')
        .collect();

    let num_islands = count_islands(&mut grid);
    println!("number of islands: {}", num_islands);
}

fn count_islands(grid: &mut Vec<char>) -> usize {
    let mut count = 0;

    for x in 0..128 {
        for y in 0..128 {
            if is_island(grid, x, y) {
                count += 1;
            }
        }
    }

    count
}

fn is_island(grid: &mut Vec<char>, x: usize, y: usize) -> bool {
    if get_from_grid(grid, x, y) == '1' {
        set_in_grid(grid, x, y, 'x');
        if x < 127 {
            is_island(grid, x + 1, y);
        }
        if x > 0 {
            is_island(grid, x - 1, y);
        }
        if y < 127 {
            is_island(grid, x, y + 1);
        }
        if y > 0 {
            is_island(grid, x, y - 1);
        }
        true
    } else {
        false
    }
}

fn get_from_grid(grid: &Vec<char>, x: usize, y: usize) -> char {
    grid[y*128 + x]
}

fn set_in_grid(grid: &mut Vec<char>, x: usize, y: usize, v: char) {
    grid[y*128 + x] = v;
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
        // this is bloody nice!
        hex.push_str(format!("{:0width$b}", xord, width = 8).as_str());
    }

    hex
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
