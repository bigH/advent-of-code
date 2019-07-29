use std::env;
use std::fs;

fn main() {
    let args: Vec<String> = env::args().collect();
    let filename = &args[1];

    let text = fs::read_to_string(filename)
        .expect("Something went wrong reading the file");

    let lines = text.split("\n");

    let mut count = 0;

    let grid: Vec<Vec<char>> = lines.map(|line| line.chars().collect()).collect();

    let mut row: usize = 0;
    let mut col: usize = 0;

    while grid[row][col] == ' ' {
        col += 1;
    }

    let mut vrow: i32 = 1;
    let mut vcol: i32 = 0;

    loop {
        row = (row as i32 + vrow) as usize;
        col = (col as i32 + vcol) as usize;
        count += 1;

        if grid[row][col].is_ascii_whitespace() {
            break;
        }

        if grid[row][col] == '+' {
            match find_next_direction(&grid, row, col, vrow, vcol) {
                Some((r, c)) => {
                    vrow = r;
                    vcol = c;
                },
                None => {
                    break;
                }
            }
        }
    }

    println!("count = {}", count);
}

fn find_next_direction(grid: &Vec<Vec<char>>, row: usize, col: usize, vrow: i32, vcol: i32) -> Option<(i32, i32)> {
    if vrow != 0 && vcol != 1 && col < grid[row].len() - 1 && row < grid.len() {
        let potential_next = grid[row][col + 1];
        if potential_next != ' ' {
            return Some((0, 1));
        }
    }
    if vrow != 0 && vcol != -1 && col > 0 && row < grid.len() {
        let potential_next = grid[row][col - 1];
        if potential_next != ' ' {
            return Some((0, -1));
        }
    }
    if vrow != 1 && vcol != 0 && row < grid.len() - 1 && col < grid[row + 1].len() {
        let potential_next = grid[row + 1][col];
        if potential_next != ' ' {
            return Some((1, 0));
        }
    }
    if vrow != -1 && vcol != 0 && row > 0 && col < grid[row - 1].len() {
        let potential_next = grid[row - 1][col];
        if potential_next != ' ' {
            return Some((-1, 0));
        }
    }
    return None;
}
