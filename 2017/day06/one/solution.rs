use std::env;
use std::fs;

fn main() {
    let args: Vec<String> = env::args().collect();
    let filename = &args[1];

    let text = fs::read_to_string(filename)
        .expect("Something went wrong reading the file");

    let tokens = text.trim().split("\t");
    let mut current_state: Vec<usize> = tokens
        .map(|t|
            t.trim().parse::<usize>()
                .expect("Couldn't parse")
        )
        .collect();

    let length = current_state.len();

    let mut seen_states: Vec<Vec<usize>> = vec!(current_state.clone());

    loop {
        let mut cells_to_distribute: usize = *current_state.iter().max()
            .expect("Couldn't calculate `max`");

        let mut curr_index: usize = current_state.iter()
            .position(|&s| s == cells_to_distribute)
            .expect("Couldn't calculate `max`");

        current_state[curr_index] = 0;

        while cells_to_distribute > 0 {
            curr_index = (curr_index + 1) % length;
            current_state[curr_index] += 1;
            cells_to_distribute -= 1;
        }

        let seen_new_state_before = seen_states.iter().any(|state|
            current_state.iter().zip((*state).iter()).all(|(l, r)|
                l == r
            )
        );

        if seen_new_state_before {
            break;
        }

        seen_states.push(current_state.clone());
    }

    println!("{}", seen_states.len());
}
