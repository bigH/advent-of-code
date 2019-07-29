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

        let first_occurrence_position = seen_states.iter().position(|state|
            current_state.iter().zip((*state).iter()).all(|(l, r)|
                l == r
            )
        );

        match first_occurrence_position {
            Some(first_occurrence_position) => {
                println!("{}", seen_states.len() - first_occurrence_position);
                break;
            },
            None => {},
        }

        seen_states.push(current_state.clone());
    }
}
