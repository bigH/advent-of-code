extern crate regex;

use regex::Regex;
use std::env;
use std::fs;

fn main() {
    let args: Vec<String> = env::args().collect();
    let filename = &args[1];

    let text = fs::read_to_string(filename)
        .expect("Something went wrong reading the file");

    let lines = text.trim().split("\n");

    let re = Regex::new(r"([a-z]+) \((\d+)\)( -> ([a-z, ]+))?").unwrap();

    let programs: Vec<(String, usize, Vec<String>)> = lines.map(|line| {
        let cap = re.captures(line).unwrap();
        let potential_sub_programs = cap.get(4);
        let sub_programs = match potential_sub_programs {
            Some(contents) => {
                contents.as_str().split(", ").map(|s| String::from(s)).collect()
            },
            _ => {
                Vec::new()
            },
        };
        (String::from(cap.get(1).unwrap().as_str()),
         cap.get(2).unwrap().as_str().parse::<usize>().unwrap(),
         sub_programs)
    }).collect();

    let (first_program, _, _) = &programs[0];

    let mut program = first_program.to_string();

    loop {
        let next_program_data = programs.iter().find(|(_s, _w, riders)|
            riders.contains(&program)
        );

        match next_program_data {
            Some((subject, _w, _r)) => {
                program = subject.to_string();
            },
            _ => break,
        }
    }

    println!("{}", program);
}

/*
fn main() {
    let args: Vec<String> = env::args().collect();
    let filename = &args[1];

    let bottom_program = String::from("hmvwl");

    let text = fs::read_to_string(filename)
        .expect("Something went wrong reading the file");

    let lines = text.trim().split("\n");

    let re = Regex::new(r"([a-z]+) \((\d+)\)( -> ([a-z, ]+))?").unwrap();

    let mut programs: HashMap<String, (usize, Vec<String>)> = HashMap::new();

    for line in lines {
        let cap = re.captures(line).unwrap();
        let potential_sub_programs = cap.get(4);
        let sub_programs = match potential_sub_programs {
            Some(contents) => {
                contents.as_str().split(", ").map(|s| String::from(s)).collect()
            },
            _ => {
                Vec::new()
            },
        };
        programs.insert(
            String::from(cap.get(1).unwrap().as_str()),
            (cap.get(2).unwrap().as_str().parse::<usize>().unwrap(),
             sub_programs)
        );
    }

    println!("root = {}", bottom_program);

    // -- Used to find which program needs balancing --
    //
    // let (_weight, sub_programs) = programs.get(&bottom_program).unwrap();
    // let sub_program_weights: Vec<(String, usize)> =
    //     sub_programs.clone().iter().map(|p|
    //         (p.clone(), weigh(&p, &programs))
    //     ).collect();

    // for (sub_program, weight) in sub_program_weights {
    //     println!("{} is {}", sub_program, weight);
    // }

    let (program, desired_weight) = balance(&String::from("kzltfq"), 101772, &programs);

    println!("{} should be {}", program, desired_weight);
}

fn find_different_single(arr: Vec<(&String, usize)>) -> Option<(String, usize)> {
    if arr.is_empty() {
        panic!("array cannot be empty in `find_different_single`");
    }

    let (first_program, first_weight) = arr[0];
    let mut diffs: Vec<(&String, usize)> = Vec::new();
    for (program, weight) in arr {
        if weight != first_weight {
            diffs.push((program, weight));
        }
    }

    match diffs.len() {
        0 => None,
        1 => {
            let (p, w) = diffs[0];
            Some((p.clone(), w))
        },
        _ => {
            Some((first_program.clone(), first_weight))
        },
    }
}

fn balance<'a>(program: &'a String, target_weight: usize, programs: &'a HashMap<String, (usize, Vec<String>)>) -> (String, usize) {
    let mut next_program: &String = program;
    let mut next_target_weight = target_weight;
    loop {
        let (_weight, sub_programs) = programs.get(next_program).unwrap();
        let weights: Vec<usize> = sub_programs.iter().map(|p| weigh(&p, &programs)).collect();
        let sub_program_weights: Vec<(&String, usize)> = sub_programs.iter().clone().zip(weights).collect();

        match find_different_single(sub_program_weights) {
            Some((sub_program, weight)) => {
                let actual_weight = weigh(&next_program, &programs);
                next_target_weight = next_target_weight - actual_weight + weight;
                next_program = &sub_program;
            },
            None => {
                let total_sub_program_weight = sub_program_weights.iter().fold(0,
                    |acc, (_sub_program, weight)| acc + weight
                );
                break (next_program.clone(), target_weight - total_sub_program_weight)
            },
        }
    }
}

fn weigh(program: &String, programs: &HashMap<String, (usize, Vec<String>)>) -> usize {
    let (weight, sub_programs) = programs.get(program).unwrap();
    *weight + sub_programs.iter().map(|p| weigh(&p, &programs)).sum::<usize>()
}
*/
