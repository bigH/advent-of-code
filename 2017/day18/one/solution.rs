use std::collections::HashMap;

#[derive(Debug)]
enum Source {
    Register(char),
    Value(i64),
}

enum Instruction {
    Sound(char),
    Set(char, Source),
    Add(char, Source),
    Multiply(char, Source),
    Modulo(char, Source),
    Recover(char),
    Jump(Source, Source),
}

const INSTRUCTIONS: &[Instruction] = &[
    Instruction::Set('i', Source::Value(31)),
    Instruction::Set('a', Source::Value(1)),
    Instruction::Multiply('p', Source::Value(17)),
    Instruction::Jump(Source::Register('p'), Source::Register('p')),
    Instruction::Multiply('a', Source::Value(2)),
    Instruction::Add('i', Source::Value(-1)),
    Instruction::Jump(Source::Register('i'), Source::Value(-2)),
    Instruction::Add('a', Source::Value(-1)),
    Instruction::Set('i', Source::Value(127)),
    Instruction::Set('p', Source::Value(464)),
    Instruction::Multiply('p', Source::Value(8505)),
    Instruction::Modulo('p', Source::Register('a')),
    Instruction::Multiply('p', Source::Value(129749)),
    Instruction::Add('p', Source::Value(12345)),
    Instruction::Modulo('p', Source::Register('a')),
    Instruction::Set('b', Source::Register('p')),
    Instruction::Modulo('b', Source::Value(10000)),
    Instruction::Sound('b'),
    Instruction::Add('i', Source::Value(-1)),
    Instruction::Jump(Source::Register('i'), Source::Value(-9)),
    Instruction::Jump(Source::Register('a'), Source::Value(3)),
    Instruction::Recover('b'),
    Instruction::Jump(Source::Register('b'), Source::Value(-1)),
    Instruction::Set('f', Source::Value(0)),
    Instruction::Set('i', Source::Value(126)),
    Instruction::Recover('a'),
    Instruction::Recover('b'),
    Instruction::Set('p', Source::Register('a')),
    Instruction::Multiply('p', Source::Value(-1)),
    Instruction::Add('p', Source::Register('b')),
    Instruction::Jump(Source::Register('p'), Source::Value(4)),
    Instruction::Sound('a'),
    Instruction::Set('a', Source::Register('b')),
    Instruction::Jump(Source::Value(1), Source::Value(3)),
    Instruction::Sound('b'),
    Instruction::Set('f', Source::Value(1)),
    Instruction::Add('i', Source::Value(-1)),
    Instruction::Jump(Source::Register('i'), Source::Value(-11)),
    Instruction::Sound('a'),
    Instruction::Jump(Source::Register('f'), Source::Value(-16)),
    Instruction::Jump(Source::Register('a'), Source::Value(-19)),
];

fn main() {
    let mut last_sound: Option<i64> = None;
    let mut registers: HashMap<char, i64> = HashMap::new();
    let mut counter: i64 = 0;

    for c in b'a'..=b'z' {
        registers.insert(c as char, 0);
    }

    loop {
        let instruction = &INSTRUCTIONS[counter as usize];
        match instruction {
            Instruction::Sound(r) => {
                last_sound = Some(registers[&r]);
                counter += 1;
            },
            Instruction::Set(r, s) => {
                registers.insert(*r, resolve(&s, &registers));
                counter += 1;
            },
            Instruction::Add(r, s) => {
                registers.insert(*r, registers[&r] + resolve(&s, &registers));
                counter += 1;
            },
            Instruction::Multiply(r, s) => {
                registers.insert(*r, registers[&r] * resolve(&s, &registers));
                counter += 1;
            },
            Instruction::Modulo(r, s) => {
                registers.insert(*r, registers[&r] % resolve(&s, &registers));
                counter += 1;
            },
            Instruction::Recover(r) => {
                if registers[&r] != 0 {
                    match last_sound {
                        Some(s) => {
                            println!("last sound played = {}", s);
                            break;
                        },
                        None => {
                            println!("no last sound played");
                        },
                    }
                }
                counter += 1;
            },
            Instruction::Jump(s1, s2) => {
                if resolve(&s1, &registers) > 0 {
                    counter += resolve(&s2, &registers);
                } else {
                    counter += 1;
                }
            },
        }
    }
}

fn resolve(s: &Source, registers: &HashMap<char, i64>) -> i64 {
    match s {
        Source::Value(v) => *v,
        Source::Register(r) => registers[&r],
    }
}

