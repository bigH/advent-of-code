use std::env;
use std::fs;

#[derive(Debug)]
enum Source {
    Register(char),
    Value(i32),
}

enum Instruction {
    Sound(Source),
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
    Instruction::Sound(Source::Register('b')),
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
    Instruction::Sound(Source::Register('a')),
    Instruction::Set('a', Source::Register('b')),
    Instruction::Jump(Source::Value(1), Source::Value(3)),
    Instruction::Sound(Source::Register('b')),
    Instruction::Set('f', Source::Value(1)),
    Instruction::Add('i', Source::Value(-1)),
    Instruction::Jump(Source::Register('i'), Source::Value(-11)),
    Instruction::Sound(Source::Register('a')),
    Instruction::Jump(Source::Register('f'), Source::Value(-16)),
    Instruction::Jump(Source::Register('a'), Source::Value(-19)),
];

fn main() {
    let registers: HashMap<char, i32> = HashMap::new();
    for instruction in INSTRUCTIONS {
        match instruction {

        }
    }
}

