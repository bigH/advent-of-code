use std::collections::HashMap;
use std::collections::VecDeque;

#[derive(Debug)]
enum Source {
    Register(char),
    Value(i64),
}

enum Instruction {
    SendData(Source),
    Set(char, Source),
    Add(char, Source),
    Multiply(char, Source),
    Modulo(char, Source),
    ReceiveData(char),
    Jump(Source, Source),
}

// const INSTRUCTIONS: &[Instruction] = &[
//     Instruction::SendData(Source::Value(1)),
//     Instruction::SendData(Source::Value(2)),
//     Instruction::SendData(Source::Register('p')),
//     Instruction::ReceiveData('a'),
//     Instruction::ReceiveData('b'),
//     Instruction::ReceiveData('c'),
//     Instruction::ReceiveData('d'),
// ];

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
    Instruction::SendData(Source::Register('b')),
    Instruction::Add('i', Source::Value(-1)),
    Instruction::Jump(Source::Register('i'), Source::Value(-9)),
    Instruction::Jump(Source::Register('a'), Source::Value(3)),
    Instruction::ReceiveData('b'),
    Instruction::Jump(Source::Register('b'), Source::Value(-1)),
    Instruction::Set('f', Source::Value(0)),
    Instruction::Set('i', Source::Value(126)),
    Instruction::ReceiveData('a'),
    Instruction::ReceiveData('b'),
    Instruction::Set('p', Source::Register('a')),
    Instruction::Multiply('p', Source::Value(-1)),
    Instruction::Add('p', Source::Register('b')),
    Instruction::Jump(Source::Register('p'), Source::Value(4)),
    Instruction::SendData(Source::Register('a')),
    Instruction::Set('a', Source::Register('b')),
    Instruction::Jump(Source::Value(1), Source::Value(3)),
    Instruction::SendData(Source::Register('b')),
    Instruction::Set('f', Source::Value(1)),
    Instruction::Add('i', Source::Value(-1)),
    Instruction::Jump(Source::Register('i'), Source::Value(-11)),
    Instruction::SendData(Source::Register('a')),
    Instruction::Jump(Source::Register('f'), Source::Value(-16)),
    Instruction::Jump(Source::Register('a'), Source::Value(-19)),
];

struct Program {
    id: i64,
    registers: HashMap<char, i64>,
    in_queue: VecDeque<i64>,
    out_queue: VecDeque<i64>,
    counter: i64,
    send_count: i64,
}

impl Program {
    fn new(id: i64) -> Program {
        let mut registers: HashMap<char, i64> = HashMap::new();

        for c in b'a'..=b'z' {
            registers.insert(c as char, 0);
        }
        registers.insert('p', id);

        Program {
            id,
            registers,
            in_queue: VecDeque::new(),
            out_queue: VecDeque::new(),
            counter: 0,
            send_count: 0,
        }
    }

    fn proceed(&mut self) -> usize {
        let mut instructions_processed = 0;

        if self.counter >= 0 && self.counter < (INSTRUCTIONS.len() as i64) {
            loop {
                let instruction = &INSTRUCTIONS[self.counter as usize];
                match instruction {
                    Instruction::SendData(s) => {
                        self.out_queue.push_back(resolve(&s, &self.registers));
                        self.send_count += 1;
                        instructions_processed += 1;
                        self.counter += 1;
                    },
                    Instruction::Set(r, s) => {
                        self.registers.insert(*r, resolve(&s, &self.registers));
                        instructions_processed += 1;
                        self.counter += 1;
                    },
                    Instruction::Add(r, s) => {
                        self.registers.insert(*r, self.registers[&r] + resolve(&s, &self.registers));
                        instructions_processed += 1;
                        self.counter += 1;
                    },
                    Instruction::Multiply(r, s) => {
                        self.registers.insert(*r, self.registers[&r] * resolve(&s, &self.registers));
                        instructions_processed += 1;
                        self.counter += 1;
                    },
                    Instruction::Modulo(r, s) => {
                        self.registers.insert(*r, self.registers[&r] % resolve(&s, &self.registers));
                        instructions_processed += 1;
                        self.counter += 1;
                    },
                    Instruction::ReceiveData(r) => {
                        if !self.in_queue.is_empty() {
                            let value = self.in_queue.pop_front().expect("checked is_empty.");
                            self.registers.insert(*r, value);
                            instructions_processed += 1;
                            self.counter += 1;
                        } else {
                            break;
                        }
                    },
                    Instruction::Jump(s1, s2) => {
                        instructions_processed += 1;
                        if resolve(&s1, &self.registers) > 0 {
                            self.counter += resolve(&s2, &self.registers);
                        } else {
                            self.counter += 1;
                        }
                    },
                }
            }
        } else {
            println!("program {} counter out of range", self.id);
        }

        instructions_processed
    }
}

fn main() {
    let mut program0 = Program::new(0);
    let mut program1 = Program::new(1);

    loop {
        let mut instructions_processed = 0;

        instructions_processed += program0.proceed();
        instructions_processed += program1.proceed();

        while !program0.out_queue.is_empty() {
            let queued = program0.out_queue.pop_front().expect("checked is_empty.");
            program1.in_queue.push_back(queued);
        }

        while !program1.out_queue.is_empty() {
            let queued = program1.out_queue.pop_front().expect("checked is_empty.");
            program0.in_queue.push_back(queued);
        }

        if instructions_processed == 0 {
            println!("send_count = {}", program1.send_count);
            break;
        }
    }
}

fn resolve(s: &Source, registers: &HashMap<char, i64>) -> i64 {
    match s {
        Source::Value(v) => *v,
        Source::Register(r) => registers[&r],
    }
}

