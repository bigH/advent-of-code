fn main() {
    let generator_a_multiplier: u64 = 16807;
    let generator_b_multiplier: u64 = 48271;

    let divisor: u64 = 2147483647;

    let mut count: u64 = 0;

    let mask: u64 = std::u16::MAX as u64;

    let mut current_a: u64 = 289;
    let mut current_b: u64 = 629;

    for _i in 0..5_000_000 {
        current_a = (current_a * generator_a_multiplier) % divisor;
        while current_a % 4 != 0 {
            current_a = (current_a * generator_a_multiplier) % divisor;
        }

        current_b = (current_b * generator_b_multiplier) % divisor;
        while current_b % 8 != 0 {
            current_b = (current_b * generator_b_multiplier) % divisor;
        }

        if (current_a & mask) == (current_b & mask) {
            count += 1;
        }
    }

    println!("count = {}", count);
}
