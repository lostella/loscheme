// TODO
// - implement more instructions
// - add pseudo-instructions (e.g. li)
// - handle error situations
// - make panic-free

use std::collections::HashMap;

#[derive(Clone, Debug, PartialEq)]
pub enum Instr {
    // R-TYPE (rd, rs1, rs2)
    Add(usize, usize, usize),
    // I-TYPE (rd, rs1, imm)
    Addi(usize, usize, i16),
    Jalr(usize, usize, usize),
    Lb(usize, usize, i16),
    Lh(usize, usize, i16),
    Lw(usize, usize, i16),
    // S-TYPE (rs1, rs2, imm)
    Sb(usize, usize, i16),
    Sh(usize, usize, i16),
    Sw(usize, usize, i16),
    // B-TYPE (rs1, rs2, imm)
    Beq(usize, usize, i16),
    Bge(usize, usize, i16),
    Blt(usize, usize, i16),
    // J-TYPE (rd, imm)
    Jal(usize, i32),
}

fn get_bits_32(word: u32, start_index: u8, num_bits: u8) -> u32 {
    (!(0xFFFFFFFFu32 << num_bits)) & (word >> start_index)
}

fn sign_extend_16(half: u16, sign_index: u8) -> u16 {
    if half & (1 << sign_index) > 0 {
        (0xFFFF << sign_index) | half
    } else {
        (0xFFFF >> (16 - sign_index)) & half
    }
}

const R_TYPE: u32 = 0b0110011;
const I_TYPE: u32 = 0b0010011;
const I_TYPE_LOAD: u32 = 0b0000011;
const I_TYPE_JALR: u32 = 0b1100111;
const S_TYPE: u32 = 0b0100011;
const B_TYPE: u32 = 0b1100011;
const J_TYPE: u32 = 0b1101111;

fn decode_r_type(word: u32) -> Instr {
    let rd = get_bits_32(word, 7, 5) as usize;
    let funct3 = get_bits_32(word, 12, 3);
    let rs1 = get_bits_32(word, 15, 5) as usize;
    let rs2 = get_bits_32(word, 20, 5) as usize;
    let funct7 = get_bits_32(word, 25, 7);
    match (funct3, funct7) {
        (0b000, 0b0000000) => Instr::Add(rd, rs1, rs2),
        _ => todo!(),
    }
}

fn decode_i_type(word: u32) -> Instr {
    let opcode = get_bits_32(word, 0, 7);
    let rd = get_bits_32(word, 7, 5) as usize;
    let funct3 = get_bits_32(word, 12, 3);
    let rs1 = get_bits_32(word, 15, 5) as usize;
    let imm = get_bits_32(word, 20, 12);
    match (opcode, funct3) {
        (I_TYPE, 0x000) => Instr::Addi(rd, rs1, imm as i16),
        (I_TYPE_JALR, 0x000) => Instr::Jalr(rd, rs1, imm as usize),
        (I_TYPE_LOAD, 0x000) => Instr::Lb(rd, rs1, imm as i16),
        (I_TYPE_LOAD, 0x001) => Instr::Lh(rd, rs1, imm as i16),
        (I_TYPE_LOAD, 0x010) => Instr::Lw(rd, rs1, imm as i16),
        _ => todo!(),
    }
}

fn decode_s_type(word: u32) -> Instr {
    let funct3 = get_bits_32(word, 12, 3);
    let rs1 = get_bits_32(word, 15, 5) as usize;
    let rs2 = get_bits_32(word, 20, 5) as usize;
    let imm = get_bits_32(word, 7, 5) | (get_bits_32(word, 25, 6) << 5);
    match funct3 {
        0b000 => Instr::Sb(rs1, rs2, imm as i16),
        0b001 => Instr::Sh(rs1, rs2, imm as i16),
        0b010 => Instr::Sw(rs1, rs2, imm as i16),
        _ => todo!("funct3 = 0b{:b}", funct3),
    }
}

fn decode_b_type_imm(word: u32) -> i16 {
    let imm_4_1_11 = get_bits_32(word, 7, 5);
    let imm_12_10_5 = get_bits_32(word, 25, 7);
    let pre = ((imm_4_1_11 >> 1) << 1)
        | (imm_12_10_5 << 5)
        | ((imm_4_1_11 & 1) << 11)
        | ((imm_12_10_5 & 128) << 12);
    sign_extend_16(pre as u16, 11) as i16
}

fn decode_b_type(word: u32) -> Instr {
    let funct3 = get_bits_32(word, 12, 3);
    let rs1 = get_bits_32(word, 15, 5) as usize;
    let rs2 = get_bits_32(word, 20, 5) as usize;
    let imm = decode_b_type_imm(word);
    match funct3 {
        0b000 => Instr::Beq(rs1, rs2, imm),
        0b101 => Instr::Bge(rs1, rs2, imm),
        0b100 => Instr::Blt(rs1, rs2, imm),
        _ => todo!("funct3 = 0b{:b}", funct3),
    }
}

fn decode_j_type_imm(word: u32) -> i32 {
    let imm = get_bits_32(word, 12, 20);
    (((0xFF & imm) << 12)
        | (((1 << 8) & imm) << 3)
        | (((0b11_1111_1111 << 9) & imm) >> 8)
        | (((1 << 19) & imm) << 1)) as i32
}

fn decode_j_type(word: u32) -> Instr {
    let rd = get_bits_32(word, 7, 5) as usize;
    Instr::Jal(rd, decode_j_type_imm(word))
}

impl From<u32> for Instr {
    fn from(word: u32) -> Self {
        let opcode = get_bits_32(word, 0, 7);
        match opcode {
            R_TYPE => decode_r_type(word),
            I_TYPE | I_TYPE_LOAD | I_TYPE_JALR => decode_i_type(word),
            S_TYPE => decode_s_type(word),
            B_TYPE => decode_b_type(word),
            J_TYPE => decode_j_type(word),
            _ => todo!(),
        }
    }
}

const REGISTER_ABI_NAMES: [&str; 32] = [
    "zero", "ra", "sp", "gp", "tp", "t0", "t1", "t2", "s0", "s1", "a0", "a1", "a2", "a3", "a4",
    "a5", "a6", "a7", "s2", "s3", "s4", "s5", "s6", "s7", "s8", "s9", "s10", "s11", "t3", "t4",
    "t5", "t6",
];

fn parse_register_index(name: &str) -> Result<usize, String> {
    if name.starts_with("x") {
        if let Ok(idx) = name[1..].parse::<usize>() {
            if idx >= 32 {
                return Err(format!("Invalid register index: {}", name));
            }
            return Ok(idx);
        }
        return Err(format!("Cannot parse register index: {}", name));
    };
    if name == "fp" {
        return Ok(8);
    }
    if let Some(idx) = REGISTER_ABI_NAMES
        .iter()
        .position(|&abi_name| abi_name == name)
    {
        Ok(idx)
    } else {
        Err(format!("Uknown register: {}", name))
    }
}

fn assemble_instruction(
    ip: u32,
    instr: &str,
    labels: &HashMap<String, u32>,
) -> Result<Instr, String> {
    let Some((opcode, args)) = instr.split_once(char::is_whitespace) else {
        return Err(format!("No arguments in instruction: {}", instr));
    };
    let split_args = args.split(",").map(|s| s.trim()).collect::<Vec<&str>>();
    if opcode.to_lowercase() == "add" {
        Ok(Instr::Add(
            parse_register_index(split_args[0])?,
            parse_register_index(split_args[1])?,
            parse_register_index(split_args[2])?,
        ))
    } else if opcode.to_lowercase() == "addi" {
        Ok(Instr::Addi(
            parse_register_index(split_args[0])?,
            parse_register_index(split_args[1])?,
            split_args[2]
                .parse::<i16>()
                .ok()
                .ok_or("Cannot parse immediate")?,
        ))
    } else if opcode.to_lowercase() == "jalr" {
        Ok(Instr::Jalr(
            parse_register_index(split_args[0])?,
            parse_register_index(split_args[1])?,
            split_args[2]
                .parse::<usize>()
                .ok()
                .ok_or("Cannot parse immediate")?,
        ))
    } else if opcode.to_lowercase() == "lw" {
        let memloc: Vec<&str> = split_args[1]
            .split(")")
            .next()
            .ok_or("Malformed memory location")?
            .split("(")
            .collect();

        Ok(Instr::Lw(
            parse_register_index(split_args[0])?,
            parse_register_index(memloc[1])?,
            memloc[0].parse::<i16>().ok().ok_or("Cannot parse offset")?,
        ))
    } else if opcode.to_lowercase() == "lh" {
        let memloc: Vec<&str> = split_args[1]
            .split(")")
            .next()
            .ok_or("Malformed memory location")?
            .split("(")
            .collect();

        Ok(Instr::Lh(
            parse_register_index(split_args[0])?,
            parse_register_index(memloc[1])?,
            memloc[0].parse::<i16>().ok().ok_or("Cannot parse offset")?,
        ))
    } else if opcode.to_lowercase() == "lb" {
        let memloc: Vec<&str> = split_args[1]
            .split(")")
            .next()
            .ok_or("Malformed memory location")?
            .split("(")
            .collect();

        Ok(Instr::Lb(
            parse_register_index(split_args[0])?,
            parse_register_index(memloc[1])?,
            memloc[0].parse::<i16>().ok().ok_or("Cannot parse offset")?,
        ))
    } else if opcode.to_lowercase() == "sw" {
        let memloc: Vec<&str> = split_args[1]
            .split(")")
            .next()
            .ok_or("Malformed memory location")?
            .split("(")
            .collect();

        Ok(Instr::Sw(
            parse_register_index(split_args[0])?,
            parse_register_index(memloc[1])?,
            memloc[0].parse::<i16>().ok().ok_or("Cannot parse offset")?,
        ))
    } else if opcode.to_lowercase() == "sh" {
        let memloc: Vec<&str> = split_args[1]
            .split(")")
            .next()
            .ok_or("Malformed memory location")?
            .split("(")
            .collect();

        Ok(Instr::Sh(
            parse_register_index(split_args[0])?,
            parse_register_index(memloc[1])?,
            memloc[0].parse::<i16>().ok().ok_or("Cannot parse offset")?,
        ))
    } else if opcode.to_lowercase() == "sb" {
        let memloc: Vec<&str> = split_args[1]
            .split(")")
            .next()
            .ok_or("Malformed memory location")?
            .split("(")
            .collect();

        Ok(Instr::Sb(
            parse_register_index(split_args[0])?,
            parse_register_index(memloc[1])?,
            memloc[0].parse::<i16>().ok().ok_or("Cannot parse offset")?,
        ))
    } else if opcode.to_lowercase() == "beq" {
        let dest = *labels.get(split_args[2]).ok_or("Label not found")?;
        let offset = dest.wrapping_sub(ip) as i16;
        Ok(Instr::Beq(
            parse_register_index(split_args[0])?,
            parse_register_index(split_args[1])?,
            offset,
        ))
    } else if opcode.to_lowercase() == "bge" {
        let dest = *labels.get(split_args[2]).ok_or("Label not found")?;
        let offset = dest.wrapping_sub(ip) as i16;
        Ok(Instr::Bge(
            parse_register_index(split_args[0])?,
            parse_register_index(split_args[1])?,
            offset,
        ))
    } else if opcode.to_lowercase() == "blt" {
        let dest = *labels.get(split_args[2]).ok_or("Label not found")?;
        let offset = dest.wrapping_sub(ip) as i16;
        Ok(Instr::Blt(
            parse_register_index(split_args[0])?,
            parse_register_index(split_args[1])?,
            offset,
        ))
    } else if opcode.to_lowercase() == "jal" {
        let dest = *labels.get(split_args[1]).ok_or("Label not found")?;
        let offset = dest.wrapping_sub(ip) as i32;
        Ok(Instr::Jal(parse_register_index(split_args[0])?, offset))
    } else {
        Err(format!("Unknown instruction: {}", instr))
    }
}

pub fn assemble(asm: &str) -> Result<Vec<Instr>, String> {
    let mut ip: u32 = 0;
    let mut labels = HashMap::<String, u32>::new();
    let mut instructions_code: Vec<&str> = vec![];
    let mut instructions = vec![];
    for line in asm.lines() {
        let blocks = line.split("#").collect::<Vec<&str>>();
        let Some(code) = blocks.first() else { continue };
        let trimmed = code.trim();
        if trimmed.is_empty() {
            continue;
        }
        if let Some((label, instr)) = trimmed.split_once(":") {
            if let Some(loc) = labels.get(label) {
                return Err(format!(
                    "Ambiguous label: {} (occurs at {} and {})",
                    label, ip, loc
                ));
            }
            labels.insert(label.to_string(), ip);
            if instr.is_empty() {
                continue;
            }
            instructions_code.push(instr.trim());
        };
        instructions_code.push(trimmed);
        ip += 4;
    }
    ip = 0;
    for instr in instructions_code {
        instructions.push(assemble_instruction(ip, instr, &labels)?);
        ip += 4;
    }
    Ok(instructions)
}

#[derive(Debug)]
pub struct VM {
    regs: [u32; 32],
    code: Vec<Instr>,
    ip: u32,
    memory: Vec<u8>,
}

impl VM {
    pub fn new(code: Vec<Instr>, memory_size: usize) -> Self {
        let mut regs = [0; 32];
        regs[1] = 4 * (code.len() as u32); // initialize return address
        regs[2] = memory_size as u32; // initialize stack pointer
        Self {
            regs,
            code,
            ip: 0,
            memory: vec![0; memory_size],
        }
    }

    pub fn write_register(&mut self, idx: usize, value: u32) {
        if idx > 0 {
            self.regs[idx] = value
        }
    }

    pub fn read_register(&self, idx: usize) -> u32 {
        if idx > 0 {
            self.regs[idx]
        } else {
            0
        }
    }

    pub fn load_byte(&self, address: u32) -> u32 {
        let value = self.memory[address as usize] as u32;
        // sign extension
        if value > 0x80 {
            0xFFFFFF00 | value
        } else {
            value
        }
    }

    pub fn load_half(&self, address: u32) -> u32 {
        // little-endian layout
        let value = self.memory[address as usize] as u32
            | ((self.memory[address as usize + 1] as u32) << 8);
        // sign extension
        if value > 0x8000 {
            0xFFFF0000 | value
        } else {
            value
        }
    }

    pub fn load_word(&self, address: u32) -> u32 {
        // little-endian layout
        self.memory[address as usize] as u32
            | ((self.memory[address as usize + 1] as u32) << 8)
            | ((self.memory[address as usize + 2] as u32) << 16)
            | ((self.memory[address as usize + 3] as u32) << 24)
    }

    pub fn store_byte(&mut self, address: u32, value: u32) {
        self.memory[address as usize] = (value & 0xFF) as u8;
    }

    pub fn store_half(&mut self, address: u32, value: u32) {
        // little-endian layout
        self.memory[address as usize] = (value & 0x00FF) as u8;
        self.memory[address as usize + 1] = ((value & 0xFF00) >> 8) as u8;
    }

    pub fn store_word(&mut self, address: u32, value: u32) {
        // little-endian layout
        self.memory[address as usize] = (value & 0x000000FF) as u8;
        self.memory[address as usize + 1] = ((value & 0x0000FF00) >> 8) as u8;
        self.memory[address as usize + 2] = ((value & 0x00FF0000) >> 16) as u8;
        self.memory[address as usize + 3] = ((value & 0xFF000000) >> 24) as u8;
    }

    fn step(&mut self) {
        if self.ip % 4 != 0 {
            panic!("instruction pointer is not word-aligned")
        }
        match self.code[(self.ip / 4) as usize] {
            Instr::Add(rd, rs1, rs2) => {
                self.regs[rd] = self.regs[rs1].wrapping_add(self.regs[rs2]);
                self.ip += 4;
            }
            Instr::Addi(rd, rs1, val) => {
                self.regs[rd] = self.regs[rs1].wrapping_add(val as u32);
                self.ip += 4;
            }
            Instr::Beq(rs1, rs2, offset) => {
                if self.regs[rs1] == self.regs[rs2] {
                    self.ip = self.ip.wrapping_add(offset as u32);
                } else {
                    self.ip += 4;
                }
            }
            Instr::Bge(rs1, rs2, offset) => {
                if self.regs[rs1] >= self.regs[rs2] {
                    self.ip = self.ip.wrapping_add(offset as u32);
                } else {
                    self.ip += 4;
                }
            }
            Instr::Blt(rs1, rs2, offset) => {
                if self.regs[rs1] < self.regs[rs2] {
                    self.ip = self.ip.wrapping_add(offset as u32);
                } else {
                    self.ip += 4;
                }
            }
            Instr::Jal(rd, offset) => {
                self.regs[rd] = self.ip + 4;
                self.ip = self.ip.wrapping_add(offset as u32);
            }
            Instr::Jalr(rd, rs1, offset) => {
                self.regs[rd] = self.ip + 4;
                self.ip = self.regs[rs1].wrapping_add(offset as u32);
            }
            Instr::Lb(rd, rs1, offset) => {
                let address = self.regs[rs1].wrapping_add(offset as u32);
                self.regs[rd] = self.load_byte(address);
                self.ip += 4;
            }
            Instr::Lh(rd, rs1, offset) => {
                let address = self.regs[rs1].wrapping_add(offset as u32);
                self.regs[rd] = self.load_half(address);
                self.ip += 4;
            }
            Instr::Lw(rd, rs1, offset) => {
                let address = self.regs[rs1].wrapping_add(offset as u32);
                self.regs[rd] = self.load_word(address);
                self.ip += 4;
            }
            Instr::Sb(rs1, rs2, offset) => {
                let address = self.regs[rs2].wrapping_add(offset as u32);
                self.store_byte(address, self.regs[rs1]);
                self.ip += 4;
            }
            Instr::Sh(rs1, rs2, offset) => {
                let address = self.regs[rs2].wrapping_add(offset as u32);
                self.store_half(address, self.regs[rs1]);
                self.ip += 4;
            }
            Instr::Sw(rs1, rs2, offset) => {
                let address = self.regs[rs2].wrapping_add(offset as u32);
                self.store_word(address, self.regs[rs1]);
                self.ip += 4;
            }
        }
    }

    pub fn run(&mut self) {
        loop {
            // NOTE: the following is to "hard-wire" the register
            // NOTE: arguably the register should just not be there
            self.regs[0] = 0;
            if self.ip >= (self.code.len() as u32) << 2 {
                break;
            }
            self.step()
        }
    }

    pub fn debug(&mut self, steps: u32) {
        for _ in 0..steps {
            // NOTE: the following is to "hard-wire" the register
            // NOTE: arguably the register should just not be there
            self.regs[0] = 0;
            println!("regs: {:?}", self.regs);
            if self.ip >= (self.code.len() as u32) << 2 {
                break;
            }
            println!(">>> {:?}", self.code[(self.ip / 4) as usize]);
            self.step();
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_get_bits() {
        assert_eq!(get_bits_32(0x00F23D00, 0, 9), 0x100);
        assert_eq!(get_bits_32(0x00F23D00, 11, 11), 0b11_0010_0011_1);
    }

    #[test]
    fn test_sign_extend() {
        assert_eq!(
            sign_extend_16(0b0000_0100_1010_1100, 10),
            0b1111_1100_1010_1100
        );
        assert_eq!(
            sign_extend_16(0b0000_0100_1010_1100, 11),
            0b0000_0100_1010_1100
        );
        assert_eq!(
            sign_extend_16(0b0000_0100_1010_1100, 7),
            0b1111_1111_1010_1100
        );
        assert_eq!(
            sign_extend_16(0b0000_0100_1010_1100, 6),
            0b0000_0000_0010_1100
        );
    }

    #[test]
    fn test_decode_b_type_imm() {
        assert_eq!(
            decode_b_type_imm(0b0000_0000_0111_0101_0101_0110_0110_0011),
            12
        )
    }

    #[test]
    fn test_write_to_zero() {
        let mut vm = VM::new(vec![Instr::Addi(0, 0, 1)], 0);
        vm.run();
        assert_eq!(vm.regs[0], 0)
    }

    #[test]
    fn test_store_word_load_word() {
        let mut vm = VM::new(vec![], 1024);
        vm.store_word(512, 0x12345678);
        assert_eq!(vm.load_word(512), 0x12345678)
    }

    #[test]
    fn test_store_half_load_half() {
        let mut vm = VM::new(vec![], 1024);
        vm.store_half(256, 0x7654);
        assert_eq!(vm.load_half(256), 0x7654);
        vm.store_half(256, 0x8654);
        assert_eq!(vm.load_half(256), 0xFFFF8654);
    }

    #[test]
    fn test_store_byte_load_byte() {
        let mut vm = VM::new(vec![], 1024);
        vm.store_byte(256, 0x7F);
        assert_eq!(vm.load_byte(256), 0x7F);
        vm.store_byte(256, 0x8F);
        assert_eq!(vm.load_byte(256), 0xFFFFFF8F);
    }

    #[test]
    fn test_store_byte_load_half() {
        let mut vm = VM::new(vec![], 1024);

        vm.store_byte(256, 0x12);
        vm.store_byte(257, 0x34);
        assert_eq!(vm.load_half(256), 0x3412);

        vm.store_byte(257, 0x84);
        assert_eq!(vm.load_half(256), 0xFFFF8412)
    }

    #[test]
    fn test_store_half_load_word() {
        let mut vm = VM::new(vec![], 1024);

        vm.store_half(256, 0x1234);
        vm.store_half(258, 0x5678);
        assert_eq!(vm.load_word(256), 0x56781234)
    }

    const FIB_ITER_ASM: &str = r#"
# Compute Fib(n) iteratively
# Input n is passed in x10, output will be stored in x10

    addi x5, x0, 0          # x5 = Fib(0) = 0
    addi x6, x0, 1          # x6 = Fib(1) = 1
    addi x7, x0, 1          # x7 = Loop counter i = 1
    bge x10, x7, fib_loop   # If n >= 1, enter loop
    add x10, x0, x5         # Return Fib(0) for n=0
    jalr x0, x1, 0

fib_loop:
    add x28, x5, x6         # x28 = Fib(i) = Fib(i-1) + Fib(i-2)
    add x5, x0, x6          # x5 = Fib(i-1)
    add x6, x0, x28         # x6 = Fib(i)
    addi x7, x7, 1          # i++
    blt x7, x10, fib_loop   # If i < n, continue loop
    add x10, x0, x28        # Store result in x10
"#;

    const FIB_ITER_OPS: [Instr; 12] = [
        Instr::Addi(5, 0, 0),
        Instr::Addi(6, 0, 1),
        Instr::Addi(7, 0, 1),
        Instr::Bge(10, 7, 12),
        Instr::Add(10, 0, 5),
        Instr::Jalr(0, 1, 0),
        Instr::Add(28, 5, 6),
        Instr::Add(5, 0, 6),
        Instr::Add(6, 0, 28),
        Instr::Addi(7, 7, 1),
        Instr::Blt(7, 10, -16),
        Instr::Add(10, 0, 28),
    ];

    const FIB_ITER_BIN: [u32; 12] = [
        0b0000_0000_0000_0000_0000_0010_1001_0011,
        0b0000_0000_0001_0000_0000_0011_0001_0011,
        0b0000_0000_0001_0000_0000_0011_1001_0011,
        0b0000_0000_0111_0101_0101_0110_0110_0011,
        0b0000_0000_0101_0000_0000_0101_0011_0011,
        0b0000_0000_0000_0000_1000_0000_0110_0111,
        0b0000_0000_0110_0010_1000_1110_0011_0011,
        0b0000_0000_0110_0000_0000_0010_1011_0011,
        0b0000_0001_1100_0000_0000_0011_0011_0011,
        0b0000_0000_0001_0011_1000_0011_1001_0011,
        0b1111_1110_1010_0011_1100_1000_1110_0011,
        0b0000_0001_1100_0000_0000_0101_0011_0011,
    ];

    #[test]
    fn test_fib_iter_assemble() {
        let ops = assemble(FIB_ITER_ASM).unwrap();
        assert_eq!(ops.len(), FIB_ITER_OPS.len());
        for (op, op_expected) in ops.iter().zip(FIB_ITER_OPS) {
            assert_eq!(*op, op_expected);
        }
    }

    #[test]
    fn test_fib_iter_decode() {
        for (word, op) in FIB_ITER_BIN.iter().zip(FIB_ITER_OPS) {
            assert_eq!(Instr::from(*word), op)
        }
    }

    #[test]
    fn test_fib_iter_run() {
        let mut vm = VM::new(FIB_ITER_OPS.to_vec(), 0);
        vm.write_register(10, 17);
        vm.run();
        assert_eq!(vm.read_register(10), 1597)
    }

    const FIB_RECUR_ASM: &str = r#"
# Compute Fib(n) recursively
# Input n is passed in a0, output will be stored in a0

begin:
    addi t0, zero, 1     # t0 = 1
    blt t0, a0, recurse  # if 1 < n, recurse
    jalr zero, ra, 0

recurse:
    addi sp, sp, -12     # Allocate 12 bytes
    sw ra, 8(sp)         # Save return address
    sw a0, 4(sp)         # Save n

    addi a0, a0, -1      # n = n - 1
    jal ra, begin        # Recursive call
    sw a0, 0(sp)         # Save fib(n-1)

    lw a0, 4(sp)         # Restore original n
    addi a0, a0, -2      # n = n - 2
    jal ra, begin        # Recursive call

    lw t0, 0(sp)         # Load fib(n-1)
    add a0, a0, t0       # fib(n) = fib(n-1) + fib(n-2)

    lw ra, 8(sp)         # Restore return address
    addi sp, sp, 12      # Deallocate stack space

    jalr zero, ra, 0     # Return
"#;

    const FIB_RECUR_OPS: [Instr; 17] = [
        Instr::Addi(5, 0, 1),
        Instr::Blt(5, 10, 8),
        Instr::Jalr(0, 1, 0),
        Instr::Addi(2, 2, -12),
        Instr::Sw(1, 2, 8),
        Instr::Sw(10, 2, 4),
        Instr::Addi(10, 10, -1),
        Instr::Jal(1, -28),
        Instr::Sw(10, 2, 0),
        Instr::Lw(10, 2, 4),
        Instr::Addi(10, 10, -2),
        Instr::Jal(1, -44),
        Instr::Lw(5, 2, 0),
        Instr::Add(10, 10, 5),
        Instr::Lw(1, 2, 8),
        Instr::Addi(2, 2, 12),
        Instr::Jalr(0, 1, 0),
    ];

    #[test]
    fn test_fib_recur_run() {
        let mut vm = VM::new(FIB_RECUR_OPS.to_vec(), 1024);
        vm.write_register(10, 13);
        vm.run();
        assert_eq!(vm.read_register(10), 233)
    }

    #[test]
    fn test_fib_recur_assemble() {
        let ops = assemble(FIB_RECUR_ASM).unwrap();
        assert_eq!(ops.len(), FIB_RECUR_OPS.len());
        for (op, op_expected) in ops.iter().zip(FIB_RECUR_OPS) {
            assert_eq!(*op, op_expected);
        }
    }
}
