// TODO
// - implement more instructions
// - simplify `assemble_instruction`
// - add pseudo-instructions (e.g. li)
// - implement default instruction args (zeros?)
// - handle error situations
// - make panic-free

use std::collections::HashMap;

#[derive(Clone, Debug, PartialEq)]
pub enum Instr {
    // R-TYPE (rd, rs1, rs2)
    Add(u8, u8, u8),
    Sub(u8, u8, u8),
    Xor(u8, u8, u8),
    Or(u8, u8, u8),
    And(u8, u8, u8),
    // I-TYPE (rd, rs1, imm)
    Addi(u8, u8, i16),
    Jalr(u8, u8, i16),
    Lb(u8, u8, i16),
    Lh(u8, u8, i16),
    Lw(u8, u8, i16),
    // S-TYPE (rs1, rs2, imm)
    Sb(u8, u8, i16),
    Sh(u8, u8, i16),
    Sw(u8, u8, i16),
    // B-TYPE (rs1, rs2, imm)
    Beq(u8, u8, i16),
    Bne(u8, u8, i16),
    Bge(u8, u8, i16),
    Blt(u8, u8, i16),
    // J-TYPE (rd, imm)
    Jal(u8, i32),
}

fn get_bits_32(word: u32, start_index: u8, num_bits: u8) -> u32 {
    (!(0xFFFFFFFFu32 << num_bits)) & (word >> start_index)
}

fn sign_extend_16(half: u16, sign_index: u8) -> i16 {
    ((half & (0xFFFF >> (15 - sign_index))) ^ (1 << sign_index)).wrapping_sub(1 << sign_index)
        as i16
}

fn sign_extend_32(word: u32, sign_index: u8) -> i32 {
    ((word & (0xFFFFFFFF >> (31 - sign_index))) ^ (1 << sign_index)).wrapping_sub(1 << sign_index)
        as i32
}

const R_TYPE: u32 = 0b0110011;
const I_TYPE: u32 = 0b0010011;
const I_TYPE_LOAD: u32 = 0b0000011;
const I_TYPE_JALR: u32 = 0b1100111;
const S_TYPE: u32 = 0b0100011;
const B_TYPE: u32 = 0b1100011;
const J_TYPE: u32 = 0b1101111;

fn decode_r_type(word: u32) -> Instr {
    let rd = get_bits_32(word, 7, 5) as u8;
    let funct3 = get_bits_32(word, 12, 3);
    let rs1 = get_bits_32(word, 15, 5) as u8;
    let rs2 = get_bits_32(word, 20, 5) as u8;
    let funct7 = get_bits_32(word, 25, 7);
    match (funct3, funct7) {
        (0b000, 0b0000000) => Instr::Add(rd, rs1, rs2),
        (0b000, 0b0100000) => Instr::Sub(rd, rs1, rs2),
        (0b100, 0b0000000) => Instr::Xor(rd, rs1, rs2),
        (0b110, 0b0000000) => Instr::Or(rd, rs1, rs2),
        (0b111, 0b0000000) => Instr::And(rd, rs1, rs2),
        _ => todo!(),
    }
}

fn decode_i_type(word: u32) -> Instr {
    let opcode = get_bits_32(word, 0, 7);
    let rd = get_bits_32(word, 7, 5) as u8;
    let funct3 = get_bits_32(word, 12, 3);
    let rs1 = get_bits_32(word, 15, 5) as u8;
    let imm = sign_extend_16(get_bits_32(word, 20, 12) as u16, 11);
    match (opcode, funct3) {
        (I_TYPE, 0b000) => Instr::Addi(rd, rs1, imm),
        (I_TYPE_JALR, 0b000) => Instr::Jalr(rd, rs1, imm),
        (I_TYPE_LOAD, 0b000) => Instr::Lb(rd, rs1, imm),
        (I_TYPE_LOAD, 0b001) => Instr::Lh(rd, rs1, imm),
        (I_TYPE_LOAD, 0b010) => Instr::Lw(rd, rs1, imm),
        _ => todo!("opcode = 0b{:07b}, funct3 = 0b{:03b}", opcode, funct3),
    }
}

fn decode_s_type(word: u32) -> Instr {
    let funct3 = get_bits_32(word, 12, 3);
    let rs1 = get_bits_32(word, 15, 5) as u8;
    let rs2 = get_bits_32(word, 20, 5) as u8;
    let imm = sign_extend_16(
        (get_bits_32(word, 7, 5) | (get_bits_32(word, 25, 7) << 5)) as u16,
        11,
    );
    match funct3 {
        0b000 => Instr::Sb(rs1, rs2, imm as i16),
        0b001 => Instr::Sh(rs1, rs2, imm as i16),
        0b010 => Instr::Sw(rs1, rs2, imm as i16),
        _ => todo!("funct3 = 0b{:3b}", funct3),
    }
}

fn decode_b_type_imm(word: u32) -> i16 {
    let bits_4_1_11 = get_bits_32(word, 7, 5);
    let bits_12_10_5 = get_bits_32(word, 25, 7);
    let pre = ((bits_4_1_11 >> 1) << 1)
        | (bits_12_10_5 << 5)
        | ((bits_4_1_11 & 1) << 11)
        | ((bits_12_10_5 & 128) << 12);
    sign_extend_16(pre as u16, 11)
}

fn decode_b_type(word: u32) -> Instr {
    let funct3 = get_bits_32(word, 12, 3);
    let rs1 = get_bits_32(word, 15, 5) as u8;
    let rs2 = get_bits_32(word, 20, 5) as u8;
    let imm = decode_b_type_imm(word);
    match funct3 {
        0b000 => Instr::Beq(rs1, rs2, imm),
        0b001 => Instr::Bne(rs1, rs2, imm),
        0b101 => Instr::Bge(rs1, rs2, imm),
        0b100 => Instr::Blt(rs1, rs2, imm),
        _ => todo!("funct3 = 0b{:b}", funct3),
    }
}

fn decode_j_type_imm(word: u32) -> i32 {
    let bits = get_bits_32(word, 12, 20);
    let pre = ((0xFF & bits) << 12)
        | (((1 << 8) & bits) << 3)
        | (((0b11_1111_1111 << 9) & bits) >> 8)
        | (((1 << 19) & bits) << 1);
    sign_extend_32(pre, 20)
}

fn decode_j_type(word: u32) -> Instr {
    let rd = get_bits_32(word, 7, 5) as u8;
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

fn parse_register_index(name: &str) -> Result<u8, String> {
    if let Some(stripped) = name.strip_prefix("x") {
        if let Ok(idx) = stripped.parse::<u8>() {
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
        Ok(idx as u8)
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
    } else if opcode.to_lowercase() == "sub" {
        Ok(Instr::Sub(
            parse_register_index(split_args[0])?,
            parse_register_index(split_args[1])?,
            parse_register_index(split_args[2])?,
        ))
    } else if opcode.to_lowercase() == "xor" {
        Ok(Instr::Xor(
            parse_register_index(split_args[0])?,
            parse_register_index(split_args[1])?,
            parse_register_index(split_args[2])?,
        ))
    } else if opcode.to_lowercase() == "or" {
        Ok(Instr::Or(
            parse_register_index(split_args[0])?,
            parse_register_index(split_args[1])?,
            parse_register_index(split_args[2])?,
        ))
    } else if opcode.to_lowercase() == "and" {
        Ok(Instr::And(
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
                .parse::<i16>()
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
    } else if opcode.to_lowercase() == "bne" {
        let dest = *labels.get(split_args[2]).ok_or("Label not found")?;
        let offset = dest.wrapping_sub(ip) as i16;
        Ok(Instr::Bne(
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

    #[inline(always)]
    fn load_byte(&self, address: u32) -> u32 {
        self.memory[address as usize] as i8 as i32 as u32
    }

    #[inline(always)]
    fn load_half(&self, address: u32) -> u32 {
        let addr = address as usize;
        let value = u16::from_le_bytes([self.memory[addr], self.memory[addr + 1]]);
        value as i16 as i32 as u32
    }

    #[inline(always)]
    fn load_word(&self, address: u32) -> u32 {
        let addr = address as usize;
        let bytes = &self.memory[addr..addr + 4];
        u32::from_le_bytes([bytes[0], bytes[1], bytes[2], bytes[3]])
    }

    #[inline(always)]
    fn store_byte(&mut self, address: u32, value: u32) {
        self.memory[address as usize] = value as u8;
    }

    #[inline(always)]
    fn store_half(&mut self, address: u32, value: u32) {
        let addr = address as usize;
        let bytes = (value as u16).to_le_bytes();
        self.memory[addr..addr + 2].copy_from_slice(&bytes);
    }

    #[inline(always)]
    fn store_word(&mut self, address: u32, value: u32) {
        let addr = address as usize;
        let bytes = value.to_le_bytes();
        self.memory[addr..addr + 4].copy_from_slice(&bytes);
    }

    #[inline(always)]
    fn step(&mut self) {
        if self.ip % 4 != 0 {
            panic!("instruction pointer is not word-aligned")
        }

        let mut next_ip = self.ip.wrapping_add(4);

        match self.code[(self.ip / 4) as usize] {
            Instr::Addi(rd, rs1, val) => {
                self.regs[rd as usize] = self.regs[rs1 as usize].wrapping_add(val as u32);
            }
            Instr::Add(rd, rs1, rs2) => {
                self.regs[rd as usize] =
                    self.regs[rs1 as usize].wrapping_add(self.regs[rs2 as usize]);
            }
            Instr::Sub(rd, rs1, rs2) => {
                self.regs[rd as usize] =
                    self.regs[rs1 as usize].wrapping_sub(self.regs[rs2 as usize]);
            }
            Instr::Lw(rd, rs1, offset) => {
                let address = self.regs[rs1 as usize].wrapping_add(offset as u32);
                self.regs[rd as usize] = self.load_word(address);
            }
            Instr::Sw(rs1, rs2, offset) => {
                let address = self.regs[rs2 as usize].wrapping_add(offset as u32);
                self.store_word(address, self.regs[rs1 as usize]);
            }
            Instr::Beq(rs1, rs2, offset) => {
                if self.regs[rs1 as usize] == self.regs[rs2 as usize] {
                    next_ip = self.ip.wrapping_add(offset as u32);
                }
            }
            Instr::Bne(rs1, rs2, offset) => {
                if self.regs[rs1 as usize] != self.regs[rs2 as usize] {
                    next_ip = self.ip.wrapping_add(offset as u32);
                }
            }
            Instr::Bge(rs1, rs2, offset) => {
                if self.regs[rs1 as usize] >= self.regs[rs2 as usize] {
                    next_ip = self.ip.wrapping_add(offset as u32);
                }
            }
            Instr::Blt(rs1, rs2, offset) => {
                if self.regs[rs1 as usize] < self.regs[rs2 as usize] {
                    next_ip = self.ip.wrapping_add(offset as u32);
                }
            }
            Instr::Jal(rd, offset) => {
                self.regs[rd as usize] = next_ip;
                next_ip = self.ip.wrapping_add(offset as u32);
            }
            Instr::Jalr(rd, rs1, offset) => {
                self.regs[rd as usize] = next_ip;
                next_ip = self.regs[rs1 as usize].wrapping_add(offset as u32);
            }
            Instr::Xor(rd, rs1, rs2) => {
                self.regs[rd as usize] = self.regs[rs1 as usize] ^ self.regs[rs2 as usize];
            }
            Instr::Or(rd, rs1, rs2) => {
                self.regs[rd as usize] = self.regs[rs1 as usize] | self.regs[rs2 as usize];
            }
            Instr::And(rd, rs1, rs2) => {
                self.regs[rd as usize] = self.regs[rs1 as usize] & self.regs[rs2 as usize];
            }
            Instr::Lh(rd, rs1, offset) => {
                let address = self.regs[rs1 as usize].wrapping_add(offset as u32);
                self.regs[rd as usize] = self.load_half(address);
            }
            Instr::Sh(rs1, rs2, offset) => {
                let address = self.regs[rs2 as usize].wrapping_add(offset as u32);
                self.store_half(address, self.regs[rs1 as usize]);
            }
            Instr::Lb(rd, rs1, offset) => {
                let address = self.regs[rs1 as usize].wrapping_add(offset as u32);
                self.regs[rd as usize] = self.load_byte(address);
            }
            Instr::Sb(rs1, rs2, offset) => {
                let address = self.regs[rs2 as usize].wrapping_add(offset as u32);
                self.store_byte(address, self.regs[rs1 as usize]);
            }
        }

        self.ip = next_ip;
        self.regs[0] = 0; // hard-wire the zero register
    }

    pub fn run(&mut self) {
        let code_length = 4 * (self.code.len() as u32);
        loop {
            if self.ip >= code_length {
                break;
            }
            self.step()
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
        let cases = vec![
            (0b0000_0100_1010_1111, 0, 0b1111_1111_1111_1111u16 as i16),
            (0b0000_0100_1010_1110, 0, 0b0000_0000_0000_0000),
            (0b0000_0100_1010_1110, 1, 0b1111_1111_1111_1110u16 as i16),
            (0b0000_0100_1010_1100, 10, 0b1111_1100_1010_1100u16 as i16),
            (0b0000_0100_1010_1100, 11, 0b0000_0100_1010_1100),
            (0b0000_0100_1010_1100, 7, 0b1111_1111_1010_1100u16 as i16),
            (0b0000_0100_1010_1100, 6, 0b0000_0000_0010_1100),
        ];

        for (unsigned, bit_idx, expected) in cases {
            assert_eq!(sign_extend_16(unsigned, bit_idx), expected)
        }
    }

    #[test]
    fn test_decode_b_type_imm() {
        assert_eq!(
            decode_b_type_imm(0b0000_0000_0111_0101_0101_0110_0110_0011),
            12
        )
    }

    #[test]
    fn test_decode() {
        let cases = vec![
            (
                0b0000_0001_0000_0000_0000_0000_0110_0111,
                Instr::Jalr(0, 0, 16),
            ),
            (
                0b1111_1100_1100_0000_0000_0000_0110_0111,
                Instr::Jalr(0, 0, -52),
            ),
            (
                0b0000_0100_0101_0000_0000_0000_0001_0011,
                Instr::Addi(0, 0, 69),
            ),
            (
                0b1111_0111_0001_0000_0000_0000_0001_0011,
                Instr::Addi(0, 0, -143),
            ),
            (
                0b0000_0011_1100_0000_0010_0000_0000_0011,
                Instr::Lw(0, 0, 60),
            ),
            (
                0b1111_1011_1000_0000_0010_0000_0000_0011,
                Instr::Lw(0, 0, -72),
            ),
            (
                0b0000_0100_0000_0000_0010_0000_0010_0011,
                Instr::Sw(0, 0, 64),
            ),
            (
                0b1111_1010_0000_0000_0010_1110_0010_0011,
                Instr::Sw(0, 0, -68),
            ),
            (
                0b0000_1110_0000_0000_0100_1000_0110_0011,
                Instr::Blt(0, 0, 240),
            ),
            (
                0b1111_0000_0000_0000_0100_0000_1110_0011,
                Instr::Blt(0, 0, -256),
            ),
            (0b0000_0011_0000_0000_0000_0000_0110_1111, Instr::Jal(0, 48)),
            (
                0b1111_1110_1001_1111_1111_0000_0110_1111,
                Instr::Jal(0, -24),
            ),
        ];

        for (enc, dec) in cases.iter() {
            assert_eq!(&Instr::from(*enc), dec)
        }
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
