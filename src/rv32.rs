// TODO
// - optimize instruction execution performance
// - make assembler case-insensitive
// - allow default args in assembly (zeros?)
// - implement more instructions
// - add pseudo-instructions (e.g. li)
// - handle error situations
// - make panic-free

use std::collections::HashMap;

fn set_bits(word: u32, start_index: u8, num_bits: u8) -> u32 {
    (!(0xFFFF_FFFF_u32 << num_bits) & word) << start_index
}

fn sign_extend(word: u32, sign_index: u8) -> u32 {
    ((word & (0xFFFF_FFFF >> (31 - sign_index))) ^ (1 << sign_index)).wrapping_sub(1 << sign_index)
}

fn decode_rd(word: u32) -> usize {
    ((word & 0xF80) >> 7) as usize
}

fn decode_rs1(word: u32) -> usize {
    ((word & 0xF8000) >> 15) as usize
}

fn decode_rs2(word: u32) -> usize {
    ((word & 0x01F0_0000) >> 20) as usize
}

fn decode_funct3(word: u32) -> u32 {
    (word & 0x7000) >> 12
}

fn decode_funct7(word: u32) -> u32 {
    (word & 0xFE00_0000) >> 25
}

fn decode_imm_i_type(word: u32) -> u32 {
    let pre = (word & 0xFFF0_0000) >> 20;
    sign_extend(pre, 11)
}

fn decode_imm_s_type(word: u32) -> u32 {
    let pre = ((word & 0xFE00_0000) >> 20) | ((word & 0xF80) >> 7);
    sign_extend(pre, 11)
}

fn decode_imm_b_type(word: u32) -> u32 {
    let pre = ((word & 0xF00) >> 7)
        | ((word & 0x7E00_0000) >> 20)
        | ((word & 0x80) << 4)
        | ((word & 0x8000_0000) >> 19);
    sign_extend(pre, 12)
}

fn decode_imm_j_type(word: u32) -> u32 {
    let pre = ((word & 0x7FE0_0000) >> 20)
        | ((word & 0x0010_0000) >> 9)
        | (word & 0xFF000)
        | ((word & 0x8000_0000) >> 11);
    sign_extend(pre, 20)
}

const REGISTER_ABI_NAMES: [&str; 32] = [
    "zero", "ra", "sp", "gp", "tp", "t0", "t1", "t2", "s0", "s1", "a0", "a1", "a2", "a3", "a4",
    "a5", "a6", "a7", "s2", "s3", "s4", "s5", "s6", "s7", "s8", "s9", "s10", "s11", "t3", "t4",
    "t5", "t6",
];

fn parse_register_index(name: &str) -> Result<u8, String> {
    if let Some(stripped) = name.to_lowercase().strip_prefix("x") {
        if let Ok(idx) = stripped.parse::<u8>() {
            if idx >= 32 {
                return Err(format!("Invalid register index: {name}"));
            }
            return Ok(idx);
        }
        return Err(format!("Cannot parse register index: {name}"));
    };
    if name == "fp" {
        return Ok(8);
    }
    if let Some(idx) = REGISTER_ABI_NAMES
        .iter()
        .position(|&abi_name| abi_name == name.to_lowercase())
    {
        Ok(idx as u8)
    } else {
        Err(format!("Uknown register: {name}"))
    }
}

const INSTRUCTION_TEMPLATES: [(u32, &str); 29] = [
    (0b0000_0000_0000_0000_0000_0000_0011_0111, "lui"),
    (0b0000_0000_0000_0000_0000_0000_0001_0111, "auipc"),
    (0b0000_0000_0000_0000_0000_0000_0011_0011, "add"),
    (0b0100_0000_0000_0000_0000_0000_0011_0011, "sub"),
    (0b0000_0000_0000_0000_0100_0000_0011_0011, "xor"),
    (0b0000_0000_0000_0000_0110_0000_0011_0011, "or"),
    (0b0000_0000_0000_0000_0111_0000_0011_0011, "and"),
    (0b0000_0000_0000_0000_0000_0000_0001_0011, "addi"),
    (0b0000_0000_0000_0000_0010_0000_0001_0011, "slti"),
    (0b0000_0000_0000_0000_0011_0000_0001_0011, "sltiu"),
    (0b0000_0000_0000_0000_0100_0000_0001_0011, "xori"),
    (0b0000_0000_0000_0000_0110_0000_0001_0011, "ori"),
    (0b0000_0000_0000_0000_0111_0000_0001_0011, "andi"),
    (0b0000_0000_0000_0000_0000_0000_0110_0111, "jalr"),
    (0b0000_0000_0000_0000_0010_0000_0000_0011, "lw"),
    (0b0000_0000_0000_0000_0001_0000_0000_0011, "lh"),
    (0b0000_0000_0000_0000_0000_0000_0000_0011, "lb"),
    (0b0000_0000_0000_0000_0101_0000_0000_0011, "lhu"),
    (0b0000_0000_0000_0000_0100_0000_0000_0011, "lbu"),
    (0b0000_0000_0000_0000_0010_0000_0010_0011, "sw"),
    (0b0000_0000_0000_0000_0001_0000_0010_0011, "sh"),
    (0b0000_0000_0000_0000_0000_0000_0010_0011, "sb"),
    (0b0000_0000_0000_0000_0000_0000_0110_0011, "beq"),
    (0b0000_0000_0000_0000_0001_0000_0110_0011, "bne"),
    (0b0000_0000_0000_0000_0101_0000_0110_0011, "bge"),
    (0b0000_0000_0000_0000_0100_0000_0110_0011, "blt"),
    (0b0000_0000_0000_0000_0111_0000_0110_0011, "bgeu"),
    (0b0000_0000_0000_0000_0110_0000_0110_0011, "bltu"),
    (0b0000_0000_0000_0000_0000_0000_0110_1111, "jal"),
];

fn encode_instruction(ip: u32, instr: &str, labels: &HashMap<String, u32>) -> Result<u32, String> {
    let Some((opname, args)) = instr.split_once(char::is_whitespace) else {
        return Err(format!("No arguments in instruction: {instr}"));
    };

    let split_args = args.split(',').map(str::trim).collect::<Vec<&str>>();

    let Some((template, _)) = INSTRUCTION_TEMPLATES
        .iter()
        .find(|(_, name)| *name == opname)
    else {
        return Err(format!("Unsupported operation: {opname}"));
    };

    if ["lui", "auipc"].contains(&opname) {
        let rd = u32::from(parse_register_index(split_args[0])?);
        let imm = split_args[2]
            .parse::<u32>()
            .ok()
            .ok_or(format!("Cannot parse immediate: {instr}"))?;
        Ok(template | set_bits(rd, 7, 5) | (imm & 0xFFFF_F000))
    } else if ["lw", "lh", "lb", "lhu", "lbu"].contains(&opname) {
        let memloc: Vec<&str> = split_args[1]
            .split(')')
            .next()
            .ok_or(format!("Malformed memory location: {instr}"))?
            .split('(')
            .collect();
        let imm = memloc[0]
            .parse::<i32>()
            .ok()
            .ok_or(format!("Cannot parse immediate: {instr}"))? as u32;
        let rd = u32::from(parse_register_index(split_args[0])?);
        let rs1 = u32::from(parse_register_index(memloc[1])?);
        Ok(template | set_bits(rd, 7, 5) | set_bits(rs1, 15, 5) | set_bits(imm, 20, 12))
    } else if ["sw", "sh", "sb"].contains(&opname) {
        let memloc: Vec<&str> = split_args[1]
            .split(')')
            .next()
            .ok_or(format!("Malformed memory location: {instr}"))?
            .split('(')
            .collect();
        let imm = memloc[0]
            .parse::<i32>()
            .ok()
            .ok_or(format!("Cannot parse immediate: {instr}"))? as u32;
        let rs2 = u32::from(parse_register_index(split_args[0])?);
        let rs1 = u32::from(parse_register_index(memloc[1])?);
        Ok(template
            | set_bits(rs1, 15, 5)
            | set_bits(rs2, 20, 5)
            | set_bits(imm & 0b0000_0001_1111, 7, 5)
            | set_bits((imm & 0b1111_1110_0000) >> 5, 25, 7))
    } else if ["add", "sub", "xor", "or", "and"].contains(&opname) {
        let rd = u32::from(parse_register_index(split_args[0])?);
        let rs1 = u32::from(parse_register_index(split_args[1])?);
        let rs2 = u32::from(parse_register_index(split_args[2])?);
        Ok(template | set_bits(rd, 7, 5) | set_bits(rs1, 15, 5) | set_bits(rs2, 20, 5))
    } else if ["beq", "bne", "blt", "bge", "bltu", "bgeu"].contains(&opname) {
        let rs1 = u32::from(parse_register_index(split_args[0])?);
        let rs2 = u32::from(parse_register_index(split_args[1])?);
        let dest = *labels
            .get(split_args[2])
            .ok_or(format!("Label not found: {instr}"))?;
        let imm = dest.wrapping_sub(ip);
        Ok(template
            | set_bits(rs1, 15, 5)
            | set_bits(rs2, 20, 5)
            | set_bits(
                ((imm & 0b1000_0000_0000) >> 11) | (imm & 0b0000_0001_1110),
                7,
                5,
            )
            | set_bits(
                ((imm & 0b0001_0000_0000_0000) >> 6) | ((imm & 0b0111_1110_0000) >> 5),
                25,
                7,
            ))
    } else if ["addi", "jalr", "slti", "sltiu", "xori", "ori", "andi"].contains(&opname) {
        let rd = u32::from(parse_register_index(split_args[0])?);
        let rs1 = u32::from(parse_register_index(split_args[1])?);
        let imm = split_args[2]
            .parse::<i16>()
            .ok()
            .ok_or(format!("Cannot parse immediate: {instr}"))? as u32;
        Ok(template | set_bits(rd, 7, 5) | set_bits(rs1, 15, 5) | set_bits(imm, 20, 12))
    } else if "jal" == opname {
        let rd = u32::from(parse_register_index(split_args[0])?);
        let dest = *labels
            .get(split_args[1])
            .ok_or(format!("Label not found: {instr}"))?;
        let imm = dest.wrapping_sub(ip);
        Ok(template
            | set_bits(rd, 7, 5)
            | set_bits(
                ((imm & (1 << 20)) >> 1)
                    | ((imm & 0b0111_1111_1110) << 8)
                    | ((imm & 0b1000_0000_0000) >> 3)
                    | ((imm & 0xFF000) >> 12),
                12,
                20,
            ))
    } else {
        Err(format!("Unknown instruction: {instr}"))
    }
}

pub fn assemble(asm: &str) -> Result<Vec<u32>, String> {
    let mut ip: u32 = 0;
    let mut labels = HashMap::<String, u32>::new();
    let mut instructions_code: Vec<&str> = vec![];
    let mut instructions = vec![];
    for line in asm.lines() {
        let blocks = line.split('#').collect::<Vec<&str>>();
        let Some(code) = blocks.first() else { continue };
        let trimmed = code.trim();
        if trimmed.is_empty() {
            continue;
        }
        if let Some((label, instr)) = trimmed.split_once(':') {
            if let Some(loc) = labels.get(label) {
                return Err(format!(
                    "Ambiguous label: {label} (occurs at {ip} and {loc})"
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
        instructions.push(encode_instruction(ip, instr, &labels)?);
        ip += 4;
    }
    Ok(instructions)
}

#[derive(Debug)]
pub struct VM {
    regs: [u32; 32],
    code: Vec<u32>,
    ip: u32,
    memory: Vec<u8>,
}

const OPCODE_LUI: u32 = 0b011_0111;
const OPCODE_AUIPC: u32 = 0b001_0111;
const OPCODE_JAL: u32 = 0b110_1111;
const OPCODE_JALR: u32 = 0b110_0111;
const OPCODE_BRANCH: u32 = 0b110_0011;
const OPCODE_LOAD: u32 = 0b000_0011;
const OPCODE_STORE: u32 = 0b010_0011;
const OPCODE_IMM: u32 = 0b001_0011;
const OPCODE_ARLOG: u32 = 0b011_0011;

impl VM {
    #[must_use]
    pub fn new(memory_size: usize) -> Self {
        let mut regs = [0; 32];
        regs[2] = memory_size as u32; // initialize stack pointer
        Self {
            regs,
            code: vec![],
            ip: 0,
            memory: vec![0; memory_size],
        }
    }

    pub fn reset_code(&mut self, code: &[u32]) {
        self.code = code.to_vec();
        self.regs[1] = 4 * (code.len() as u32); // initialize return address
    }

    pub fn write_register(&mut self, idx: usize, value: u32) {
        if idx > 0 {
            self.regs[idx] = value
        }
    }

    #[must_use]
    pub fn read_register(&self, idx: usize) -> u32 {
        self.regs[idx]
    }

    fn load_byte(&self, address: usize) -> u32 {
        self.memory[address] as i8 as i32 as u32
    }

    fn load_half(&self, address: usize) -> u32 {
        let value = u16::from_le_bytes([self.memory[address], self.memory[address + 1]]);
        value as i16 as i32 as u32
    }

    fn load_byte_unsigned(&self, address: usize) -> u32 {
        u32::from(self.memory[address])
    }

    fn load_half_unsigned(&self, address: usize) -> u32 {
        u16::from_le_bytes([self.memory[address], self.memory[address + 1]]) as u32
    }

    fn load_word(&self, address: usize) -> u32 {
        let bytes = &self.memory[address..address + 4];
        u32::from_le_bytes([bytes[0], bytes[1], bytes[2], bytes[3]])
    }

    fn store_byte(&mut self, address: usize, value: u32) {
        self.memory[address] = value as u8;
    }

    fn store_half(&mut self, address: usize, value: u32) {
        let bytes = (value as u16).to_le_bytes();
        self.memory[address..address + 2].copy_from_slice(&bytes);
    }

    fn store_word(&mut self, address: usize, value: u32) {
        let bytes = value.to_le_bytes();
        self.memory[address..address + 4].copy_from_slice(&bytes);
    }

    fn execute_arlog(&mut self, word: u32) {
        let rd = decode_rd(word);
        let (rs1, rs2) = (decode_rs1(word), decode_rs2(word));
        let (funct3, funct7) = (decode_funct3(word), decode_funct7(word));
        let (op1, op2) = (self.regs[rs1], self.regs[rs2]);
        self.regs[rd] = match (funct3, funct7) {
            (0b000, 0b000_0000) => op1.wrapping_add(op2), // add
            (0b000, 0b010_0000) => op1.wrapping_sub(op2), // sub
            (0b100, 0b000_0000) => op1 ^ op2,             // xor
            (0b110, 0b000_0000) => op1 | op2,             // or
            (0b111, 0b000_0000) => op1 & op2,             // and
            _ => todo!("funct3 = 0b{:03b}, funct7 = 0b{:07b}", funct3, funct7),
        };
        self.ip = self.ip.wrapping_add(4);
    }

    fn execute_imm(&mut self, word: u32) {
        let rd = decode_rd(word);
        let rs1 = decode_rs1(word);
        let funct3 = decode_funct3(word);
        let imm = decode_imm_i_type(word);
        self.regs[rd] = match funct3 {
            0b000 => self.regs[rs1].wrapping_add(imm), // addi
            0b010 => u32::from((self.regs[rs1] as i32) < (imm as i32)), // slti
            0b011 => u32::from(self.regs[rs1] < imm),  // sltiu
            0b100 => self.regs[rs1] ^ imm,             // xori
            0b110 => self.regs[rs1] | imm,             // ori
            0b111 => self.regs[rs1] & imm,             // andi
            _ => todo!("funct3 = 0b{:03b}", funct3),
        };
        self.ip = self.ip.wrapping_add(4);
    }

    fn execute_load(&mut self, word: u32) {
        let rd = decode_rd(word);
        let rs1 = decode_rs1(word);
        let funct3 = decode_funct3(word);
        let imm = decode_imm_i_type(word);
        let address = self.regs[rs1].wrapping_add(imm) as usize;
        match funct3 {
            0b010 => self.regs[rd] = self.load_word(address), // lw
            0b001 => self.regs[rd] = self.load_half(address), // lh
            0b000 => self.regs[rd] = self.load_byte(address), // lb
            0b100 => self.regs[rd] = self.load_byte_unsigned(address), // lbu
            0b101 => self.regs[rd] = self.load_half_unsigned(address), // lhu
            _ => todo!("funct3 = 0b{:03b}", funct3),
        }
        self.ip = self.ip.wrapping_add(4);
    }

    fn execute_store(&mut self, word: u32) {
        let rs1 = decode_rs1(word);
        let rs2 = decode_rs2(word);
        let funct3 = decode_funct3(word);
        let imm = decode_imm_s_type(word);
        let address = self.regs[rs1].wrapping_add(imm) as usize;
        match funct3 {
            0b010 => self.store_word(address, self.regs[rs2]), // sw
            0b001 => self.store_half(address, self.regs[rs2]), // sh
            0b000 => self.store_byte(address, self.regs[rs2]), // sb
            _ => todo!("funct3 = 0b{:03b}", funct3),
        }
        self.ip = self.ip.wrapping_add(4);
    }

    fn execute_branch(&mut self, word: u32) {
        let rs1 = decode_rs1(word);
        let rs2 = decode_rs2(word);
        let funct3 = decode_funct3(word);
        let imm = decode_imm_b_type(word);
        let (op1, op2) = (self.regs[rs1], self.regs[rs2]);
        let cond = match funct3 {
            0b000 => op1 == op2,                   // beq
            0b001 => op1 != op2,                   // bne
            0b100 => (op1 as i32) < (op2 as i32),  // blt
            0b101 => (op1 as i32) >= (op2 as i32), // bge
            0b110 => op1 < op2,                    // bltu
            0b111 => op1 >= op2,                   // bgeu
            _ => todo!("funct3 = 0b{:03b}", funct3),
        };
        self.ip = if cond {
            self.ip.wrapping_add(imm)
        } else {
            self.ip.wrapping_add(4)
        };
    }

    fn execute_jalr(&mut self, word: u32) {
        let rd = decode_rd(word);
        let rs1 = decode_rs1(word);
        let funct3 = decode_funct3(word);
        let imm = decode_imm_i_type(word);
        match funct3 {
            0b000 => {
                // jalr
                self.regs[rd] = self.ip.wrapping_add(4);
                self.ip = self.regs[rs1].wrapping_add(imm);
            }
            _ => todo!("funct3 = 0b{:03b}", funct3),
        }
    }

    fn execute(&mut self, word: u32) {
        let opcode = word & 0x7F;
        match opcode {
            OPCODE_ARLOG => self.execute_arlog(word),
            OPCODE_IMM => self.execute_imm(word),
            OPCODE_JALR => self.execute_jalr(word),
            OPCODE_LOAD => self.execute_load(word),
            OPCODE_STORE => self.execute_store(word),
            OPCODE_BRANCH => self.execute_branch(word),
            OPCODE_JAL => {
                let rd = decode_rd(word);
                let imm = decode_imm_j_type(word);
                self.regs[rd] = self.ip.wrapping_add(4);
                self.ip = self.ip.wrapping_add(imm);
            }
            OPCODE_LUI => {
                let rd = decode_rd(word);
                self.regs[rd] = word & 0xFFFF_F000;
            }
            OPCODE_AUIPC => {
                let rd = decode_rd(word);
                let imm = word & 0xFFFF_F000;
                self.regs[rd] = self.ip.wrapping_add(imm);
            }
            _ => todo!("opcode = 0b{:07b}", opcode),
        }
        self.regs[0] = 0; // NOTE: hard-wire zero register
    }

    pub fn run(&mut self) {
        let code_length = 4 * (self.code.len() as u32);
        loop {
            if self.ip >= code_length {
                break;
            }
            assert!(self.ip % 4 == 0, "instruction pointer is not word-aligned");
            self.execute(self.code[(self.ip / 4) as usize]);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_sign_extend() {
        let cases = vec![
            (
                0b0000_0000_0000_0000_0000_0100_1010_1111,
                0,
                0b1111_1111_1111_1111_1111_1111_1111_1111,
            ),
            (
                0b0000_0000_0000_0000_0000_0100_1010_1110,
                0,
                0b0000_0000_0000_0000_0000_0000_0000_0000,
            ),
            (
                0b0000_0000_0000_0000_0000_0100_1010_1110,
                1,
                0b1111_1111_1111_1111_1111_1111_1111_1110,
            ),
            (
                0b0000_0000_0000_0000_0000_0100_1010_1100,
                10,
                0b1111_1111_1111_1111_1111_1100_1010_1100,
            ),
            (
                0b0000_0000_0000_0000_0000_0100_1010_1100,
                11,
                0b0000_0000_0000_0000_0000_0100_1010_1100,
            ),
            (
                0b0000_0000_0000_0000_0000_0100_1010_1100,
                7,
                0b1111_1111_1111_1111_1111_1111_1010_1100,
            ),
            (
                0b0000_0000_0000_0000_0000_0100_1010_1100,
                6,
                0b0000_0000_0000_0000_0000_0000_0010_1100,
            ),
        ];

        for (base, sign_idx, expected) in cases {
            assert_eq!(sign_extend(base, sign_idx), expected)
        }
    }

    #[test]
    fn test_decode_imm_b_type() {
        assert_eq!(
            decode_imm_b_type(0b0000_0000_0111_0101_0101_0110_0110_0011),
            12
        )
    }

    #[test]
    fn test_write_to_zero() {
        let mut vm = VM::new(0);
        vm.reset_code(&[
            0x00100013, // addi x0, x0, 1
        ]);
        vm.run();
        assert_eq!(vm.regs[0], 0)
    }

    #[test]
    fn test_store_word_load_word() {
        let mut vm = VM::new(1024);
        vm.store_word(512, 0x12345678);
        assert_eq!(vm.load_word(512), 0x12345678)
    }

    #[test]
    fn test_store_half_load_half() {
        let mut vm = VM::new(1024);
        vm.store_half(256, 0x7654);
        assert_eq!(vm.load_half(256), 0x7654);
        vm.store_half(256, 0x8654);
        assert_eq!(vm.load_half(256), 0xFFFF8654);
    }

    #[test]
    fn test_store_byte_load_byte() {
        let mut vm = VM::new(1024);
        vm.store_byte(256, 0x7F);
        assert_eq!(vm.load_byte(256), 0x7F);
        vm.store_byte(256, 0x8F);
        assert_eq!(vm.load_byte(256), 0xFFFFFF8F);
    }

    #[test]
    fn test_store_byte_load_half() {
        let mut vm = VM::new(1024);

        vm.store_byte(256, 0x12);
        vm.store_byte(257, 0x34);
        assert_eq!(vm.load_half(256), 0x3412);

        vm.store_byte(257, 0x84);
        assert_eq!(vm.load_half(256), 0xFFFF8412)
    }

    #[test]
    fn test_store_half_load_word() {
        let mut vm = VM::new(1024);

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
    bge x10, x7, loop       # If n >= 1, enter loop
    add x10, x0, x5         # Return Fib(0) for n=0
    jalr x0, x1, 0

loop:
    add x28, x5, x6         # x28 = Fib(i) = Fib(i-1) + Fib(i-2)
    add x5, x0, x6          # x5 = Fib(i-1)
    add x6, x0, x28         # x6 = Fib(i)
    addi x7, x7, 1          # i++
    blt x7, x10, loop       # If i < n, continue loop
    add x10, x0, x28        # Store result in x10
"#;

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
        let code = assemble(FIB_ITER_ASM).unwrap();
        assert_eq!(code.len(), FIB_ITER_BIN.len());
        for (instr, instr_expected) in code.iter().zip(FIB_ITER_BIN) {
            assert_eq!(*instr, instr_expected);
        }
    }

    #[test]
    fn test_fib_iter_run() {
        let mut vm = VM::new(0);
        vm.reset_code(&FIB_ITER_BIN);
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

    const FIB_RECUR_BIN: [u32; 17] = [
        0b0000_0000_0001_0000_0000_0010_1001_0011,
        0b0000_0000_1010_0010_1100_0100_0110_0011,
        0b0000_0000_0000_0000_1000_0000_0110_0111,
        0b1111_1111_0100_0001_0000_0001_0001_0011,
        0b0000_0000_0001_0001_0010_0100_0010_0011,
        0b0000_0000_1010_0001_0010_0010_0010_0011,
        0b1111_1111_1111_0101_0000_0101_0001_0011,
        0b1111_1110_0101_1111_1111_0000_1110_1111,
        0b0000_0000_1010_0001_0010_0000_0010_0011,
        0b0000_0000_0100_0001_0010_0101_0000_0011,
        0b1111_1111_1110_0101_0000_0101_0001_0011,
        0b1111_1101_0101_1111_1111_0000_1110_1111,
        0b0000_0000_0000_0001_0010_0010_1000_0011,
        0b0000_0000_0101_0101_0000_0101_0011_0011,
        0b0000_0000_1000_0001_0010_0000_1000_0011,
        0b0000_0000_1100_0001_0000_0001_0001_0011,
        0b0000_0000_0000_0000_1000_0000_0110_0111,
    ];

    #[test]
    fn test_fib_recur_assemble() {
        let code = assemble(FIB_RECUR_ASM).unwrap();
        assert_eq!(code.len(), FIB_RECUR_BIN.len());
        for (instr, instr_expected) in code.iter().zip(FIB_RECUR_BIN) {
            assert_eq!(*instr, instr_expected);
        }
    }

    #[test]
    fn test_fib_recur_run() {
        let mut vm = VM::new(1024);
        vm.reset_code(&FIB_RECUR_BIN);
        vm.write_register(10, 13);
        vm.run();
        assert_eq!(vm.read_register(10), 233)
    }
}
