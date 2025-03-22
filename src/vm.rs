// TODO
// - implement more instructions
// - implement assembler
// - add register aliases
// - add pseudo-instructions (e.g. li)
// - handle error situations
// - make panic-free

#[derive(Clone, Debug, PartialEq)]
pub enum Op {
    // R-TYPE (rd, rs1, rs2)
    Add(usize, usize, usize),
    // I-TYPE (rd, rs1, imm)
    Addi(usize, usize, i32),
    Jalr(usize, usize, usize),
    Lb(usize, usize, i32),
    Lh(usize, usize, i32),
    Lw(usize, usize, i32),
    // S-TYPE (rs1, rs2, imm)
    Sb(usize, usize, i32),
    Sh(usize, usize, i32),
    Sw(usize, usize, i32),
    // B-TYPE (rs1, rs2, imm)
    Beq(usize, usize, usize),
    Bge(usize, usize, usize),
    Blt(usize, usize, usize),
    // J-TYPE (rd, imm)
    Jal(usize, usize),
}

fn get_bits(word: u32, start_index: u8, num_bits: u8) -> u32 {
    (!(0xFFFFFFFFu32 << num_bits)) & (word >> start_index)
}

const R_TYPE: u32 = 0b0110011;
const I_TYPE: u32 = 0b0010011;
const I_TYPE_LOAD: u32 = 0b0000011;
const I_TYPE_JALR: u32 = 0b1100111;
const S_TYPE: u32 = 0b0100011;
const B_TYPE: u32 = 0b1100011;
const J_TYPE: u32 = 0b1101111;

fn decode_r_type(word: u32) -> Op {
    let rd = get_bits(word, 7, 5) as usize;
    let funct3 = get_bits(word, 12, 3);
    let rs1 = get_bits(word, 15, 5) as usize;
    let rs2 = get_bits(word, 20, 5) as usize;
    let funct7 = get_bits(word, 25, 7);
    match (funct3, funct7) {
        (0b000, 0b0000000) => Op::Add(rd, rs1, rs2),
        _ => todo!(),
    }
}

fn decode_i_type(word: u32) -> Op {
    let opcode = get_bits(word, 0, 7);
    let rd = get_bits(word, 7, 5) as usize;
    let funct3 = get_bits(word, 12, 3);
    let rs1 = get_bits(word, 15, 5) as usize;
    let imm = get_bits(word, 20, 12);
    match (opcode, funct3) {
        (I_TYPE, 0x000) => Op::Addi(rd, rs1, imm as i32),
        (I_TYPE_JALR, 0x000) => Op::Jalr(rd, rs1, imm as usize),
        (I_TYPE_LOAD, 0x000) => Op::Lb(rd, rs1, imm as i32),
        (I_TYPE_LOAD, 0x001) => Op::Lh(rd, rs1, imm as i32),
        (I_TYPE_LOAD, 0x010) => Op::Lw(rd, rs1, imm as i32),
        _ => todo!(),
    }
}

fn decode_s_type(word: u32) -> Op {
    let funct3 = get_bits(word, 12, 3);
    let rs1 = get_bits(word, 15, 5) as usize;
    let rs2 = get_bits(word, 20, 5) as usize;
    let imm = get_bits(word, 7, 5) | (get_bits(word, 25, 6) << 5);
    match funct3 {
        0b000 => Op::Sb(rs1, rs2, imm as i32),
        0b001 => Op::Sh(rs1, rs2, imm as i32),
        0b010 => Op::Sw(rs1, rs2, imm as i32),
        _ => todo!("funct3 = 0b{:b}", funct3),
    }
}

fn decode_b_type_imm(word: u32) -> u32 {
    let imm_4_1_11 = get_bits(word, 7, 5);
    let imm_12_10_5 = get_bits(word, 25, 7);
    ((imm_4_1_11 >> 1) << 1)
        | (imm_12_10_5 << 5)
        | ((imm_4_1_11 & 1) << 11)
        | ((imm_12_10_5 & 128) << 12)
}

fn decode_b_type(word: u32) -> Op {
    let funct3 = get_bits(word, 12, 3);
    let rs1 = get_bits(word, 15, 5) as usize;
    let rs2 = get_bits(word, 20, 5) as usize;
    let imm = decode_b_type_imm(word);
    match funct3 {
        0b000 => Op::Beq(rs1, rs2, imm as usize),
        0b101 => Op::Bge(rs1, rs2, imm as usize),
        0b100 => Op::Blt(rs1, rs2, imm as usize),
        _ => todo!("funct3 = 0b{:b}", funct3),
    }
}

fn decode_j_type_imm(word: u32) -> u32 {
    let imm = get_bits(word, 12, 20);
    ((0xFF & imm) << 12)
        | (((1 << 8) & imm) << 3)
        | (((0b11_1111_1111 << 9) & imm) >> 8)
        | (((1 << 19) & imm) << 1)
}

fn decode_j_type(word: u32) -> Op {
    let rd = get_bits(word, 7, 5) as usize;
    let imm = decode_j_type_imm(word) as usize;
    Op::Jal(rd, imm)
}

impl From<u32> for Op {
    fn from(word: u32) -> Self {
        let opcode = get_bits(word, 0, 7);
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

#[derive(Debug)]
pub struct VM {
    regs: [u32; 32],
    code: Vec<Op>,
    ip: usize,
    memory: Vec<u8>,
}

impl VM {
    pub fn new(code: Vec<Op>, memory_size: usize) -> Self {
        Self {
            regs: [0; 32],
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

    pub fn load_byte(&self, address: usize) -> u32 {
        let value = self.memory[address] as u32;
        // sign extension
        if value > 0x80 {
            0xFFFFFF00 | value
        } else {
            value
        }
    }

    pub fn load_half(&self, address: usize) -> u32 {
        // little-endian layout
        let value = self.memory[address] as u32 | (self.memory[address + 1] as u32) << 8;
        // sign extension
        if value > 0x8000 {
            0xFFFF0000 | value
        } else {
            value
        }
    }

    pub fn load_word(&self, address: usize) -> u32 {
        // little-endian layout
        self.memory[address] as u32
            | (self.memory[address + 1] as u32) << 8
            | (self.memory[address + 2] as u32) << 16
            | (self.memory[address + 3] as u32) << 24
    }

    pub fn store_byte(&mut self, address: usize, value: u32) {
        self.memory[address] = (value & 0xFF) as u8;
    }

    pub fn store_half(&mut self, address: usize, value: u32) {
        // little-endian layout
        self.memory[address] = (value & 0x00FF) as u8;
        self.memory[address + 1] = ((value & 0xFF00) >> 8) as u8;
    }

    pub fn store_word(&mut self, address: usize, value: u32) {
        // little-endian layout
        self.memory[address] = (value & 0x000000FF) as u8;
        self.memory[address + 1] = ((value & 0x0000FF00) >> 8) as u8;
        self.memory[address + 2] = ((value & 0x00FF0000) >> 16) as u8;
        self.memory[address + 3] = ((value & 0xFF000000) >> 24) as u8;
    }

    fn step(&mut self) {
        if self.ip % 4 != 0 {
            panic!("instruction pointer is not word-aligned")
        }
        match self.code[self.ip / 4] {
            Op::Add(rd, rs1, rs2) => {
                self.regs[rd] = (self.regs[rs1] as i32 + self.regs[rs2] as i32) as u32;
                self.ip += 4;
            }
            Op::Addi(rd, rs1, val) => {
                self.regs[rd] = (self.regs[rs1] as i32 + val) as u32;
                self.ip += 4;
            }
            Op::Beq(rs1, rs2, ip) => {
                if self.regs[rs1] == self.regs[rs2] {
                    self.ip = ip;
                } else {
                    self.ip += 4;
                }
            }
            Op::Bge(rs1, rs2, ip) => {
                if self.regs[rs1] >= self.regs[rs2] {
                    self.ip = ip;
                } else {
                    self.ip += 4;
                }
            }
            Op::Blt(rs1, rs2, ip) => {
                if self.regs[rs1] < self.regs[rs2] {
                    self.ip = ip;
                } else {
                    self.ip += 4;
                }
            }
            Op::Jal(rd, ip) => {
                self.regs[rd] = self.ip as u32 + 4;
                self.ip = ip;
            }
            Op::Jalr(rd, rs1, offset) => {
                self.regs[rd] = (self.ip as u32) + 4;
                self.ip = self.regs[rs1] as usize + offset;
            }
            Op::Lb(rd, rs1, offset) => {
                let address = (self.regs[rs1] as i32 + offset) as usize;
                self.regs[rd] = self.load_byte(address);
            }
            Op::Lh(rd, rs1, offset) => {
                let address = (self.regs[rs1] as i32 + offset) as usize;
                self.regs[rd] = self.load_half(address);
            }
            Op::Lw(rd, rs1, offset) => {
                let address = (self.regs[rs1] as i32 + offset) as usize;
                self.regs[rd] = self.load_word(address);
            }
            Op::Sb(rs1, rs2, offset) => {
                let address = (self.regs[rs2] as i32 + offset) as usize;
                self.store_byte(address, self.regs[rs1]);
            }
            Op::Sh(rs1, rs2, offset) => {
                let address = (self.regs[rs2] as i32 + offset) as usize;
                self.store_half(address, self.regs[rs1]);
            }
            Op::Sw(rs1, rs2, offset) => {
                let address = (self.regs[rs2] as i32 + offset) as usize;
                self.store_word(address, self.regs[rs1]);
            }
        }
    }

    pub fn run(&mut self) {
        loop {
            // NOTE: the following is to "hard-wire" the register
            // NOTE: arguably the register should just not be there
            self.regs[0] = 0;
            if self.ip >= 4 * self.code.len() {
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
    fn test_write_to_zero() {
        let mut vm = VM::new(vec![Op::Addi(0, 0, 1)], 0);
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

    // # Compute Fibonacci(n) where n is passed in x10, result will be stored in x10
    //
    //     addi x5, x0, 0   # x5 = Fib(0) = 0
    //     addi x6, x0, 1   # x6 = Fib(1) = 1
    //     addi x7, x0, 1   # x7 = Loop counter i = 1
    //     bge x10, x7, fib_loop  # If n >= 1, enter loop
    //     add x10, x0, x5      # Return Fib(0) for n=0
    //     jal x1, end
    // fib_loop:
    //     add x28, x5, x6   # x28 = Fib(i) = Fib(i-1) + Fib(i-2)
    //     add x5, x0, x6 # x5 = Fib(i-2) = Fib(i-1)
    //     add x6, x0, x28 # x6 = Fib(i-1) = Fib(i)
    //     addi x7, x7, 1   # i++
    //     blt x7, x10, fib_loop  # If i < n, continue loop
    //     add x10, x0, x28  # Store result in x10
    // end:

    const FIB_ITER_BIN: [u32; 12] = [
        0b00000000000000000000_00101_0010011,
        0b00000000000100000000_00110_0010011,
        0b00000000000100000000_00111_0010011,
        0b00000000011101010101_11000_1100011,
        0b00000000010100000000_01010_0110011,
        0b00000011000000000000_00001_1101111,
        0b00000000011000101000_11100_0110011,
        0b00000000011000000000_00101_0110011,
        0b00000001110000000000_00110_0110011,
        0b00000000000100111000_00111_0010011,
        0b00000000101000111100_11000_1100011,
        0b00000001110000000000_01010_0110011,
    ];

    const FIB_ITER_OPS: [Op; 12] = [
        Op::Addi(5, 0, 0),
        Op::Addi(6, 0, 1),
        Op::Addi(7, 0, 1),
        Op::Bge(10, 7, 24),
        Op::Add(10, 0, 5),
        Op::Jal(1, 48),
        Op::Add(28, 5, 6),
        Op::Add(5, 0, 6),
        Op::Add(6, 0, 28),
        Op::Addi(7, 7, 1),
        Op::Blt(7, 10, 24),
        Op::Add(10, 0, 28),
    ];

    #[test]
    fn test_fibonacci_decode() {
        for (word, op) in FIB_ITER_BIN.iter().zip(FIB_ITER_OPS) {
            assert_eq!(Op::from(*word), op)
        }
    }

    #[test]
    fn test_fibonacci_run() {
        let mut vm = VM::new(FIB_ITER_BIN.map(Op::from).to_vec(), 0);
        vm.write_register(10, 17);
        vm.run();
        assert_eq!(vm.read_register(10), 1597)
    }
}
