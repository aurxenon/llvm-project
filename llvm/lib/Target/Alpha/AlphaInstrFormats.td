//=====- AlphaInstrFormats.td - Alpha Instruction Formats --*- tblgen -*-=====//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

class InstFormat<bits<3> val> {
  bits<3> Value = val;
}
def InstFormatPseudo         : InstFormat<0>;
def InstFormatIntOperate     : InstFormat<1>;
def InstFormatLitOperate     : InstFormat<2>;
def InstFormatFPOperate      : InstFormat<3>;
def InstFormatMemory         : InstFormat<4>;
def InstFormatBranch         : InstFormat<5>;
def InstFormatPALcode        : InstFormat<6>;

// The following opcode names match those given in Table C-6 in the
// Alpha Architecture Reference Manual 4th edition
class AlphaOpcode<string name, bits<6> val> {
  string Name = name;
  bits<6> Value = val;
}

def OPC_PAL       : AlphaOpcode<"PAL",       0b000000>; // 0x0
def OPC_RES_1     : AlphaOpcode<"RES1",      0b000001>; // 0x1
def OPC_RES_2     : AlphaOpcode<"RES2",      0b000010>; // 0x2
def OPC_RES_3     : AlphaOpcode<"RES3",      0b000011>; // 0x3
def OPC_RES_4     : AlphaOpcode<"RES4",      0b000100>; // 0x4
def OPC_RES_5     : AlphaOpcode<"RES5",      0b000101>; // 0x5
def OPC_RES_6     : AlphaOpcode<"RES6",      0b000110>; // 0x6
def OPC_RES_7     : AlphaOpcode<"RES7",      0b000111>; // 0x7
def OPC_LDA       : AlphaOpcode<"LDA",       0b001000>; // 0x8
def OPC_LDAH      : AlphaOpcode<"LDAH",      0b001001>; // 0x9
def OPC_LDBU      : AlphaOpcode<"LDBU",      0b001010>; // 0xa
def OPC_LDQ_U     : AlphaOpcode<"LDQ_U",     0b001011>; // 0xb
def OPC_LDWU      : AlphaOpcode<"LDWU",      0b001100>; // 0xc
def OPC_STW       : AlphaOpcode<"STW",       0b001101>; // 0xd
def OPC_STB       : AlphaOpcode<"STB",       0b001110>; // 0xe
def OPC_STQ_U     : AlphaOpcode<"STQ_U",     0b001111>; // 0xf
def OPC_INTA      : AlphaOpcode<"INTA",      0b010000>; // 0x10
def OPC_INTL      : AlphaOpcode<"INTL",      0b010001>; // 0x11
def OPC_INTS      : AlphaOpcode<"INTS",      0b010010>; // 0x12
def OPC_INTM      : AlphaOpcode<"INTM",      0b010011>; // 0x13
def OPC_ITFP      : AlphaOpcode<"ITFP",      0b010100>; // 0x14
def OPC_FLTV      : AlphaOpcode<"FLTV",      0b010101>; // 0x15
def OPC_FLTI      : AlphaOpcode<"FLTI",      0b010110>; // 0x16
def OPC_FLTL      : AlphaOpcode<"FLTL",      0b010111>; // 0x17
def OPC_MISC      : AlphaOpcode<"MISC",      0b011000>; // 0x18
def OPC_PAL_RES_1 : AlphaOpcode<"PAL_RES_1", 0b011001>; // 0x19
def OPC_JSR       : AlphaOpcode<"JSR",       0b011010>; // 0x1a
def OPC_PAL_RES_2 : AlphaOpcode<"PAL_RES_2", 0b011011>; // 0x1b
def OPC_FPTI      : AlphaOpcode<"FPTI",      0b011100>; // 0x1c
def OPC_PAL_RES_3 : AlphaOpcode<"PAL_RES_3", 0b011101>; // 0x1d
def OPC_PAL_RES_4 : AlphaOpcode<"PAL_RES_4", 0b011110>; // 0x1e
def OPC_PAL_RES_5 : AlphaOpcode<"PAL_RES_5", 0b011111>; // 0x1f
def OPC_LDF       : AlphaOpcode<"LDF",       0b100000>; // 0x20
def OPC_LDG       : AlphaOpcode<"LDG",       0b100001>; // 0x21
def OPC_LDS       : AlphaOpcode<"LDS",       0b100010>; // 0x22
def OPC_LDT       : AlphaOpcode<"LDT",       0b100011>; // 0x23
def OPC_STF       : AlphaOpcode<"STF",       0b100100>; // 0x24
def OPC_STG       : AlphaOpcode<"STG",       0b100101>; // 0x25
def OPC_STS       : AlphaOpcode<"STS",       0b100110>; // 0x26
def OPC_STT       : AlphaOpcode<"STT",       0b100111>; // 0x27
def OPC_LDL       : AlphaOpcode<"LDL",       0b101000>; // 0x28
def OPC_LDQ       : AlphaOpcode<"LDQ",       0b101001>; // 0x29
def OPC_LDL_L     : AlphaOpcode<"LDL_L",     0b101010>; // 0x2a
def OPC_LDQ_L     : AlphaOpcode<"LDQ_L",     0b101011>; // 0x2b
def OPC_STL       : AlphaOpcode<"STL",       0b101100>; // 0x2c
def OPC_STQ       : AlphaOpcode<"STQ",       0b101101>; // 0x2d
def OPC_STL_C     : AlphaOpcode<"STL_C",     0b101110>; // 0x2e
def OPC_STQ_C     : AlphaOpcode<"STQ_C",     0b101111>; // 0x2f
def OPC_BR        : AlphaOpcode<"BR",        0b110000>; // 0x30
def OPC_FBEQ      : AlphaOpcode<"FBEQ",      0b110001>; // 0x31
def OPC_FBLT      : AlphaOpcode<"FBLT",      0b110010>; // 0x32
def OPC_FBLE      : AlphaOpcode<"FBLE",      0b110011>; // 0x33
def OPC_BSR       : AlphaOpcode<"BSR",       0b110100>; // 0x34
def OPC_FBNE      : AlphaOpcode<"FBNE",      0b110101>; // 0x35
def OPC_FBGE      : AlphaOpcode<"FBGE",      0b110110>; // 0x36
def OPC_FBGT      : AlphaOpcode<"FBGT",      0b110111>; // 0x37
def OPC_BLBC      : AlphaOpcode<"BLBC",      0b111000>; // 0x38
def OPC_BEQ       : AlphaOpcode<"BEQ",       0b111001>; // 0x39
def OPC_BLT       : AlphaOpcode<"BLT",       0b111010>; // 0x3a
def OPC_BLE       : AlphaOpcode<"BLE",       0b111011>; // 0x3b
def OPC_BLBS      : AlphaOpcode<"BLBS",      0b111100>; // 0x3c
def OPC_BNE       : AlphaOpcode<"BNE",       0b111101>; // 0x3d
def OPC_BGE       : AlphaOpcode<"BGE",       0b111110>; // 0x3e
def OPC_BGT       : AlphaOpcode<"BGT",       0b111111>; // 0x3f

class AlphaInst<dag outs, dag ins, string asmstr,
             list<dag> pattern, InstFormat format>
    : Instruction {
  field bits<32> Inst;
  // SoftFail is a field the disassembler can use to provide a way for
  // instructions to not match without killing the whole decode process. It is
  // mainly used for ARM, but Tablegen expects this field to exist or it fails
  // to build the decode table.
  field bits<32> SoftFail = 0;
  let Size = 4;

  bits<6> Opcode = 0;

  let Inst{31-26} = Opcode;

  let Namespace = "Alpha";

  dag OutOperandList = outs;
  dag InOperandList = ins;
  let AsmString = asmstr;
  let Pattern = pattern;
}

// Pseudo instructions
class Pseudo<dag outs, dag ins, list<dag> pattern, string asmstr = "">
    : AlphaInst<outs, ins, asmstr, pattern, InstFormatPseudo> {
  let isPseudo = 1;
  let isCodeGenOnly = 1;
}

// Pseudo IEEE quiet comparison operations
// The Alpha is very close to RISC-V in terms of how it handles
// comparisons of signaling vs quiet NaN comparisons, so we can
// mostly just reuse what they've done.
// NOTE that the Alpha requires the /S qualifier to be set for 
// the following to be done in hardware. (Alpha Architecture Reference
// Manual appendix B.1)
// Both treat floating point eq's as being silent (unless receiving 
// an SNaN), while other comparisons are treated as being signaling 
// regardless of if the input is an SNaN or a QNaN. Thus some custom
// instruction lowering logic is required. 
// For further reference, check Table B-2 in the appendix of the Alpha
// Architecture Reference Manual (Applies mainly to /S qualifier).
class PseudoQuietFCMP<RegisterClass Ty>
    : Pseudo<(outs GPR:$rd), (ins Ty:$rs1, Ty:$rs2), []> {
  let hasSideEffects = 1;
  let mayLoad = 0;
  let mayStore = 0;
}

class AlphaInstMemory<AlphaOpcode opcode, dag outs, dag ins, string asmstr>
    : AlphaInst<outs, ins, asmstr, [], InstFormatMemory> {
  bits<5> ra;
  bits<5> rb;

  let Opcode = opcode.Value;
  let Inst{25-21} = ra;
  let Inst{20-16} = rb;
}

class AlphaInstMemoryDisp<AlphaOpcode opcode, dag outs, dag ins, string asmstr>
    : AlphaInstMemory<opcode, outs, ins, asmstr> {
  bits<16> disp;

  let Inst{15-0} = disp;
}

class AlphaInstMemoryFunction<AlphaOpcode opcode, bits<16> func, dag outs, dag ins, string asmstr>
    : AlphaInstMemory<opcode, outs, ins, asmstr> {
  let Inst{15-0} = func;
}

class AlphaInstMemoryJumpHint<AlphaOpcode opcode, bits<16> hint, dag outs, dag ins, string asmstr>
    : AlphaInstMemory<opcode, outs, ins, asmstr> {
  let Inst{15-0} = hint;
}

class AlphaInstBranch<AlphaOpcode opcode, dag outs, dag ins, string asmstr>
    : AlphaInst<outs, ins, asmstr, [], InstFormatMemory> {
  bits<5> ra;
  bits<21> disp;

  let Opcode = opcode.Value;
  let Inst{25-21} = ra;
  let Inst{20-0} = disp;
}

class AlphaInstOperate<AlphaOpcode opcode, bits<7> func, dag outs, dag ins, string asmstr, InstFormat format>
    : AlphaInst<outs, ins, asmstr, [], format> {
  bits<5> ra;
  
  bits<1> is_literal = 0;
  bits<5> rc;

  let Opcode = opcode.Value;
  let Inst{25-21} = ra;
  
  let Inst{12} = is_literal;
  let Inst{11-5} = func;
  let Inst{4-0} = rc;
}

class AlphaInstOperateReg<AlphaOpcode opcode, bits<7> func, dag outs, dag ins, string asmstr>
    : AlphaInstOperate<opcode, func, outs, ins, asmstr, InstFormatIntOperate> {
  bits<5> rb;
  bits<3> sbz = 0;

  let Inst{20-16} = rb;
  let Inst{15-13} = sbz;
  let is_literal = 0;
}

class AlphaInstOperateLit<AlphaOpcode opcode, bits<7> func, dag outs, dag ins, string asmstr>
    : AlphaInstOperate<opcode, func, outs, ins, asmstr, InstFormatLitOperate> {
  bits<8> literal;

  let Inst{20-13} = literal;
  let is_literal = 1;
}

class AlphaInstFPOperateReg<AlphaOpcode opcode, bits<11> func, dag outs, dag ins, string asmstr>
    : AlphaInst<outs, ins, asmstr, [], InstFormatFPOperate> {
  bits<5> fa;
  bits<5> fb;
  bits<5> fc;

  let Opcode = opcode.Value;
  let Inst{25-21} = fa;
  let Inst{20-16} = fb;
  let Inst{15-5} = func;
  let Inst{4-0} = fc;
}

class AlphaInsPALcode<AlphaOpcode opcode, dag outs, dag ins, string asmstr>
    : AlphaInst<outs, ins, asmstr, [], InstFormatPALcode> {
  bits<26> func;

  let Opcode = opcode.Value;
  let Inst{25-0} = func;
}
