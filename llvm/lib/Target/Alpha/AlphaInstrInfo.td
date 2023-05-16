//===-- AlphaInstrInfo.td - Target Description for Alpha ---*- tablegen -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https     ://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier     : Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

include "AlphaInstrFormats.td"

class ImmAsmOperand<string prefix, int width, string suffix> : AsmOperandClass {
  let Name = prefix # "Imm" # width # suffix;
  let DiagnosticType = !strconcat("Invalid", Name);
  let RenderMethod = "addImmOperands";
}

class SImmAsmOperand<int width, string suffix = "">
         : ImmAsmOperand<"S", width, suffix> {
}

class UImmAsmOperand<int width, string suffix = "">
         : ImmAsmOperand<"U", width, suffix> {
}

def uimm8 : Operand<i64> {
  let ParserMatchClass = UImmAsmOperand<8>;
  let DecoderMethod = "decodeUImmOperand<8>";
}

def simm21 : Operand<i64> {
  let ParserMatchClass = SImmAsmOperand<21>;
  let DecoderMethod = "decodeSImmOperand<21>";
}

def simm16 : Operand<i64> {
  let ParserMatchClass = SImmAsmOperand<16>;
  let DecoderMethod = "decodeSImmOperand<16>";
}

//===----------------------------------------------------------------------===//
// Basic Integer Instructions
//===----------------------------------------------------------------------===//

let hasSideEffects = 0, mayLoad = 0, mayStore = 0 in {
def ADDLri        : AlphaInstOperateLit<
        OPC_INTA, 0x00, (outs GPR:$rc), (ins GPR:$ra, uimm8:$imm), "addl $ra, $imm, $rc">;
def ADDQri        : AlphaInstOperateLit<
        OPC_INTA, 0x20, (outs GPR:$rc), (ins GPR:$ra, uimm8:$imm), "addq $ra, $imm, $rc">;
def MULLri        : AlphaInstOperateLit<
        OPC_INTM, 0x00, (outs GPR:$rc), (ins GPR:$ra, uimm8:$imm), "mull $ra, $imm, $rc">;
def MULQri        : AlphaInstOperateLit<
        OPC_INTM, 0x20, (outs GPR:$rc), (ins GPR:$ra, uimm8:$imm), "mulq $ra, $imm, $rc">;
def S4ADDLri      : AlphaInstOperateLit<
        OPC_INTA, 0x02, (outs GPR:$rc), (ins GPR:$ra, uimm8:$imm), "s4addl $ra, $imm, $rc">;
def S4ADDQri      : AlphaInstOperateLit<
        OPC_INTA, 0x22, (outs GPR:$rc), (ins GPR:$ra, uimm8:$imm), "s4addq $ra, $imm, $rc">;
def S8ADDLri      : AlphaInstOperateLit<
        OPC_INTA, 0x12, (outs GPR:$rc), (ins GPR:$ra, uimm8:$imm), "s8addl $ra, $imm, $rc">;
def S8ADDQri      : AlphaInstOperateLit<
        OPC_INTA, 0x32, (outs GPR:$rc), (ins GPR:$ra, uimm8:$imm), "s8addq $ra, $imm, $rc">;
def S4SUBLri      : AlphaInstOperateLit<
        OPC_INTA, 0x0b, (outs GPR:$rc), (ins GPR:$ra, uimm8:$imm), "s4subl $ra, $imm, $rc">;
def S4SUBQri      : AlphaInstOperateLit<
        OPC_INTA, 0x2b, (outs GPR:$rc), (ins GPR:$ra, uimm8:$imm), "s4subq $ra, $imm, $rc">;
def S8SUBLri      : AlphaInstOperateLit<
        OPC_INTA, 0x1b, (outs GPR:$rc), (ins GPR:$ra, uimm8:$imm), "s8subl $ra, $imm, $rc">;
def S8SUBQri      : AlphaInstOperateLit<
        OPC_INTA, 0x3b, (outs GPR:$rc), (ins GPR:$ra, uimm8:$imm), "s8subq $ra, $imm, $rc">;
def SUBLri        : AlphaInstOperateLit<
        OPC_INTA, 0x09, (outs GPR:$rc), (ins GPR:$ra, uimm8:$imm), "subl $ra, $imm, $rc">;
def SUBQri        : AlphaInstOperateLit<
        OPC_INTA, 0x29, (outs GPR:$rc), (ins GPR:$ra, uimm8:$imm), "subq $ra, $imm, $rc">;
def ANDri         : AlphaInstOperateLit<
        OPC_INTL, 0x00, (outs GPR:$rc), (ins GPR:$ra, uimm8:$imm), "and $ra, $imm, $rc">;
def BISri         : AlphaInstOperateLit<
        OPC_INTL, 0x20, (outs GPR:$rc), (ins GPR:$ra, uimm8:$imm), "bis $ra, $imm, $rc">;
def XORri         : AlphaInstOperateLit<
        OPC_INTL, 0x40, (outs GPR:$rc), (ins GPR:$ra, uimm8:$imm), "xor $ra, $imm, $rc">;
def SLLri          : AlphaInstOperateLit<
        OPC_INTS, 0x39, (outs GPR:$rc), (ins GPR:$ra, uimm8:$imm), "sll $ra, $imm, $rc">;
def SRAri         : AlphaInstOperateLit<
        OPC_INTS, 0x3c, (outs GPR:$rc), (ins GPR:$ra, uimm8:$imm), "sra $ra, $imm, $rc">;
def SRLri         : AlphaInstOperateLit<
        OPC_INTS, 0x34, (outs GPR:$rc), (ins GPR:$ra, uimm8:$imm), "srl $ra, $imm, $rc">;
def UMULHri       : AlphaInstOperateLit<
        OPC_INTM, 0x30, (outs GPR:$rc), (ins GPR:$ra, uimm8:$imm), "umulh $ra, $imm, $rc">;
def BICri         : AlphaInstOperateLit<
        OPC_INTL, 0x08, (outs GPR:$rc), (ins GPR:$ra, uimm8:$imm), "bic $ra, $imm, $rc">;
def ORNOTri       : AlphaInstOperateLit<
        OPC_INTL, 0x28, (outs GPR:$rc), (ins GPR:$ra, uimm8:$imm), "ornot $ra, $imm, $rc">;
def EQVri         : AlphaInstOperateLit<
        OPC_INTL, 0x48, (outs GPR:$rc), (ins GPR:$ra, uimm8:$imm), "eqv $ra, $imm, $rc">;

def ADDLrr        : AlphaInstOperateReg<
        OPC_INTA, 0x00, (outs GPR:$rc), (ins GPR:$ra, GPR:$rb), "addl $ra, $rb, $rc">;
def ADDQrr        : AlphaInstOperateReg<
        OPC_INTA, 0x20, (outs GPR:$rc), (ins GPR:$ra, GPR:$rb), "addq $ra, $rb, $rc">;
def MULLrr        : AlphaInstOperateReg<
        OPC_INTM, 0x00, (outs GPR:$rc), (ins GPR:$ra, GPR:$rb), "mull $ra, $rb, $rc">;
def MULQrr        : AlphaInstOperateReg<
        OPC_INTM, 0x20, (outs GPR:$rc), (ins GPR:$ra, GPR:$rb), "mulq $ra, $rb, $rc">;
def S4ADDLrr      : AlphaInstOperateReg<
        OPC_INTA, 0x02, (outs GPR:$rc), (ins GPR:$ra, GPR:$rb), "s4addl $ra, $rb, $rc">;
def S4ADDQrr      : AlphaInstOperateReg<
        OPC_INTA, 0x22, (outs GPR:$rc), (ins GPR:$ra, GPR:$rb), "s4addq $ra, $rb, $rc">;
def S8ADDLrr      : AlphaInstOperateReg<
        OPC_INTA, 0x12, (outs GPR:$rc), (ins GPR:$ra, GPR:$rb), "s8addl $ra, $rb, $rc">;
def S8ADDQrr      : AlphaInstOperateReg<
        OPC_INTA, 0x32, (outs GPR:$rc), (ins GPR:$ra, GPR:$rb), "s8addq $ra, $rb, $rc">;
def S4SUBLrr      : AlphaInstOperateReg<
        OPC_INTA, 0x0b, (outs GPR:$rc), (ins GPR:$ra, GPR:$rb), "s4subl $ra, $rb, $rc">;
def S4SUBQrr      : AlphaInstOperateReg<
        OPC_INTA, 0x2b, (outs GPR:$rc), (ins GPR:$ra, GPR:$rb), "s4subq $ra, $rb, $rc">;
def S8SUBLrr      : AlphaInstOperateReg<
        OPC_INTA, 0x1b, (outs GPR:$rc), (ins GPR:$ra, GPR:$rb), "s8subl $ra, $rb, $rc">;
def S8SUBQrr      : AlphaInstOperateReg<
        OPC_INTA, 0x3b, (outs GPR:$rc), (ins GPR:$ra, GPR:$rb), "s8subq $ra, $rb, $rc">;
def SUBLrr        : AlphaInstOperateReg<
        OPC_INTA, 0x09, (outs GPR:$rc), (ins GPR:$ra, GPR:$rb), "subl $ra, $rb, $rc">;
def SUBQrr        : AlphaInstOperateReg<
        OPC_INTA, 0x29, (outs GPR:$rc), (ins GPR:$ra, GPR:$rb), "subq $ra, $rb, $rc">;
def ANDrr         : AlphaInstOperateReg<
        OPC_INTL, 0x00, (outs GPR:$rc), (ins GPR:$ra, GPR:$rb), "and $ra, $rb, $rc">;
def BISrr         : AlphaInstOperateReg<
        OPC_INTL, 0x20, (outs GPR:$rc), (ins GPR:$ra, GPR:$rb), "bis $ra, $rb, $rc">;
def XORrr         : AlphaInstOperateReg<
        OPC_INTL, 0x40, (outs GPR:$rc), (ins GPR:$ra, GPR:$rb), "xor $ra, $rb, $rc">;
def SLLrr          : AlphaInstOperateReg<
        OPC_INTS, 0x39, (outs GPR:$rc), (ins GPR:$ra, GPR:$rb), "sll $ra, $rb, $rc">;
def SRArr         : AlphaInstOperateReg<
        OPC_INTS, 0x3c, (outs GPR:$rc), (ins GPR:$ra, GPR:$rb), "sra $ra, $rb, $rc">;
def SRLrr         : AlphaInstOperateReg<
        OPC_INTS, 0x34, (outs GPR:$rc), (ins GPR:$ra, GPR:$rb), "srl $ra, $rb, $rc">;
def UMULHrr       : AlphaInstOperateReg<
        OPC_INTM, 0x30, (outs GPR:$rc), (ins GPR:$ra, GPR:$rb), "umulh $ra, $rb, $rc">;
def BICrr         : AlphaInstOperateReg<
        OPC_INTL, 0x08, (outs GPR:$rc), (ins GPR:$ra, GPR:$rb), "bic $ra, $rb, $rc">;
def ORNOTrr       : AlphaInstOperateReg<
        OPC_INTL, 0x28, (outs GPR:$rc), (ins GPR:$ra, GPR:$rb), "ornot $ra, $rb, $rc">;
def EQVrr         : AlphaInstOperateReg<
        OPC_INTL, 0x48, (outs GPR:$rc), (ins GPR:$ra, GPR:$rb), "eqv $ra, $rb, $rc">;

def CMPEQri       : AlphaInstOperateLit<
        OPC_INTA, 0x2D, (outs GPR:$rc), (ins GPR:$ra, uimm8:$imm), "cmpeq $ra, $imm, $rc">;
def CMPLEri       : AlphaInstOperateLit<
        OPC_INTA, 0x6D, (outs GPR:$rc), (ins GPR:$ra, uimm8:$imm), "cmple $ra, $imm, $rc">;
def CMPLTri       : AlphaInstOperateLit<
        OPC_INTA, 0x4D, (outs GPR:$rc), (ins GPR:$ra, uimm8:$imm), "cmplt $ra, $imm, $rc">;
def CMPULEri      : AlphaInstOperateLit<
        OPC_INTA, 0x3D, (outs GPR:$rc), (ins GPR:$ra, uimm8:$imm), "cmpule $ra, $imm, $rc">;
def CMPULTri      : AlphaInstOperateLit<
        OPC_INTA, 0x1D, (outs GPR:$rc), (ins GPR:$ra, uimm8:$imm), "cmpult $ra, $imm, $rc">;

def CMPEQrr       : AlphaInstOperateReg<
        OPC_INTA, 0x2D, (outs GPR:$rc), (ins GPR:$ra, GPR:$rb), "cmpeq $ra, $rb, $rc">;
def CMPLErr       : AlphaInstOperateReg<
        OPC_INTA, 0x6D, (outs GPR:$rc), (ins GPR:$ra, GPR:$rb), "cmple $ra, $rb, $rc">;
def CMPLTrr       : AlphaInstOperateReg<
        OPC_INTA, 0x4D, (outs GPR:$rc), (ins GPR:$ra, GPR:$rb), "cmplt $ra, $rb, $rc">;
def CMPULErr      : AlphaInstOperateReg<
        OPC_INTA, 0x3D, (outs GPR:$rc), (ins GPR:$ra, GPR:$rb), "cmpule $ra, $rb, $rc">;
def CMPULTrr      : AlphaInstOperateReg<
        OPC_INTA, 0x1D, (outs GPR:$rc), (ins GPR:$ra, GPR:$rb), "cmpult $ra, $rb, $rc">;

def CMOVEQri      : AlphaInstOperateLit<
        OPC_INTL, 0x24, (outs GPR:$rc), (ins GPR:$ra, uimm8:$imm), "cmoveq $ra, $imm, $rc">;
def CMOVGEri      : AlphaInstOperateLit<
        OPC_INTL, 0x46, (outs GPR:$rc), (ins GPR:$ra, uimm8:$imm), "cmovge $ra, $imm, $rc">;
def CMOVGTri      : AlphaInstOperateLit<
        OPC_INTL, 0x66, (outs GPR:$rc), (ins GPR:$ra, uimm8:$imm), "cmovgt $ra, $imm, $rc">;
def CMOVLBCri     : AlphaInstOperateLit<
        OPC_INTL, 0x16, (outs GPR:$rc), (ins GPR:$ra, uimm8:$imm), "cmovlbc $ra, $imm, $rc">;
def CMOVLBSri     : AlphaInstOperateLit<
        OPC_INTL, 0x14, (outs GPR:$rc), (ins GPR:$ra, uimm8:$imm), "cmovlbs $ra, $imm, $rc">;
def CMOVLEri      : AlphaInstOperateLit<
        OPC_INTL, 0x64, (outs GPR:$rc), (ins GPR:$ra, uimm8:$imm), "cmovle $ra, $imm, $rc">;
def CMOVLTri      : AlphaInstOperateLit<
        OPC_INTL, 0x44, (outs GPR:$rc), (ins GPR:$ra, uimm8:$imm), "cmovlt $ra, $imm, $rc">;
def CMOVNEri      : AlphaInstOperateLit<
        OPC_INTL, 0x26, (outs GPR:$rc), (ins GPR:$ra, uimm8:$imm), "cmovne $ra, $imm, $rc">;

def CMOVEQrr      : AlphaInstOperateReg<
        OPC_INTL, 0x24, (outs GPR:$rc), (ins GPR:$ra, GPR:$rb), "cmoveq $ra, $rb, $rc">;
def CMOVGErr      : AlphaInstOperateReg<
        OPC_INTL, 0x46, (outs GPR:$rc), (ins GPR:$ra, GPR:$rb), "cmovge $ra, $rb, $rc">;
def CMOVGTrr      : AlphaInstOperateReg<
        OPC_INTL, 0x66, (outs GPR:$rc), (ins GPR:$ra, GPR:$rb), "cmovgt $ra, $rb, $rc">;
def CMOVLBCrr     : AlphaInstOperateReg<
        OPC_INTL, 0x16, (outs GPR:$rc), (ins GPR:$ra, GPR:$rb), "cmovlbc $ra, $rb, $rc">;
def CMOVLBSrr     : AlphaInstOperateReg<
        OPC_INTL, 0x14, (outs GPR:$rc), (ins GPR:$ra, GPR:$rb), "cmovlbs $ra, $rb, $rc">;
def CMOVLErr      : AlphaInstOperateReg<
        OPC_INTL, 0x64, (outs GPR:$rc), (ins GPR:$ra, GPR:$rb), "cmovle $ra, $rb, $rc">;
def CMOVLTrr      : AlphaInstOperateReg<
        OPC_INTL, 0x44, (outs GPR:$rc), (ins GPR:$ra, GPR:$rb), "cmovlt $ra, $rb, $rc">;
def CMOVNErr      : AlphaInstOperateReg<
        OPC_INTL, 0x26, (outs GPR:$rc), (ins GPR:$ra, GPR:$rb), "cmovne $ra, $rb, $rc">;

let hasSideEffects = 0, mayLoad = 0, mayStore = 0, isBranch = 1, isTerminator = 1 in {
def BR            : AlphaInstBranch<OPC_BR, (outs GPR:$ra), (ins simm21:$disp), "br $ra, $disp">;
def BSR           : AlphaInstBranch<OPC_BSR, (outs GPR:$ra), (ins simm21:$disp), "bsr $ra, $disp">;
def BEQ           : AlphaInstBranch<OPC_BEQ, (outs), (ins GPR:$ra, simm21:$disp), "beq $ra, $disp">;
def BGE           : AlphaInstBranch<OPC_BGE, (outs), (ins GPR:$ra, simm21:$disp), "bge $ra, $disp">;
def BGT           : AlphaInstBranch<OPC_BGT, (outs), (ins GPR:$ra, simm21:$disp), "bgt $ra, $disp">;
def BLBC          : AlphaInstBranch<OPC_BLBC, (outs), (ins GPR:$ra, simm21:$disp), "blbc $ra, $disp">;
def BLBS          : AlphaInstBranch<OPC_BLBS, (outs), (ins GPR:$ra, simm21:$disp), "blbs $ra, $disp">;
def BLE           : AlphaInstBranch<OPC_BLE, (outs), (ins GPR:$ra, simm21:$disp), "ble $ra, $disp">;
def BLT           : AlphaInstBranch<OPC_BLT, (outs), (ins GPR:$ra, simm21:$disp), "blt $ra, $disp">;
def BNE           : AlphaInstBranch<OPC_BNE, (outs), (ins GPR:$ra, simm21:$disp), "bne $ra, $disp">;
}

let hasSideEffects = 0, mayLoad = 0, mayStore = 0, isCall = 1, isTerminator = 1 in {
def JMP           : AlphaInstMemoryJumpHint<OPC_JSR, 0x0, (outs GPR:$ra), (ins GPR:$rb), "jmp $ra, (${rb})">;
def JSR           : AlphaInstMemoryJumpHint<OPC_JSR, 0x1, (outs GPR:$ra), (ins GPR:$rb), "jsr $ra, (${rb})">;
def RET           : AlphaInstMemoryJumpHint<OPC_JSR, 0x2, (outs GPR:$ra), (ins GPR:$rb), "ret $ra, (${rb})">;
def JSR_COROUTINE : AlphaInstMemoryJumpHint<OPC_JSR, 0x3, (outs GPR:$ra), (ins GPR:$rb), "jsr_coroutine $ra, (${rb})">;
}

let hasSideEffects = 0, mayLoad = 1, mayStore = 0 in {
def LDA           : AlphaInstMemoryDisp<
        OPC_LDA, (outs GPR:$ra), (ins GPR:$rb, simm16:$disp), "lda $ra,${disp}(${rb})">;
def LDAH          : AlphaInstMemoryDisp<
        OPC_LDAH, (outs GPR:$ra), (ins GPR:$rb, simm16:$disp), "ldah $ra,${disp}(${rb})">;
def LDBU          : AlphaInstMemoryDisp<
        OPC_LDBU, (outs GPR:$ra), (ins GPR:$rb, simm16:$disp), "ldbu $ra,${disp}(${rb})">;
def LDL           : AlphaInstMemoryDisp<
        OPC_LDL, (outs GPR:$ra), (ins GPR:$rb, simm16:$disp), "ldl $ra,${disp}(${rb})">;
def LDL_L         : AlphaInstMemoryDisp<
        OPC_LDL_L, (outs GPR:$ra), (ins GPR:$rb, simm16:$disp), "ldl_l $ra,${disp}(${rb})">;
def LDQ           : AlphaInstMemoryDisp<
        OPC_LDQ, (outs GPR:$ra), (ins GPR:$rb, simm16:$disp), "ldq $ra,${disp}(${rb})">;
def LDQ_L         : AlphaInstMemoryDisp<
        OPC_LDQ_L, (outs GPR:$ra), (ins GPR:$rb, simm16:$disp), "ldq_l $ra,${disp}(${rb})">;
def LDQ_U         : AlphaInstMemoryDisp<
        OPC_LDQ_U, (outs GPR:$ra), (ins GPR:$rb, simm16:$disp), "ldq_u $ra,${disp}(${rb})">;
def LDWU          : AlphaInstMemoryDisp<
        OPC_LDWU, (outs GPR:$ra), (ins GPR:$rb, simm16:$disp), "ldwu $ra,${disp}(${rb})">;
}

let hasSideEffects = 0, mayLoad = 0, mayStore = 1 in {
def STB           : AlphaInstMemoryDisp<
        OPC_STB, (outs), (ins GPR:$ra, GPR:$rb, simm16:$disp), "stb $ra,${disp}(${rb})">;
def STL           : AlphaInstMemoryDisp<
        OPC_STL, (outs), (ins GPR:$ra, GPR:$rb, simm16:$disp), "stl $ra,${disp}(${rb})">;
def STL_C         : AlphaInstMemoryDisp<
        OPC_STL_C, (outs), (ins GPR:$ra, GPR:$rb, simm16:$disp), "stl_c $ra,${disp}(${rb})">;
def STQ           : AlphaInstMemoryDisp<
        OPC_STQ, (outs), (ins GPR:$ra, GPR:$rb, simm16:$disp), "stq $ra,${disp}(${rb})">;
def STQ_C         : AlphaInstMemoryDisp<
        OPC_STQ_C, (outs), (ins GPR:$ra, GPR:$rb, simm16:$disp), "stq_c $ra,${disp}(${rb})">;
def STQ_U         : AlphaInstMemoryDisp<
        OPC_STQ_U, (outs), (ins GPR:$ra, GPR:$rb, simm16:$disp), "stq_u $ra,${disp}(${rb})">;
def STW           : AlphaInstMemoryDisp<
        OPC_STW, (outs), (ins GPR:$ra, GPR:$rb, simm16:$disp), "stw $ra,${disp}(${rb})">;
}

def NOP           : Pseudo<(outs), (ins), "nop">;
def LDGP          : Pseudo<(outs), (ins), "ldgp">;

//===----------------------------------------------------------------------===//
// Byte Word eXtension Instructions
//===----------------------------------------------------------------------===//
def CMPBGEri      : AlphaInstOperateLit<
        OPC_INTA, 0x0F, (outs GPR:$rc), (ins GPR:$ra, uimm8:$imm), "cmpbge $ra, $imm, $rc">;
def EXTBLri       : AlphaInstOperateLit<
        OPC_INTS, 0x06, (outs GPR:$rc), (ins GPR:$ra, uimm8:$imm), "extbl $ra, $imm, $rc">;
def EXTWLri       : AlphaInstOperateLit<
        OPC_INTS, 0x16, (outs GPR:$rc), (ins GPR:$ra, uimm8:$imm), "extwl $ra, $imm, $rc">;
def EXTLLri       : AlphaInstOperateLit<
        OPC_INTS, 0x26, (outs GPR:$rc), (ins GPR:$ra, uimm8:$imm), "extll $ra, $imm, $rc">;
def EXTQLri       : AlphaInstOperateLit<
        OPC_INTS, 0x36, (outs GPR:$rc), (ins GPR:$ra, uimm8:$imm), "extql $ra, $imm, $rc">;
def EXTWHri       : AlphaInstOperateLit<
        OPC_INTS, 0x5A, (outs GPR:$rc), (ins GPR:$ra, uimm8:$imm), "extwh $ra, $imm, $rc">;
def EXTLHri       : AlphaInstOperateLit<
        OPC_INTS, 0x6A, (outs GPR:$rc), (ins GPR:$ra, uimm8:$imm), "extlh $ra, $imm, $rc">;
def EXTQHri       : AlphaInstOperateLit<
        OPC_INTS, 0x7A, (outs GPR:$rc), (ins GPR:$ra, uimm8:$imm), "extqh $ra, $imm, $rc">;
def INSBLri       : AlphaInstOperateLit<
        OPC_INTS, 0x0B, (outs GPR:$rc), (ins GPR:$ra, uimm8:$imm), "insbl $ra, $imm, $rc">;
def INSWLri       : AlphaInstOperateLit<
        OPC_INTS, 0x1B, (outs GPR:$rc), (ins GPR:$ra, uimm8:$imm), "inswl $ra, $imm, $rc">;
def INSLLri       : AlphaInstOperateLit<
        OPC_INTS, 0x2B, (outs GPR:$rc), (ins GPR:$ra, uimm8:$imm), "insll $ra, $imm, $rc">;
def INSQLri       : AlphaInstOperateLit<
        OPC_INTS, 0x3B, (outs GPR:$rc), (ins GPR:$ra, uimm8:$imm), "insql $ra, $imm, $rc">;
def INSWHri       : AlphaInstOperateLit<
        OPC_INTS, 0x57, (outs GPR:$rc), (ins GPR:$ra, uimm8:$imm), "inswh $ra, $imm, $rc">;
def INSLHri       : AlphaInstOperateLit<
        OPC_INTS, 0x67, (outs GPR:$rc), (ins GPR:$ra, uimm8:$imm), "inslh $ra, $imm, $rc">;
def INSQHri       : AlphaInstOperateLit<
        OPC_INTS, 0x77, (outs GPR:$rc), (ins GPR:$ra, uimm8:$imm), "insqh $ra, $imm, $rc">;
def MSKBLri       : AlphaInstOperateLit<
        OPC_INTS, 0x02, (outs GPR:$rc), (ins GPR:$ra, uimm8:$imm), "mskbl $ra, $imm, $rc">;
def MSKWLri       : AlphaInstOperateLit<
        OPC_INTS, 0x12, (outs GPR:$rc), (ins GPR:$ra, uimm8:$imm), "mskwl $ra, $imm, $rc">;
def MSKLLri       : AlphaInstOperateLit<
        OPC_INTS, 0x22, (outs GPR:$rc), (ins GPR:$ra, uimm8:$imm), "mskll $ra, $imm, $rc">;
def MSKQLri       : AlphaInstOperateLit<
        OPC_INTS, 0x32, (outs GPR:$rc), (ins GPR:$ra, uimm8:$imm), "mskql $ra, $imm, $rc">;
def MSKWHri       : AlphaInstOperateLit<
        OPC_INTS, 0x52, (outs GPR:$rc), (ins GPR:$ra, uimm8:$imm), "mskwh $ra, $imm, $rc">;
def MSKLHri       : AlphaInstOperateLit<
        OPC_INTS, 0x62, (outs GPR:$rc), (ins GPR:$ra, uimm8:$imm), "msklh $ra, $imm, $rc">;
def MSKQHri       : AlphaInstOperateLit<
        OPC_INTS, 0x72, (outs GPR:$rc), (ins GPR:$ra, uimm8:$imm), "mskqh $ra, $imm, $rc">;
def SEXTBri       : AlphaInstOperateLit<
        OPC_FPTI, 0x00, (outs GPR:$rc), (ins GPR:$ra, uimm8:$imm), "sextb $ra, $imm, $rc">;
def SEXTWri       : AlphaInstOperateLit<
        OPC_FPTI, 0x01, (outs GPR:$rc), (ins GPR:$ra, uimm8:$imm), "sextw $ra, $imm, $rc">;
def ZAPri         : AlphaInstOperateLit<
        OPC_INTS, 0x30, (outs GPR:$rc), (ins GPR:$ra, uimm8:$imm), "zap $ra, $imm, $rc">;
def ZAPNOTri      : AlphaInstOperateLit<
        OPC_INTS, 0x31, (outs GPR:$rc), (ins GPR:$ra, uimm8:$imm), "zapnot $ra, $imm, $rc">;

def CMPBGErr      : AlphaInstOperateReg<
        OPC_INTA, 0x0F, (outs GPR:$rc), (ins GPR:$ra, GPR:$rb), "cmpbge $ra, $rb, $rc">;
def EXTBLrr       : AlphaInstOperateReg<
        OPC_INTS, 0x06, (outs GPR:$rc), (ins GPR:$ra, GPR:$rb), "extbl $ra, $rb, $rc">;
def EXTWLrr       : AlphaInstOperateReg<
        OPC_INTS, 0x16, (outs GPR:$rc), (ins GPR:$ra, GPR:$rb), "extwl $ra, $rb, $rc">;
def EXTLLrr       : AlphaInstOperateReg<
        OPC_INTS, 0x26, (outs GPR:$rc), (ins GPR:$ra, GPR:$rb), "extll $ra, $rb, $rc">;
def EXTQLrr       : AlphaInstOperateReg<
        OPC_INTS, 0x36, (outs GPR:$rc), (ins GPR:$ra, GPR:$rb), "extql $ra, $rb, $rc">;
def EXTWHrr       : AlphaInstOperateReg<
        OPC_INTS, 0x5A, (outs GPR:$rc), (ins GPR:$ra, GPR:$rb), "extwh $ra, $rb, $rc">;
def EXTLHrr       : AlphaInstOperateReg<
        OPC_INTS, 0x6A, (outs GPR:$rc), (ins GPR:$ra, GPR:$rb), "extlh $ra, $rb, $rc">;
def EXTQHrr       : AlphaInstOperateReg<
        OPC_INTS, 0x7A, (outs GPR:$rc), (ins GPR:$ra, GPR:$rb), "extqh $ra, $rb, $rc">;
def INSBLrr       : AlphaInstOperateReg<
        OPC_INTS, 0x0B, (outs GPR:$rc), (ins GPR:$ra, GPR:$rb), "insbl $ra, $rb, $rc">;
def INSWLrr       : AlphaInstOperateReg<
        OPC_INTS, 0x1B, (outs GPR:$rc), (ins GPR:$ra, GPR:$rb), "inswl $ra, $rb, $rc">;
def INSLLrr       : AlphaInstOperateReg<
        OPC_INTS, 0x2B, (outs GPR:$rc), (ins GPR:$ra, GPR:$rb), "insll $ra, $rb, $rc">;
def INSQLrr       : AlphaInstOperateReg<
        OPC_INTS, 0x3B, (outs GPR:$rc), (ins GPR:$ra, GPR:$rb), "insql $ra, $rb, $rc">;
def INSWHrr       : AlphaInstOperateReg<
        OPC_INTS, 0x57, (outs GPR:$rc), (ins GPR:$ra, GPR:$rb), "inswh $ra, $rb, $rc">;
def INSLHrr       : AlphaInstOperateReg<
        OPC_INTS, 0x67, (outs GPR:$rc), (ins GPR:$ra, GPR:$rb), "inslh $ra, $rb, $rc">;
def INSQHrr       : AlphaInstOperateReg<
        OPC_INTS, 0x77, (outs GPR:$rc), (ins GPR:$ra, GPR:$rb), "insqh $ra, $rb, $rc">;
def MSKBLrr       : AlphaInstOperateReg<
        OPC_INTS, 0x02, (outs GPR:$rc), (ins GPR:$ra, GPR:$rb), "mskbl $ra, $rb, $rc">;
def MSKWLrr       : AlphaInstOperateReg<
        OPC_INTS, 0x12, (outs GPR:$rc), (ins GPR:$ra, GPR:$rb), "mskwl $ra, $rb, $rc">;
def MSKLLrr       : AlphaInstOperateReg<
        OPC_INTS, 0x22, (outs GPR:$rc), (ins GPR:$ra, GPR:$rb), "mskll $ra, $rb, $rc">;
def MSKQLrr       : AlphaInstOperateReg<
        OPC_INTS, 0x32, (outs GPR:$rc), (ins GPR:$ra, GPR:$rb), "mskql $ra, $rb, $rc">;
def MSKWHrr       : AlphaInstOperateReg<
        OPC_INTS, 0x52, (outs GPR:$rc), (ins GPR:$ra, GPR:$rb), "mskwh $ra, $rb, $rc">;
def MSKLHrr       : AlphaInstOperateReg<
        OPC_INTS, 0x62, (outs GPR:$rc), (ins GPR:$ra, GPR:$rb), "msklh $ra, $rb, $rc">;
def MSKQHrr       : AlphaInstOperateReg<
        OPC_INTS, 0x72, (outs GPR:$rc), (ins GPR:$ra, GPR:$rb), "mskqh $ra, $rb, $rc">;
def SEXTBrr       : AlphaInstOperateReg<
        OPC_FPTI, 0x00, (outs GPR:$rc), (ins GPR:$ra, GPR:$rb), "sextb $ra, $rb, $rc">;
def SEXTWrr       : AlphaInstOperateReg<
        OPC_FPTI, 0x01, (outs GPR:$rc), (ins GPR:$ra, GPR:$rb), "sextw $ra, $rb, $rc">;
def ZAPrr         : AlphaInstOperateReg<
        OPC_INTS, 0x30, (outs GPR:$rc), (ins GPR:$ra, GPR:$rb), "zap $ra, $rb, $rc">;
def ZAPNOTrr      : AlphaInstOperateReg<
        OPC_INTS, 0x31, (outs GPR:$rc), (ins GPR:$ra, GPR:$rb), "zapnot $ra, $rb, $rc">;
}

//===----------------------------------------------------------------------===//
// Count Instruction eXtension Instructions
//===----------------------------------------------------------------------===//
def CTLZ          : AlphaInstOperateReg<
        OPC_FPTI, 0x32, (outs GPR:$rc), (ins GPR:$rb), "ctlz $rb, $rc">;
def CTPOP         : AlphaInstOperateReg<
        OPC_FPTI, 0x30, (outs GPR:$rc), (ins GPR:$rb), "ctpop $rb, $rc">;
def CTTZ          : AlphaInstOperateReg<
        OPC_FPTI, 0x33, (outs GPR:$rc), (ins GPR:$rb), "cttz $rb, $rc">;