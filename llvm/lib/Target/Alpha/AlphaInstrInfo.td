//===-- AlphaInstrInfo.td - Target Description for Alpha ---*- tablegen -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
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
}

//===----------------------------------------------------------------------===//
// Basic Integer Instructions
//===----------------------------------------------------------------------===//

let hasSideEffects = 0, mayLoad = 0, mayStore = 0 in {
def ADDLri   : AlphaInstOperateLit<
        OPC_INTA, 0x00, (outs GPR:$rc), (ins GPR:$ra, uimm8:$imm), "addl $ra, $imm, $rc">;
def ADDQri   : AlphaInstOperateLit<
        OPC_INTA, 0x20, (outs GPR:$rc), (ins GPR:$ra, uimm8:$imm), "addq $ra, $imm, $rc">;
def MULLri   : AlphaInstOperateLit<
        OPC_INTM, 0x00, (outs GPR:$rc), (ins GPR:$ra, uimm8:$imm), "mull $ra, $imm, $rc">;
def MULQri   : AlphaInstOperateLit<
        OPC_INTM, 0x20, (outs GPR:$rc), (ins GPR:$ra, uimm8:$imm), "mulq $ra, $imm, $rc">;
def S4ADDLri : AlphaInstOperateLit<
        OPC_INTA, 0x02, (outs GPR:$rc), (ins GPR:$ra, uimm8:$imm), "s4addl $ra, $imm, $rc">;
def S4ADDQri : AlphaInstOperateLit<
        OPC_INTA, 0x22, (outs GPR:$rc), (ins GPR:$ra, uimm8:$imm), "s4addq $ra, $imm, $rc">;
def S8ADDLri : AlphaInstOperateLit<
        OPC_INTA, 0x12, (outs GPR:$rc), (ins GPR:$ra, uimm8:$imm), "s8addl $ra, $imm, $rc">;
def S8ADDQri : AlphaInstOperateLit<
        OPC_INTA, 0x32, (outs GPR:$rc), (ins GPR:$ra, uimm8:$imm), "s8addq $ra, $imm, $rc">;
def S4SUBLri : AlphaInstOperateLit<
        OPC_INTA, 0x0b, (outs GPR:$rc), (ins GPR:$ra, uimm8:$imm), "s4subl $ra, $imm, $rc">;
def S4SUBQri : AlphaInstOperateLit<
        OPC_INTA, 0x2b, (outs GPR:$rc), (ins GPR:$ra, uimm8:$imm), "s4subq $ra, $imm, $rc">;
def S8SUBLri : AlphaInstOperateLit<
        OPC_INTA, 0x1b, (outs GPR:$rc), (ins GPR:$ra, uimm8:$imm), "s8subl $ra, $imm, $rc">;
def S8SUBQri : AlphaInstOperateLit<
        OPC_INTA, 0x3b, (outs GPR:$rc), (ins GPR:$ra, uimm8:$imm), "s8subq $ra, $imm, $rc">;
def SUBLri   : AlphaInstOperateLit<
        OPC_INTA, 0x09, (outs GPR:$rc), (ins GPR:$ra, uimm8:$imm), "subl $ra, $imm, $rc">;
def SUBQri   : AlphaInstOperateLit<
        OPC_INTA, 0x29, (outs GPR:$rc), (ins GPR:$ra, uimm8:$imm), "subq $ra, $imm, $rc">;
def ANDri    : AlphaInstOperateLit<
        OPC_INTL, 0x00, (outs GPR:$rc), (ins GPR:$ra, uimm8:$imm), "and $ra, $imm, $rc">;
def BISri    : AlphaInstOperateLit<
        OPC_INTL, 0x20, (outs GPR:$rc), (ins GPR:$ra, uimm8:$imm), "bis $ra, $imm, $rc">;
def XORri    : AlphaInstOperateLit<
        OPC_INTL, 0x40, (outs GPR:$rc), (ins GPR:$ra, uimm8:$imm), "xor $ra, $imm, $rc">;
def SLri     : AlphaInstOperateLit<
        OPC_INTL, 0x12, (outs GPR:$rc), (ins GPR:$ra, uimm8:$imm), "sl $ra, $imm, $rc">;
def SRAri    : AlphaInstOperateLit<
        OPC_INTS, 0x3c, (outs GPR:$rc), (ins GPR:$ra, uimm8:$imm), "sra $ra, $imm, $rc">;
def SRLri    : AlphaInstOperateLit<
        OPC_INTS, 0x34, (outs GPR:$rc), (ins GPR:$ra, uimm8:$imm), "srl $ra, $imm, $rc">;
def UMULHri  : AlphaInstOperateLit<
        OPC_INTM, 0x30, (outs GPR:$rc), (ins GPR:$ra, uimm8:$imm), "umulh $ra, $imm, $rc">;
def BICri    : AlphaInstOperateLit<
        OPC_INTL, 0x08, (outs GPR:$rc), (ins GPR:$ra, uimm8:$imm), "bic $ra, $imm, $rc">;
def ORNOTri  : AlphaInstOperateLit<
        OPC_INTL, 0x28, (outs GPR:$rc), (ins GPR:$ra, uimm8:$imm), "ornot $ra, $imm, $rc">;
def EQVri    : AlphaInstOperateLit<
        OPC_INTL, 0x48, (outs GPR:$rc), (ins GPR:$ra, uimm8:$imm), "eqv $ra, $imm, $rc">;
}

let hasSideEffects = 0, mayLoad = 0, mayStore = 0 in {
def ADDLrr   : AlphaInstOperateReg<
        OPC_INTA, 0x00, (outs GPR:$rc), (ins GPR:$ra, GPR:$rb), "addl $ra, $rb, $rc">;
def ADDQrr   : AlphaInstOperateReg<
        OPC_INTA, 0x20, (outs GPR:$rc), (ins GPR:$ra, GPR:$rb), "addq $ra, $rb, $rc">;
def MULLrr   : AlphaInstOperateReg<
        OPC_INTM, 0x00, (outs GPR:$rc), (ins GPR:$ra, GPR:$rb), "mull $ra, $rb, $rc">;
def MULQrr   : AlphaInstOperateReg<
        OPC_INTM, 0x20, (outs GPR:$rc), (ins GPR:$ra, GPR:$rb), "mulq $ra, $rb, $rc">;
def S4ADDLrr : AlphaInstOperateReg<
        OPC_INTA, 0x02, (outs GPR:$rc), (ins GPR:$ra, GPR:$rb), "s4addl $ra, $rb, $rc">;
def S4ADDQrr : AlphaInstOperateReg<
        OPC_INTA, 0x22, (outs GPR:$rc), (ins GPR:$ra, GPR:$rb), "s4addq $ra, $rb, $rc">;
def S8ADDLrr : AlphaInstOperateReg<
        OPC_INTA, 0x12, (outs GPR:$rc), (ins GPR:$ra, GPR:$rb), "s8addl $ra, $rb, $rc">;
def S8ADDQrr : AlphaInstOperateReg<
        OPC_INTA, 0x32, (outs GPR:$rc), (ins GPR:$ra, GPR:$rb), "s8addq $ra, $rb, $rc">;
def S4SUBLrr : AlphaInstOperateReg<
        OPC_INTA, 0x0b, (outs GPR:$rc), (ins GPR:$ra, GPR:$rb), "s4subl $ra, $rb, $rc">;
def S4SUBQrr : AlphaInstOperateReg<
        OPC_INTA, 0x2b, (outs GPR:$rc), (ins GPR:$ra, GPR:$rb), "s4subq $ra, $rb, $rc">;
def S8SUBLrr : AlphaInstOperateReg<
        OPC_INTA, 0x1b, (outs GPR:$rc), (ins GPR:$ra, GPR:$rb), "s8subl $ra, $rb, $rc">;
def S8SUBQrr : AlphaInstOperateReg<
        OPC_INTA, 0x3b, (outs GPR:$rc), (ins GPR:$ra, GPR:$rb), "s8subq $ra, $rb, $rc">;
def SUBLrr   : AlphaInstOperateReg<
        OPC_INTA, 0x09, (outs GPR:$rc), (ins GPR:$ra, GPR:$rb), "subl $ra, $rb, $rc">;
def SUBQrr   : AlphaInstOperateReg<
        OPC_INTA, 0x29, (outs GPR:$rc), (ins GPR:$ra, GPR:$rb), "subq $ra, $rb, $rc">;
def ANDrr    : AlphaInstOperateReg<
        OPC_INTL, 0x00, (outs GPR:$rc), (ins GPR:$ra, GPR:$rb), "and $ra, $rb, $rc">;
def BISrr    : AlphaInstOperateReg<
        OPC_INTL, 0x20, (outs GPR:$rc), (ins GPR:$ra, GPR:$rb), "bis $ra, $rb, $rc">;
def XORrr    : AlphaInstOperateReg<
        OPC_INTL, 0x40, (outs GPR:$rc), (ins GPR:$ra, GPR:$rb), "xor $ra, $rb, $rc">;
def SLrr     : AlphaInstOperateReg<
        OPC_INTL, 0x12, (outs GPR:$rc), (ins GPR:$ra, GPR:$rb), "sl $ra, $rb, $rc">;
def SRArr    : AlphaInstOperateReg<
        OPC_INTS, 0x3c, (outs GPR:$rc), (ins GPR:$ra, GPR:$rb), "sra $ra, $rb, $rc">;
def SRLrr    : AlphaInstOperateReg<
        OPC_INTS, 0x34, (outs GPR:$rc), (ins GPR:$ra, GPR:$rb), "srl $ra, $rb, $rc">;
def UMULHrr  : AlphaInstOperateReg<
        OPC_INTM, 0x30, (outs GPR:$rc), (ins GPR:$ra, GPR:$rb), "umulh $ra, $rb, $rc">;
def BICrr    : AlphaInstOperateReg<
        OPC_INTL, 0x08, (outs GPR:$rc), (ins GPR:$ra, GPR:$rb), "bic $ra, $rb, $rc">;
def ORNOTrr  : AlphaInstOperateReg<
        OPC_INTL, 0x28, (outs GPR:$rc), (ins GPR:$ra, GPR:$rb), "ornot $ra, $rb, $rc">;
def EQVrr    : AlphaInstOperateReg<
        OPC_INTL, 0x48, (outs GPR:$rc), (ins GPR:$ra, GPR:$rb), "eqv $ra, $rb, $rc">;
}
