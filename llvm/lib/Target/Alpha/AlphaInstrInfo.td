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
def ADDLri : AlphaInstOperateLit<
        OPC_INTA, 0x0, (outs GPR:$rc), (ins GPR:$ra, uimm8:$imm), "addl $ra, $imm, $rc">;
def ADDLVri : AlphaInstOperateLit<
        OPC_INTA, 0x40, (outs GPR:$rc), (ins GPR:$ra, uimm8:$imm), "addl $ra, $imm, $rc">;
}

let hasSideEffects = 0, mayLoad = 0, mayStore = 0 in {
def ADDLrr : AlphaInstOperateReg<
        OPC_INTA, 0x0, (outs GPR:$rc), (ins GPR:$ra, GPR:$rb), "addl $ra, $rb, $rc">;
def ADDLVrr : AlphaInstOperateLit<
        OPC_INTA, 0x40, (outs GPR:$rc), (ins GPR:$ra, GPR:$rb), "addl $ra, $rb, $rc">;
}
