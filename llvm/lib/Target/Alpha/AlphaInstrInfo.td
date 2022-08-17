//===-- AlphaInstrInfo.td - Target Description for Alpha ---*- tablegen -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

include "AlphaInstrFormats.td"

def uimm8 : Operand<i64>;

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
