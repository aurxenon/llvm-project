//===-- AlphaRegisterInfo.td - Alpha Register defs --------*- tablegen -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

let Namespace = "Alpha" in {
class AlphaReg<bits<5> Enc, string n, list<string> alt = []> : Register<n> {
  let HWEncoding{4-0} = Enc;
  let AltNames = alt;
}
def ABIRegAltName : RegAltNameIndex;
} // Namespace = "Alpha"

// Integer registers
let RegAltNameIndices = [ABIRegAltName] in {
  def R0  : AlphaReg<0, "0", []>,      DwarfRegNum<[0]>;
  def R1  : AlphaReg<1, "1", []>,      DwarfRegNum<[1]>;
  def R2  : AlphaReg<2, "2", []>,      DwarfRegNum<[2]>;
  def R3  : AlphaReg<3, "3", []>,      DwarfRegNum<[3]>;
  def R4  : AlphaReg<4, "4", []>,      DwarfRegNum<[4]>;
  def R5  : AlphaReg<5, "5", []>,      DwarfRegNum<[5]>;
  def R6  : AlphaReg<6, "6", []>,      DwarfRegNum<[6]>;
  def R7  : AlphaReg<7, "7", []>,      DwarfRegNum<[7]>;
  def R8  : AlphaReg<8, "8", []>,      DwarfRegNum<[8]>;
  def R9  : AlphaReg<9, "9", []>,      DwarfRegNum<[9]>;
  def R10 : AlphaReg<10,"10", []>,     DwarfRegNum<[10]>;
  def R11 : AlphaReg<11,"11", []>,     DwarfRegNum<[11]>;
  def R12 : AlphaReg<12,"12", []>,     DwarfRegNum<[12]>;
  def R13 : AlphaReg<13,"13", []>,     DwarfRegNum<[13]>;
  def R14 : AlphaReg<14,"14", []>,     DwarfRegNum<[14]>;
  def R15 : AlphaReg<15,"15", ["fp"]>, DwarfRegNum<[15]>;
  def R16 : AlphaReg<16,"16", []>,     DwarfRegNum<[16]>;
  def R17 : AlphaReg<17,"17", []>,     DwarfRegNum<[17]>;
  def R18 : AlphaReg<18,"18", []>,     DwarfRegNum<[18]>;
  def R19 : AlphaReg<19,"19", []>,     DwarfRegNum<[19]>;
  def R20 : AlphaReg<20,"20", []>,     DwarfRegNum<[20]>;
  def R21 : AlphaReg<21,"21", []>,     DwarfRegNum<[21]>;
  def R22 : AlphaReg<22,"22", []>,     DwarfRegNum<[22]>;
  def R23 : AlphaReg<23,"23", []>,     DwarfRegNum<[23]>;
  def R24 : AlphaReg<24,"24", []>,     DwarfRegNum<[24]>;
  def R25 : AlphaReg<25,"25", []>,     DwarfRegNum<[25]>;
  def R26 : AlphaReg<26,"26", []>,     DwarfRegNum<[26]>;
  def R27 : AlphaReg<27,"27", []>,     DwarfRegNum<[27]>;
  def R28 : AlphaReg<28,"28", ["at"]>, DwarfRegNum<[28]>;
  def R29 : AlphaReg<29,"29", ["gp"]>, DwarfRegNum<[29]>;
  def R30 : AlphaReg<30,"30", ["sp"]>, DwarfRegNum<[30]>;
  def R31 : AlphaReg<31,"31", []>,     DwarfRegNum<[31]>;
}

// Ordered from top to bottom as caller-saved, callee-saved, special
def GPR : RegisterClass< "Alpha", [i64], 64, (add
    (sequence "R%u", 0, 8),
    (sequence "R%u", 16, 21),
    (sequence "R%u", 22, 25),
    (sequence "R%u", 9, 15),
    (sequence "R%u", 26, 31)
)>;