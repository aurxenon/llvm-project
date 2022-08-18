//===-- AlphaAsmBackend.cpp - Alpha Assembler Backend ---------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "AlphaAsmBackend.h"
#include "AlphaMCTargetDesc.h"
#include "llvm/MC/MCAsmBackend.h"
#include "llvm/MC/MCAssembler.h"
#include "llvm/MC/MCDirectives.h"
#include "llvm/MC/MCELFObjectWriter.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCFixupKindInfo.h"
#include "llvm/MC/MCObjectWriter.h"
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/MC/MCSymbol.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/raw_ostream.h"

using namespace llvm;

bool AlphaAsmBackend::fixupNeedsRelaxation(const MCFixup &Fixup, uint64_t Value,
                                           const MCRelaxableFragment *DF,
                                           const MCAsmLayout &Layout) const {
  return false;
}

unsigned AlphaAsmBackend::getNumFixupKinds() const { return 1; }

bool AlphaAsmBackend::mayNeedRelaxation(const MCInst &Inst,
                                        const MCSubtargetInfo &STI) const {
  return false;
}

void AlphaAsmBackend::relaxInstruction(MCInst &Inst,
                                       const MCSubtargetInfo &STI) const {
  llvm_unreachable("no relaxations implemented!");
}

bool AlphaAsmBackend::writeNopData(raw_ostream &OS, uint64_t Count,
                                   const MCSubtargetInfo *STI) const {
  if ((Count % 4) != 0)
    return false;

  // Alpha "universal" NOP is ldq_u $31,0($sp)
  for (; Count >= 4; Count -= 4)
    OS.write("\0\0\xfe\x2f", 4);

  return true;
}

void AlphaAsmBackend::applyFixup(const MCAssembler &Asm, const MCFixup &Fixup,
                                 const MCValue &Target,
                                 MutableArrayRef<char> Data, uint64_t Value,
                                 bool IsResolved,
                                 const MCSubtargetInfo *STI) const {
  return;
}

std::unique_ptr<MCObjectTargetWriter>
AlphaAsmBackend::createObjectTargetWriter() const {
  return createAlphaELFObjectWriter(OSABI);
}

MCAsmBackend *llvm::createAlphaAsmBackend(const Target &T,
                                          const MCSubtargetInfo &STI,
                                          const MCRegisterInfo &MRI,
                                          const MCTargetOptions &Options) {
  const Triple &TT = STI.getTargetTriple();
  uint8_t OSABI = MCELFObjectTargetWriter::getOSABI(TT.getOS());
  return new AlphaAsmBackend(OSABI);
}