//===-- AlphaAsmBackend.h - Alpha Assembler Backend -----------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_ALPHA_MCTARGETDESC_ALPHAASMBACKEND_H
#define LLVM_LIB_TARGET_ALPHA_MCTARGETDESC_ALPHAASMBACKEND_H

#include "llvm/MC/MCAsmBackend.h"
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/MC/MCTargetOptions.h"

namespace llvm {

class AlphaAsmBackend : public MCAsmBackend {
  uint8_t OSABI;

public:
  AlphaAsmBackend(uint8_t OSABI)
      : MCAsmBackend(support::little), OSABI(OSABI) {}
  ~AlphaAsmBackend() override {}

  void applyFixup(const MCAssembler &Asm, const MCFixup &Fixup,
                  const MCValue &Target, MutableArrayRef<char> Data,
                  uint64_t Value, bool IsResolved,
                  const MCSubtargetInfo *STI) const override;

  std::unique_ptr<MCObjectTargetWriter>
  createObjectTargetWriter() const override;

  bool fixupNeedsRelaxation(const MCFixup &Fixup, uint64_t Value,
                            const MCRelaxableFragment *DF,
                            const MCAsmLayout &Layout) const override;

  unsigned getNumFixupKinds() const override;

  bool mayNeedRelaxation(const MCInst &Inst,
                         const MCSubtargetInfo &STI) const override;

  void relaxInstruction(MCInst &Inst,
                        const MCSubtargetInfo &STI) const override;

  bool writeNopData(raw_ostream &OS, uint64_t Count,
                    const MCSubtargetInfo *STI) const override;
};

} // namespace llvm

#endif // LLVM_LIB_TARGET_ALPHA_MCTARGETDESC_ALPHAASMBACKEND_H
