//===-- AlphaELFObjectWriter.cpp - Alpha ELF Writer -----------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "AlphaMCTargetDesc.h"
#include "llvm/MC/MCELFObjectWriter.h"
#include "llvm/MC/MCFixup.h"
#include "llvm/MC/MCObjectWriter.h"
#include "llvm/Support/ErrorHandling.h"

using namespace llvm;

namespace {
class AlphaELFObjectWriter : public MCELFObjectTargetWriter {
public:
  AlphaELFObjectWriter(uint8_t OSABI);

  ~AlphaELFObjectWriter() override;

protected:
  unsigned getRelocType(MCContext &Ctx, const MCValue &Target,
                        const MCFixup &Fixup, bool IsPCRel) const override;
};
} // namespace

AlphaELFObjectWriter::AlphaELFObjectWriter(uint8_t OSABI)
    : MCELFObjectTargetWriter(/*Is64Bit*/ true, OSABI, ELF::EM_ALPHA,
                              /*HasRelocationAddend*/ true) {}

AlphaELFObjectWriter::~AlphaELFObjectWriter() {}

unsigned AlphaELFObjectWriter::getRelocType(MCContext &Ctx,
                                            const MCValue &Target,
                                            const MCFixup &Fixup,
                                            bool IsPCRel) const {
  report_fatal_error("unhandled relocation type!");
}

std::unique_ptr<MCObjectTargetWriter>
llvm::createAlphaELFObjectWriter(uint8_t OSABI) {
  return std::make_unique<AlphaELFObjectWriter>(OSABI);
}