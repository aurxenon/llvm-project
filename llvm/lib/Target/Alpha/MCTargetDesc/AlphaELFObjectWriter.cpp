//===-- AlphaELFObjectWriter.cpp - Alpha ELF Writer -----------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "MCTargetDesc/AlphaFixupKinds.h"
#include "MCTargetDesc/AlphaMCExpr.h"
#include "MCTargetDesc/AlphaMCTargetDesc.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCELFObjectWriter.h"
#include "llvm/MC/MCFixup.h"
#include "llvm/MC/MCObjectWriter.h"
#include "llvm/Support/ErrorHandling.h"

using namespace llvm;

namespace {
class AlphaELFObjectWriter : public MCELFObjectTargetWriter {
public:
  AlphaELFObjectWriter(uint8_t OSABI);

  bool needsRelocateWithSymbol(const MCSymbol &Sym,
                               unsigned Type) const override;

  bool relocationIsRelativeWithinSection(unsigned Type) const override;

  ~AlphaELFObjectWriter() override;

protected:
  unsigned getRelocType(MCContext &Ctx, const MCValue &Target,
                        const MCFixup &Fixup, bool IsPCRel) const override;
};
} // namespace

AlphaELFObjectWriter::AlphaELFObjectWriter(uint8_t OSABI)
    : MCELFObjectTargetWriter(/*Is64Bit*/ true, OSABI, ELF::EM_ALPHA,
                              /*HasRelocationAddend*/ true) {}

bool AlphaELFObjectWriter::needsRelocateWithSymbol(const MCSymbol &Sym,
                                                   unsigned Type) const {
  switch (Type) {
  default:
    return false;
  case ELF::R_ALPHA_GPDISP:
    return true;
  }
}

bool AlphaELFObjectWriter::relocationIsRelativeWithinSection(
    unsigned Type) const {
  switch (Type) {
  default:
    return false;
  case ELF::R_ALPHA_GPDISP:
    return true;
  }
}

AlphaELFObjectWriter::~AlphaELFObjectWriter() {}

unsigned AlphaELFObjectWriter::getRelocType(MCContext &Ctx,
                                            const MCValue &Target,
                                            const MCFixup &Fixup,
                                            bool IsPCRel) const {
  unsigned Kind = Fixup.getTargetKind();
  if (Kind >= FirstLiteralRelocationKind)
    return Kind - FirstLiteralRelocationKind;
  switch (Kind) {
  default:
    report_fatal_error("unhandled relocation type!");
  case FK_Data_4:
    return ELF::R_ALPHA_REFLONG;
  case FK_Data_8:
    return ELF::R_ALPHA_REFQUAD;
  case Alpha::fixup_alpha_literal:
    return ELF::R_ALPHA_LITERAL;
  case Alpha::fixup_alpha_gpdisp:
    return ELF::R_ALPHA_GPDISP;
  case Alpha::fixup_alpha_gprelhigh:
    return ELF::R_ALPHA_GPRELHIGH;
  case Alpha::fixup_alpha_gprellow:
    return ELF::R_ALPHA_GPRELLOW;
  case Alpha::fixup_alpha_lituse_base:
  case Alpha::fixup_alpha_lituse_jsr:
  case Alpha::fixup_alpha_lituse_jsrdirect:
  case Alpha::fixup_alpha_lituse_bytoff:
  case Alpha::fixup_alpha_lituse_addr:
  case Alpha::fixup_alpha_lituse_tlsgd:
  case Alpha::fixup_alpha_lituse_tlsldm:
    return ELF::R_ALPHA_LITUSE;
  }
}

std::unique_ptr<MCObjectTargetWriter>
llvm::createAlphaELFObjectWriter(uint8_t OSABI) {
  return std::make_unique<AlphaELFObjectWriter>(OSABI);
}