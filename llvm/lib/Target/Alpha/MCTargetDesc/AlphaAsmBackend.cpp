//===-- AlphaAsmBackend.cpp - Alpha Assembler Backend ---------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "MCTargetDesc/AlphaAsmBackend.h"
#include "MCTargetDesc/AlphaFixupKinds.h"
#include "MCTargetDesc/AlphaMCExpr.h"
#include "MCTargetDesc/AlphaMCTargetDesc.h"
#include "llvm/MC/MCAsmBackend.h"
#include "llvm/MC/MCAsmLayout.h"
#include "llvm/MC/MCAssembler.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCDirectives.h"
#include "llvm/MC/MCELFObjectWriter.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCFixupKindInfo.h"
#include "llvm/MC/MCObjectWriter.h"
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/MC/MCSymbol.h"
#include "llvm/MC/MCValue.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/raw_ostream.h"

using namespace llvm;

bool AlphaAsmBackend::fixupNeedsRelaxation(const MCFixup &Fixup, uint64_t Value,
                                           const MCRelaxableFragment *DF,
                                           const MCAsmLayout &Layout) const {
  return false;
}

unsigned AlphaAsmBackend::getNumFixupKinds() const {
  return Alpha::NumTargetFixupKinds;
}

const MCFixupKindInfo &
AlphaAsmBackend::getFixupKindInfo(MCFixupKind Kind) const {
  const static MCFixupKindInfo Infos[] = {
      // This table *must* be in the order that the fixup_* kinds are defined in
      // AlphaFixupKinds.h.
      //
      // name                 offset bits  flags
      {"fixup_alpha_literal", 0, 16, MCFixupKindInfo::FKF_IsTarget},
      {"fixup_alpha_gpdisp", 0, 16, MCFixupKindInfo::FKF_IsTarget},
      {"fixup_alpha_gpdisp_lda", 0, 16, MCFixupKindInfo::FKF_IsTarget},
      {"fixup_alpha_gprelhigh", 0, 16, MCFixupKindInfo::FKF_IsTarget},
      {"fixup_alpha_gprellow", 0, 16, MCFixupKindInfo::FKF_IsTarget},
      {"fixup_alpha_lituse_base", 0, 0, MCFixupKindInfo::FKF_IsTarget},
      {"fixup_alpha_lituse_jsr", 0, 0, MCFixupKindInfo::FKF_IsTarget},
      {"fixup_alpha_lituse_jsrdirect", 0, 0, MCFixupKindInfo::FKF_IsTarget},
      {"fixup_alpha_lituse_bytoff", 0, 0, MCFixupKindInfo::FKF_IsTarget},
      {"fixup_alpha_lituse_addr", 0, 0, MCFixupKindInfo::FKF_IsTarget},
      {"fixup_alpha_lituse_tlsgd", 0, 0, MCFixupKindInfo::FKF_IsTarget},
      {"fixup_alpha_lituse_tlsldm", 0, 0, MCFixupKindInfo::FKF_IsTarget}};
  static_assert((array_lengthof(Infos)) == Alpha::NumTargetFixupKinds,
                "Not all fixup kinds added to Infos array");

  if (FirstTargetFixupKind <= Kind && Kind < FirstLiteralRelocationKind) {
    assert(unsigned(Kind - FirstTargetFixupKind) < getNumFixupKinds() &&
           "Invalid kind!");

    return Infos[Kind - FirstTargetFixupKind];
  } else if (Kind < FirstTargetFixupKind) {
    return MCAsmBackend::getFixupKindInfo(Kind);
  } else {
    return MCAsmBackend::getFixupKindInfo(FK_NONE);
  }
}

bool AlphaAsmBackend::shouldForceRelocation(const MCAssembler &Asm,
                                            const MCFixup &Fixup,
                                            const MCValue &Target) {
  if (Fixup.getKind() >= FirstLiteralRelocationKind)
    return true;
  switch (Fixup.getTargetKind()) {
  default:
    break;
  case FK_Data_1:
  case FK_Data_2:
  case FK_Data_4:
  case FK_Data_8:
    if (Target.isAbsolute())
      return false;
    break;
  case Alpha::fixup_alpha_gpdisp:
  case Alpha::fixup_alpha_literal:
    return true;
  }

  return false;
}

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

static uint64_t adjustFixupValue(const MCFixup &Fixup, uint64_t Value,
                                 MCContext &Ctx) {
  switch (Fixup.getTargetKind()) {
  default:
    llvm_unreachable("Unknown fixup kind!");
  case FK_Data_1:
  case FK_Data_2:
  case FK_Data_4:
  case FK_Data_8:
  case FK_SecRel_2:
  case FK_SecRel_4:
  case Alpha::fixup_alpha_lituse_base:
  case Alpha::fixup_alpha_lituse_jsr:
  case Alpha::fixup_alpha_lituse_jsrdirect:
  case Alpha::fixup_alpha_lituse_bytoff:
  case Alpha::fixup_alpha_lituse_addr:
  case Alpha::fixup_alpha_lituse_tlsgd:
  case Alpha::fixup_alpha_lituse_tlsldm:
    return Value;
  case Alpha::fixup_alpha_gprelhigh:
  case Alpha::fixup_alpha_gprellow:
  case Alpha::fixup_alpha_literal:
    return Value & UINT16_MAX;
  }
}

bool AlphaAsmBackend::evaluateGPDisp(const MCAssembler &Asm,
                                     const MCAsmLayout &Layout,
                                     const MCFixup &Fixup, const MCFragment *DF,
                                     const MCValue &Target, uint64_t &Value,
                                     bool &WasForced) {
  const MCFixup *GPDispFixup;
  const MCFragment *GPDispDF;
  MCValue GPDispTarget;
  switch (Fixup.getTargetKind()) {
  default:
    llvm_unreachable("Unexpected fixup kind!");
  case Alpha::fixup_alpha_gpdisp:
    GPDispFixup = &Fixup;
    GPDispDF = DF;
    GPDispTarget = Target;
    break;
  }

  if (!GPDispTarget.getSymA() || GPDispTarget.getSymB()) 
    return false;
  }

  const MCSymbolRefExpr *A = GPDispTarget.getSymA();
  const MCSymbol &SA = A->getSymbol();
  if (A->getKind() != MCSymbolRefExpr::VK_None) 
    return false;

  if (SA.isUndefined()) 
    return false;

  auto *Writer = Asm.getWriterPtr();
  if (!Writer) 
    return false;

  bool IsResolved = Writer->isSymbolRefDifferenceFullyResolvedImpl(
      Asm, SA, *GPDispDF, false, true);
  if (!IsResolved) 
    return false;

  Value = Layout.getSymbolOffset(SA) + GPDispTarget.getConstant();
  Value -= Layout.getFragmentOffset(GPDispDF) + GPDispFixup->getOffset();

  if (shouldForceRelocation(Asm, *GPDispFixup, GPDispTarget)) {
    WasForced = true;
    return false;
  }

  return true;
}

bool AlphaAsmBackend::evaluateTargetFixup(const MCAssembler &Asm,
                                          const MCAsmLayout &Layout,
                                          const MCFixup &Fixup,
                                          const MCFragment *DF,
                                          const MCValue &Target,
                                          uint64_t &Value, bool &WasForced) {
  switch (Fixup.getTargetKind()) {
  default:
    llvm_unreachable("Unexpected fixup kind!");
  case Alpha::fixup_alpha_gpdisp_lda:
    return true;
  case Alpha::fixup_alpha_gpdisp:
    return evaluateGPDisp(Asm, Layout, Fixup, DF, Target, Value, WasForced);
  }

  return true;
}

void AlphaAsmBackend::applyFixup(const MCAssembler &Asm, const MCFixup &Fixup,
                                 const MCValue &Target,
                                 MutableArrayRef<char> Data, uint64_t Value,
                                 bool IsResolved,
                                 const MCSubtargetInfo *STI) const {
  MCFixupKind Kind = Fixup.getKind();
  if (Kind >= FirstLiteralRelocationKind)
    return;
  MCContext &Ctx = Asm.getContext();
  MCFixupKindInfo Info = getFixupKindInfo(Kind);
  if (!Value)
    return; // Doesn't change encoding.
  // Apply any target-specific value adjustments.
  Value = adjustFixupValue(Fixup, Value, Ctx);

  // Shift the value into position.
  Value <<= Info.TargetOffset;

  unsigned Offset = Fixup.getOffset();
  unsigned NumBytes = alignTo(Info.TargetSize + Info.TargetOffset, 8) / 8;

  assert(Offset + NumBytes <= Data.size() && "Invalid fixup offset!");

  // For each byte of the fragment that the fixup touches, mask in the
  // bits from the fixup value.
  for (unsigned i = 0; i != NumBytes; ++i) {
    Data[Offset + i] |= uint8_t((Value >> (i * 8)) & 0xff);
  }
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