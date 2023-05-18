//===-- AlphaMCCodeEmitter.cpp - Convert Alpha code to machine code -------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file implements the AlphaMCCodeEmitter class.
//
//===----------------------------------------------------------------------===//

#include "MCTargetDesc/AlphaFixupKinds.h"
#include "MCTargetDesc/AlphaMCExpr.h"
#include "MCTargetDesc/AlphaMCTargetDesc.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/MC/MCAsmInfo.h"
#include "llvm/MC/MCCodeEmitter.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCInst.h"
#include "llvm/MC/MCRegisterInfo.h"
#include "llvm/MC/MCSymbol.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/EndianStream.h"
#include "llvm/Support/raw_ostream.h"

using namespace llvm;

#define DEBUG_TYPE "mccodeemitter"

STATISTIC(MCNumEmitted, "Number of MC instructions emitted");
STATISTIC(MCNumFixups, "Number of MC fixups created");

namespace {
class AlphaMCCodeEmitter : public MCCodeEmitter {
  AlphaMCCodeEmitter(const AlphaMCCodeEmitter &) = delete;
  void operator=(const AlphaMCCodeEmitter &) = delete;
  MCContext &Ctx;
  const MCInstrInfo &MII;

public:
  AlphaMCCodeEmitter(MCContext &Ctx, const MCInstrInfo &MII)
      : Ctx(Ctx), MII(MII) {}

  ~AlphaMCCodeEmitter() override {}

  void encodeInstruction(const MCInst &MI, raw_ostream &OS,
                         SmallVectorImpl<MCFixup> &Fixups,
                         const MCSubtargetInfo &STI) const override;

  /// TableGen'erated function for getting the binary encoding for an
  /// instruction.
  uint64_t getBinaryCodeForInstr(const MCInst &MI,
                                 SmallVectorImpl<MCFixup> &Fixups,
                                 const MCSubtargetInfo &STI) const;

  /// Return binary encoding of operand. If the machine operand requires
  /// relocation, record the relocation and return zero.
  unsigned getMachineOpValue(const MCInst &MI, const MCOperand &MO,
                             SmallVectorImpl<MCFixup> &Fixups,
                             const MCSubtargetInfo &STI) const;

  unsigned getImmOpValue(const MCInst &MI, unsigned OpNo,
                         SmallVectorImpl<MCFixup> &Fixups,
                         const MCSubtargetInfo &STI) const;
};
} // end anonymous namespace

MCCodeEmitter *llvm::createAlphaMCCodeEmitter(const MCInstrInfo &MCII,
                                              MCContext &Ctx) {
  return new AlphaMCCodeEmitter(Ctx, MCII);
}

void AlphaMCCodeEmitter::encodeInstruction(const MCInst &MI, raw_ostream &OS,
                                           SmallVectorImpl<MCFixup> &Fixups,
                                           const MCSubtargetInfo &STI) const {
  uint32_t Bits = getBinaryCodeForInstr(MI, Fixups, STI);
  support::endian::write(OS, Bits, support::little);
  ++MCNumEmitted; // Keep track of the # of mi's emitted.
}

unsigned
AlphaMCCodeEmitter::getMachineOpValue(const MCInst &MI, const MCOperand &MO,
                                      SmallVectorImpl<MCFixup> &Fixups,
                                      const MCSubtargetInfo &STI) const {

  if (MO.isReg())
    return Ctx.getRegisterInfo()->getEncodingValue(MO.getReg());

  if (MO.isImm())
    return static_cast<unsigned>(MO.getImm());

  llvm_unreachable("Unhandled expression!");
  return 0;
}

unsigned AlphaMCCodeEmitter::getImmOpValue(const MCInst &MI, unsigned OpNo,
                                           SmallVectorImpl<MCFixup> &Fixups,
                                           const MCSubtargetInfo &STI) const {
  const MCOperand &MO = MI.getOperand(OpNo);

  // If the destination is an immediate, there is nothing to do.
  if (MO.isImm())
    return MO.getImm();

  assert(MO.isExpr() && "getImmOpValue expects only expressions or immediates");
  const MCExpr *Expr = MO.getExpr();
  MCExpr::ExprKind Kind = Expr->getKind();
  Alpha::Fixups FixupKind = Alpha::fixup_alpha_invalid;
  if (Kind == MCExpr::Target) {
    const AlphaMCExpr *AlphaExpr = cast<AlphaMCExpr>(Expr);

    switch (AlphaExpr->getKind()) {
    case AlphaMCExpr::VK_ALPHA_None:
    case AlphaMCExpr::VK_ALPHA_Invalid:
      llvm_unreachable("Unhandled fixup kind!");
    case AlphaMCExpr::VK_ALPHA_LITERAL:
      FixupKind = Alpha::fixup_alpha_literal;
      break;
    case AlphaMCExpr::VK_ALPHA_GPDISP:
      FixupKind = Alpha::fixup_alpha_gpdisp;
      break;
    case AlphaMCExpr::VK_ALPHA_GPDISP_LDA:
      FixupKind = Alpha::fixup_alpha_gpdisp_lda;
      break;
    case AlphaMCExpr::VK_ALPHA_GPRELHIGH:
      FixupKind = Alpha::fixup_alpha_gprelhigh;
      break;
    case AlphaMCExpr::VK_ALPHA_GPRELLOW:
      FixupKind = Alpha::fixup_alpha_gprellow;
      break;
    case AlphaMCExpr::VK_ALPHA_LITUSE_ADDR:
      FixupKind = Alpha::fixup_alpha_lituse_addr;
      break;
    case AlphaMCExpr::VK_ALPHA_LITUSE_BASE:
      FixupKind = Alpha::fixup_alpha_lituse_base;
      break;
    case AlphaMCExpr::VK_ALPHA_LITUSE_BYTOFF:
      FixupKind = Alpha::fixup_alpha_lituse_bytoff;
      break;
    case AlphaMCExpr::VK_ALPHA_LITUSE_JSR:
      FixupKind = Alpha::fixup_alpha_lituse_jsr;
      break;
    case AlphaMCExpr::VK_ALPHA_LITUSE_JSRDIRECT:
      FixupKind = Alpha::fixup_alpha_lituse_jsrdirect;
      break;
    case AlphaMCExpr::VK_ALPHA_LITUSE_TLSGD:
      FixupKind = Alpha::fixup_alpha_lituse_tlsgd;
      break;
    case AlphaMCExpr::VK_ALPHA_LITUSE_TLSLDM:
      FixupKind = Alpha::fixup_alpha_lituse_tlsldm;
      break;
    }
  }

  assert(FixupKind != Alpha::fixup_alpha_invalid && "Unhandled expression!");

  Fixups.push_back(
      MCFixup::create(0, Expr, MCFixupKind(FixupKind), MI.getLoc()));
  ++MCNumFixups;

  return 0;
}

#include "AlphaGenMCCodeEmitter.inc"