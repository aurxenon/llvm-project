//===-- AlphaMCExpr.cpp - Alpha specific MC expression classes ------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file contains the implementation of the assembly expression modifiers
// accepted by the Alpha architecture (e.g. "!literal!", "!gpdisp!", ...).
//
//===----------------------------------------------------------------------===//

#include "AlphaMCExpr.h"
#include "AlphaFixupKinds.h"
#include "MCTargetDesc/AlphaAsmBackend.h"
#include "llvm/BinaryFormat/ELF.h"
#include "llvm/MC/MCAsmLayout.h"
#include "llvm/MC/MCAssembler.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCStreamer.h"
#include "llvm/MC/MCSymbolELF.h"
#include "llvm/MC/MCValue.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/ErrorHandling.h"

using namespace llvm;

#define DEBUG_TYPE "alphamcexpr"

const AlphaMCExpr *AlphaMCExpr::create(const MCExpr *Expr, VariantKind Kind,
                                       MCContext &Ctx, int SequenceNumber) {
  return new (Ctx) AlphaMCExpr(Expr, Kind, SequenceNumber);
}

void AlphaMCExpr::printImpl(raw_ostream &OS, const MCAsmInfo *MAI) const {
  VariantKind Kind = getKind();
  bool HasVariant = (Kind != VK_ALPHA_None);

  if (HasVariant) {
    Expr->print(OS, MAI);
    OS << " !" << getVariantKindName(getKind());
    if (getSequenceNumber())
      OS << "!" << getSequenceNumber();
  }
}

const MCFixup *AlphaMCExpr::getGPDisp(const MCFragment **DFOut) const {
  MCValue GPDispLoc;
  if (!getSubExpr()->evaluateAsRelocatable(GPDispLoc, nullptr, nullptr))
    return nullptr;

  const MCSymbolRefExpr *GPDispSRE = GPDispLoc.getSymA();
  if (!GPDispSRE)
    return nullptr;

  const MCSymbol *GPDispSymbol = &GPDispSRE->getSymbol();
  const auto *DF = dyn_cast_or_null<MCDataFragment>(GPDispSymbol->getFragment());

  if (!DF)
    return nullptr;

  uint64_t Offset = GPDispSymbol->getOffset();
  if (DF->getContents().size() == Offset) {
    DF = dyn_cast_or_null<MCDataFragment>(DF->getNextNode());
    if (!DF)
      return nullptr;
    Offset = 0;
  }

  for (const MCFixup &F : DF->getFixups()) {
    if (F.getOffset() != Offset)
      continue;

    switch ((unsigned)F.getKind()) {
    default:
      continue;
    case Alpha::fixup_alpha_gpdisp_lda:
      if (DFOut)
        *DFOut = DF;
      return &F;
    }
  }

  return nullptr;
}

bool AlphaMCExpr::evaluateAsRelocatableImpl(MCValue &Res,
                                            const MCAsmLayout *Layout,
                                            const MCFixup *Fixup) const {
  // Explicitly drop the layout and assembler to prevent any symbolic folding in
  // the expression handling.  This is required to preserve symbolic difference
  // expressions to emit the paired relocations.
  if (!getSubExpr()->evaluateAsRelocatable(Res, nullptr, nullptr))
    return false;

  Res =
      MCValue::get(Res.getSymA(), Res.getSymB(), Res.getConstant(), getKind());
  // Custom fixup types are not valid with symbol difference expressions.
  return Res.getSymB() ? getKind() == VK_ALPHA_None : true;
}

bool AlphaMCExpr::evaluateAsConstant(int64_t &Res) const {
  MCValue Value;

  if (Kind == VK_ALPHA_GPDISP || Kind == VK_ALPHA_GPDISP_LDA ||
      Kind == VK_ALPHA_LITERAL)
    return false;

  if (!getSubExpr()->evaluateAsRelocatable(Value, nullptr, nullptr))
    return false;

  if (!Value.isAbsolute())
    return false;

  Res = evaluateAsInt64(Value.getConstant());
  return true;
}

int64_t AlphaMCExpr::evaluateAsInt64(int64_t Value) const {
  switch (Kind) {
  default:
    llvm_unreachable("Invalid kind");
  case VK_ALPHA_LITERAL:
    return SignExtend64<16>(Value);
  }
}

void AlphaMCExpr::visitUsedExpr(MCStreamer &Streamer) const {
  Streamer.visitUsedExpr(*getSubExpr());
}

StringRef AlphaMCExpr::getVariantKindName(VariantKind Kind) {
  switch (Kind) {
  case VK_ALPHA_Invalid:
  case VK_ALPHA_None:
    llvm_unreachable("Invalid ELF symbol kind");
  case VK_ALPHA_LITERAL:
    return "literal";
  case VK_ALPHA_GPDISP:
    return "gpdisp";
  case VK_ALPHA_GPDISP_LDA:
    return "gpdisp";
  case VK_ALPHA_GPRELHIGH:
    return "gprelhigh";
  case VK_ALPHA_GPRELLOW:
    return "gprellow";
  case VK_ALPHA_LITUSE_BASE:
    return "lituse_base";
  case VK_ALPHA_LITUSE_JSR:
    return "lituse_jsr";
  case VK_ALPHA_LITUSE_JSRDIRECT:
    return "lituse_jsrdirect";
  case VK_ALPHA_LITUSE_BYTOFF:
    return "lituse_bytoff";
  case VK_ALPHA_LITUSE_ADDR:
    return "lituse_addr";
  case VK_ALPHA_LITUSE_TLSGD:
    return "lituse_tlsgd";
  case VK_ALPHA_LITUSE_TLSLDM:
    return "lituse_tlsldm";
  }
  llvm_unreachable("Invalid ELF symbol kind");
}