//===-- AlphaMCExpr.h - Alpha specific MC expression classes ----*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file describes Alpha-specific MCExprs, used for modifiers like
// "!literal" or "!gpdisp!N" etc.,
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_ALPHA_MCTARGETDESC_ALPHAMCEXPR_H
#define LLVM_LIB_TARGET_ALPHA_MCTARGETDESC_ALPHAMCEXPR_H

#include "llvm/MC/MCExpr.h"

namespace llvm {

class StringRef;

class AlphaMCExpr : public MCTargetExpr {
public:
  enum VariantKind {
    VK_ALPHA_None,
    VK_ALPHA_LITERAL,
    VK_ALPHA_GPDISP,
    VK_ALPHA_GPDISP_LDA,
    VK_ALPHA_GPRELHIGH,
    VK_ALPHA_GPRELLOW,
    VK_ALPHA_LITUSE_BASE,
    VK_ALPHA_LITUSE_JSR,
    VK_ALPHA_LITUSE_JSRDIRECT,
    VK_ALPHA_LITUSE_BYTOFF,
    VK_ALPHA_LITUSE_ADDR,
    VK_ALPHA_LITUSE_TLSGD,
    VK_ALPHA_LITUSE_TLSLDM,
    VK_ALPHA_Invalid // Must be the last item
  };

private:
  const MCExpr *Expr;
  const VariantKind Kind;
  const int SequenceNumber;

  explicit AlphaMCExpr(const MCExpr *Expr, VariantKind Kind, int SequenceNumber)
      : Expr(Expr), Kind(Kind), SequenceNumber(SequenceNumber) {}

  int64_t evaluateAsInt64(int64_t Value) const;

public:
  static const AlphaMCExpr *create(const MCExpr *Expr, VariantKind Kind,
                                   MCContext &Ctx,
                                   int SequenceNumber = -1);

  int getSequenceNumber() const { return SequenceNumber; }

  VariantKind getKind() const { return Kind; }

  const MCExpr *getSubExpr() const { return Expr; }

  void printImpl(raw_ostream &OS, const MCAsmInfo *MAI) const override;
  const MCFixup *getGPDisp(const MCFragment **DFOut) const;
  bool evaluateAsRelocatableImpl(MCValue &Res, const MCAsmLayout *Layout,
                                 const MCFixup *Fixup) const override;
  void visitUsedExpr(MCStreamer &Streamer) const override;
  MCFragment *findAssociatedFragment() const override {
    return getSubExpr()->findAssociatedFragment();
  }

  // There are no TLS AlphaMCExprs at the moment.
  void fixELFSymbolsInTLSFixups(MCAssembler &Asm) const override {}

  bool evaluateAsConstant(int64_t &Res) const;

  static bool classof(const MCExpr *E) {
    return E->getKind() == MCExpr::Target;
  }

  static bool classof(const AlphaMCExpr *) { return true; }

  static VariantKind getVariantKindForName(StringRef name);
  static StringRef getVariantKindName(VariantKind Kind);
};

} // end namespace llvm.

#endif
