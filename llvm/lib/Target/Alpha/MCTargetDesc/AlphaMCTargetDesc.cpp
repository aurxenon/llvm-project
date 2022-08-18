//===-- AlphaMCTargetDesc.cpp - Alpha Target Descriptions -----------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// This file provides Alpha-specific target descriptions.
///
//===----------------------------------------------------------------------===//

#include "AlphaMCTargetDesc.h"
#include "AlphaMCAsmInfo.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/MC/MCAsmInfo.h"
#include "llvm/MC/MCInstrInfo.h"
#include "llvm/MC/MCRegisterInfo.h"
#include "llvm/MC/MCStreamer.h"
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Support/ErrorHandling.h"

#define GET_INSTRINFO_MC_DESC
#include "AlphaGenInstrInfo.inc"

#define GET_REGINFO_MC_DESC
#include "AlphaGenRegisterInfo.inc"

using namespace llvm;

static MCInstrInfo *createAlphaMCInstrInfo() {
  MCInstrInfo *X = new MCInstrInfo();
  InitAlphaMCInstrInfo(X);
  return X;
}

static MCRegisterInfo *createAlphaMCRegisterInfo(const Triple &TT) {
  MCRegisterInfo *X = new MCRegisterInfo();
  InitAlphaMCRegisterInfo(X, Alpha::R26);
  return X;
}

static MCAsmInfo *createAlphaMCAsmInfo(const MCRegisterInfo &MRI,
                                       const Triple &TT,
                                       const MCTargetOptions &Options) {
  return new AlphaMCAsmInfo(TT);
}

extern "C" void LLVMInitializeAlphaTargetMC() {
  TargetRegistry::RegisterMCAsmInfo(getTheAlphaTarget(), createAlphaMCAsmInfo);
  TargetRegistry::RegisterMCInstrInfo(getTheAlphaTarget(),
                                      createAlphaMCInstrInfo);
  TargetRegistry::RegisterMCRegInfo(getTheAlphaTarget(),
                                    createAlphaMCRegisterInfo);
  TargetRegistry::RegisterMCAsmBackend(getTheAlphaTarget(),
                                       createAlphaAsmBackend);
  TargetRegistry::RegisterMCCodeEmitter(getTheAlphaTarget(),
                                        createAlphaMCCodeEmitter);
}