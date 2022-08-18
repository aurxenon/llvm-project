//===-- AlphaMCTargetDesc.h - Alpha Target Descriptions ---------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file provides Alpha specific target descriptions.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_ALPHA_MCTARGETDESC_ALPHAMCTARGETDESC_H
#define LLVM_LIB_TARGET_ALPHA_MCTARGETDESC_ALPHAMCTARGETDESC_H

#include "llvm/Config/config.h"
#include "llvm/MC/MCAsmBackend.h"
#include "llvm/MC/MCTargetOptions.h"
#include "llvm/Support/DataTypes.h"
#include <memory>

namespace llvm {
class MCAsmBackend;
class MCCodeEmitter;
class MCContext;
class MCInstrInfo;
class MCRegisterInfo;
class MCObjectTargetWriter;
class MCRegisterInfo;
class MCSubtargetInfo;
class Target;
class Triple;

Target &getTheAlphaTarget();

MCCodeEmitter *createAlphaMCCodeEmitter(const MCInstrInfo &MCII,
                                        MCContext &Ctx);

MCAsmBackend *createAlphaAsmBackend(const Target &T, const MCSubtargetInfo &STI,
                                    const MCRegisterInfo &MRI,
                                    const MCTargetOptions &Options);

std::unique_ptr<MCObjectTargetWriter> createAlphaELFObjectWriter(uint8_t OSABI);
} // namespace llvm

// Defines symbolic names for the Alpha registers.
#define GET_REGINFO_ENUM
#include "AlphaGenRegisterInfo.inc"

// Defines symbolic names for the Alpha instructions.
#define GET_INSTRINFO_ENUM
#include "AlphaGenInstrInfo.inc"

#endif