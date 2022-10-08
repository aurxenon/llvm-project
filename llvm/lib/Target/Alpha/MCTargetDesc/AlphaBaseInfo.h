//===-- AlphaBaseInfo.h - Top level definitions for Alpha MC ----*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file contains small standalone enum definitions for the Alpha target
// useful for the compiler back-end and the MC libraries.
//
//===----------------------------------------------------------------------===//
#ifndef LLVM_LIB_TARGET_ALPHA_MCTARGETDESC_ALPHABASEINFO_H
#define LLVM_LIB_TARGET_ALPHA_MCTARGETDESC_ALPHABASEINFO_H

#include "AlphaMCTargetDesc.h"

namespace llvm {

// AlphaII - This namespace holds all of the target specific flags that
// instruction info tracks. All definitions must match AlphaInstrFormats.td.
namespace AlphaII {
enum {
  InstFormatPseudo = 0,
  InstFormatIntOperate = 1,
  InstFormatLitOperate = 2,
  InstFormatFPOperate = 3,
  InstFormatMemory = 4,
  InstFormatBranch = 5,
  InstFormatPALcode = 6
};
} // namespace AlphaII

} // namespace llvm

#endif