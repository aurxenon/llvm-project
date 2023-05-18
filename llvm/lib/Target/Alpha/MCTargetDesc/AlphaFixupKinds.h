//===-- AlphaFixupKinds.h - Alpha Specific Fixup Entries --------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_ALPHA_MCTARGETDESC_ALPHAFIXUPKINDS_H
#define LLVM_LIB_TARGET_ALPHA_MCTARGETDESC_ALPHAFIXUPKINDS_H

#include "llvm/MC/MCFixup.h"

namespace llvm {
namespace Alpha {
enum Fixups {
  // 16-bit GP-relative fixup for loading GOT addresses via ldq instruction
  fixup_alpha_literal = FirstTargetFixupKind,
  // Fixup for loading the GP address, this attaches to LDAH
  fixup_alpha_gpdisp,
  // Fixup for loading the GP address, this attaches to LDA
  fixup_alpha_gpdisp_lda,
  // 16-bit fixup for high bits of 32-bit GP-relative offset loaded by ldah
  fixup_alpha_gprelhigh,
  // 16-bit fixup for low bits of 32-bit GP-relative offset
  fixup_alpha_gprellow,
  
  // Notification for linker of how literal is being used
  fixup_alpha_lituse_base,
  fixup_alpha_lituse_jsr,
  fixup_alpha_lituse_jsrdirect,
  fixup_alpha_lituse_bytoff,
  fixup_alpha_lituse_addr,
  fixup_alpha_lituse_tlsgd,
  fixup_alpha_lituse_tlsldm,
  
  // Used as a sentinel, must be the last
  fixup_alpha_invalid,
  NumTargetFixupKinds = fixup_alpha_invalid - FirstTargetFixupKind
};
} // end namespace Alpha
} // end namespace llvm

#endif