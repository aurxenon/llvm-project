//===-- AlphaTargetInfo.cpp - Alpha Target Implementation -----------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "llvm/MC/TargetRegistry.h"

using namespace llvm;

namespace llvm {
Target &getTheAlphaTarget() {
  static Target TheAlphaTarget;
  return TheAlphaTarget;
}
} // namespace llvm

extern "C" void LLVMInitializeAlphaTargetInfo() {
  RegisterTarget<Triple::alpha> X(getTheAlphaTarget(), "alpha", "Alpha",
                                  "Alpha");
}