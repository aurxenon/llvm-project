//===-- AlphaMCAsmInfo.cpp - Alpha Asm Properties -------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file contains the declarations of the AlphaMCAsmInfo properties.
//
//===----------------------------------------------------------------------===//

#include "AlphaMCAsmInfo.h"
#include "llvm/ADT/Triple.h"

using namespace llvm;

void AlphaMCAsmInfo::anchor() {}

AlphaMCAsmInfo::AlphaMCAsmInfo(const Triple &TT) {
  CodePointerSize = 8;
  CalleeSaveStackSlotSize = 8;
  CommentString = "#";
  AlignmentIsInBytes = false;
  SupportsDebugInformation = true;
}
