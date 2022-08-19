//===-- Alpha.td - Describe the Alpha Target Machine -------*- tablegen -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

include "llvm/Target/Target.td"

def FeatureBWX : SubtargetFeature<"bwx", "HasBWX", "true",
                                    "Use Byte-Word Extensions">;
def FeatureMVI : SubtargetFeature<"mvi", "HasMVI", "true",
                                    "Use Motion Video Instructions">;
def FeatureFIX : SubtargetFeature<"fix", "HasFIX", "true",
                                    "Use Floating-point Extensions">;
def FeatureCIX : SubtargetFeature<"cix", "HasCIX", "true",
                                    "Use Count Extensions">;

include "AlphaRegisterInfo.td"
include "AlphaInstrInfo.td"

def : ProcessorModel<"ev5", NoSchedModel, []>;
def : ProcessorModel<"ev56", NoSchedModel, [FeatureBWX]>;
def : ProcessorModel<"pca56", NoSchedModel, [FeatureBWX, FeatureMVI]>;
def : ProcessorModel<"ev6", NoSchedModel, [FeatureBWX, FeatureMVI, FeatureFIX]>;
def : ProcessorModel<"ev67", NoSchedModel, [FeatureBWX, FeatureMVI, FeatureFIX, FeatureCIX]>;

def AlphaInstrInfo : InstrInfo;

def AlphaAsmParser : AsmParser {
  let ShouldEmitMatchRegisterAltName = 1;
}

def Alpha : Target {
  let InstructionSet = AlphaInstrInfo;
  let AssemblyParsers = [AlphaAsmParser];
}