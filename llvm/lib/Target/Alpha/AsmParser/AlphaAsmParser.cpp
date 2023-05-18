//===-- AlphaAsmParser.cpp - Parse Alpha assembly to MCInst instructions --===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
#define DEBUG_TYPE "asm-matcher"
#include "MCTargetDesc/AlphaMCExpr.h"
#include "MCTargetDesc/AlphaMCTargetDesc.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCInst.h"
#include "llvm/MC/MCInstrInfo.h"
#include "llvm/MC/MCParser/MCAsmLexer.h"
#include "llvm/MC/MCParser/MCParsedAsmOperand.h"
#include "llvm/MC/MCParser/MCTargetAsmParser.h"
#include "llvm/MC/MCRegisterInfo.h"
#include "llvm/MC/MCStreamer.h"
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/MC/MCValue.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Support/Casting.h"

using namespace llvm;

namespace {
struct AlphaOperand;

class AlphaAsmParser : public MCTargetAsmParser {

  struct RelocationTag {
    AlphaMCExpr::VariantKind VK;
    int64_t SequenceNumber;
  };
  std::unordered_map<SMLoc, RelocationTag> RelocationInfo;
  DenseMap<int64_t, MCSymbol *> TempSequenceNumberSymbols;

  SMLoc getLoc() const { return getParser().getTok().getLoc(); }

  bool generateImmOutOfRangeError(OperandVector &Operands, uint64_t ErrorInfo,
                                  int64_t Lower, int64_t Upper, Twine Msg);

  bool MatchAndEmitInstruction(SMLoc IDLoc, unsigned &Opcode,
                               OperandVector &Operands, MCStreamer &Out,
                               uint64_t &ErrorInfo,
                               bool MatchingInlineAsm) override;

  bool processInstruction(MCInst &Inst, SMLoc &IDLoc, OperandVector &Operands,
                          MCStreamer &Out);

  bool shouldEmitLabelForPairedRelocation(AlphaMCExpr::VariantKind VK);

  bool ParseRegister(unsigned &RegNo, SMLoc &StartLoc, SMLoc &EndLoc) override;

  OperandMatchResultTy tryParseRegister(unsigned &RegNo, SMLoc &StartLoc,
                                        SMLoc &EndLoc) override;

  bool ParseInstruction(ParseInstructionInfo &Info, StringRef Name,
                        SMLoc NameLoc, OperandVector &Operands) override;

  bool ParseDirective(AsmToken DirectiveID) override;

// Auto-generated instruction matching functions
#define GET_ASSEMBLER_HEADER
#include "AlphaGenAsmMatcher.inc"

  OperandMatchResultTy parseImmediate(OperandVector &Operands);
  OperandMatchResultTy parseRegister(OperandVector &Operands);
  OperandMatchResultTy parseTag(StringRef InstName, SMLoc &InstLoc);

  bool parseOperand(OperandVector &Operands);

public:
  enum AlphaMatchResultTy {
    Match_Dummy = FIRST_TARGET_MATCH_RESULT_TY,
#define GET_OPERAND_DIAGNOSTIC_TYPES
#include "AlphaGenAsmMatcher.inc"
#undef GET_OPERAND_DIAGNOSTIC_TYPES
  };

  static bool classifySymbolRef(const MCExpr *Expr,
                                AlphaMCExpr::VariantKind &Kind);

  AlphaAsmParser(const MCSubtargetInfo &STI, MCAsmParser &Parser,
                 const MCInstrInfo &MII, const MCTargetOptions &Options)
      : MCTargetAsmParser(Options, STI, MII) {
    setAvailableFeatures(ComputeAvailableFeatures(STI.getFeatureBits()));
  }
};

/// AlphaOperand - Instances of this class represent a parsed machine
/// instruction
struct AlphaOperand : public MCParsedAsmOperand {

  enum KindTy {
    Token,
    Register,
    Immediate,
  } Kind;

  struct RegOp {
    unsigned RegNum;
  };

  struct ImmOp {
    const MCExpr *Val;
  };

  SMLoc StartLoc, EndLoc;
  union {
    StringRef Tok;
    RegOp Reg;
    ImmOp Imm;
  };

  AlphaOperand(KindTy K) : MCParsedAsmOperand(), Kind(K) {}

public:
  AlphaOperand(const AlphaOperand &o) : MCParsedAsmOperand() {
    Kind = o.Kind;
    StartLoc = o.StartLoc;
    EndLoc = o.EndLoc;
    switch (Kind) {
    case Register:
      Reg = o.Reg;
      break;
    case Immediate:
      Imm = o.Imm;
      break;
    case Token:
      Tok = o.Tok;
      break;
    }
  }

  bool isToken() const override { return Kind == Token; }
  bool isReg() const override { return Kind == Register; }
  bool isImm() const override { return Kind == Immediate; }
  bool isMem() const override { return false; }

  static bool evaluateConstantImm(const MCExpr *Expr, int64_t &Imm,
                                  AlphaMCExpr::VariantKind &VK) {
    if (auto *AE = dyn_cast<AlphaMCExpr>(Expr)) {
      VK = AE->getKind();
      return AE->evaluateAsConstant(Imm);
    }

    if (auto CE = dyn_cast<MCConstantExpr>(Expr)) {
      VK = AlphaMCExpr::VK_ALPHA_None;
      Imm = CE->getValue();
      return true;
    }

    return false;
  }

  template <unsigned N> bool IsUImm() const {
    AlphaMCExpr::VariantKind VK = AlphaMCExpr::VK_ALPHA_None;
    int64_t Imm;
    if (!isImm())
      return false;
    bool IsConstantImm = evaluateConstantImm(getImm(), Imm, VK);
    return IsConstantImm && isUInt<N>(Imm);
  }

  template <unsigned N> bool IsSImm() const {
    AlphaMCExpr::VariantKind VK = AlphaMCExpr::VK_ALPHA_None;
    int64_t Imm;
    if (!isImm())
      return false;
    bool IsConstantImm = evaluateConstantImm(getImm(), Imm, VK);
    if (!IsConstantImm) {
      bool IsValid = AlphaAsmParser::classifySymbolRef(getImm(), VK);
      return IsValid && (VK == AlphaMCExpr::VK_ALPHA_GPDISP ||
                         VK == AlphaMCExpr::VK_ALPHA_GPDISP_LDA);
    } else {
      return IsConstantImm && isInt<N>(Imm);
    }
  }

  static bool tagNeedsSequenceNumber(AlphaMCExpr::VariantKind Tag) {
    switch (Tag) {
    default:
      return false;
    case AlphaMCExpr::VariantKind::VK_ALPHA_GPDISP:
    case AlphaMCExpr::VariantKind::VK_ALPHA_GPDISP_LDA:
    case AlphaMCExpr::VariantKind::VK_ALPHA_LITUSE_BASE:
    case AlphaMCExpr::VariantKind::VK_ALPHA_LITUSE_JSR:
    case AlphaMCExpr::VariantKind::VK_ALPHA_LITUSE_JSRDIRECT:
    case AlphaMCExpr::VariantKind::VK_ALPHA_LITUSE_BYTOFF:
    case AlphaMCExpr::VariantKind::VK_ALPHA_LITUSE_ADDR:
    case AlphaMCExpr::VariantKind::VK_ALPHA_LITUSE_TLSGD:
    case AlphaMCExpr::VariantKind::VK_ALPHA_LITUSE_TLSLDM:
      return true;
    }
  }

  static AlphaMCExpr::VariantKind getVariantKindForName(StringRef name) {
    return StringSwitch<AlphaMCExpr::VariantKind>(name)
        .Case("literal", AlphaMCExpr::VariantKind::VK_ALPHA_LITERAL)
        .Case("gpdisp", AlphaMCExpr::VariantKind::VK_ALPHA_GPDISP)
        .Case("gprelhigh", AlphaMCExpr::VariantKind::VK_ALPHA_GPRELHIGH)
        .Case("gprellow", AlphaMCExpr::VariantKind::VK_ALPHA_GPRELLOW)
        .Case("lituse_base", AlphaMCExpr::VariantKind::VK_ALPHA_LITUSE_BASE)
        .Case("lituse_jsr", AlphaMCExpr::VariantKind::VK_ALPHA_LITUSE_JSR)
        .Case("lituse_jsrdirect",
              AlphaMCExpr::VariantKind::VK_ALPHA_LITUSE_JSRDIRECT)
        .Case("lituse_bytoff", AlphaMCExpr::VariantKind::VK_ALPHA_LITUSE_BYTOFF)
        .Case("lituse_addr", AlphaMCExpr::VariantKind::VK_ALPHA_LITUSE_ADDR)
        .Case("lituse_tlsgd", AlphaMCExpr::VariantKind::VK_ALPHA_LITUSE_TLSGD)
        .Case("lituse_tlsldm", AlphaMCExpr::VariantKind::VK_ALPHA_LITUSE_TLSLDM)
        .Default(AlphaMCExpr::VariantKind::VK_ALPHA_Invalid);
  }

  bool isUImm8() { return IsUImm<8>(); }
  bool isSImm21() { return IsSImm<21>(); }
  bool isSImm16() { return IsSImm<16>(); }

  /// getStartLoc - Gets location of the first token of this operand
  SMLoc getStartLoc() const override { return StartLoc; }
  /// getEndLoc - Gets location of the last token of this operand
  SMLoc getEndLoc() const override { return EndLoc; }

  unsigned getReg() const override {
    assert(Kind == Register && "Invalid type access!");
    return Reg.RegNum;
  }

  const MCExpr *getImm() const {
    assert(Kind == Immediate && "Invalid type access!");
    return Imm.Val;
  }

  StringRef getToken() const {
    assert(Kind == Token && "Invalid type access!");
    return Tok;
  }

  void print(raw_ostream &OS) const override {
    switch (Kind) {
    case Immediate:
      OS << *getImm();
      break;
    case Register:
      OS << "<register ";
      OS << getReg() << ">";
      break;
    case Token:
      OS << "'" << getToken() << "'";
      break;
    }
  }

  static std::unique_ptr<AlphaOperand> createToken(StringRef Str, SMLoc S) {
    auto Op = std::make_unique<AlphaOperand>(Token);
    Op->Tok = Str;
    Op->StartLoc = S;
    Op->EndLoc = S;
    return Op;
  }

  static std::unique_ptr<AlphaOperand> createReg(unsigned RegNo, SMLoc S,
                                                 SMLoc E) {
    auto Op = std::make_unique<AlphaOperand>(Register);
    Op->Reg.RegNum = RegNo;
    Op->StartLoc = S;
    Op->EndLoc = E;
    return Op;
  }

  static std::unique_ptr<AlphaOperand> createImm(const MCExpr *Val, SMLoc S,
                                                 SMLoc E) {
    auto Op = std::make_unique<AlphaOperand>(Immediate);
    Op->Imm.Val = Val;
    Op->StartLoc = S;
    Op->EndLoc = E;
    return Op;
  }

  void addExpr(MCInst &Inst, const MCExpr *Expr) const {
    assert(Expr && "Expr shouldn't be null!");
    if (auto *CE = dyn_cast<MCConstantExpr>(Expr))
      Inst.addOperand(MCOperand::createImm(CE->getValue()));
    else
      Inst.addOperand(MCOperand::createExpr(Expr));
  }

  // Used by the TableGen Code
  void addRegOperands(MCInst &Inst, unsigned N) const {
    assert(N == 1 && "Invalid number of operands!");
    Inst.addOperand(MCOperand::createReg(getReg()));
  }

  void addImmOperands(MCInst &Inst, unsigned N) const {
    assert(N == 1 && "Invalid number of operands!");
    addExpr(Inst, getImm());
  }
};
} // end anonymous namespace.

#define GET_REGISTER_MATCHER
#define GET_MATCHER_IMPLEMENTATION
#include "AlphaGenAsmMatcher.inc"

bool AlphaAsmParser::generateImmOutOfRangeError(
    OperandVector &Operands, uint64_t ErrorInfo, int64_t Lower, int64_t Upper,
    Twine Msg = "immediate must be an integer in the range") {
  SMLoc ErrorLoc = ((AlphaOperand &)*Operands[ErrorInfo]).getStartLoc();
  return Error(ErrorLoc, Msg + " [" + Twine(Lower) + ", " + Twine(Upper) + "]");
}

bool AlphaAsmParser::MatchAndEmitInstruction(SMLoc IDLoc, unsigned &Opcode,
                                             OperandVector &Operands,
                                             MCStreamer &Out,
                                             uint64_t &ErrorInfo,
                                             bool MatchingInlineAsm) {
  MCInst Inst;
  SMLoc ErrorLoc;

  switch (MatchInstructionImpl(Operands, Inst, ErrorInfo, MatchingInlineAsm)) {
  default:
    break;
  case Match_Success:
    return processInstruction(Inst, IDLoc, Operands, Out);
  case Match_MissingFeature:
    return Error(IDLoc, "instruction use requires an option to be enabled");
  case Match_MnemonicFail:
    return Error(IDLoc, "unrecognized instruction mnemonic");
  case Match_InvalidOperand:
    ErrorLoc = IDLoc;
    if (ErrorInfo != ~0U) {
      if (ErrorInfo >= Operands.size())
        return Error(ErrorLoc, "too few operands for instruction");

      ErrorLoc = ((AlphaOperand &)*Operands[ErrorInfo]).getStartLoc();
      if (ErrorLoc == SMLoc())
        ErrorLoc = IDLoc;
    }
    return Error(ErrorLoc, "invalid operand for instruction");
  case Match_InvalidUImm8:
    return generateImmOutOfRangeError(Operands, ErrorInfo, 1, (1 << 8) - 1);
  }

  llvm_unreachable("Unknown match type detected!");
}

bool AlphaAsmParser::processInstruction(MCInst &Inst, SMLoc &IDLoc,
                                        OperandVector &Operands,
                                        MCStreamer &Out) {
  Inst.setLoc(IDLoc);
  MCContext &Ctx = getContext();

  // Check if there's an associated relocation tag for this insn
  auto RelocIt = RelocationInfo.find(IDLoc);
  if (RelocIt != RelocationInfo.end()) {
    MCSymbol *TempSym;
    RelocationTag &RelTag = RelocIt->second;

    auto SeqIt = TempSequenceNumberSymbols.find(RelTag.SequenceNumber);
    if (SeqIt != TempSequenceNumberSymbols.end()) {
      TempSym = SeqIt->second;
      if (shouldEmitLabelForPairedRelocation(RelTag.VK))
        Out.emitLabel(TempSym);
    } else {
      TempSym = Ctx.createTempSymbol();
      Out.emitLabel(TempSym);
    }

    const MCExpr *RefToSeqTmp = AlphaMCExpr::create(
        MCSymbolRefExpr::create(TempSym, Ctx), RelTag.VK, Ctx);
    auto ImmIt =
        std::find_if(Inst.begin(), Inst.end(),
                     [&](MCOperand &Operand) { return Operand.isImm(); });
    assert(ImmIt != Inst.end() &&
           "Failed to find index to apply relocation tag!");
    Inst.erase(ImmIt);
    Inst.insert(ImmIt, MCOperand::createExpr(RefToSeqTmp));
  }

  Out.emitInstruction(Inst, getSTI());
  return false;
}

bool AlphaAsmParser::shouldEmitLabelForPairedRelocation(
    AlphaMCExpr::VariantKind VK) {
  switch (VK) {
  default:
    return false;
  case AlphaMCExpr::VK_ALPHA_GPDISP_LDA:
    return true;
  }
}

bool AlphaAsmParser::ParseRegister(unsigned &RegNo, SMLoc &StartLoc,
                                   SMLoc &EndLoc) {
  const AsmToken &Tok = getParser().getTok();
  StartLoc = Tok.getLoc();
  EndLoc = Tok.getEndLoc();
  RegNo = 0;
  StringRef Name = getLexer().getTok().getIdentifier();

  if (!MatchRegisterName(Name)) {
    getParser().Lex(); // Eat identifier token.
    return false;
  }

  return Error(StartLoc, "invalid register name");
}

OperandMatchResultTy AlphaAsmParser::tryParseRegister(unsigned &RegNo,
                                                      SMLoc &StartLoc,
                                                      SMLoc &EndLoc) {
  llvm_unreachable("Unimplemented function.");
}

OperandMatchResultTy AlphaAsmParser::parseRegister(OperandVector &Operands) {
  SMLoc S = getLoc();
  SMLoc E = SMLoc::getFromPointer(S.getPointer() - 1);

  // Eat the $ prefix.
  if (getLexer().getKind() == AsmToken::Dollar) {
    getLexer().Lex();
  } else {
    return MatchOperand_NoMatch;
  }

  switch (getLexer().getKind()) {
  default:
    return MatchOperand_NoMatch;
  case AsmToken::Integer:
    StringRef Name = getLexer().getTok().getString();
    unsigned RegNo = MatchRegisterName(Name);
    if (!RegNo) {
        return MatchOperand_NoMatch;
    }
    getLexer().Lex();
    Operands.push_back(AlphaOperand::createReg(RegNo, S, E));
  }
  return MatchOperand_Success;
}

OperandMatchResultTy AlphaAsmParser::parseImmediate(OperandVector &Operands) {
  switch (getLexer().getKind()) {
  default:
    return MatchOperand_NoMatch;
  case AsmToken::LParen:
  case AsmToken::Minus:
  case AsmToken::Plus:
  case AsmToken::Integer:
  case AsmToken::String:
  case AsmToken::Identifier:
    break;
  }

  const MCExpr *IdVal;
  SMLoc S = getLoc();
  if (getParser().parseExpression(IdVal))
    return MatchOperand_ParseFail;

  SMLoc E = SMLoc::getFromPointer(S.getPointer() - 1);
  Operands.push_back(AlphaOperand::createImm(IdVal, S, E));
  return MatchOperand_Success;
}

/// Looks at a token type and creates the relevant operand
/// from this information, adding to Operands.
/// If operand was parsed, returns false, else true.
bool AlphaAsmParser::parseOperand(OperandVector &Operands) {
  // Attempt to parse token as register
  if (parseRegister(Operands) == MatchOperand_Success)
    return false;

  // Attempt to parse token as an immediate
  if (parseImmediate(Operands) == MatchOperand_Success)
    return false;

  // Finally we have exhausted all options and must declare defeat.
  Error(getLoc(), "unknown operand");
  return true;
}

OperandMatchResultTy AlphaAsmParser::parseTag(StringRef InstName,
                                              SMLoc &InstLoc) {
  MCContext &Ctx = getContext();
  int64_t SequenceNumber = -1;

  /// Alpha relocation tags are of the form !relocation!N with
  /// the last exclamation point and number N depending on the
  /// specific relocation being used.
  if (getLexer().is(AsmToken::Exclaim)) {
    getLexer().Lex(); // Consume exclamation point token

    if (getLexer().getKind() != AsmToken::Identifier) {
      Error(getLoc(), "expected valid identifier for operand modifier");
      return MatchOperand_ParseFail;
    }
    StringRef Identifier = getParser().getTok().getIdentifier();
    AlphaMCExpr::VariantKind VK =
        AlphaOperand::getVariantKindForName(Identifier);
    if (VK == AlphaMCExpr::VK_ALPHA_Invalid) {
      Error(getLoc(), "unrecognized operand modifier");
      return MatchOperand_ParseFail;
    } else if (VK == AlphaMCExpr::VK_ALPHA_GPDISP && InstName == "lda") {
      VK = AlphaMCExpr::VK_ALPHA_GPDISP_LDA;
    }
    getLexer().Lex(); // Consume the identifier

    /// Some Alpha relocation tags contain another exclamation point
    /// along with a sequence number, we need to consume these too.
    if (getLexer().is(AsmToken::Exclaim)) {
      if (VK != AlphaMCExpr::VariantKind::VK_ALPHA_LITERAL &&
          !AlphaOperand::tagNeedsSequenceNumber(VK)) {
        Error(getLoc(), "relocation tag does not accept a sequence number");
        return MatchOperand_ParseFail;
      }

      getLexer().Lex(); // Consume exclamation point token

      if (getParser().parseIntToken(SequenceNumber, "missing sequence number"))
        return MatchOperand_ParseFail;

      auto it = TempSequenceNumberSymbols.find(SequenceNumber);
      MCSymbol *SequenceSym;
      if (it == TempSequenceNumberSymbols.end()) {
        SequenceSym = Ctx.createTempSymbol();
        TempSequenceNumberSymbols.insert(
            std::make_pair(SequenceNumber, SequenceSym));
      } else {
        SequenceSym = it->second;
      }
    }

    RelocationInfo.insert(
        std::make_pair(InstLoc, RelocationTag{VK, SequenceNumber}));
    return MatchOperand_Success;
  }

  return MatchOperand_ParseFail;
}

bool AlphaAsmParser::ParseInstruction(ParseInstructionInfo &Info,
                                      StringRef Name, SMLoc NameLoc,
                                      OperandVector &Operands) {
  // First operand is token for instruction
  Operands.push_back(AlphaOperand::createToken(Name, NameLoc));

  // If there are no more operands, then finish
  if (getLexer().is(AsmToken::EndOfStatement))
    return false;

  // Parse first operand
  if (parseOperand(Operands))
    return true;

  // Parse until end of statement, consuming commas between operands
  while (getLexer().is(AsmToken::Comma)) {
    // Consume comma token
    getLexer().Lex();

    // Parse next operand
    if (parseOperand(Operands))
      return true;
  }

  if (getLexer().isNot(AsmToken::EndOfStatement)) {
    // Some instructions like ldah and lda contain a register within
    // parentheses.
    if (getLexer().is(AsmToken::LParen)) {
      SMLoc LParenLoc = getLexer().getLoc();
      getLexer().Lex();
      Operands.push_back(AlphaOperand::createToken("(", LParenLoc));

      if (parseRegister(Operands)) {
        SMLoc Loc = getLexer().getLoc();
        getParser().eatToEndOfStatement();
        return Error(Loc, "invalid register name");
      }

      if (getLexer().isNot(AsmToken::RParen)) {
        SMLoc Loc = getLexer().getLoc();
        getParser().eatToEndOfStatement();
        return Error(Loc, "improper terminator");
      }
      SMLoc RParenLoc = getLexer().getLoc();
      getLexer().Lex();
      Operands.push_back(AlphaOperand::createToken(")", RParenLoc));
    }

    // There might be a relocation tag and sequence number.
    if (getLexer().is(AsmToken::Exclaim)) {
      if (parseTag(Name, NameLoc)) {
        SMLoc Loc = getLexer().getLoc();
        getParser().eatToEndOfStatement();
        return Error(Loc, "unexpected token");
      }
    }
  }

  getParser().Lex(); // Consume the EndOfStatement.

  return false;
}

bool AlphaAsmParser::classifySymbolRef(const MCExpr *Expr,
                                       AlphaMCExpr::VariantKind &Kind) {
  Kind = AlphaMCExpr::VK_ALPHA_None;

  if (const AlphaMCExpr *AE = dyn_cast<AlphaMCExpr>(Expr)) {
    Kind = AE->getKind();
    Expr = AE->getSubExpr();
  }

  MCValue Res;
  MCFixup Fixup;
  if (Expr->evaluateAsRelocatable(Res, nullptr, &Fixup))
    return Res.getRefKind() == AlphaMCExpr::VK_ALPHA_None;
  return false;
}

bool AlphaAsmParser::ParseDirective(AsmToken DirectiveID) { return true; }

extern "C" void LLVMInitializeAlphaAsmParser() {
  RegisterMCAsmParser<AlphaAsmParser> X(getTheAlphaTarget());
}