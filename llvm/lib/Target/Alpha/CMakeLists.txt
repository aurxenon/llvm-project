add_llvm_component_group(Alpha)

set(LLVM_TARGET_DEFINITIONS Alpha.td)

tablegen(LLVM AlphaGenRegisterInfo.inc -gen-register-info)
tablegen(LLVM AlphaGenInstrInfo.inc -gen-instr-info)
tablegen(LLVM AlphaGenMCCodeEmitter.inc -gen-emitter)
tablegen(LLVM AlphaGenAsmMatcher.inc -gen-asm-matcher)

add_public_tablegen_target(AlphaCommonTableGen)

add_llvm_target(AlphaCodeGen
  AlphaTargetMachine.cpp

  LINK_COMPONENTS
  Analysis
  AsmPrinter
  CodeGen
  Core
  MC
  IPO
  Scalar
  SelectionDAG
  Support
  Target
  TransformUtils

  ADD_TO_COMPONENT
  Alpha
)

add_subdirectory(AsmParser)
add_subdirectory(MCTargetDesc)
add_subdirectory(TargetInfo)