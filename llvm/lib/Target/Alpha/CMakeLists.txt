add_llvm_component_group(Alpha)

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

add_subdirectory(TargetInfo)