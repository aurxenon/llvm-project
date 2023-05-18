add_llvm_component_library(LLVMAlphaDesc
  AlphaAsmBackend.cpp
  AlphaELFObjectWriter.cpp
  AlphaInstPrinter.cpp
  AlphaMCAsmInfo.cpp
  AlphaMCCodeEmitter.cpp
  AlphaMCExpr.cpp
  AlphaMCTargetDesc.cpp

  LINK_COMPONENTS
  AlphaInfo
  MC
  Support

  ADD_TO_COMPONENT
  Alpha
)