add_llvm_component_library(LLVMAlphaDesc
  AlphaAsmBackend.cpp
  AlphaELFObjectWriter.cpp
  AlphaMCAsmInfo.cpp
  AlphaMCCodeEmitter.cpp
  AlphaMCTargetDesc.cpp

  LINK_COMPONENTS
  AlphaInfo
  MC
  Support

  ADD_TO_COMPONENT
  Alpha
)