add_llvm_component_library(LLVMAlphaDisassembler
  AlphaDisassembler.cpp

  LINK_COMPONENTS
  MC
  MCDisassembler
  AlphaDesc
  AlphaInfo
  Support

  ADD_TO_COMPONENT
  Alpha
  )
