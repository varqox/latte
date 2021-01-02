#pragma once

#include "src/ast/ast.hh"
#include "src/ir/ir.hh"

namespace ir {

Program
translate_ast_to_ir(const ast::Program& prog, const ast::GlobalSymbols& global_symbols);

} // namespace ir
