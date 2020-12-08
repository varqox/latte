#pragma once

#include "src/ast/ast.hh"
#include "src/frontend/error.hh"

namespace frontend {

// On error throws frontend::ErrorPrinter::ErrorOccurred
void check_and_annotate_types(
    ast::Program& prog, const ast::GlobalSymbols& global_symbols, ErrorPrinter& errp);

} // namespace frontend
