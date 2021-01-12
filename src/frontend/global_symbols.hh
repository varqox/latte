#pragma once

#include "src/ast/ast.hh"
#include "src/frontend/error.hh"

namespace frontend {

// On error throws frontend::ErrorPrinter::ErrorOccurred
[[nodiscard]] ast::GlobalSymbols
collect_global_symbols(const ast::Program& prog, ErrorPrinter& errp);

} // namespace frontend
