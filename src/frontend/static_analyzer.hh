#pragma once

#include "src/ast/ast.hh"
#include "src/frontend/error.hh"

namespace frontend {

// On error throws frontend::ErrorPrinter::ErrorOccurred
void static_analyze(ast::Program& prog, ErrorPrinter& errp);

} // namespace frontend
