#pragma once

#include "src/ast/ast.hh"
#include "src/frontend/error.hh"

namespace ast {

// On error throws frontend::ErrorPrinter::ErrorOccurred
Program build(const std::string& file_contents, frontend::ErrorPrinter& errp);

} // namespace ast
