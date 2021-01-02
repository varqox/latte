#include "src/ast/ast.hh"
#include "src/ast/build.hh"
#include "src/concat.hh"
#include "src/frontend/error.hh"
#include "src/frontend/global_symbols.hh"
#include "src/frontend/static_analyzer.hh"
#include "src/frontend/type_checker.hh"
#include "src/ir/ast_to_ir.hh"

#include <cerrno>
#include <cstddef>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <fstream>
#include <iostream>
#include <string_view>
#include <type_traits>
#include <unistd.h>

using std::cerr;
using std::endl;
using std::string;
using std::string_view;

template <
    class Arg1, class... Args,
    std::enable_if_t<not std::is_constructible_v<const ast::SrcLoc&, Arg1&&>, int> = 0>
static void fail(Arg1&& arg1, Args&&... args) {
    cerr << "\033[1;31merror: \033[0;1m" << std::forward<Arg1>(arg1);
    (cerr << ... << std::forward<Args>(args)) << "\033[m" << endl;
    exit(1);
}

static bool ends_with(string_view str, string_view suffix) {
    return str.size() >= suffix.size() and str.substr(str.size() - suffix.size()) == suffix;
}

int main(int argc, char** argv) {
    if (argc != 2) {
        fail("invalid number of arguments, expected one argument");
    }
    string_view filename = argv[1];
    string_view suffix = ".lat";
    if (not ends_with(filename, suffix)) {
        fail("argument does not end with ", suffix);
    }
    string base_path = concat(filename.substr(0, filename.size() - suffix.size()));

    string file_contents;
    {
        FILE* file = fopen(filename.data(), "rbe");
        if (not file) {
            fail("failed to open file: ", filename);
        }
        if (fseek(file, 0, SEEK_END) < 0) {
            fail("failed to fseek()", strerror(errno));
        }
        file_contents.resize(ftell(file));
        if (fseek(file, 0, SEEK_SET) < 0) {
            fail("failed to fseek()", strerror(errno));
        }
        file_contents.resize(fread(file_contents.data(), 1, file_contents.size(), file));
        if (ferror(file)) {
            fail("reading error");
        }
        fclose(file);
    }
    frontend::ErrorPrinter error_printer{filename, file_contents};
    try {
        auto prog_ast = ast::build(file_contents, error_printer);
        auto global_symbols = frontend::collect_global_symbols(prog_ast, error_printer);
        frontend::check_and_annotate_types(prog_ast, global_symbols, error_printer);
        frontend::static_analyze(prog_ast, error_printer);
        auto ir_prog = ir::translate_ast_to_ir(prog_ast, global_symbols);
        std::cerr << "OK\n" << error_printer.get_all_diagnostics();
        return 0;
    } catch (const frontend::ErrorPrinter::ErrorOccurred&) {
        std::cerr << "ERROR\n" << error_printer.get_all_diagnostics();
        return 1;
    }
}
