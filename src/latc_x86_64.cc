#include "src/ast/ast.hh"
#include "src/ast/build.hh"
#include "src/backend/x86_64.hh"
#include "src/concat.hh"
#include "src/ends_with.hh"
#include "src/frontend/error.hh"
#include "src/frontend/global_symbols.hh"
#include "src/frontend/static_analyzer.hh"
#include "src/frontend/type_checker.hh"
#include "src/ir/ast_to_ir.hh"

#include <cerrno>
#include <cstddef>
#include <sys/types.h>
#include <sys/wait.h>
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
[[noreturn]] static void fail(Arg1&& arg1, Args&&... args) {
    cerr << "\033[1;31merror: \033[0;1m" << std::forward<Arg1>(arg1);
    (cerr << ... << std::forward<Args>(args)) << "\033[m" << endl;
    _exit(1);
}

int execute_command(std::string command, std::vector<std::string> args, const char* stdout_file, const char* stderr_file) {
    pid_t child = fork();
    if (child < 0) {
        fail("failed to fork() - ", strerror(errno));
    }
    if (child == 0) {
        if (stdout_file and freopen(stdout_file, "w", stdout) == nullptr) {
            fail("failed to freopen() - ", strerror(errno));
        }
        if (stderr_file and freopen(stderr_file, "w", stderr) == nullptr) {
            fail("failed to freopen() - ", strerror(errno));
        }
        std::vector<char*> argv(args.size() + 2);
        argv[0] = command.data();
        for (size_t i = 0; i < args.size(); ++i) {
            argv[i + 1] = args[i].data();
        }
        argv.back() = nullptr;
        execvp(command.data(), argv.data());
        fail("failed to execvp() - ", strerror(errno));
    }
    // Parent
    int status{};
    if (waitpid(child, &status, 0) < 0) {
        fail("failed to waitpid() - ", strerror(errno));
    }
    return status;
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
            fail("failed to fseek() - ", strerror(errno));
        }
        file_contents.resize(ftell(file));
        if (fseek(file, 0, SEEK_SET) < 0) {
            fail("failed to fseek() - ", strerror(errno));
        }
        file_contents.resize(fread(file_contents.data(), 1, file_contents.size(), file));
        if (ferror(file)) {
            fail("reading error");
        }
        fclose(file);
    }
    frontend::ErrorPrinter error_printer{filename, file_contents};
    auto execute = [](std::string command, std::vector<std::string> args) {
        if (execute_command(command, args, "/dev/null", "/dev/null") == 0) {
            return;
        }
        std::cerr << "ERROR\n";
        execute_command(std::move(command), std::move(args), nullptr, nullptr);
        exit(1);
    };
    try {
        auto prog_ast = ast::build(file_contents, error_printer);
        auto global_symbols = frontend::collect_global_symbols(prog_ast, error_printer);
        frontend::check_and_annotate_types(prog_ast, global_symbols, error_printer);
        frontend::static_analyze(prog_ast, error_printer);
        auto ir_prog = ir::translate_ast_to_ir(prog_ast, global_symbols);
        auto asm_file_path = concat(base_path, ".s");
        {
            std::ofstream out{asm_file_path};
            backend::emit_x86_64(std::move(ir_prog), out);
        }
        auto obj_file_path = concat(base_path, ".o");
        execute("nasm", {"-f", "elf64", "-g", asm_file_path, "-o", obj_file_path});
        execute("gcc", {obj_file_path, "-o", base_path});
        std::cerr << "OK\n" << error_printer.get_all_diagnostics();
        return 0;
    } catch (const frontend::ErrorPrinter::ErrorOccurred&) {
        std::cerr << "ERROR\n" << error_printer.get_all_diagnostics();
        return 1;
    }
}
