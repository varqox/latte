#pragma once

#include "src/ast/ast.hh"
#include "src/concat.hh"

#include <cstddef>
#include <exception>
#include <iostream>
#include <optional>
#include <stdexcept>
#include <string>
#include <string_view>
#include <utility>

namespace frontend {

class ErrorPrinter {
    const std::string_view source_file_path;
    const std::string_view source_file_contents;
    std::vector<size_t> lines_pos;
    std::string all_diagnostics;

public:
    ErrorPrinter(std::string_view source_file_path, std::string_view source_file_contents)
    : source_file_path{source_file_path}
    , source_file_contents(source_file_contents) {}

    ErrorPrinter(const ErrorPrinter&) = delete;
    ErrorPrinter(ErrorPrinter&&) = delete;
    ErrorPrinter& operator=(const ErrorPrinter&) = delete;
    ErrorPrinter& operator=(ErrorPrinter&&) = delete;
    ~ErrorPrinter() = default;

private:
    [[nodiscard]] std::optional<size_t> get_line_pos(size_t line) {
        if (lines_pos.empty()) {
            lines_pos.emplace_back(0);
            for (size_t pos = 0; pos < source_file_contents.size(); ++pos) {
                if (source_file_contents[pos] == '\n') {
                    lines_pos.emplace_back(pos + 1);
                }
            }
        }
        if (line <= 0 or static_cast<size_t>(line) > lines_pos.size()) {
            return std::nullopt;
        }
        return lines_pos[line - 1];
    }

    template <class... Args>
    void print(Args&&... args) {
        back_insert(all_diagnostics, std::forward<Args>(args)...);
    }

    void print_line_with_caret(ast::SrcLoc sloc) {
        auto line_pos_opt = get_line_pos(sloc.line);
        if (not line_pos_opt) {
            return; // Do not log invalid lines
        }
        size_t line_pos = *line_pos_opt;
        for (size_t pos = line_pos; pos < source_file_contents.size(); ++pos) {
            if (source_file_contents[pos] == '\n') {
                break;
            }
            print(source_file_contents[pos]);
        }
        print('\n');
        for (size_t pos = line_pos; pos < source_file_contents.size(); ++pos) {
            if (source_file_contents[pos] == '\n' or
                static_cast<int>(pos - line_pos + 1) == sloc.column) {
                break;
            }
            if (isspace(source_file_contents[pos])) {
                print(source_file_contents[pos]);
            } else if (isprint(source_file_contents[pos])) {
                print(' ');
            }
        }
        print("\033[1;32m^\033[m\n");
    }

public:
    struct ErrorOccurred : std::runtime_error {
        ErrorOccurred()
        : std::runtime_error("") {}
    };

private:
    template <class... Args>
    void note(ast::SrcLoc sloc, Args&&... description) {
        print(
            "\033[1m", source_file_path, ":", ast::as_str(sloc),
            ": \033[1;34mnote: \033[0;1m");
        print(std::forward<Args>(description)...);
        print("\033[m\n");
        print_line_with_caret(sloc);
    }

    struct ErrorNotes {
        ErrorPrinter& ep;

        explicit ErrorNotes(ErrorPrinter& ep)
        : ep{ep} {}

        ErrorNotes(const ErrorNotes&) = delete;
        ErrorNotes(ErrorNotes&&) = delete;
        ErrorNotes& operator=(const ErrorNotes&) = delete;
        ErrorNotes& operator=(ErrorNotes&&) = delete;

        ~ErrorNotes() noexcept(false) { throw ErrorOccurred{}; }

        template <class... Args>
        ErrorNotes& note(ast::SrcLoc sloc, Args&&... description) {
            ep.note(sloc, std::forward<Args>(description)...);
            return *this;
        }
    };

public:
    template <class... Args>
    ErrorNotes error(ast::SrcLoc sloc, Args&&... description) {
        print(
            "\033[1m", source_file_path, ":", ast::as_str(sloc),
            ": \033[1;31merror: \033[0;1m");
        print(std::forward<Args>(description)...);
        print("\033[m\n");
        print_line_with_caret(sloc);
        return ErrorNotes{*this};
    }

private:
    struct WarningNotes {
        ErrorPrinter& ep;

        explicit WarningNotes(ErrorPrinter& ep)
        : ep{ep} {}

        WarningNotes(const WarningNotes&) = delete;
        WarningNotes(WarningNotes&&) = delete;
        WarningNotes& operator=(const WarningNotes&) = delete;
        WarningNotes& operator=(WarningNotes&&) = delete;
        ~WarningNotes() = default;

        template <class... Args>
        WarningNotes& note(ast::SrcLoc sloc, Args&&... description) {
            ep.note(sloc, std::forward<Args>(description)...);
            return *this;
        }
    };

public:
    template <class... Args>
    WarningNotes warning(ast::SrcLoc sloc, Args&&... description) {
        print(
            "\033[1m", source_file_path, ":", ast::as_str(sloc),
            ": \033[1;35mwarning: \033[0;1m");
        print(std::forward<Args>(description)...);
        print("\033[m\n");
        print_line_with_caret(sloc);
        return WarningNotes{*this};
    }

    const std::string& get_all_diagnostics() noexcept { return all_diagnostics; }
};

} // namespace frontend
