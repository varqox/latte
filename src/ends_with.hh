#pragma once

#include <string_view>

inline bool ends_with(std::string_view str, std::string_view suffix) {
    return str.size() >= suffix.size() and str.substr(str.size() - suffix.size()) == suffix;
}
