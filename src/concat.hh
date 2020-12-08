#pragma once

#include <string>
#include <type_traits>

namespace detail {

template <class T, class = decltype(std::declval<T>().size())>
constexpr auto has_method_size(int) -> std::true_type;
template <class>
constexpr auto has_method_size(...) -> std::false_type;

} // namespace detail

template <class T>
constexpr inline bool has_method_size = decltype(detail::has_method_size<T>(0))::value;

namespace detail {

template <class T, class = decltype(std::declval<T>().data())>
constexpr auto has_method_data(int) -> std::true_type;
template <class>
constexpr auto has_method_data(...) -> std::false_type;

} // namespace detail

template <class T>
constexpr inline bool has_method_data = decltype(detail::has_method_data<T>(0))::value;

template <class T, std::enable_if_t<has_method_size<const T&>, int> = 0>
constexpr auto string_length(const T& str) noexcept {
    return str.size();
}

constexpr auto string_length(const char* str) noexcept {
    return std::char_traits<char>::length(str);
}

constexpr auto string_length(char* str) noexcept {
    return std::char_traits<char>::length(str);
}

template <class T, std::enable_if_t<has_method_data<const T&>, int> = 0>
constexpr auto data(const T& x) noexcept {
    return x.data();
}

constexpr auto data(const char* x) noexcept { return x; }

constexpr auto data(char* x) noexcept { return x; }

template <class T>
constexpr decltype(auto) stringify(T&& x) {
    if constexpr (std::is_same_v<std::decay_t<T>, char>) {
        return std::string(1, x);
    } else if constexpr (std::is_integral_v<std::remove_cv_t<std::remove_reference_t<T>>>) {
        return std::to_string(x);
    } else {
        return std::forward<T>(x);
    }
}

namespace detail {

template <
    class T, class = decltype(string_length(stringify(std::forward<T>(std::declval<T>()))))>
constexpr auto is_string_argument(int) -> std::true_type;

template <class>
constexpr auto is_string_argument(...) -> std::false_type;

} // namespace detail

template <class T>
constexpr inline bool is_string_argument = decltype(detail::is_string_argument<T>(0))::value;

template <class... Args, std::enable_if_t<(is_string_argument<Args> and ...), int> = 0>
std::string concat(Args&&... args) {
    return [](auto&&... str) {
        size_t total_length = (0 + ... + string_length(str));
        std::string res;
        res.reserve(total_length);
        (void)(res += ... += std::forward<decltype(str)>(str));
        return res;
    }(stringify(std::forward<Args>(args))...);
}

template <class... Args, std::enable_if_t<(is_string_argument<Args> and ...), int> = 0>
std::string& back_insert(std::string& str, Args&&... args) {
    return [&str](auto&&... xx) -> std::string& {
        size_t total_length = (str.size() + ... + string_length(xx));
        // Allocate more space (reserve() is inefficient)
        size_t old_size = str.size();
        str.resize(total_length);
        str.resize(old_size);
        return (str += ... += std::forward<decltype(xx)>(xx));
    }(stringify(std::forward<Args>(args))...);
}
