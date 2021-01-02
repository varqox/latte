#pragma once

#define DEFINE_CMP_OPERATORS_BY_FIELD(Type, field)                  \
    friend bool operator==(const Type& a, const Type& b) noexcept { \
        return a.field == b.field;                                  \
    }                                                               \
    friend bool operator!=(const Type& a, const Type& b) noexcept { \
        return a.field != b.field;                                  \
    }                                                               \
    friend bool operator<(const Type& a, const Type& b) noexcept {  \
        return a.field < b.field;                                   \
    }                                                               \
    friend bool operator<=(const Type& a, const Type& b) noexcept { \
        return a.field <= b.field;                                  \
    }                                                               \
    friend bool operator>(const Type& a, const Type& b) noexcept {  \
        return a.field > b.field;                                   \
    }                                                               \
    friend bool operator>=(const Type& a, const Type& b) noexcept { \
        return a.field >= b.field;                                  \
    }
