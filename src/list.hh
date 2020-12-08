#pragma once

#include <memory>
#include <type_traits>

template <class T>
class List {
    struct Elem {
        T val;
        std::unique_ptr<Elem> next;
    };

    std::unique_ptr<Elem> head;
    Elem* tail = nullptr;

public:
    List() noexcept = default;

    template <
        class... Arg, std::enable_if_t<(std::is_constructible_v<T, Arg> and ...), int> = 0>
    explicit List(T first, Arg&&... rest) {
        append(std::move(first));
        (append(std::forward<Arg>(rest)), ...);
    }

    void append(T val) {
        Elem elem = {
            .val = std::move(val),
            .next = nullptr,
        };
        auto uptr = std::make_unique<Elem>(std::move(elem));
        auto ptr = uptr.get();
        (tail ? tail->next : head) = std::move(uptr);
        tail = ptr;
    }

    friend List merge(List first, List second) {
        if (not first.head) {
            return second;
        }
        if (second.head) {
            first.tail->next = std::move(second.head);
            first.tail = second.tail;
        }
        return first;
    }

    [[nodiscard]] bool is_empty() const noexcept { return not head; }

    List clone() const {
        List copy;
        for (auto const& elem : *this) {
            copy.append(elem);
        }
        return copy;
    }

    template <class TElem>
    class Iterator {
        friend class List;
        TElem* elem = nullptr;

        explicit Iterator(TElem* elem)
        : elem{elem} {}

    public:
        Iterator() noexcept = default;
        Iterator(const Iterator&) noexcept = default;
        Iterator(Iterator&&) noexcept = default;
        Iterator& operator=(const Iterator&) noexcept = default;
        Iterator& operator=(Iterator&&) noexcept = default;
        ~Iterator() = default;

        Iterator& operator++() noexcept {
            elem = elem->next.get();
            return *this;
        }

        Iterator operator++() const noexcept { return ++Iterator(this); }

        auto& operator*() const noexcept { return elem->val; }

        auto* operator->() const noexcept { return &elem->val; }

        friend bool operator==(Iterator a, Iterator b) noexcept { return a.elem == b.elem; }

        friend bool operator!=(Iterator a, Iterator b) noexcept { return a.elem != b.elem; }
    };

    using iterator = Iterator<Elem>;
    using const_iterator = Iterator<const Elem>;

    iterator begin() noexcept { return iterator(head.get()); }
    const_iterator begin() const noexcept { return const_iterator(head.get()); }

    iterator end() noexcept { return iterator(nullptr); }
    const_iterator end() const noexcept { return const_iterator(nullptr); }
};
