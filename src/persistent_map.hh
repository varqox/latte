#pragma once

#include <chrono>
#include <cstdint>
#include <memory>
#include <optional>
#include <random>
#include <type_traits>
#include <utility>

namespace detail {

[[nodiscard]] inline auto random_rank() {
    thread_local std::mt19937_64 rd_gen(
        std::chrono::system_clock::now().time_since_epoch().count());
    return rd_gen();
}

template <class K, class V>
struct PersistentTreap {
    std::shared_ptr<K> key;
    std::shared_ptr<V> val;
    std::shared_ptr<PersistentTreap> left;
    std::shared_ptr<PersistentTreap> right;
    uint64_t rank = random_rank();
};

template <class K, class V>
[[nodiscard]] std::shared_ptr<PersistentTreap<K, V>> merge(
    const std::shared_ptr<PersistentTreap<K, V>>& a,
    const std::shared_ptr<PersistentTreap<K, V>>& b) {
    if (not a) {
        return b;
    }
    if (not b) {
        return a;
    }
    if (a->rank > b->rank) {
        return std::make_shared<PersistentTreap<K, V>>(PersistentTreap<K, V>{
            .key = a->key,
            .val = a->val,
            .left = a->left,
            .right = merge(a->right, b),
            .rank = a->rank,
        });
    }
    return std::make_shared<PersistentTreap<K, V>>(PersistentTreap<K, V>{
        .key = b->key,
        .val = b->val,
        .left = merge(a, b->left),
        .right = b->right,
        .rank = b->rank,
    });
}

enum SplitVariant {
    BEFORE,
    AFTER,
};

template <SplitVariant where, class K, class V>
[[nodiscard]] std::pair<
    std::shared_ptr<PersistentTreap<K, V>>, std::shared_ptr<PersistentTreap<K, V>>>
split(const std::shared_ptr<PersistentTreap<K, V>>& tree, const K& key) {
    if (not tree) {
        return {nullptr, nullptr};
    }
    bool on_right_side{};
    if constexpr (where == BEFORE) {
        on_right_side = *tree->key < key;
    } else {
        static_assert(where == AFTER);
        on_right_side = !(key < *tree->key);
    }
    if (on_right_side) {
        auto [new_right, other] = split<where>(tree->right, key);
        return {
            std::make_shared<PersistentTreap<K, V>>(PersistentTreap<K, V>{
                .key = tree->key,
                .val = tree->val,
                .left = tree->left,
                .right = std::move(new_right),
                .rank = tree->rank,
            }),
            std::move(other)};
    }
    auto [other, new_left] = split<where>(tree->left, key);
    return {
        other,
        std::make_shared<PersistentTreap<K, V>>(PersistentTreap<K, V>{
            .key = tree->key,
            .val = tree->val,
            .left = std::move(new_left),
            .right = tree->right,
            .rank = tree->rank,
        })};
}

} // namespace detail

template <class K, class V>
class PersistentMap {
    std::shared_ptr<detail::PersistentTreap<K, V>> tree;
    size_t size_ = 0;

    PersistentMap(std::shared_ptr<detail::PersistentTreap<K, V>> tree, size_t size)
    : tree{std::move(tree)}
    , size_{size} {}

public:
    PersistentMap() noexcept = default;

    PersistentMap(const PersistentMap&) = default;
    PersistentMap(PersistentMap&&) noexcept = default;
    PersistentMap& operator=(const PersistentMap&) = default;
    PersistentMap& operator=(PersistentMap&&) noexcept = default;
    ~PersistentMap() = default;

    // Returns nullptr if @p key is not found
    [[nodiscard]] std::shared_ptr<const V> find(const K& key) const {
        const detail::PersistentTreap<K, V>* node = tree.get();
        while (node) {
            if (*node->key < key) {
                node = node->right.get();
            } else if (key < *node->key) {
                node = node->left.get();
            } else {
                return node->val;
            }
        }
        return nullptr;
    }

    [[nodiscard]] bool empty() const noexcept { return tree == nullptr; }

    [[nodiscard]] size_t size() const noexcept { return size_; }

    // Returns a new tree iff insertion happened
    [[nodiscard]] std::optional<PersistentMap> insert(K key, V val) const {
        auto [before, eq_or_after] = detail::split<detail::BEFORE>(tree, key);
        auto [eq, after] = detail::split<detail::AFTER>(eq_or_after, key);
        if (eq != nullptr) {
            return std::nullopt;
        }
        eq = std::make_shared<detail::PersistentTreap<K, V>>(detail::PersistentTreap<K, V>{
            .key = std::make_shared<K>(std::move(key)),
            .val = std::make_shared<V>(std::move(val)),
            .left = nullptr,
            .right = nullptr,
        });
        return PersistentMap{detail::merge(before, detail::merge(eq, after)), size_ + 1};
    }

    [[nodiscard]] PersistentMap insert_or_assign(K key, V val) const {
        auto [before, eq_or_after] = detail::split<detail::BEFORE>(tree, key);
        auto [eq, after] = detail::split<detail::AFTER>(eq_or_after, key);
        size_t new_size = size_ + (eq == nullptr);
        eq = std::make_shared<detail::PersistentTreap<K, V>>(detail::PersistentTreap<K, V>{
            .key = std::make_shared<K>(std::move(key)),
            .val = std::make_shared<V>(std::move(val)),
            .left = nullptr,
            .right = nullptr,
        });
        return PersistentMap{detail::merge(before, detail::merge(eq, after)), new_size};
    }

    // Does not fail if the element does not exist
    [[nodiscard]] PersistentMap erase(const K& key) const {
        auto [before, eq_or_after] = detail::split<detail::BEFORE>(tree, key);
        auto [eq, after] = detail::split<detail::AFTER>(eq_or_after, key);
        return PersistentMap{detail::merge(before, after), size_ - (eq != nullptr)};
    }

    template <class Func>
    void for_each(Func&& body) const {
        static_assert(std::is_invocable_v<Func, const K&, const V&>);
        auto impl = [&](auto& self, const detail::PersistentTreap<K, V>* node) {
            if (not node) {
                return;
            }
            self(self, node->left.get());
            body(static_cast<const K&>(*node->key), static_cast<const V&>(*node->val));
            self(self, node->right.get());
        };
        impl(impl, tree.get());
    }
};
