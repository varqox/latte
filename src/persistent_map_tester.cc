#include "persistent_map.hh"

#include <cassert>
#include <chrono>
#include <cstdint>
#include <iostream>
#include <map>
#include <optional>
#include <random>
#include <vector>

using LL = int64_t;

static LL rd(LL a, LL b) noexcept {
    static std::mt19937_64 gen(std::chrono::system_clock::now().time_since_epoch().count());
    return std::uniform_int_distribution<LL>(a, b)(gen);
}

static void test_efficiency() {
    PersistentMap<int, LL> pm;
    for (int i = 0; i < 1e6; ++i) {
        pm = pm.insert_or_assign(i, rd(0, 1e18));
        pm = pm.insert_or_assign(-i, rd(0, 1e18));
    }
}

int main() {
    constexpr int runs = 1e3;
    constexpr int run_iters = 1000;
    constexpr int max_key = 20;
    for (int run = 0; run < runs; ++run) {
        std::vector<PersistentMap<int, LL>> pmaps;
        std::vector<std::map<int, LL>> maps;
        pmaps.emplace_back();
        maps.emplace_back();
        for (int iter = 0; iter < run_iters; ++iter) {
            assert(pmaps.size() == maps.size());
            int tp = rd(0, pmaps.size() - 1);
            if (pmaps[tp].empty() != maps[tp].empty()) {
                std::cerr << "refuted by empty!\n";
                return 1;
            }
            if (pmaps[tp].size() != maps[tp].size()) {
                std::cerr << "refuted by size!\n";
                return 1;
            }
            switch (rd(1, 5)) {
            case 1: { // find
                int key = rd(0, max_key);
                auto pres = [&]() -> std::optional<LL> {
                    auto sp = pmaps[tp].find(key);
                    if (sp) {
                        return *sp;
                    }
                    return std::nullopt;
                }();
                auto res = [&]() -> std::optional<LL> {
                    auto it = maps[tp].find(key);
                    if (it == maps[tp].end()) {
                        return std::nullopt;
                    }
                    return it->second;
                }();
                if (pres != res) {
                    std::cerr << "refuted by find!\n";
                    return 1;
                }
            } break;
            case 2: { // insert
                int key = rd(0, max_key);
                auto val = rd(-1e18, 1e18);
                auto popt = pmaps[tp].insert(key, val);
                pmaps.emplace_back(popt.value_or(pmaps[tp]));
                auto pres = popt.has_value();

                maps.emplace_back(maps[tp]);
                auto res = maps.back().insert({key, val}).second;
                if (pres != res) {
                    std::cerr << "refuted by insert!\n";
                    return 1;
                }
            } break;
            case 3: { // insert_or_assign
                int key = rd(0, max_key);
                auto val = rd(-1e18, 1e18);
                pmaps.emplace_back(pmaps[tp].insert_or_assign(key, val));
                maps.emplace_back(maps[tp]);
                maps.back().insert_or_assign(key, val);
            } break;
            case 4: { // erase
                int key = rd(0, max_key);
                pmaps.emplace_back(pmaps[tp].erase(key));
                maps.emplace_back(maps[tp]);
                maps.back().erase(key);
            } break;
            case 5: { // for_each
                std::vector<std::pair<int, LL>> pres;
                pmaps[tp].for_each([&](int k, LL v) { pres.emplace_back(k, v); });
                std::vector<std::pair<int, LL>> res;
                for (auto [k, v] : maps[tp]) {
                    res.emplace_back(k, v);
                }
                if (pres != res) {
                    std::cerr << "refuted by for_each!\n";
                    return 1;
                }
                if constexpr ((false)) { // For debugging
                    for (auto vec : {res, pres}) {
                        std::cout << "{";
                        for (auto [k, v] : vec) {
                            std::cout << "(" << k << "," << v << ")"
                                      << " }"[vec.back() == std::pair{k, v}];
                        }
                        std::cout << "}\n";
                    }
                }
            } break;
            }
        }
    }

    test_efficiency();
}
