#include "unordered_map.h"

struct VerySpecialType {
    int x = 0;
    explicit VerySpecialType(int x): x(x) {}
    VerySpecialType(const VerySpecialType&) = delete;
    VerySpecialType& operator=(const VerySpecialType&) = delete;

    VerySpecialType(VerySpecialType&&) = default;
    VerySpecialType& operator=(VerySpecialType&&) = default;
};

struct NeitherDefaultNorCopyConstructible {
    VerySpecialType x;

    NeitherDefaultNorCopyConstructible() = delete;
    NeitherDefaultNorCopyConstructible(const NeitherDefaultNorCopyConstructible&) = delete;
    NeitherDefaultNorCopyConstructible& operator=(const NeitherDefaultNorCopyConstructible&) = delete;

    NeitherDefaultNorCopyConstructible(VerySpecialType&& x): x(std::move(x)) {}
    NeitherDefaultNorCopyConstructible(NeitherDefaultNorCopyConstructible&&) = default;
    NeitherDefaultNorCopyConstructible& operator=(NeitherDefaultNorCopyConstructible&&) = default;

    bool operator==(const NeitherDefaultNorCopyConstructible& other) const {
        return x.x == other.x.x;
    }
};

namespace std {
    template<>
    struct hash<NeitherDefaultNorCopyConstructible> {
        size_t operator()(const NeitherDefaultNorCopyConstructible& x) const {
            return std::hash<int>()(x.x.x);
        }
    };
}

int main() {
    UnorderedMap<NeitherDefaultNorCopyConstructible, NeitherDefaultNorCopyConstructible> m;
    std::pair<NeitherDefaultNorCopyConstructible, NeitherDefaultNorCopyConstructible> p{VerySpecialType(1), VerySpecialType(1)};
    m.insert(std::move(p));
}