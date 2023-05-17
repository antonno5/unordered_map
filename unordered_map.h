#pragma once

#include <iostream>
#include <memory>
#include <iterator>
#include <vector>
#include <cmath>
#include <set>

template<typename Key,
        typename Value,
        typename Hash = std::hash<Key>,
        typename Equal = std::equal_to<Key>,
        typename Alloc = std::allocator< std::pair<const Key, Value> >
>
class UnorderedMap {
    template<typename T, class Allocator = std::allocator<T>>
    class List {
    private:
        friend UnorderedMap<Key, Value, Hash, Equal, Alloc>;

        struct BaseNode {
            BaseNode* my_next = this;
            BaseNode* my_prev = this;
        };

        struct Node : BaseNode {
            T value = T();

            T& get_value() {
                return value;
            }
        };

        template<typename Type>
        struct BasicIterator {
        private:

            BaseNode* my_node = nullptr;

        public:

            friend List;

            BasicIterator() = default;

            explicit BasicIterator(BaseNode* my_node) : my_node(my_node) {}

            using difference_type = ptrdiff_t;
            using value_type = Type;
            using reference = value_type&;
            using pointer = value_type*;
            using iterator_category = std::bidirectional_iterator_tag;

            BasicIterator& operator++() {
                my_node = my_node->my_next;
                return *this;
            }

            BasicIterator operator++(int) {
                auto copy = *this;
                ++*this;
                return copy;
            }

            BasicIterator& operator--() {
                my_node = my_node->my_prev;
                return *this;
            }

            BasicIterator operator--(int) {
                auto copy = *this;
                --*this;
                return copy;
            }

            reference operator*() const {
                auto node = reinterpret_cast<Node*>(my_node);
                return node->get_value();
            }

            pointer operator->() const {
                auto node = reinterpret_cast<Node*>(my_node);
                return &node->get_value();
            }

            bool operator==(const BasicIterator<Type>& that) const {
                return my_node == that.my_node;
            }

            bool operator!=(const BasicIterator<Type>& that) const {
                return !(my_node == that.my_node);
            }

            operator BasicIterator<const Type>() const {
                return BasicIterator<const Type>(my_node);
            }

            BaseNode* getNode() {
                return my_node;
            }

        };

    public:

        using value_type = T;
        using reference = T&;
        using pointer = T*;
        using iterator = BasicIterator<T>;
        using const_iterator = BasicIterator<const T>;
        using reverse_iterator = std::reverse_iterator<iterator>;
        using const_reverse_iterator = std::reverse_iterator<const_iterator>;

    private:

        size_t my_size = 0;

        BaseNode my_end = BaseNode();

        iterator it_end = iterator(&my_end);

        Allocator my_allocator;

        using NodeAlloc = typename std::allocator_traits<Allocator>::template rebind_alloc<Node>;
        using NodeTraits = std::allocator_traits<NodeAlloc>;

        NodeAlloc my_node_allocator;

        void copy_from_other(const List& that) {
            try {
                for (iterator it = std::next(that.it_end); it != that.it_end; ++it) {
                    push_back(*it);
                }
            } catch (...) {
                while (my_size) {
                    pop_back();
                }
                throw;
            }
        }

        void construct(List&& other) {
            if (other.my_size) {
                my_end.my_next = other.my_end.my_next;
                my_end.my_prev = other.my_end.my_prev;
                my_end.my_next->my_prev = my_end.my_prev->my_next = &my_end;
                other.my_end.my_next = other.my_end.my_prev = &other.my_end;
            }
            else {
                my_end.my_next = my_end.my_prev = &my_end;
            }
            my_size = other.my_size;
            other.my_size = 0;
        }

        void move_assign(List&& other, std::true_type) {
            if (std::allocator_traits<Allocator>::propagate_on_container_move_assignment::value) {
                my_node_allocator = std::move(other.my_node_allocator);
            }
            construct(std::move(other));
        }

        void destroy(Node* node) {
            NodeTraits::destroy(my_node_allocator, node);
            NodeTraits::deallocate(my_node_allocator, node, 1);
        }

        void move_assign(List&& other, std::false_type) {
            if (my_node_allocator == other.my_node_allocator) {
                move_assign(std::move(other), std::true_type());
                return;
            }
            while (other.size()) {
                Node* node = other.extract_back_node();
                push_front(std::move(node->value));
                other.destroy(node);
            }
        }

        template<typename ...Args>
        Node* create_node(Args&& ...args) {
            Node *ptr = nullptr;

            ptr = NodeTraits::allocate(my_node_allocator, 1);

            try {
                NodeTraits::construct(my_node_allocator, &ptr->Node::value, std::forward<Args>(args)...);
            } catch(...) {
                NodeTraits::deallocate(my_node_allocator, ptr, 1);
                throw;
            }
            return ptr;
        }

        void destroy_node(Node* node) {
            NodeTraits::destroy(my_node_allocator, node);
            NodeTraits::deallocate(my_node_allocator, node, 1);
        }

    public:

        Node* extract_back_node() {
            Node* node = static_cast<Node*>(my_end.my_prev);
            node->my_prev->my_next = node->my_next;
            node->my_next->my_prev = node->my_prev;
            node->my_next = node->my_prev = nullptr;
            --my_size;
            return node;
        }

        void clear() {
            while (my_size) {
                pop_back();
            }
        }

        void swap(List& that) {
            auto prev = my_end.my_prev, next = my_end.my_next;

            prev->my_next = &that.my_end;
            next->my_prev = &that.my_end;

            prev = that.my_end.my_prev, next = that.my_end.my_next;

            prev->my_next = &my_end;
            next->my_prev = &my_end;

            if (std::allocator_traits<Allocator>::propagate_on_container_swap::value) {
                std::swap(my_allocator, that.my_allocator);
                std::swap(my_node_allocator, that.my_node_allocator);
            }

            std::swap(my_end, that.my_end);
            std::swap(my_size, that.my_size);
        }

        List(const Allocator& alloc = Allocator()) : my_allocator(alloc), my_node_allocator(alloc) {}

        List(size_t new_size, const Allocator& alloc = Allocator()) : my_allocator(alloc), my_node_allocator(alloc) {
            try {
                for (size_t i = 0; i < new_size; ++i) {
                    push_back();
                }
            } catch (...) {
                while (my_size) {
                    pop_back();
                }
                throw;
            }
        }

        List(size_t new_size, const T& value, const Allocator& alloc = Allocator()) : my_allocator(alloc), my_node_allocator(alloc) {
            try {
                for (size_t i = 0; i < new_size; ++i) {
                    push_back(value);
                }
            } catch(...) {
                while (my_size) {
                    pop_back();
                }
                throw;
            }
        }

        List(List&& other) noexcept : my_allocator(std::move(other.my_allocator))
                , my_node_allocator(other.my_node_allocator) {
            construct(std::move(other));
        }

        List(List&& other, const Allocator& alloc) noexcept : my_size(other.my_size), my_end(other.my_end), it_end(other.it_end)
                , my_allocator(alloc)
                , my_node_allocator(other.my_node_allocator) {
            construct(std::move(other));
        }

        List& operator=(List&& other) noexcept {
            clear();
            move_assign(std::move(other), typename std::allocator_traits<Allocator>::propagate_on_container_move_assignment());
            return *this;
        }

        Allocator get_allocator() const {
            return my_allocator;
        }

        List(const List& that, const Allocator& allocator) : List(allocator){
            copy_from_other(that);
        }

        List(const List& that) : List(std::allocator_traits<Allocator>::select_on_container_copy_construction(that.my_allocator)) {
            copy_from_other(that);
        }

        List& operator=(const List& that) {
            if (this != &that) {
                List copy(that, std::allocator_traits<Allocator>::propagate_on_container_copy_assignment::value
                                ? that.my_allocator : my_allocator);
                this->swap(copy);
            }
            return *this;
        }

        size_t size() const {
            return my_size;
        }

        template<typename... Args>
        iterator insert(const_iterator it, Args&&... args) {
            Node *ptr = create_node(std::forward<Args>(args)...);

            BaseNode* base_ptr = static_cast<BaseNode*>(ptr);

            BaseNode* node = it.my_node;

            base_ptr->my_prev = node->my_prev;
            base_ptr->my_next = node;
            node->my_prev->my_next = base_ptr;
            node->my_prev = base_ptr;

            ++my_size;

            return iterator(base_ptr);
        }

        iterator insert_node(const_iterator it, Node* ptr) {
            BaseNode* base_ptr = static_cast<BaseNode*>(ptr);

            BaseNode* node = it.my_node;

            base_ptr->my_prev = node->my_prev;
            base_ptr->my_next = node;
            node->my_prev->my_next = base_ptr;
            node->my_prev = base_ptr;

            ++my_size;

            return iterator(base_ptr);
        }

        iterator erase(const_iterator it) {
            --my_size;

            BaseNode* node = it.my_node;

            BaseNode* prev = node->my_prev;
            BaseNode* next = node->my_next;

            prev->my_next = next;
            next->my_prev = prev;

            iterator it_next = iterator(node->my_next);

            NodeTraits::destroy(my_node_allocator, reinterpret_cast<Node*>(it.my_node));
            NodeTraits::deallocate(my_node_allocator, reinterpret_cast<Node*>(it.my_node), 1);

            return it_next;
        }

        iterator begin() {
            return std::next(it_end);
        }

        const_iterator cbegin() const {
            return const_iterator(std::next(it_end));
        }

        const_iterator begin() const {
            return cbegin();
        }

        iterator end() {
            return it_end;
        }

        const_iterator cend() const {
            return const_iterator(it_end);
        }

        const_iterator end() const {
            return cend();
        }

        reverse_iterator rbegin() {
            return std::make_reverse_iterator(it_end);
        }

        const_reverse_iterator crbegin() const {
            return std::make_reverse_iterator(cend());
        }

        const_reverse_iterator rbegin() const {
            return crbegin();
        }

        reverse_iterator rend() {
            return std::make_reverse_iterator(end());
        }

        const_reverse_iterator crend() const {
            return std::make_reverse_iterator(cend());
        }

        const_reverse_iterator rend() const {
            return crend();
        }

        iterator push_back(const T& that) {
            return insert(it_end, that);
        }

        iterator push_back(T&& that) {
            return insert(it_end, std::forward<T>(that));
        }

        iterator push_back() {
            return insert(it_end);
        }

        template <typename... Args>
        iterator push_front(Args&&... args) {
            Node *ptr = create_node(std::forward<Args>(args)...);

            return push_front(*ptr);
        }

        iterator push_front(const T& that) {
            return insert(begin(), that);
        }

        iterator push_front(T&& that) {
            return insert(begin(), std::forward<T>(that));
        }

        iterator push_front() {
            return insert(begin());
        }

        iterator pop_back() {
            return erase(std::prev(it_end));
        }

        iterator pop_front() {
            return erase(std::next(it_end));
        }

        ~List() {
            clear();
        }

    };
public:
    using NodeType = std::pair<const Key, Value>;
    using value_type = NodeType;
    using reference = NodeType&;
    using pointer = NodeType*;
    using iterator = typename List<NodeType, Alloc>::iterator;
    using const_iterator = typename List<NodeType, Alloc>::const_iterator;

private:
    using MyList = List<NodeType, Alloc>;
    using ListIt = typename MyList::iterator;
    using ListNode = typename MyList::Node;

    static const size_t init_reserved = 32;
    constexpr static const float init_max_load_factor = 1;

    size_t my_buckets = init_reserved;
    Hash my_hash;
    Equal my_equal;
    Alloc my_alloc;
    MyList my_nodes = MyList();
    std::vector<ListIt> my_iters = std::vector<ListIt>(init_reserved);
    float my_max_load_factor = init_max_load_factor;

    size_t get_hash(const Key& key) const {
        return my_hash(key) % my_buckets;
    }

    size_t get_hash(const NodeType& node) const {
        return my_hash(node.first) % my_buckets;
    }

    void rehash(size_t count) {
        if (count <= my_buckets) {
            return;
        }
        std::vector<ListNode*
                ,typename std::allocator_traits<Alloc>::template rebind_alloc<ListNode*>> nodes(my_alloc);
        nodes.reserve(size());
        while (my_nodes.size()) {
            nodes.push_back(my_nodes.extract_back_node());
        }
        my_buckets = count;
        my_iters.resize(my_buckets);
        std::fill(my_iters.begin(), my_iters.end(), iterator());
        for (auto node : nodes) {
            insert_node(node);
        }
    }

    std::pair<iterator, bool> insert_node(ListNode* node) {
        auto iter = iterator();
        if (my_iters[get_hash(node->get_value())] != iterator()) {
            auto it = my_iters[get_hash(node->get_value())];
            for (; it != my_nodes.end() && get_hash(*it) == get_hash(node->get_value()); ++it) {
                if (my_equal(it->first, node->get_value().first)) {
                    my_nodes.destroy_node(node);
                    return {it, false};
                }
            }
            iter = my_nodes.insert_node(it, node);
        }
        else {
            iter = my_nodes.insert_node(my_nodes.begin(), node);
            my_iters[get_hash(node->get_value())] = my_nodes.begin();
        }
        check_rehash();
        return {iter, true};
    }

    void check_rehash() {
        if (load_factor() > my_max_load_factor) {
            rehash(static_cast<size_t>(std::ceil(static_cast<float>(size())) / my_max_load_factor) * 2);
        }
    }

    void restore_buckets() {
        size_t prev_bucket = my_buckets;
        for (auto it = my_nodes.begin(); it != my_nodes.end(); ++it) {
            auto current_bucket = get_hash(*it);
            if (current_bucket != prev_bucket) {
                my_iters[current_bucket] = it;
                prev_bucket = current_bucket;
            }
        }
    }

public:

    void swap(UnorderedMap<Key, Value, Hash, Equal, Alloc>& other) {
        std::swap(my_buckets, other.my_buckets);
        my_nodes.swap(other.my_nodes);
        std::swap(my_iters, other.my_iters);
        std::swap(my_hash, other.my_hash);
        std::swap(my_equal, other.my_equal);
        std::swap(my_max_load_factor, other.my_max_load_factor);
        if (std::allocator_traits<Alloc>::propagate_on_container_swap::value) {
            std::swap(my_alloc, other.my_alloc);
        }
    }

    explicit UnorderedMap(const Alloc& alloc = Alloc()) : UnorderedMap(init_reserved, alloc) {}

    explicit UnorderedMap(size_t bucket_count,
                          const Hash& hash = Hash(),
                          const Equal& equal = Equal(),
                          const Alloc& alloc = Alloc())
            : my_buckets(std::max(bucket_count, static_cast<size_t>(1))), my_hash(hash), my_equal(equal), my_alloc(alloc),
              my_nodes(my_alloc),
              my_iters(my_buckets, my_alloc) {}

    UnorderedMap(size_t bucket_count, const Alloc& alloc)
            : UnorderedMap(bucket_count, Hash(), Equal(), alloc) {}

    UnorderedMap(size_t bucket_count,
                 const Hash& hash,
                 const Alloc& alloc)
            : UnorderedMap(bucket_count, hash, Equal(), alloc) {}

    UnorderedMap(const UnorderedMap& other)
            : my_buckets(other.my_buckets), my_hash(other.my_hash), my_equal(other.my_equal),
              my_alloc(std::allocator_traits<Alloc>::select_on_container_copy_construction(other.my_alloc)),
              my_nodes(other.my_nodes), my_iters(my_buckets),
              my_max_load_factor(other.my_max_load_factor) {
        restore_buckets();
    }

    UnorderedMap(const UnorderedMap& other, const Alloc& alloc)
            : my_buckets(other.my_buckets), my_hash(other.my_hash), my_equal(other.my_equal), my_alloc(alloc),
              my_nodes(other.my_nodes, alloc), my_iters(my_buckets, alloc),
              my_max_load_factor(other.my_max_load_factor) {
        restore_buckets();
    }

    UnorderedMap(UnorderedMap&& other)
            : my_buckets(other.my_buckets), my_hash(std::move(other.my_hash)), my_equal(std::move(other.my_equal)),
              my_alloc(std::move(other.my_alloc)), my_nodes(std::move(other.my_nodes)), my_iters(std::move(other.my_iters)),
              my_max_load_factor(other.my_max_load_factor) {
        other.my_buckets = 1;
        other.my_iters.assign(1, iterator());
        other.my_max_load_factor = init_max_load_factor;
    }

    UnorderedMap(UnorderedMap&& other, const Alloc& alloc)
            : my_buckets(other.my_buckets), my_hash(std::move(other.my_hash)), my_equal(std::move(other.my_equal)),
              my_alloc(alloc), my_nodes(std::move(other.my_nodes), alloc), my_iters(std::move(other.my_iters), alloc),
              my_max_load_factor(other.my_max_load_factor) {
        other.my_buckets = 1;
        other.my_iters.assign(1, iterator());
        other.my_max_load_factor = init_max_load_factor;
    }

    UnorderedMap& operator=(const UnorderedMap& other) {
        UnorderedMap copy(other);
        copy.swap(*this);
        return *this;
    }

    UnorderedMap& operator=(UnorderedMap&& other) noexcept {
        my_buckets = other.my_buckets;
        my_hash = std::move(other.my_hash);
        my_equal = std::move(other.my_equal);
        my_nodes = std::move(other.my_nodes);
        my_iters = std::move(other.my_iters);
        my_max_load_factor = other.my_max_load_factor;
        other.my_buckets = 1;
        other.my_iters.assign(1, iterator());
        other.my_max_load_factor = init_max_load_factor;
        if (!std::allocator_traits<Alloc>::propagate_on_container_move_assignment::value && (my_alloc != other.my_alloc)) {
            restore_buckets();
        }
        else if (std::allocator_traits<Alloc>::propagate_on_container_move_assignment::value) {
            my_alloc = std::move(other.my_alloc);
        }
        return *this;
    }

    Value& at(const Key& key) {
        auto it = find(key);
        if (it == end()) {
            throw std::out_of_range("Key doesn't exist in unordered map");
        }
        else {
            return it->second;
        }
    }

    const Value& at(const Key& key) const {
        auto it = find(key);
        if (it == end()) {
            throw std::out_of_range("Key doesn't exist in unordered map");
        }
        else {
            return it->second;
        }
    }

    Value& operator[](const Key& key) {
        auto it = find(key);
        if (it == end()) {
            auto iter = insert({key, Value()});
            return iter.first->second;
        }
        else {
            return it->second;
        }
    }

    Value& operator[](Key&& key) {
        auto it = find(key);
        if (it == end()) {
            auto iter = insert({std::move(key), Value()});
            return iter.first->second;
        }
        else {
            return it->second;
        }
    }

    template<typename ...Args>
    auto emplace(Args&& ...args) {
        auto node = my_nodes.create_node(std::forward<Args>(args)...);
        return insert_node(node);
    }

    Alloc get_allocator() const noexcept {
        return my_nodes.get_allocator();
    }

    size_t bucket_count() const {
        return my_buckets;
    }

    float load_factor() const {
        return static_cast<float>(size()) / static_cast<float>(my_buckets);
    }

    float max_load_factor() const {
        return my_max_load_factor;
    }

    void max_load_factor(float new_max_load_factor) {
        my_max_load_factor = new_max_load_factor;
        check_rehash();
    }

    void reserve(size_t count) {
        rehash(std::ceil(count / max_load_factor()));
    }

    iterator find(const Key& key) {
        if (my_iters[get_hash(key)] == iterator()) {
            return end();
        }
        for (auto it = my_iters[get_hash(key)]; it != end() && get_hash(*it) == get_hash(key); ++it) {
            if (my_equal(it->first, key)) {
                return it;
            }
        }
        return end();
    }

    const_iterator find(const Key& key) const {
        if (my_iters[get_hash(key)] == iterator()) {
            return cend();
        }
        for (auto it = my_iters[get_hash(key)];; ++it) {
            if (my_equal(it->first, key)) {
                return static_cast<const_iterator>(it);
            }
        }
        return cend();
    }

    std::pair<iterator, bool> insert(const NodeType& node) {
        auto iter = iterator();
        if (my_iters[get_hash(node)] != iterator()) {
            auto it = my_iters[get_hash(node)];
            for (; it != my_nodes.end() && get_hash(*it) == get_hash(node); ++it) {
                if (my_equal(it->first, node.first)) {
                    return {it, false};
                }
            }
            iter = my_nodes.insert(it, node);
        }
        else {
            iter = my_nodes.push_front(node);
            my_iters[get_hash(node)] = my_nodes.begin();
        }
        check_rehash();
        return {iter, true};
    }

    std::pair<iterator, bool> insert(NodeType&& node) {
        auto iter = iterator();
        if (my_iters[get_hash(node)] != iterator()) {
            auto it = my_iters[get_hash(node)];
            for (; it != my_nodes.end() && get_hash(*it) == get_hash(node); ++it) {
                if (my_equal(it->first, node.first)) {
                    return {it, false};
                }
            }
            iter = my_nodes.insert(it, std::forward<NodeType>(node));
        }
        else {
            iter = my_nodes.push_front(std::forward<NodeType>(node));
            my_iters[get_hash(node)] = my_nodes.begin();
        }
        check_rehash();
        return {iter, true};
    }

    template <typename Pair>
    std::__enable_if_t<std::is_constructible<NodeType, Pair&&>::value,
            std::pair<iterator, bool>>
    insert(Pair&& value) {
        auto node = my_nodes.create_node(std::forward<Pair>(value));
        return insert_node(node);
    }

    template<typename It>
    void insert(It it_begin, It it_end) {
        auto it = it_begin;
        try {
            for (; it != it_end; ++it) {
                insert(*it);
            }
        } catch(...) {
            for (auto iter = it_begin; iter != it; ++iter) {
                auto founded = find(iter->first);
                erase(founded);
            }
        }
    }

    iterator erase(const_iterator it) {
        auto hash = get_hash(*it);
        auto prev = std::prev(it);
        auto next = std::next(it);
        auto ans = my_nodes.erase(it);
        if (get_hash(*prev) != hash) {
            if (get_hash(*next) == hash) {
                my_iters[hash] = reinterpret_cast<iterator&>(next);
            }
            else {
                my_iters[hash] = iterator();
            }
        }
        return ans;
    }

    template<typename It>
    void erase(It it_begin, It it_end) {
        auto it = it_begin;
        auto it_next = std::next(it);
        try {
            for (; it != it_end; it = it_next) {
                it_next = std::next(it);
                auto iter = find(it->first);
                if (iter != end()) {
                    erase(iter);
                }
            }
        }
        catch(...) {
            for (auto iter = it_begin; iter != it; ++iter) {
                insert(*iter);
            }
        }
    }

    iterator begin() {
        return my_nodes.begin();
    }

    const_iterator begin() const {
        return my_nodes.cbegin();
    }

    const_iterator cbegin() const {
        return my_nodes.cbegin();
    }

    iterator end() {
        return my_nodes.end();
    }

    const_iterator end() const {
        return my_nodes.cend();
    }

    const_iterator cend() const {
        return my_nodes.cend();
    }

    size_t size() const {
        return my_nodes.size();
    }

    ~UnorderedMap() = default;

};
