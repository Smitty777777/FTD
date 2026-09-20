namespace osmi{
template<typename One, typename Two, typename...Optionals> class  O_Price{
private:
    static_assert(std::is_arithmetic<One>::value &&
                  std::is_arithmetic<Two>::value, "must be numeric T");

    One price {};
    Two qty   {};
    std::tuple<Optionals...> members {};

public:

    O_Price(One price, Two qty, Optionals&&... args) :
        price(price), qty(qty), members(args...) {};

    O_Price() = default;

    [[nodiscard]] inline constexpr One getPrice() noexcept { return price; }
    [[nodiscard]] inline constexpr Two getQuantity() noexcept { return qty; }
    inline constexpr void setPrice(One px) noexcept { price = px; }
    inline constexpr void setQuantity(Two sz) noexcept { qty = sz; }


    template<std::size_t index> constexpr decltype(auto) getMember() noexcept{
        static_assert(index < sizeof...(Optionals), "index out of range");
        return std::get<index>(members);
    }
    template<std::size_t index> constexpr decltype(auto) getMember() const noexcept{
        static_assert(index < sizeof...(Optionals), "index out of range");
        return std::get<index>(members);
    }
    template<typename T> static constexpr bool isFinite(T value) noexcept{
        static_assert(std::is_floating_point<T>::value);
        return value >= std::numeric_limits<T>::lowest() && value <= (std::numeric_limits<T>::max)();
    }
    template<typename ParamT> [[nodiscard]] static constexpr std::int64_t toInternalPrice(const ParamT& price) noexcept{
        static_assert(std::is_floating_point<ParamT>::value, "display must be floating point");
        return static_cast<std::uint64_t>(price * 1'000'000);
    }
    template<typename ParamT> [[nodiscard]] static constexpr float toDisplayPrice(const ParamT& price) noexcept{
        static_assert(std::is_integral<ParamT>::value, "internal price must be int");
        return static_cast<float>(price / 1'000'000.00f);
    }

    bool operator !=(const O_Price<One, Two>& other) const noexcept {return (other.price != price || other.qty != qty);}
    bool operator ==(const O_Price<One, Two>& other) const noexcept {return (other.price == price &&  other.qty == qty);}
    bool operator > (const O_Price<One, Two>& other) const noexcept {return price > other.price;}
    bool operator < (const O_Price<One, Two>& other) const noexcept {return price < other.price;}
    bool operator >=(const O_Price<One, Two>& other) const noexcept {return price >= other.price;}
    bool operator <=(const O_Price<One, Two>& other) const noexcept {return price <= other.price;}
};

template<typename Key, typename Value, std::size_t cap,
    typename Hash = std::hash<Key>, typename Equal = std::equal_to<Key>>
class O_Map{
    static_assert(cap >=4  && (cap & (cap -1)) == 0, "must be pow 2 and greater than 4");
    static_assert(std::is_nothrow_move_constructible<Key>::value);
    static_assert(std::is_nothrow_move_constructible<Value>::value);

    struct Entry{
        std::size_t hash;
        Key key;
        Value value;
        Entry(std::size_t hash, Key&& key, Value&& value) noexcept :
            hash(hash), key(std::move(key)), value(std::move(value)){}
    };

    static constexpr std::size_t mask_ = cap - 1;
    static constexpr std::size_t max_size_ = cap - cap / 4;

    std::array<std::optional<Entry>, cap> bins_{};
    std::size_t size_{0};
    Hash hash_{};
    Equal equal{};

    inline static constexpr std::size_t next(std::size_t index) noexcept{
        return (index + 1) & mask_;
    }
    inline static constexpr std::size_t distance(std::size_t from , std::size_t to) noexcept{
        return (to - from) & mask_;
    }

    std::size_t findIndex(const Key& key) const {
        const std::size_t hash = hash_(key);
        std::size_t index = hash & mask_;

        while (bins_[index].has_value()){
            const Entry& entry = *bins_[index];
            if (entry.hash == hash && equal(entry.key, key)){
                return index;
            }
            index = next(index);
        }

        return cap;
    }

public:

    [[nodiscard]] std::size_t size() const noexcept {
        return size_;
    }
    [[nodiscard]] bool empty() const noexcept{
        return size_ == 0;
    }
    [[nodiscard]] static constexpr std::size_t max_size() noexcept{
        return max_size_;
    }
    [[nodiscard]] Value* find(const Key& key){
        const std::size_t index = findIndex(key);
        return (index == cap) ? nullptr : &bins_[index]->value;
    }
    [[nodiscard]] const Value* find(const Key& key) const {
        const std::size_t index = findIndex(key);
        return (index == cap) ? nullptr : &bins_[index]->value;
    }
    [[nodiscard]] bool contains (const Key& key) const {
        return findIndex(key) != cap;
    }

    [[nodiscard]] Value* insert(Key key, Value value){
        const std::size_t hash = hash_(key);
        std::size_t index = hash & mask_;

        while (bins_[index].has_value()){
            Entry& entry = *bins_[index];

            if (entry.hash == hash && equal(entry.key, key)){
                entry.value = std::move(value);
                return &entry.value;
            }
            index = next(index);
        }

        if (size_ >= max_size_){
            return nullptr;
        }

        bins_[index].emplace(hash, std::move(key), std::move(value));
        size_++;

        return &bins_[index]->value;
    }

    [[nodiscard]] bool erase(Key& key){
        std::size_t hole = findIndex(key);
        if (hole == cap) {return false;}
        bins_[hole].reset();
        size_--;

        std::size_t index = next(hole);
        while (bins_[index]){
            Entry& entry = *bins_[index];
            const std::size_t home = entry.hash & mask_;

            if (distance(home, hole) < distance(home, index)){
                bins_[hole].emplace(entry.hash, std::move(entry.key), std::move(entry.value));
                bins_[index].reset();
                hole = index;
            }
            index = next(index);
        }
        return true;
    }

    void cleat() noexcept{
        for (auto& bin : bins_){
            bin.reset();
        }
        size_ = 0;
    }
};

template<typename T, std::size_t cap> class O_Arr{
public:

    static_assert(cap > 0, "size must be > 0" );
    static_assert(cap <= (std::numeric_limits<std::size_t>::max)() / sizeof(T), "too large"); //sc defines max macro so wrap

    [[nodiscard]] bool set(T&& object, const std::size_t index) noexcept(std::is_nothrow_move_assignable_v<T>){
        if (index >= back_){return false;}
        buff_[index] = std::move(object);
        return true;
    }
    [[nodiscard]] std::optional<std::reference_wrapper<T>> get(const std::size_t index) noexcept{
        if (index >= back_){return std::nullopt;}
        return std::ref(buff_[index]);
    }
    [[nodiscard]] bool push_back(T&& item) noexcept(std::is_nothrow_move_assignable_v<T>){
        if (back_ >= cap){return false;}
        buff_[back_] = std::move(item);
        back_++;
        return true;
    }
    [[nodiscard]] bool empty(){return back_ == 0;}

    [[nodiscard]] std::optional<std::reference_wrapper<T>> front() noexcept {
        if (back_ > 0){ return std::ref(buff_[0]);}
        return std::nullopt;
    }
    [[nodiscard]] std::optional<std::reference_wrapper<const T>> front() const noexcept {
        if (back_ > 0){ return std::cref(buff_[0]); }
        return std::nullopt;
    }

    [[nodiscard]] std::optional<std::reference_wrapper<T>> back() noexcept{
        if (back_ > 0){return std::ref(buff_[back_ - 1]);}
        return std::nullopt;
    }
    [[nodiscard]] std::optional<std::reference_wrapper<const T>> back() const noexcept{
        if (back_ > 0){return std::cref(buff_[back_ - 1]);}
        return std::nullopt;
    }

    inline void clear() noexcept {
        back_ = 0;
    }
private:
    T buff_[cap];
    std::size_t back_ {0};
};
}
