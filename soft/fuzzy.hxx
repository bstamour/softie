//==============================================================================
// Copyright (c) 2017, Bryan St. Amour
// All rights reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
//
//   1)  Redistributions of source code must retain the above copyright notice,
//       this list of conditions and the following disclaimer.
//
//   2)  Redistributions in binary form must reproduce the above copyright
//       notice, this list of conditions and the following disclaimer in the
//       documentation and/or other materials provided with the distribution.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
// AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
// IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
// ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
// LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
// CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
// SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
// INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
// CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
// ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
// POSSIBILITY OF SUCH DAMAGE.
//------------------------------------------------------------------------------

#ifndef BST_FUZZY_HXX_
#define BST_FUZZY_HXX_

#include <cmath>
#include <functional>
#include <iosfwd>
#include <iterator>
#include <map>
#include <type_traits>
#include <utility>

namespace soft::fuzzy {

//------------------------------------------------------------------------------

namespace traits {

// Compute based on min/max.

template <typename T> struct min_max {
  constexpr auto and_(T const &x, T const &y) const { return std::min(x, y); }
  constexpr auto or_(T const &x, T const &y) const { return std::max(x, y); }
  constexpr auto not_(T const &x) const { return T{1} - x; }
};

// Compute based on multiplication.

template <typename T> struct product {
  constexpr auto and_(T const &x, T const &y) const { return x * y; }
  constexpr auto or_(T const &x, T const &y) const { return x + y - x * y; }
  constexpr auto not_(T const &x) const { return T{1} - x; }
};
} // namespace traits

//------------------------------------------------------------------------------

namespace detail {
template <typename T, typename U>
using enable_if_not_same_t =
    std::enable_if_t<!std::is_same_v<std::decay_t<T>, std::decay_t<U>>>;
}

//------------------------------------------------------------------------------

template <typename T, template <typename> typename Traits = traits::min_max>
class value final : private Traits<T> {
public:
  value() = default;

  template <typename U,
            typename = detail::enable_if_not_same_t<U, value<T, Traits>>>
  explicit constexpr value(U &&val) : value_{std::forward<U>(val)}
  {
  }

  constexpr auto get() const { return value_; }

  template <typename U> constexpr auto set(U &&x)
  {
    value_ = std::forward<U>(x);
  }

  auto operator<(value const &rhs) const { return value_ < rhs.value_; }
  auto operator==(value const &rhs) const { return value_ == rhs.value_; }

  constexpr auto operator&&(value const &y) const
  {
    return value{and_(value_, y.value_)};
  }
  constexpr auto operator||(value const &y) const
  {
    return value{or_(value_, y.value_)};
  }
  constexpr auto operator!() const { return value{not_(value_)}; }

  friend decltype(auto) operator>>(std::istream &is, value &val)
  {
    return is >> val.value_;
  }
  friend decltype(auto) operator<<(std::ostream &os, value const &val)
  {
    return os << val.value_;
  }

private:
  T value_;
};

//------------------------------------------------------------------------------

template <typename In, typename Out, typename ValueType = value<Out>,
          template <typename> typename SetTraits = traits::min_max>
class set final : private SetTraits<Out> {
public:
  using value_type = ValueType;

  // Construct from a function.
  template <typename Func, typename = detail::enable_if_not_same_t<
                               Func, set<In, Out, ValueType, SetTraits>>>
  explicit set(Func &&f) : f_{std::forward<Func>(f)}
  {
  }

  // Construct from a sequence of pairs.
  template <typename Iter>
  set(Iter first, Iter last) : f_{make_indexed_function(first, last)}
  {
  }

  template <typename U> auto operator()(U &&x) const
  {
    return value_type{f_(std::forward<U>(x))};
  }

  auto operator&(set const &t) const
  {
    return set([ =, *this ](In x) { return this->and_(f_(x), t.f_(x)); });
  }

  auto operator|(set const &t) const
  {
    return set([ =, *this ](In x) { return this->or_(f_(x), t.f_(x)); });
  }

  auto operator~() const
  {
    return set([ =, *this ](In x) { return this->not_(f_(x)); });
  }

private:
  std::function<Out(In)> f_;

  template <typename Iter>
  static constexpr auto make_indexed_function(Iter, Iter);
};

//------------------------------------------------------------------------------

template <typename In, typename Out, typename V,
          template <typename> typename ST>
template <typename Iter>
constexpr auto set<In, Out, V, ST>::make_indexed_function(Iter first, Iter last)
{
  return [m = std::map<In, Out>(first, last)](In x)
  {
    if (auto i = m.find(x); i != end(m))
      return i->second;
    else
      return Out{};
  };
}

//------------------------------------------------------------------------------

namespace detail {
template <typename F> struct inspect_functor;

template <typename G, typename In, typename Out>
struct inspect_functor<Out (G::*)(In) const> {
  using in_type = In;
  using out_type = Out;
};

template <typename G, typename In, typename Out>
struct inspect_functor<Out (G::*)(In)> {
  using in_type = In;
  using out_type = Out;
};
} // namespace detail

//------------------------------------------------------------------------------

template <typename In, typename Out> set(Out (*)(In))->set<In, Out>;

template <
    typename F, typename T = detail::inspect_functor<decltype(&F::operator())>,
    typename In = typename T::in_type, typename Out = typename T::out_type>
set(F)->set<In, Out>;

template <typename Iter,
          typename VT = typename std::iterator_traits<Iter>::value_type,
          typename In = typename VT::first_type,
          typename Out = typename VT::second_type>
set(Iter, Iter)->set<In, Out>;

<<<<<<< HEAD
} // namespace soft::fuzzy
=======
//------------------------------------------------------------------------------

template <typename Set, typename N> auto cut(Set&& s, N const& alpha)
{
  using value_type = std::decay_t<Set>::value_type;
  return set{ [=](value_type x) -> value_type {
    return s(x) > alpha ? 1 : 0;
  }};
}

template <typename Set> auto core(Set&& s)
{
  using value_type = std::decay_t<Set>::value_type;
  return set{ [=](value_type x) {
    auto const zero = value_type{0};
    auto const one = value_type{1};
    auto const sx = s(x);
    if constexpr (std::is_floating_point_v<value_type>) {
      auto next = std::nextafter(sx, one);
      return sx <= one && one <= next || next <= one && one <= xs
	? one
	: zero;
    }
    else {
      return sx == one ? one : zero;
    }
  }};
}

template <typename Set> auto support(Set&& s)
{
  using value_type = std::decay_t<Set>::value_type;
  return set{ [=](value_type x) {
    return s(x) > 0 ? value_type{1} : value_type{0};
  }};
}

} // namespace fuzzy
>>>>>>> 8caf26ea9fe555161fe436ecf12e86fefb73d0ab

#endif

//------------------------------------------------------------------------------
