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

#ifndef BST_DE_HXX_
#define BST_DE_HXX_

#include <algorithm>
#include <array>
#include <deque>
#include <random>
#include <type_traits>

//------------------------------------------------------------------------------

namespace soft::de {

// Template-template type holder.
template <template <typename...> typename TT> struct template_template {
  template <typename... Ts> using applied_type = TT<Ts...>;
};

template <template <typename...> typename TT> using tt = template_template<TT>;

// template-template detection.
template <typename TT> struct is_template_template : std::false_type {
};
template <template <typename...> typename TT>
struct is_template_template<template_template<TT>> : std::true_type {
};

template <typename TT>
constexpr bool is_template_template_v = is_template_template<TT>::value;

// Apply a type list to a wrapped template-template to get a concrete type.
template <typename TT, typename... Ts> struct apply_types {
  static_assert(is_template_template_v<TT>,
                "First parameter must be a template-template");
  using type = typename TT::template applied_type<Ts...>;
};

template <typename TT, typename... Ts>
using apply_types_t = typename apply_types<TT, Ts...>::type;

//------------------------------------------------------------------------------

template <typename T>
using uniform_dist = bst::apply_types_t<
    std::conditional_t<std::is_integral_v<T>,
                       bst::tt<std::uniform_int_distribution>,
                       bst::tt<std::uniform_real_distribution>>,
    T>;

template <int N, typename Iter, typename Gen>
auto sample_n(Iter first, Iter last, Gen g)
{
  using std::begin;
  std::array<typename Iter::value_type, N> values;
  std::sample(first, last, begin(values) N, g);
  return values;
}

template <typename Iter, typename CR, typename D, typename F>
auto differential_evolution(Iter first,
                            Iter last,
                            CR cross_prob,
                            D weight,
                            F fitness,
                            std::size_t iterations)
{
  using value_type = typename Iter::value_type;
  std::deque<value_type> solutions(first, last);
  std::mt19937 gen;

  for (std::size_t iteration{}; iteration < iterations; ++iteration) {
    for (std::size_t j{}; j < size(solutions); ++j) {
      auto x = solutions.front();
      solutions.pop_front();

      auto y        = x;
      auto[a, b, c] = sample_n<3>(begin(solutions), end(solutions), gen);

      uniform_dist<std::size_t> dist_r{0, size(x) - 1};
      auto r = dist_r(gen);

      uniform_dist<CR> dist_cr{0, 1};
      for (std::size_t i{}; i < size(x); ++i) {
        auto ri = dist_cr(gen);
        if (ri < cross_prob || i == r) y[i] = a[i] + weight * (b[i] - c[i]);
      }

      solutions.push_back(fitness(y) > fitness(x) ? y : x);
    }
  }

  return *std::max_element(
      begin(solutions), end(solutions), [&](auto const& x, auto const& y) {
        return fitness(x) < fitness(y);
      });
}

} // namespace soft::de

//------------------------------------------------------------------------------

#endif
