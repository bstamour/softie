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

#ifndef BST_SOFT_ALGO_HXX_
#define BST_SOFT_ALGO_HXX_

#include <type_traits>
#include <vector>
#include <algorithm>
#include <iterator>
#include <random>

//------------------------------------------------------------------------------

namespace soft::algorithm {

template <typename Iter, typename Distance, typename Generator, typename Func>
void sample_and_apply(Iter first, Iter last, Distance n, Generator g, Func f)
{
  using value_type = typename std::remove_reference_t<Iter>::value_type;
  auto refs        = std::vector<value_type*>{};
  refs.reserve(n);
  std::sample(first, last, std::back_inserter(refs), n, g);
  for (auto& x : refs) *x = f(*x);
}

template <typename Iter, typename Gen, typename ScoreFunc>
auto rejection_sample(Iter first, Iter last, Gen gen, ScoreFunc f) -> Iter
{
  auto const num_elements  = std::distance(first, last);
  auto const highest_value = *std::max_element(
      first, last, [&](auto const& x, auto const& y) { return f(x) < f(y); });
  auto dist        = std::uniform_int_distribution<>{0, num_elements - 1};
  using value_type = decltype(highest_value);

  while (true) {
    auto pos = std::next(first, dist(gen)); // Move to a random element.
    auto val_dist =
        std::uniform_real_distribution<value_type>{0, highest_value};
    auto y = val_dist(gen); // sample.
    if (y < f(*pos))        // accept or reject.
      return *pos;
  }
}

} // namespace soft::algorithm

//------------------------------------------------------------------------------

#endif
