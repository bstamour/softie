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

#ifndef BST_GA_HXX_
#define BST_GA_HXX_

#include <algorithm>
#include <functional>
#include <iterator>
#include <numeric>
#include <random>
#include <tuple>
#include <utility>
#include <vector>

namespace soft::ga {

//------------------------------------------------------------------------------

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

//------------------------------------------------------------------------------

struct algorithm_traits {
  int iterations;
  int max_mutations;
  int max_crossovers;
};

template <typename Pool, typename ScoreFunc, typename MutateFunc,
          typename CrossoverFunc, typename Generator>
auto run_genetic_algorithm(Pool const& pool, ScoreFunc f, MutateFunc m,
                           CrossoverFunc c, Generator gen,
                           algorithm_traits const& traits)
{
  using item_type        = typename std::remove_reference_t<Pool>::value_type;
  using score_type       = decltype(f(std::declval<item_type&>()));
  using scored_item_type = std::pair<item_type*, score_type>;

  // Score the initial population.
  auto scored_items = std::vector<scored_item_type> {}
  scored_items.reserve(size(pool) + traits.max_crossovers);
  for (auto const& item : pool) scored_items.emplace_back(&item, f(item));

  for (auto iteration = 0; iteration < traits.iterations; ++iteration) {

    // 1. Mutate.
    auto mutate_dist = std::uniform_int_distribution<>{0, traits.max_mutations};
    sample_and_apply(begin(scored_items), end(scored_items), mutate_dist(gen),
                     gen, [&](auto p) {
                       *p.first = m(*p.first); // mutate...
                       p.second = f(*p.first); // then re-score
                       return p;
                     });

    // 2. Crossover.
    auto cross_dist = std::uniform_int_distribution<>{0, traits.max_crossovers};
    auto old_end    = end(scored_items);
    std::generate_n(std::back_inserter(scored_items), cross_dist(gen), [&] {
      auto picker = [&] {
        return rejection_sample(begin(scored_items), old_end, gen,
                                [](auto const& x) { return x.second; });
      };

      auto parent_1 = picker();
      auto parent_2 = picker();
      auto child    = c(*parent_1->first, *parent_2->first);
      return std::pair{child, f(child)};
    });

    // Cull the pool back down to the original size.
    std::nth_element(
        begin(scored_items), std::next(begin(scored_items), size(pool)),
        end(scored_items),
        [](auto lhs, auto rhs) { return lhs.second < rhs.second; });
    scored_items.erase(std::next(begin(scored_items), size(pool)),
                       end(scored_items));
  } // end of main loop.

  // Return the element with the highest score.
  auto p = std::max_element(
      begin(scored_items), end(scored_items),
      [](auto lhs, auto rhs) { return lhs.second < rhs.second; });
  return *p->first;
}

//------------------------------------------------------------------------------

} // namespace soft::ga

#endif
