


//------------------------------------------------------------------------------

#ifndef BST_GA_HXX_
#define BST_GA_HXX_

#include <vector>
#include <algorithm>
#include <numeric>
#include <tuple>
#include <utility>
#include <functional>
#include <random>
#include <iterator>

namespace soft::ga {

//------------------------------------------------------------------------------

template <typename Iter, typename Distance, typename Generator, typename Func>
void sample_and_apply(Iter first, Iter last, Distance n, Generator g, Func f)
{
  using value_type = typename std::remove_reference_t<Iter>::value_type;
  std::vector<std::reference_wrapper<value_type>> refs;
  refs.reserve(n);
  std::sample(first, last, std::back_inserter(refs), n, g);
  for (auto& x : refs)
    x.get() = f(x);
}

template <typename Iter, typename Gen, typename ScoreFunc>
auto rejection_sample(Iter first, Iter last, Gen gen, ScoreFunc f)
{
  auto const num_elements = std::distance(first, last);
  auto const highest_value = *std::max_element(
    first, last,
    [&](auto const& x, auto const& y) {
      return f(x) < f(y);
    });

  std::uniform_int_distribution<> dist{0, num_elements - 1};
  while (true) {
    auto pos = std::next(first, dist(gen)); // Move to a random element.
    using value_type = decltype(highest_value);
    std::uniform_real_distribution<value_type> val_dist{0, highest_value};
    auto y = val_dist(gen);                 // sample.
    if (y < f(*pos))                        // accept or reject.
      return *pos;
  }
}

//------------------------------------------------------------------------------

struct algorithm_traits {
  int iterations;
  int max_mutations;
  int max_crossovers;
};

template <typename Pool, typename ScoreFunc,
	  typename MutateFunc, typename CrossoverFunc,
	  typename Generator>
auto run_genetic_algorithm(Pool const& pool,
			   ScoreFunc f, MutateFunc m, CrossoverFunc c,
			   Generator gen,
			   algorithm_traits const& traits)
{
  using item_type        = typename std::remove_reference_t<Pool>::value_type;
  using score_type       = decltype (f(std::declval<item_type&>()));
  using scored_item_type = std::pair<item_type*, score_type>;

  // Score the initial population.
  std::vector<scored_item_type> scored_items;
  scored_items.reserve(size(pool) + traits.max_crossovers);
  for (auto const& item : pool)
    scored_items.emplace_back(&item, f(item));

  for (int iteration = 0; iteration < traits.iterations; ++iteration) {
    // 1. Mutate.
    std::uniform_int_distribution<> mutate_dist{0, traits.max_mutations};
    sample_and_apply(
      begin(scored_items), end(scored_items),
      mutate_dist(gen),
      gen,
      [&](auto p)
      {
	*p.first = m(*p.first);    // mutate...
	p.second = f(*p.first);    // then re-score
	return p;
      });

    // 2. Crossover.
    std::uniform_int_distribution<> cross_dist{0, traits.max_crossovers};
    auto old_end = end(scored_items);
    std::generate_n(
      std::back_inserter(scored_items),
      cross_dist(gen),
      [&] {
	auto picker = [&] {
	  return rejection_sample(
	    begin(scored_items), old_end,
	    gen,
	    [](auto const& x) { return x.second; });
	};

	auto parent_1 = picker();
	auto parent_2 = picker();
	auto child = c(*parent_1->first, *parent_2->first);
	return std::pair{child, f(child)};
      });

    // Cull the pool back down to the original size.
    std::nth_element(
      begin(scored_items),
      std::next(begin(scored_items), size(pool)),
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
