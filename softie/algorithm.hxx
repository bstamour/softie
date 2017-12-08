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
