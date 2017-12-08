#include <iostream>
#include <iomanip>
#include <vector>
#include <utility>
#include <soft/fuzzy.hxx>

using namespace soft;

// First we introduce a floating type for the rest of the file. This will make
// it easier to change it later if we want double precision. We could have
// made it a property of the fuzzy set type (and maybe still will down the
// road.)

using floating_type = float;

inline constexpr auto operator "" _ft(long double x) noexcept
{
  return static_cast<floating_type>(x);
}

template <typename F> constexpr auto make_graded(F low, F high)
{
  return [=](F x) {
    if (x < low)  return F{0};
    if (x < high) return (x - low) / (high - low);
    else          return F{1};
  };
}

template <typename F> constexpr auto make_triangular(F low, F mid, F high)
{
  return [=](F x) {
    if (x < low)  return F{0};
    if (x <= mid) return (x - low) / (mid - low);
    if (x < high) return (x - mid) / (F{-1} * (high - mid)) + 1;
    else          return F{0};
  };
}

template <typename FuzzySet> auto test()
{
  auto warm = FuzzySet{ make_triangular(68.0_ft, 75.0_ft, 80.0_ft) };
  auto hot  = FuzzySet{ make_graded(75.0_ft, 80.0_ft) };

  for (auto i = 65; i < 90; ++i) {
    std::cout << std::setprecision(3)
	      << "i = "                 << i
	      << "\twarm(i) = "         << warm(i)
	      << "\thot(i) = "          << hot(i)
	      << "\t(warm & hot)(i) = " << (warm & hot)(i)
	      << "\t(warm | hot)(i) = " << (warm | hot)(i)
	      << "\t~hot(i) = "         << (~hot)(i)
	      << '\n';
  }
}

auto test2()
{
  std::vector v{ std::pair{0, 0.1_ft}, {1, 0.2_ft}, {2, 0.3_ft} };
  auto f = fuzzy::set{begin(v), end(v)};

  std::cout << "test 2:\n";
  for (auto i = 0; i < 5; ++i)
    std::cout << "f(i) = " << f(i) << '\n';
}

auto main() -> int
{
  std::cout << "min/max:\n";
  test<fuzzy::set<floating_type, floating_type>>();

  std::cout << "product:\n";
  test<fuzzy::set<floating_type, floating_type,
		  fuzzy::value<floating_type>,
		  fuzzy::traits::product>>();

  test2();
}

//------------------------------------------------------------------------------
