/* 
   Copyright (c) 2017 Andreas F. Borchert
   All rights reserved.

   Permission is hereby granted, free of charge, to any person obtaining
   a copy of this software and associated documentation files (the
   "Software"), to deal in the Software without restriction, including
   without limitation the rights to use, copy, modify, merge, publish,
   distribute, sublicense, and/or sell copies of the Software, and to
   permit persons to whom the Software is furnished to do so, subject to
   the following conditions:

   The above copyright notice and this permission notice shall be
   included in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY
   KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
   WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
   NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
   BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
   ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
   CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE.
*/

/*
   test suite for "scan.hpp"
*/

#include <cstdlib>
#include <iostream>
#include <limits>
#include <sstream>
#include <streambuf>
#include <tuple>
#include <type_traits>
#include <vector>
#include <printf.hpp>
#include "scan.hpp"

static unsigned int testcases = 0;
static unsigned int success = 0;
static unsigned int broken = 0;
static unsigned int invalidre = 0; /* regex considered invalid */

/* type trait to recognize char types which can be distinguished
   from regular numerical types, see also
   http://stackoverflow.com/questions/20958262/char16-t-and-char32-t-types-in-c11
*/
template<typename T> struct is_char : public std::false_type {};
template<> struct is_char<char> : public std::true_type {};
template<> struct is_char<wchar_t> : public std::true_type {};
template<> struct is_char<char16_t> : public std::true_type {};
template<> struct is_char<char32_t> : public std::true_type {};

/* output operator for basic_string objects where CharT != char */
template<typename CharT, typename Traits>
typename std::enable_if<!std::is_same<char, CharT>::value, std::ostream&>::type
operator<<(std::ostream& out,
      const std::basic_string<CharT, Traits>& s) {
   auto& f = std::use_facet<std::codecvt<CharT, char, std::mbstate_t>>(
      out.getloc());
   std::mbstate_t state{};
   std::basic_string<char> converted(f.max_length() * s.size(), 0);
   const CharT* from_next;
   char* to_next;
   auto result = f.out(state,
      /* from */ &s[0], &s[s.size()], from_next,
      /* to */ &converted[0], &converted[converted.size()], to_next);
   if (result == std::codecvt_base::ok) {
      converted.resize(to_next - &converted[0]);
      out << converted;
   } else {
      out.setstate(std::ios_base::failbit);
   }
   return out;
}

/* output operator for std::tuple,
   see also https://stackoverflow.com/questions/9247723/overloading-operator-for-stdtuple-possible-simplications
*/
template <std::size_t index, typename CharT, typename Traits, typename... T>
typename std::enable_if<index >= sizeof...(T)>::type
print_tuple(std::basic_ostream<CharT, Traits>& os,
      std::tuple<T...> tuple) {
}
template <std::size_t index, typename CharT, typename Traits, typename... T>
typename std::enable_if<index < sizeof...(T)>::type
print_tuple(std::basic_ostream<CharT, Traits>& os,
      std::tuple<T...> tuple) {
   if (index > 0) {
      os << ", ";
   }
   os << std::get<index>(tuple);
   print_tuple<index + 1>(os, std::forward<std::tuple<T...>>(tuple));
}
template <typename CharT, typename Traits, typename... T>
std::basic_ostream<CharT, Traits>& operator<<(
      std::basic_ostream<CharT, Traits>& os,
      std::tuple<T...> tuple) {
   os << "[";
   print_tuple<0>(os, std::forward<std::tuple<T...>>(tuple));
   os << "]";
   return os;
}

/* similar to std::integer_sequence of C++14 */
using integer = std::ptrdiff_t;
template<integer... Is> struct seq {
   typedef seq<Is..., sizeof...(Is)> next;
};
template<integer N> struct gen_seq {
   typedef typename gen_seq<N-1>::type::next type;
};
template<> struct gen_seq<0> {
   typedef seq<> type;
};

/* map value types to types which can actually store these values */
template<typename T>
struct simple_type {
   using type = typename
      std::remove_cv<
	 typename std::remove_reference<T>::type
      >::type;
};
template<typename T, typename Enable = void>
struct store_type_helper {
   using type = typename simple_type<T>::type;
};
template<typename T>
struct store_type_helper<T*,
      typename std::enable_if<is_char<T>::value, void>::type> {
   using type = typename std::basic_string<typename simple_type<T>::type>;
};
template<typename T>
struct store_type_helper<T[],
      typename std::enable_if<is_char<T>::value, void>::type> {
   using type = typename std::basic_string<typename simple_type<T>::type>;
};
template<typename T, std::size_t N>
struct store_type_helper<T[N],
      typename std::enable_if<is_char<T>::value, void>::type> {
   using type = typename std::basic_string<typename simple_type<T>::type>;
};
template<typename T>
struct store_type {
   using type = typename store_type_helper<typename simple_type<T>::type>::type;
};

/* special streambuf type that provides control over buffering,
   this allows us to test fmt::scan with cases where multiple
   buffer refills are required */
template<typename CharT, typename Traits = std::char_traits<CharT>>
class testing_streambuf: public std::basic_streambuf<CharT, Traits> {
   public:
      testing_streambuf(std::basic_string<CharT, Traits>& input,
	    std::size_t chunk_size, bool lookahead = false) :
	    input(input), chunk_size(chunk_size),
	    pos(0), len(input.size()), lookahead(lookahead) {
	 if (chunk_size > len) {
	    chunk_size = len;
	 }
	 set_pointers(0);
      }
   protected:
      virtual typename Traits::pos_type seekoff(typename Traits::off_type off,
	       std::ios_base::seekdir dir,
	       std::ios_base::openmode which = std::ios_base::in) {
	    using off_type = typename Traits::off_type;
	    off_type wanted;
	    switch (dir) {
	       case std::ios_base::beg:
		  wanted = off; break;
	       case std::ios_base::end:
		  wanted = len + off; break;
	       case std::ios_base::cur:
		  wanted = pos + off; break;
	       default:
		  return -1;
	    }
	    if (wanted < 0 || wanted > static_cast<off_type>(len)) {
	       return -1;
	    }
	    pos = wanted;
	    set_pointers(pos);
	    return pos;
	 }
      virtual typename Traits::int_type underflow() {
	 if (!this->eback()) {
	    set_pointers(len); return Traits::eof();
	 }
	 pos += this->egptr() - this->eback();
	 if (pos >= len) {
	    set_pointers(len); return Traits::eof();
	 }
	 std::size_t take = len - pos;
	 if (take > chunk_size) {
	    take = chunk_size;
	 }
	 set_pointers(pos);
	 return input[pos];
      }
      virtual std::streamsize showmanyc() {
	 if (lookahead) {
	    if (pos + chunk_size >= len) {
	       return -1;
	    } else {
	       return len - (pos + chunk_size);
	    }
	 } else {
	    return 0;
	 }
      }
   private:
      void set_pointers(std::size_t newpos) {
	 pos = newpos;
	 std::size_t take = len - pos;
	 if (take > chunk_size) {
	    take = chunk_size;
	 }
	 if (take == 0) {
	    this->setg(nullptr, nullptr, nullptr);
	 } else {
	    this->setg(&input[pos], &input[pos], &input[pos+take]);
	 }
      }
      std::basic_string<CharT, Traits>& input;
      std::size_t chunk_size;
      std::size_t pos; /* position of the beginning of the current buf */
      std::size_t len; /* total input length */
      bool lookahead; /* in regard to showmanyc() */
};

/* special streambuf type that provides no buffer access
   much like std::cin etc. unless std::ios::sync_with_stdio(false)
   has been called */
template<typename CharT, typename Traits = std::char_traits<CharT>>
class unbuffered_streambuf: public std::basic_streambuf<CharT, Traits> {
   public:
      unbuffered_streambuf(std::basic_string<CharT, Traits>& input) :
	    input(input),
	    pos(0), len(input.size()) {
      }
   protected:
      using int_type = typename Traits::int_type;
      virtual typename Traits::pos_type seekoff(typename Traits::off_type off,
	       std::ios_base::seekdir dir,
	       std::ios_base::openmode which = std::ios_base::in) {
	    using off_type = typename Traits::off_type;
	    off_type wanted;
	    switch (dir) {
	       case std::ios_base::beg:
		  wanted = off; break;
	       case std::ios_base::end:
		  wanted = len + off; break;
	       case std::ios_base::cur:
		  wanted = pos + off; break;
	       default:
		  return -1;
	    }
	    if (wanted < 0 || wanted > static_cast<off_type>(len)) {
	       return -1;
	    }
	    pos = wanted;
	    return pos;
	 }
      virtual int_type pbackfail(int_type ch) {
	 if (pos == 0) {
	    return Traits::eof();
	 } else if (ch == Traits::eof() ||
	       ch == static_cast<int_type>(input[pos-1])) {
	    return input[--pos];
	 } else {
	    return Traits::eof();
	 }
      }
      virtual int_type uflow() {
	 if (pos >= len) {
	    return Traits::eof();
	 } else {
	    return input[pos++];
	 }
      }
   private:
      std::basic_string<CharT, Traits>& input;
      std::size_t pos; /* position of the beginning of the current buf */
      std::size_t len; /* total input length */
};

template<typename CharT, typename Traits, typename... Values, integer... Is>
std::ptrdiff_t invoke_scan_with_tuple(std::basic_istream<CharT, Traits>& is,
      fmt::regex<CharT>& re, std::tuple<Values...>& vars,
      seq<Is...>) {
   return fmt::scan(is, re, std::get<Is>(vars)...);
}

template<typename CharT, typename Traits, typename... Values>
bool test_scan_from_stream(const std::basic_string<CharT, Traits>& input,
      const std::basic_string<CharT, Traits>& pattern,
      std::basic_istream<CharT, Traits>& is,
      fmt::regex<CharT>& re,
      Values&&... expected_values) {
   ++testcases;
   std::tuple<typename store_type<Values>::type...> vars;
   constexpr auto number_of_results = sizeof...(Values);
   auto res = invoke_scan_with_tuple(is, re, vars,
      typename gen_seq<number_of_results>::type());
   if (res != number_of_results) {
      ++broken;
      fmt::printf("fmt::scan returns %d instead of %d"
	 " for pattern '%s' and input '%s'\n", res, number_of_results,
	 pattern, input);
      return false;
   } else {
      std::tuple<typename store_type<Values>::type...>
	 expected(std::forward<Values>(expected_values)...);
      /* note that expected==expected is not redundant,
         as NaN is always != to itself */
      if (expected != vars && expected == expected) {
	 ++broken;
	 fmt::printf("fmt::scan delivers wrong result"
	    " for pattern '%s' and input '%s':\n", 
	    pattern, input);
	 fmt::printf("   expected: %s\n", expected);
	 fmt::printf("   got:      %s\n", vars);
	 return false;
      } else {
	 ++success;
      }
   }
   return true;
}

/* common test framework that
    - checks the regular expression for validity and bails out, if not,
    - runs a test with and without JIT
    - stress tests the fmt::scan algorithm by using testing_streambuf
      with input in varying chunk sizes
    - where showmanyc is supported or simply returns 0
*/
template<typename CharT, typename Traits, typename Test>
void test_framework(std::basic_string<CharT, Traits> input,
      std::basic_string<CharT, Traits>& pattern, Test&& test) {
   fmt::regex<CharT> re(pattern);
   if (!re.valid()) {
      ++testcases; ++invalidre;
      fmt::printf("invalid regex: '%s'\n", pattern);
      return;
   }
   for (bool enable_jit: {false, true}) {
      std::basic_istringstream<CharT, Traits> is(input);
      if (enable_jit) {
	 re.enable_jit();
      } else {
	 re.disable_jit();
      }
      /* test with a stream where the entire input is immediately
	 visible to fmt::scan */
      if (test(is, re)) {
	 /* stress test the fmt::scan algorithm by using testing_streambuf
	    which delivers the input in small chunk sizes;
	    lookahead allows the engine to recognize eof earlier */
	 for (bool lookahead: {false, true}) {
	    for (std::size_t chunk_size = 1; chunk_size < input.size();
		  ++chunk_size) {
	       testing_streambuf<CharT, Traits> sbuf(input, chunk_size,
		  lookahead);
	       std::basic_istream<CharT, Traits> is(&sbuf);
	       if (!test(is, re)) {
		  fmt::printf("   failed with a buffer chunk size of %d,"
		     " look-ahead = %d, and enable-jit = %d\n",
			chunk_size, lookahead, enable_jit);
		  return;
	       }
	    }
	 }
	 unbuffered_streambuf<CharT, Traits> unbuffered(input);
	 std::basic_istream<CharT, Traits> in(&unbuffered);
	 if (!test(in, re)) {
	    fmt::printf("   failed for an unbuffered stream\n");
	    return;
	 }
      } else {
	 if (enable_jit) {
	    fmt::printf("   failed with JIT enabled\n");
	 } else {
	    fmt::printf("   failed without having JIT enabled\n");
	 }
	 return;
      }
   }
}

template<typename CharT, typename Traits, typename... Values>
void test_scan(std::basic_string<CharT, Traits> input,
      std::basic_string<CharT, Traits> pattern,
      Values&&... values) {
   test_framework(input, pattern,
      [&](std::basic_istream<CharT, Traits>& in,
	    fmt::regex<CharT>& re) -> bool {
	 return test_scan_from_stream(input, pattern,
	    in, re, std::forward<Values>(values)...);
   });
}

template<typename CharT, typename... Values>
void test_scan(const CharT* input, const CharT* pattern,
      Values&&... values) {
   test_scan(std::basic_string<CharT>(input),
      std::basic_string<CharT>(pattern),
      std::forward<Values>(values)...);
}

template<typename CharT>
struct tokenre;
template<>
struct tokenre<char> {
   const char* operator()() { return R"(\s*(.*))"; }
};
template<>
struct tokenre<wchar_t> {
   const wchar_t* operator()() { return LR"(\s*(.*))"; }
};
template<>
struct tokenre<char16_t> {
   const char16_t* operator()() { return uR"(\s*(.*))"; }
};
template<>
struct tokenre<char32_t> {
   const char32_t* operator()() { return UR"(\s*(.*))"; }
};

template<typename CharT>
struct decfmt;
template<>
struct decfmt<char> {
   const char* operator()() { return R"(%d)"; }
};
template<>
struct decfmt<wchar_t> {
   const wchar_t* operator()() { return LR"(%d)"; }
};
template<>
struct decfmt<char16_t> {
   const char16_t* operator()() { return uR"(%d)"; }
};
template<>
struct decfmt<char32_t> {
   const char32_t* operator()() { return UR"(%d)"; }
};

template<typename CharT, typename T>
void run_integer_tests() {
   T values[] = {0, 1, 1234,
      std::numeric_limits<T>::min(),
      std::numeric_limits<T>::max()};
   for (auto val: values) {
      std::basic_ostringstream<CharT> os;
      fmt::printf(os, decfmt<CharT>()(), val);
      test_scan(os.str().c_str(), tokenre<CharT>()(), val);
   }
}

template<typename CharT, typename T>
typename std::enable_if<
   std::is_signed<T>::value && std::is_integral<T>::value &&
   sizeof(T) < sizeof(std::intmax_t)
>::type
run_out_of_range_integer_tests() {
   std::intmax_t values[] = {
      static_cast<std::intmax_t>(std::numeric_limits<T>::min()) - 1,
      static_cast<std::intmax_t>(std::numeric_limits<T>::min()) - 10,
      static_cast<std::intmax_t>(std::numeric_limits<T>::max()) + 1,
      static_cast<std::intmax_t>(std::numeric_limits<T>::max()) + 10,
   };
   for (auto val: values) {
      ++testcases;
      std::basic_ostringstream<CharT> os;
      fmt::printf(os, decfmt<CharT>()(), val);
      auto len = os.str().size(); auto buf = os.str().c_str();
      T value{};
      fmt::regex<CharT> re{tokenre<CharT>()()};
      auto result = fmt::scan_from_buf<CharT>(buf, len, re, value);
      if (result <= 0) {
	 ++success;
      } else {
	 ++broken;
	 fmt::printf("fmt::scan_from_buf returns %d and value %d for "
	    "out-of-range signed input %d\n", result, value, val);
      }
   }
}

template<typename CharT, typename T>
typename std::enable_if<
   !std::is_signed<T>::value && std::is_integral<T>::value &&
   sizeof(T) < sizeof(std::uintmax_t)
>::type
run_out_of_range_integer_tests() {
   std::uintmax_t values[] = {
      static_cast<std::uintmax_t>(std::numeric_limits<T>::max()) + 1,
      static_cast<std::uintmax_t>(std::numeric_limits<T>::max()) + 10,
   };
   for (auto val: values) {
      ++testcases;
      std::basic_ostringstream<CharT> os;
      fmt::printf(os, decfmt<CharT>()(), val);
      auto len = os.str().size(); auto buf = os.str().c_str();
      T value{};
      fmt::regex<CharT> re{tokenre<CharT>()()};
      auto result = fmt::scan_from_buf<CharT>(buf, len, re, value);
      if (result <= 0) {
	 ++success;
      } else {
	 ++broken;
	 fmt::printf("fmt::scan_from_buf returns %d and value %d for "
	    "out-of-range unsigned input %d\n", result, value, val);
      }
   }
}

template<typename T>
void run_float_tests() {
   T values[] = {0, -0.0, -1, 42, 1234.5678, 1.25E-10, 3E+10,
      std::numeric_limits<T>::min() / 2,
      std::numeric_limits<T>::max() * 2,
      std::numeric_limits<T>::min(),
      std::numeric_limits<T>::max(),
      std::numeric_limits<T>::lowest(),
      std::numeric_limits<T>::epsilon(),
      std::sqrt(-1.0f), std::nanf("1"), -std::nanf("1")};
   for (auto value: values) {
      std::ostringstream os;
      fmt::printf(os, "%.*g",
	 std::numeric_limits<T>::max_digits10, value);
      test_scan(os.str().c_str(), R"(\s*(.*))", value);
   }
}

template<typename CharT, typename Traits, typename... Values>
bool test_skip_from_stream(std::basic_string<CharT, Traits> input,
      std::basic_string<CharT, Traits> pattern,
      std::basic_istream<CharT, Traits>& is,
      fmt::regex<CharT>& re,
      std::ptrdiff_t expected_retval,
      typename Traits::pos_type expected_offset) {
   ++testcases;
   auto retval = fmt::scan(is, re);
   if (retval != expected_retval) {
      fmt::printf("fmt::scan returns %d where %d was expected for"
	 " pattern '%s' and input '%s'\n", pattern, input);
      ++broken;
      return false;
   } else if (expected_offset != is.tellg()) {
      fmt::printf("fmt::scan left input stream at pos %d"
	 " instead of %d for pattern '%s' and input '%s'\n",
	 is.tellg(), expected_offset, pattern, input);
      ++broken;
      return false;
   } else {
      ++success;
      return true;
   }
}

template<typename CharT, typename Traits>
void test_skip(std::basic_string<CharT, Traits> input,
      std::basic_string<CharT, Traits> pattern,
      std::ptrdiff_t expected_retval,
      typename Traits::pos_type expected_offset = 0) {
   test_framework(input, pattern,
      [&](std::basic_istream<CharT, Traits>& in,
	    fmt::regex<CharT>& re) -> bool {
	 return test_skip_from_stream(input, pattern,
	    in, re, expected_retval, expected_offset);
   });
}

template<typename CharT>
void test_skip(const CharT* input,
      const CharT* pattern,
      std::ptrdiff_t expected_retval,
      std::ptrdiff_t expected_offset = 0) {
   test_skip(std::basic_string<CharT>(input),
      std::basic_string<CharT>(pattern),
      expected_retval, expected_offset);
}

template<typename CharT, typename Traits, typename... Values>
bool test_scan_with_callouts_from_stream(
      const std::basic_string<CharT, Traits>& input,
      const std::basic_string<CharT, Traits>& pattern,
      std::basic_istream<CharT, Traits>& is,
      fmt::regex<CharT>& re,
      Values&&... expected_values) {
   ++testcases;
   std::vector<std::basic_string<CharT, Traits>> expected({expected_values...});
   std::size_t index = 0; bool aborted = false;
   auto res = fmt::scan_with_callouts(is, re,
	 [&](const fmt::capture<char>& cap) -> bool {
      std::basic_string<CharT, Traits> capture(cap.s, cap.len);
      if (index < expected.size() && capture != expected[index]) {
	 fmt::printf("fmt::scan_with_callouts delivers '%s' instead of '%s'"
	    " for callout #%d"
	    " for pattern '%s' and input '%s'\n",
	    capture, expected[index], index, pattern, input);
	 aborted = true; return false;
      }
      ++index; return true;
   });
   if (aborted) {
      ++broken; return false;
   } else if (res < 0) {
      fmt::printf("fmt::scan_with_callouts returns %d"
	 " but %d results were expected"
	 " for pattern '%s' and input '%s'\n",
	 res, sizeof...(Values));
      ++broken; return false;
   } else {
      std::size_t results = static_cast<std::size_t>(res);
      if (index != results) {
	 fmt::printf("fmt::scan_with_callouts returns %d"
	    " but there were %d callouts"
	    " for pattern '%s' and input '%s'\n",
	    res, index, pattern, input);
	 ++broken; return false;
      } else if (results != sizeof...(Values)) {
	 fmt::printf("fmt::scan_with_callouts returns %d instead of %d"
	    " for pattern '%s' and input '%s'\n",
	    res, sizeof...(Values), pattern, input);
	 ++broken; return false;
      } else {
	 ++success; return true;
      }
   }
}

template<typename CharT, typename Traits, typename... Values>
void test_scan_with_callouts(std::basic_string<CharT, Traits> input,
      std::basic_string<CharT, Traits> pattern,
      Values&&... values) {
   test_framework(input, pattern,
      [&](std::basic_istream<CharT, Traits>& in,
	    fmt::regex<CharT>& re) -> bool {
	 return test_scan_with_callouts_from_stream(input, pattern,
	    in, re, std::forward<Values>(values)...);
   });
}

template<typename CharT, typename... Values>
void test_scan_with_callouts(const CharT* input, const CharT* pattern,
      Values&&... values) {
   test_scan_with_callouts(std::basic_string<CharT>(input),
      std::basic_string<CharT>(pattern),
      std::forward<Values>(values)...);
}

int main() {
   test_scan("", R"((.*))", "");
   test_scan("17", R"((\d+))", 17);
   test_scan("John", R"((.*))", "John");
   test_scan("123 456", R"((\d+)\s+(\d+))", 123, 456);
   test_scan("one\ntwo", R"((.*))", "one");
   test_scan("one\ntwo", R"((?:.*\n)*(.*))", "two");

   /* UTF-8 tests */
   test_scan(u8"\u00fc\u017f", R"((*UTF)(.)(.))", u8"\u00fc", u8"\u017f");
   test_scan(u8"\u00fc\u017f", R"((*UTF)(*UCP)(\w+))", u8"\u00fc\u017f");

   run_integer_tests<char, short>();
   run_integer_tests<char, unsigned short>();
   run_integer_tests<char, int>();
   run_integer_tests<char, unsigned int>();
   run_integer_tests<char, long>();
   run_integer_tests<char, unsigned long>();
   run_integer_tests<char, long long>();
   run_integer_tests<char, unsigned long long>();

   run_integer_tests<wchar_t, short>();
   run_integer_tests<wchar_t, unsigned short>();
   run_integer_tests<wchar_t, int>();
   run_integer_tests<wchar_t, unsigned int>();
   run_integer_tests<wchar_t, long>();
   run_integer_tests<wchar_t, unsigned long>();
   run_integer_tests<wchar_t, long long>();
   run_integer_tests<wchar_t, unsigned long long>();

   run_out_of_range_integer_tests<char, short>();
   run_out_of_range_integer_tests<char, int>();
   run_out_of_range_integer_tests<char, long>();
   run_out_of_range_integer_tests<char, unsigned short>();
   run_out_of_range_integer_tests<char, unsigned int>();
   run_out_of_range_integer_tests<char, unsigned long>();
   run_out_of_range_integer_tests<wchar_t, short>();
   run_out_of_range_integer_tests<wchar_t, int>();
   run_out_of_range_integer_tests<wchar_t, long>();
   run_out_of_range_integer_tests<wchar_t, unsigned short>();
   run_out_of_range_integer_tests<wchar_t, unsigned int>();
   run_out_of_range_integer_tests<wchar_t, unsigned long>();

   run_float_tests<float>();
   run_float_tests<double>();
   run_float_tests<long double>();

   test_skip("17x", R"(\d+)", 0, 2);
   test_skip("x", R"(\d+)", -1, 0);
   test_skip("123456789x", R"(\d{10})", -1, 0);

   test_scan_with_callouts("This is\na text across\nmultiple lines.\n",
      R"((?:(.*)\n(?C))*)", "This is", "a text across", "multiple lines.");
   test_scan_with_callouts("1234 48 28 -13", R"((?:\s*([+-]?\d+)(?C))*)",
      "1234", "48", "28", "-13");
   test_scan_with_callouts("multiple:fields:within:a:line\nmore:stuff\n",
      R"((?:([^:\n]*):(?C))*([^:\n]*)\n(?C))", "multiple",
      "fields", "within", "a", "line");
   test_scan_with_callouts("  multiple  :fields : within:a:: :  line \n",
      R"((?:\h*([^:\n]*?)\h*:(?C))*\h*([^:\n]*?)\h*\n(?C))",
      "multiple", "fields", "within", "a", "", "", "line");

   fmt::printf("%u/%u tests succeeded\n", success, testcases);
}
