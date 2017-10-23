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
   This header-only C++11 package provides fmt::scan which
   allows to process formatted input from a stream using
   regular expressions. To compile it, you need a compiler
   for C++11 and the pcre2 library from http://www.pcre.org/.

   A regular expression specifies how much input is to be
   read. If not specified, the regular expressions are
   greedy but ".*" does not match a newline, "\n" has to
   be given explicitly. See
      http://www.pcre.org/current/doc/html/pcre2pattern.html
   for a comprehensive manual page covering the syntax of
   support regular expressions which is quite similar to
   those supported by Perl. Internally, following options
   of pcre2 are used:

    * PCRE2_ANCHORED, i.e. scanning stops as soon as the
      input is not matched by the pattern. To be skipped
      input must be specified by the pattern.

    * PCRE2_MULTILINE, i.e. "^" and "$" match the beginning
      and end of a line. This library assumes the begin of
      the current input to be at the beginning of a line.
      However, if the character preceding it is still available
      from the input buffer, a beginning of a line is assumed
      if and only if it is either a linefeed or carriage return.

    * PCRE2_BSR_ANYCRLF, i.e. \R matches only CR, LF, or CRLF.
      This behaviour can be overwritten within the pattern,
      i.e. "(*ANY)" includes also all Unicode newline sequences.

   Subpatterns that are enclosed in parentheses are considered
   as captures where the corresponding matched substring is
   used as formatted input for the corresponding variable in
   the parameter list. If the capturing functionality is not
   wanted, "?:" can be inserted behind the opening "(" to
   suppress this feature. Example: "(?:fred)*" scans arbitrary
   sequences of "fred" strings without capturing them.

   Following example shows how an input lines can be read
   from the stream _in_ without capturing the line terminator:

      fmt::regex<char> regex(R"((.*)\n)");
      std::string line;
      while (fmt::scan(in, regex, line) > 0) {
	 // process line
      }

   fmt::scan returns the number of successful captures.
   Multiple captures are supported. Following example
   expects an input line to consist of two colon-separated
   fields where the first is a string and the second an
   integer. Leading and trailing whitespaces are to be
   skipped:

      fmt::regex<char> regex(R"(\s*(.*?)\s*:\s*(\d+)\s*\n)");
      std::string s; int val;
      while (fmt::scan(in, regex, s, val) == 2) {
	 // process s and val
      }

   Callouts can be used if the number of to be captured
   items is not known beforehand. Captures with callouts
   use a different syntax in the regular expressions.
   A callout is triggered by "(?C)", or, when necessary,
   with an identifying mark which can be an integer or a
   string:

   Callouts are triggered with the construct "(?C...)"
   which delivers the last captured subpattern. Hence,
   you will use a regular subpattern as before followed
   by a callout. Example: "(\d+)(?C)".
   Where necessary, a callout capture can be marked with
   an integer or a string:

      (?C17)          callout capture with number 17
      (?C"val")       callout with name "val"

   For callouts function objects are to be used which
   accept as parameter a capture object with following
   components:

      struct capture {
	 const CharT* s;
	 std::size_t len;
	 std::uint32_t callout_number;
	 const CharT* callout_string;
      };

   _s_ points to the captured object with _len_
   bytes into the input buffer. _callout_number_ or _callout_string_
   are set to the corresponding marks when used. Otherwise, they
   are initialized to 0 and nullptr, respectively.

   Callouts are not working as intuitively as the other approach
   as they may be invoked within the course of backtracking.
   Assume you want to extract all lines of the input without
   the line terminators. This can be done as follows:

      std::list<std::string> lines;
      fmt::regex<char> regex(R"((?:(.*)\n(?C))*)");
      std::ptrdiff_t res;
      while ((res = fmt::scan_with_callouts(std::cin, regex,
		  [&](const fmt::capture<char>& cap) -> bool {
	       lines.emplace(cap.s, cap.len);
	       return true;
	    })) > 0) {
      }

   Note that (?C) has been inserted behind the line terminator.
   Alternatively, you could have tried to insert (?C) immediately
   behind the the to-be-captured line contents:

      fmt::regex<char> regex(R"((?:(.*)(?C)\n)*)");

   This may cause the callout to be invoked behind the last line
   as (.*) may match the empty string and (?C) was given in
   front of \n which is not found.

   The return value of the function object determines whether
   pattern matching continues or is to be aborted with
   fmt::scan_with_callouts returning -1.
*/

#ifndef FMT_SCAN_HPP
#define FMT_SCAN_HPP

#if __cplusplus < 201103L
#error This file requires compiler and library support for the \
ISO C++ 2011 standard.
#else

#include <cmath>
#include <codecvt>
#include <cstdlib>
#include <functional>
#include <iostream>
#include <limits>
#include <list>
#include <locale>
#include <memory>
#include <sstream>
#include <stdexcept>
#include <string>
#include <tuple>
#include <utility>

/* we are using all three libraries, i.e. those for 8, 16, and 32 bits
   to support char, wchar_t, char16_t and char32_t;
   this means that we have to use the fully qualified
   function names, e.g. pcre2_compile_8(), pcre2_compile_16(),
   and pcre2_compile_32() and the corresponding types
   PCRE2_UCHAR8, PCRE2_UCHAR16, and PCRE2_UCHAR32 */
#define PCRE2_CODE_UNIT_WIDTH 0
#include <pcre2.h>

namespace fmt {

namespace scan_impl {

/* type trait to recognize char types which can be distinguished
   from regular numerical types, see also
   http://stackoverflow.com/questions/20958262/char16-t-and-char32-t-types-in-c11
*/
template<typename T> struct is_char : public std::false_type {};
template<> struct is_char<char> : public std::true_type {};
template<> struct is_char<wchar_t> : public std::true_type {};
template<> struct is_char<char16_t> : public std::true_type {};
template<> struct is_char<char32_t> : public std::true_type {};

/* map character types to the corresponding pcre2 character types */
template<std::size_t N> struct pcre_char_by_size;
template<> struct pcre_char_by_size<1> { using type = PCRE2_UCHAR8; };
template<> struct pcre_char_by_size<2> { using type = PCRE2_UCHAR16; };
template<> struct pcre_char_by_size<4> { using type = PCRE2_UCHAR32; };
template<typename T> struct pcre_char {
   using type = typename pcre_char_by_size<sizeof(T)>::type;
};

/* map char* types to the corresponding pcre2 char* types */
template<std::size_t N> struct pcre_string_by_size;
template<> struct pcre_string_by_size<1> { using type = PCRE2_SPTR8; };
template<> struct pcre_string_by_size<2> { using type = PCRE2_SPTR16; };
template<> struct pcre_string_by_size<4> { using type = PCRE2_SPTR32; };
template<typename T> struct pcre_string {
   using type = typename pcre_string_by_size<sizeof(T)>::type;
};

template<std::size_t N> struct pcre_code_by_size;
template<> struct pcre_code_by_size<1> { using type = pcre2_code_8; };
template<> struct pcre_code_by_size<2> { using type = pcre2_code_16; };
template<> struct pcre_code_by_size<4> { using type = pcre2_code_32; };
template<typename T> struct pcre_code {
   using type = typename pcre_code_by_size<sizeof(T)>::type;
};

template<std::size_t N> struct pcre_match_data_by_size;
template<> struct pcre_match_data_by_size<1> {
   using type = pcre2_match_data_8;
};
template<> struct pcre_match_data_by_size<2> {
   using type = pcre2_match_data_16;
};
template<> struct pcre_match_data_by_size<4> {
   using type = pcre2_match_data_32;
};
template<typename T> struct pcre_match_data {
   using type = typename pcre_match_data_by_size<sizeof(T)>::type;
};

template<std::size_t N> struct pcre_match_context_by_size;
template<> struct pcre_match_context_by_size<1> {
   using type = pcre2_match_context_8;
};
template<> struct pcre_match_context_by_size<2> {
   using type = pcre2_match_context_16;
};
template<> struct pcre_match_context_by_size<4> {
   using type = pcre2_match_context_32;
};
template<typename T> struct pcre_match_context {
   using type = typename pcre_match_context_by_size<sizeof(T)>::type;
};

template<std::size_t N> struct pcre_callout_block_by_size;
template<> struct pcre_callout_block_by_size<1> {
   using type = pcre2_callout_block_8;
};
template<> struct pcre_callout_block_by_size<2> {
   using type = pcre2_callout_block_16;
};
template<> struct pcre_callout_block_by_size<4> {
   using type = pcre2_callout_block_32;
};
template<typename T> struct pcre_callout_block {
   using type = typename pcre_callout_block_by_size<sizeof(T)>::type;
};

template<std::size_t size> struct callout_function_by_size {
   using type = std::function<int(typename pcre_callout_block_by_size<size>::type* block)>;
};
template<typename T> struct callout_function {
   using type = typename callout_function_by_size<sizeof(T)>::type;
};

extern "C" {
   inline int callout_function_8(pcre2_callout_block_8* block, void* f) {
      return (*static_cast<callout_function_by_size<1>::type*>(f))(block);
   }
   inline int callout_function_16(pcre2_callout_block_16* block, void* f) {
      return (*static_cast<callout_function_by_size<2>::type*>(f))(block);
   }
   inline int callout_function_32(pcre2_callout_block_32* block, void* f) {
      return (*static_cast<callout_function_by_size<4>::type*>(f))(block);
   }
}

template<std::size_t N> struct pcre2_jit_stack_by_size;
template<> struct pcre2_jit_stack_by_size<1> {
   using type = pcre2_jit_stack_8;
};
template<> struct pcre2_jit_stack_by_size<2> {
   using type = pcre2_jit_stack_16;
};
template<> struct pcre2_jit_stack_by_size<4> {
   using type = pcre2_jit_stack_32;
};
template<typename T> struct pcre2_jit_stack {
   using type = typename pcre2_jit_stack_by_size<sizeof(T)>::type;
};

/* overloaded functions for pcre2_get_error_message */
inline int pcre2_get_error_message(int error_number,
      PCRE2_UCHAR8* buffer, std::size_t size) {
   return pcre2_get_error_message_8(error_number, buffer, size);
}
inline int pcre2_get_error_message(int error_number,
      PCRE2_UCHAR16* buffer, std::size_t size) {
   return pcre2_get_error_message_16(error_number, buffer, size);
}
inline int pcre2_get_error_message(int error_number,
      PCRE2_UCHAR32* buffer, std::size_t size) {
   return pcre2_get_error_message_32(error_number, buffer, size);
}

/* wrapper for pcre2_get_error_message that delivers a std::string */
template<typename CharT>
std::basic_string<CharT> get_error_message(int error_number) {
   std::size_t bufsize = 64;
   using char_type = typename pcre_char<CharT>::type;
   for(;;) {
      std::unique_ptr<char_type[]> buf(new char_type[bufsize]);
      int retval = pcre2_get_error_message(error_number, buf.get(), bufsize);
      if (retval > 0) {
	 return std::basic_string<CharT>(
	    reinterpret_cast<CharT*>(buf.get()),
	    static_cast<std::size_t>(retval));
      }
      bufsize *= 2;
   }
}

/* overloaded functions for pcre2_compile;
   as we do not need to change the defaults of the compile context
   we omit this parameter here */
inline pcre2_code_8* pcre2_compile(PCRE2_SPTR8 pattern,
      PCRE2_SIZE length, std::uint32_t options,
      int* errorcode, PCRE2_SIZE* erroroffset) {
   return pcre2_compile_8(pattern, length, options,
      errorcode, erroroffset, nullptr);
}
inline pcre2_code_16* pcre2_compile(PCRE2_SPTR16 pattern,
      PCRE2_SIZE length, std::uint32_t options,
      int* errorcode, PCRE2_SIZE* erroroffset) {
   return pcre2_compile_16(pattern, length, options,
      errorcode, erroroffset, nullptr);
}
inline pcre2_code_32* pcre2_compile(PCRE2_SPTR32 pattern,
      PCRE2_SIZE length, std::uint32_t options,
      int* errorcode, PCRE2_SIZE* erroroffset) {
   return pcre2_compile_32(pattern, length, options,
      errorcode, erroroffset, nullptr);
}

/* overloaded functions for pcre2_jit_compile */
inline int pcre2_jit_compile(pcre2_code_8* code, std::uint32_t options) {
   return pcre2_jit_compile_8(code, options);
}
inline int pcre2_jit_compile(pcre2_code_16* code, std::uint32_t options) {
   return pcre2_jit_compile_16(code, options);
}
inline int pcre2_jit_compile(pcre2_code_32* code, std::uint32_t options) {
   return pcre2_jit_compile_32(code, options);
}

/* overloaded functions for pcre2_code_free */
inline void pcre2_code_free(pcre2_code_8* code) {
   pcre2_code_free_8(code);
}
inline void pcre2_code_free(pcre2_code_16* code) {
   pcre2_code_free_16(code);
}
inline void pcre2_code_free(pcre2_code_32* code) {
   pcre2_code_free_32(code);
}

/* overloaded functions for pcre2_match_data_create_from_pattern */
inline pcre2_match_data_8* pcre2_match_data_create_from_pattern(
      const pcre2_code_8* code) {
   return pcre2_match_data_create_from_pattern_8(code, nullptr);
}
inline pcre2_match_data_16* pcre2_match_data_create_from_pattern(
      const pcre2_code_16* code) {
   return pcre2_match_data_create_from_pattern_16(code, nullptr);
}
inline pcre2_match_data_32* pcre2_match_data_create_from_pattern(
      const pcre2_code_32* code) {
   return pcre2_match_data_create_from_pattern_32(code, nullptr);
}

/* overloaded functions for pcre2_match_data_free */
inline void pcre2_match_data_free(pcre2_match_data_8* match_data) {
   pcre2_match_data_free_8(match_data);
}
inline void pcre2_match_data_free(pcre2_match_data_16* match_data) {
   pcre2_match_data_free_16(match_data);
}
inline void pcre2_match_data_free(pcre2_match_data_32* match_data) {
   pcre2_match_data_free_32(match_data);
}

/* overloaded functions for pcre2_match_context_create
   we accept the default pcre2_general_context and pass nullptr */
template<std::size_t size>
inline typename pcre_match_context_by_size<size>::type* pcre2_match_context_create();
template<>
inline pcre2_match_context_8* pcre2_match_context_create<1>() {
   return pcre2_match_context_create_8(nullptr);
}
template<>
inline pcre2_match_context_16* pcre2_match_context_create<2>() {
   return pcre2_match_context_create_16(nullptr);
}
template<>
inline pcre2_match_context_32* pcre2_match_context_create<4>() {
   return pcre2_match_context_create_32(nullptr);
}

/* overloaded functions for pcre2_match_context_free */
inline void pcre2_match_context_free(pcre2_match_context_8* match_context) {
   pcre2_match_context_free_8(match_context);
}
inline void pcre2_match_context_free(pcre2_match_context_16* match_context) {
   pcre2_match_context_free_16(match_context);
}
inline void pcre2_match_context_free(pcre2_match_context_32* match_context) {
   pcre2_match_context_free_32(match_context);
}

/* overloaded functions for pcre2_match */
inline int pcre2_match(const pcre2_code_8* code, PCRE2_SPTR8 subject,
      PCRE2_SIZE length, PCRE2_SIZE startoffset,
      std::uint32_t options,
      pcre2_match_data_8* match_data,
      pcre2_match_context_8* mcontext) {
   return pcre2_match_8(code, subject, length, startoffset,
      options, match_data, mcontext);
}
inline int pcre2_match(const pcre2_code_16* code, PCRE2_SPTR16 subject,
      PCRE2_SIZE length, PCRE2_SIZE startoffset,
      std::uint32_t options,
      pcre2_match_data_16* match_data,
      pcre2_match_context_16* mcontext) {
   return pcre2_match_16(code, subject, length, startoffset,
      options, match_data, mcontext);
}
inline int pcre2_match(const pcre2_code_32* code, PCRE2_SPTR32 subject,
      PCRE2_SIZE length, PCRE2_SIZE startoffset,
      std::uint32_t options,
      pcre2_match_data_32* match_data,
      pcre2_match_context_32* mcontext) {
   return pcre2_match_32(code, subject, length, startoffset,
      options, match_data, mcontext);
}

/* overloaded functions for pcre2_get_ovector_count */
inline std::uint32_t pcre2_get_ovector_count(pcre2_match_data_8* match_data) {
   return pcre2_get_ovector_count_8(match_data);
}
inline std::uint32_t pcre2_get_ovector_count(pcre2_match_data_16* match_data) {
   return pcre2_get_ovector_count_16(match_data);
}
inline std::uint32_t pcre2_get_ovector_count(pcre2_match_data_32* match_data) {
   return pcre2_get_ovector_count_32(match_data);
}

/* overloaded functions for pcre2_get_ovector_pointer */
inline PCRE2_SIZE* pcre2_get_ovector_pointer(pcre2_match_data_8* match_data) {
   return pcre2_get_ovector_pointer_8(match_data);
}
inline PCRE2_SIZE* pcre2_get_ovector_pointer(pcre2_match_data_16* match_data) {
   return pcre2_get_ovector_pointer_16(match_data);
}
inline PCRE2_SIZE* pcre2_get_ovector_pointer(pcre2_match_data_32* match_data) {
   return pcre2_get_ovector_pointer_32(match_data);
}

/* overloaded functions for pcre2_set_callout
   which are already specialized for our external "C" callout functions defined above
*/
inline int pcre2_set_callout(pcre2_match_context_8* mcontext,
      typename callout_function_by_size<1>::type* fp) {
   return pcre2_set_callout_8(mcontext, callout_function_8, static_cast<void*>(fp));
}
inline int pcre2_set_callout(pcre2_match_context_16* mcontext,
      typename callout_function_by_size<2>::type* fp) {
   return pcre2_set_callout_16(mcontext, callout_function_16, static_cast<void*>(fp));
}
inline int pcre2_set_callout(pcre2_match_context_32* mcontext,
      typename callout_function_by_size<4>::type* fp) {
   return pcre2_set_callout_32(mcontext, callout_function_32, static_cast<void*>(fp));
}

/* JIT stack management functions;
   note that we always pass a nullptr to gcontext */
template<std::size_t size>
inline typename pcre2_jit_stack_by_size<size>::type* pcre2_jit_stack_create(
   PCRE2_SIZE startsize, PCRE2_SIZE maxsize);
template<>
inline pcre2_jit_stack_8* pcre2_jit_stack_create<1>(PCRE2_SIZE startsize,
      PCRE2_SIZE maxsize) {
   return pcre2_jit_stack_create_8(startsize, maxsize, nullptr);
}
template<>
inline pcre2_jit_stack_16* pcre2_jit_stack_create<2>(PCRE2_SIZE startsize,
      PCRE2_SIZE maxsize) {
   return pcre2_jit_stack_create_16(startsize, maxsize, nullptr);
}
template<>
inline pcre2_jit_stack_32* pcre2_jit_stack_create<4>(PCRE2_SIZE startsize,
      PCRE2_SIZE maxsize) {
   return pcre2_jit_stack_create_32(startsize, maxsize, nullptr);
}
inline void pcre2_jit_stack_assign(pcre2_match_context_8* mcontext,
      pcre2_jit_stack_8* jit_stack) {
   pcre2_jit_stack_assign_8(mcontext, nullptr, jit_stack);
}
inline void pcre2_jit_stack_assign(pcre2_match_context_16* mcontext,
      pcre2_jit_stack_16* jit_stack) {
   pcre2_jit_stack_assign_16(mcontext, nullptr, jit_stack);
}
inline void pcre2_jit_stack_assign(pcre2_match_context_32* mcontext,
      pcre2_jit_stack_32* jit_stack) {
   pcre2_jit_stack_assign_32(mcontext, nullptr, jit_stack);
}
inline void pcre2_jit_stack_free(pcre2_jit_stack_8* jit_stack) {
   pcre2_jit_stack_free_8(jit_stack);
}
inline void pcre2_jit_stack_free(pcre2_jit_stack_16* jit_stack) {
   pcre2_jit_stack_free_16(jit_stack);
}
inline void pcre2_jit_stack_free(pcre2_jit_stack_32* jit_stack) {
   pcre2_jit_stack_free_32(jit_stack);
}

/* the destructor of std::num_get is protected,
   hence we need a derived class to make it public;
   see http://en.cppreference.com/w/cpp/locale/num_get/%7Enum_get */
template<typename CharT>
struct destructible_num_get: public std::num_get<CharT, const CharT*> {
   destructible_num_get(std::size_t refs = 0) :
      std::num_get<CharT, const CharT*>(refs) {
   }
};

/* compare sptr[0..len-1] against other[0..len-1]
   where all characters are converted to lower case
   using the given locale */
template<typename CharT>
bool compare_lcstring(const std::locale& loc,
      const CharT* sptr, std::size_t len, const char* other) {
   for (std::size_t i = 0; i < len; ++i) {
      CharT ch1 = sptr[i];
      char ch2 = other[i];
      if (ch1 != ch2 && std::tolower(ch1, loc) != std::tolower(ch2, loc)) {
	 return false;
      }
   }
   return true;
}

class regex_error : public std::runtime_error {
   public:
      regex_error(int errorcode, PCRE2_SIZE erroroffset) :
	 std::runtime_error(get_error_message<char>(errorcode)),
	 errorcode(errorcode), erroroffset(erroroffset) {
      }
      int code() const {
	 return errorcode;
      }
      std::size_t offset() const {
	 return static_cast<std::size_t>(erroroffset);
      }
   private:
      int errorcode;
      PCRE2_SIZE erroroffset;
};

template<typename CharT> class match_result;

template<typename CharT>
class regex {
   private:
      using code = typename pcre_code<CharT>::type;
      using sptr = typename pcre_string<CharT>::type;
      using match_data = typename pcre_match_data<CharT>::type;
      using match_context = typename pcre_match_context<CharT>::type;
      int errorcode;
      PCRE2_SIZE erroroffset;
      code* codeptr;
      mutable bool jit; /* has codeptr been compiled for JIT? */
      bool suppress_jit; /* suppress compilation for JIT */
      friend class match_result<CharT>;
   public:
      regex() : errorcode(0), erroroffset(PCRE2_UNSET), codeptr(nullptr),
	    jit(false), suppress_jit(false) {
      }
      regex(const CharT* pattern, std::size_t length, uint32_t options = 0)
	    throw(regex_error, std::bad_alloc)
	    :
	    errorcode(0), erroroffset(PCRE2_UNSET),
	    codeptr(
	       pcre2_compile(
		  reinterpret_cast<sptr>(pattern),
		  static_cast<PCRE2_SIZE>(length),
		  PCRE2_MULTILINE | PCRE2_ANCHORED,
		  &errorcode, &erroroffset)
	    ),
	    jit(false), suppress_jit(false) {
	 if (!codeptr) {
	    throw regex_error(errorcode, erroroffset);
	 }
      }
      regex(const std::basic_string<CharT>& pattern) :
	 regex(pattern.c_str(), pattern.length()) {
      }
      regex(const CharT* pattern) :
	    regex(std::basic_string<CharT>(pattern)) {
      }
      regex(const regex& other) = delete;
      friend void swap(regex& re1, regex& re2) {
	 std::swap(re1.errorcode, re2.errorcode);
	 std::swap(re1.erroroffset, re2.erroroffset);
	 std::swap(re1.codeptr, re2.codeptr);
	 std::swap(re1.jit, re2.jit);
	 std::swap(re1.suppress_jit, re2.suppress_jit);
      }
      regex(regex&& other) : regex() {
	 swap(*this, other);
      }
      ~regex() {
	 if (codeptr) {
	    pcre2_code_free(codeptr);
	 }
      }
      regex& operator=(const regex& other) = delete;
      regex& operator=(regex&& other) {
	 swap(*this, other);
      }
      bool valid() const {
	 return codeptr != nullptr;
      }
      bool prepare() const {
	 if (jit || suppress_jit) {
	    return true;
	 } else {
	    if (pcre2_jit_compile(codeptr,
		  PCRE2_JIT_COMPLETE | PCRE2_JIT_PARTIAL_HARD) < 0) {
	       return false;
	    } else {
	       jit = true;
	       return true;
	    }
	 }
      }
      bool disable_jit() {
	 if (jit) {
	    return false;
	 } else {
	    suppress_jit = true;
	    return true;
	 }
      }
      bool enable_jit() {
	 suppress_jit = false;
	 return prepare();
      }
};

using match_status = enum : int { no_match = -2, partial_match = -1 };

template<typename CharT>
class match_result {
   private:
      using code = typename pcre_code<CharT>::type;
      using sptr = typename pcre_string<CharT>::type;
      using match_data = typename pcre_match_data<CharT>::type;
      using match_context = typename pcre_match_context<CharT>::type;
      using callout_function = typename callout_function<CharT>::type;
      using jit_stack = typename pcre2_jit_stack<CharT>::type;

      code* codeptr;
      match_data* match_data_ptr;
      match_context* match_context_ptr;
      jit_stack* jit_stack_ptr;
      uint32_t count;
      PCRE2_SIZE* ovector;
      callout_function callout_f;
      std::function<void()> reset_callouts;

   public:
      match_result() :
	    codeptr(nullptr), match_data_ptr(nullptr),
	    match_context_ptr(nullptr), jit_stack_ptr(nullptr),
	    count(0), ovector(nullptr) {
      }
      match_result(const regex<CharT>& re) throw(std::bad_alloc) :
	    codeptr(re.codeptr), match_data_ptr(nullptr),
	    match_context_ptr(nullptr), jit_stack_ptr(nullptr),
	    count(0), ovector(nullptr) {
	 if (codeptr) {
	    match_data_ptr = pcre2_match_data_create_from_pattern(codeptr);
	    if (!match_data_ptr) {
	       throw std::bad_alloc();
	    }
	    re.prepare(); /* enable JIT, if not suppressed */
	 }
      }
      match_result(const match_result& other) = delete;
      friend void swap(match_result& mr1, match_result& mr2) {
	 std::swap(mr1.codeptr, mr2.codeptr);
	 std::swap(mr1.match_data_ptr, mr2.match_data_ptr);
	 std::swap(mr1.match_context_ptr, mr2.match_context_ptr);
	 std::swap(mr1.jit_stack_ptr, mr2.jit_stack_ptr);
	 std::swap(mr1.count, mr2.count);
	 std::swap(mr1.ovector, mr2.ovector);
	 std::swap(mr1.callout_f, mr2.callout_f);
	 std::swap(mr1.reset_callouts, mr2.reset_callouts);
      }
      match_result(match_result&& other) : match_result() {
	 swap(*this, other);
      }
      ~match_result() {
	 if (codeptr) {
	    if (jit_stack_ptr) {
	       pcre2_jit_stack_free(jit_stack_ptr);
	    }
	    if (match_context_ptr) {
	       pcre2_match_context_free(match_context_ptr);
	    }
	    if (match_data_ptr) {
	       pcre2_match_data_free(match_data_ptr);
	    }
	 }
      }
      match_result& operator=(const match_result& other) = delete;
      match_result& operator=(match_result&& other) {
	 swap(*this, other);
      }

      match_status match(const CharT* subject, std::size_t length,
	    bool bol, bool eof) {
	 int options = PCRE2_BSR_ANYCRLF;
	 if (!bol) options |= PCRE2_NOTBOL;
	 if (!eof) options |= PCRE2_NOTEOL | PCRE2_PARTIAL_HARD;

	 /* we retry this if we need a stack for JIT */
	 int retval;
	 for(;;) {
	    retval = pcre2_match(codeptr,
	       reinterpret_cast<sptr>(subject),
	       static_cast<PCRE2_SIZE>(length), 0,
	       options, match_data_ptr, match_context_ptr);
	    if (retval < 0) {
	       count = 0;
	       if (retval == PCRE2_ERROR_NOMATCH) return no_match;
	       if (retval == PCRE2_ERROR_PARTIAL) return partial_match;
	       if (retval == PCRE2_ERROR_JIT_STACKLIMIT && !jit_stack_ptr) {
		  /* we are running out of stack;
		     the default stack of 32k is quite small,
		     we allocate a larger stack and give it just
		     one more try */
		  jit_stack_ptr = pcre2_jit_stack_create<sizeof(CharT)>(
		     64 * 1024 * 1024, /* twice the default of 32k */
		     1024 * 1024 * 1024 /* quote: A maximum stack of
			512K to 1M should be more than enough for any
			pattern */
		  );
		  if (jit_stack_ptr) {
		     pcre2_jit_stack_assign(match_context_ptr, jit_stack_ptr);
		     continue; /* retry pcre2_match */
		  }
	       }
	       if (!eof) {
		  /* UTF-8 or UTF-16 errors resulting from incomplete
		     input; in this case we consider this to be
		     a partial match */
		  switch (retval) {
		     case PCRE2_ERROR_UTF8_ERR1:
		     case PCRE2_ERROR_UTF8_ERR2:
		     case PCRE2_ERROR_UTF8_ERR3:
		     case PCRE2_ERROR_UTF8_ERR4:
		     case PCRE2_ERROR_UTF8_ERR5:
		     case PCRE2_ERROR_UTF16_ERR1:
			return partial_match;
		  }
	       }
	       throw regex_error(retval, 0);
	    } else {
	       break; /* successful match */
	    }
	 }
	 count = pcre2_get_ovector_count(match_data_ptr);
	 ovector = pcre2_get_ovector_pointer(match_data_ptr);
	 return static_cast<match_status>(retval-1);
      }
      std::size_t size() const {
	 return count;
      }
      std::pair<std::size_t, std::size_t> operator()(std::size_t index) const
	    throw(std::range_error) {
	 if (index >= count)
	    throw std::range_error("match_result ovector index out of range");
	 return std::make_pair(
	    static_cast<std::size_t>(ovector[index*2]),
	    static_cast<std::size_t>(ovector[index*2+1]));
      }
      std::size_t length() const {
	 return (*this)(0).second;
      }

      template<typename Callout>
      void set_callout(Callout&& callout) {
	 allocate_context_ptr();
	 callout_f = callout_function(std::forward<Callout>(callout));
	 int retval = pcre2_set_callout(match_context_ptr, &callout_f);
	 if (retval < 0) {
	    throw regex_error(retval, 0);
	 }
      }

      std::function<void()>& reset_callouts_handler() {
	 return reset_callouts;
      }
      const std::function<void()>& reset_callouts_handler() const {
	 return reset_callouts;
      }

   private:
      void allocate_context_ptr() {
	 if (!match_context_ptr) {
	    match_context_ptr = pcre2_match_context_create<sizeof(CharT)>();
	    if (!match_context_ptr) {
	       throw std::bad_alloc();
	    }
	 }
      }
};

/* we need access to the pointers of the get area of a stream;
   unfortunately these methods are protected;
   we circumvent this by constructing sneaking_streambuf
   objects that are copy-constructed from the original stream buffer */
template <typename charT, typename traits = std::char_traits<charT>>
struct sneaking_streambuf: std::basic_streambuf<charT, traits> {
   sneaking_streambuf(const std::basic_streambuf<charT, traits>* buf) :
      std::basic_streambuf<charT, traits>(*buf) {
   }
   /* pointer to the beginning of the input buffer */
   charT* get_eback() const {
      return this->eback();
   }
   /* pointer to the current position in the input buffer */
   charT* get_gptr() const {
      return this->gptr();
   }
   /* pointer to the end of the input buffer */
   charT* get_egptr() const {
      return this->egptr();
   }
   /* number of characters available beyond the buffer contents */
   std::streamsize get_showmanyc() {
      return this->showmanyc();
   }
};

template <typename charT, typename traits = std::char_traits<charT>>
std::tuple<charT*, charT*, charT*, std::streamsize> get_streambuf_pointers(
      const std::basic_istream<charT, traits>& in) {
   sneaking_streambuf<charT, traits> sbuf(in.rdbuf());
   return std::make_tuple(sbuf.get_eback(), sbuf.get_gptr(), sbuf.get_egptr(),
      sbuf.get_showmanyc());
}

/* internal signed integer type which is used for indices and byte counts */
using integer = std::ptrdiff_t;

/* similar to std::integer_sequence of C++14 */
template<integer... Is> struct seq {
   typedef seq<Is..., sizeof...(Is)> next;
};
template<integer N> struct gen_seq {
   typedef typename gen_seq<N-1>::type::next type;
};
template<> struct gen_seq<0> {
   typedef seq<> type;
};

/* idea taken from
   http://stackoverflow.com/questions/21062864/optimal-way-to-access-stdtuple-element-in-runtime-by-index
*/

/* apply f on the n-th element of a tuple for compile-time n */
template<integer N, typename Tuple, typename Function>
inline auto apply(const Tuple& tuple, Function&& f)
      -> decltype(f(std::get<N>(tuple))) {
   return f(std::get<N>(tuple));
}

/* helper to apply f on the n-th element of a tuple for runtime n */
template<typename Tuple, typename Function, integer... Is>
inline auto apply(const Tuple& tuple, integer index, Function&& f, seq<Is...>)
      -> decltype(f(std::get<0>(tuple))) {
   using apply_t = decltype(&apply<0, Tuple, Function>);
   static const apply_t apply_functions[] = {&apply<Is, Tuple, Function>...};
   return apply_functions[index](tuple, std::forward<Function>(f));
}

/* apply f on the n-th element of a tuple for runtime n */
template<typename Tuple, typename Function>
inline auto apply(const Tuple& tuple, integer index, Function&& f)
      -> decltype(f(std::get<0>(tuple))) {
   return apply(tuple, index, std::forward<Function>(f),
      typename gen_seq<std::tuple_size<Tuple>::value>::type());
}

/* template that delivers a quiet NaN */
template<typename T>
struct get_quiet_nan {
   T operator()() {
      return std::numeric_limits<T>::quiet_NaN();
   }
};

/* streambuf that represents a stretch of memory */
template<typename CharT, typename Traits = std::char_traits<CharT>>
class mem_streambuf: public std::basic_streambuf<CharT, Traits> {
   public:
      mem_streambuf(CharT* buf, std::size_t len) :
	    buf(buf), len(len), pos(0) {
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
	 return Traits::eof();
      }
   private:
      void set_pointers(std::size_t newpos) {
	 pos = newpos;
	 this->setg(buf, buf + pos, buf + len);
      }
      CharT* buf;
      std::size_t pos; /* position of the beginning of the current buf */
      std::size_t len; /* total input length */
};

/* catch-all variant that tries to apply the >>-operator */
template<typename Variable, typename CharT, typename Traits>
typename std::enable_if<
      !std::is_floating_point<Variable>::value &&
      !std::is_integral<Variable>::value,
bool>::type
copy_capture(std::basic_istream<CharT, Traits>& in,
      Variable& var, const CharT* sptr, std::size_t len) {
   /* make [sptr, sptr+len) available as stream */
   mem_streambuf<CharT, Traits> sbuf(sptr, len);
   std::basic_istream<CharT, Traits> ins(&sbuf);
   /* copy locale and formatting flags from in to ins */
   ins.imbue(in.getloc());
   ins.flags(in.flags());
   /* invoke input operator */
   ins >> var;
   return !!ins;
}

/* special variants of copy_capture */

/* copy a capture to a corresponding std::basic_string object */
template<typename CharT, typename StreamTraits, typename StringTraits>
bool copy_capture(std::basic_istream<CharT, StreamTraits>& in,
      std::basic_string<CharT, StringTraits>& s,
      const CharT* sptr, std::size_t len) {
   s.assign(sptr, len);
   return true;
}

/* copy a capture to a single character of the associated type */
template<typename CharT, typename Traits>
bool copy_capture(std::basic_istream<CharT, Traits>& in,
      CharT& ch, const CharT* sptr, std::size_t len) {
   if (len >= 1) {
      ch = *sptr;
      return true;
   } else {
      return false;
   }
}

/* copy a capture to a single character of the same size */
template<typename CharT, typename Traits, typename T>
typename std::enable_if<
   is_char<T>::value &&
   !std::is_same<CharT, T>::value &&
   sizeof(T) == sizeof(CharT),
bool>::type
copy_capture(std::basic_istream<char, Traits>& in,
      T& value, const CharT* sptr, std::size_t len) {
   if (len == 0) return false;
   value = static_cast<T>(*sptr);
   return true;
}

/* copy a capture to a single character of a different size */
template<typename CharT, typename Traits, typename T>
typename std::enable_if<
   is_char<T>::value &&
   !std::is_same<CharT, T>::value &&
   sizeof(T) != sizeof(CharT),
bool>::type
copy_capture(std::basic_istream<char, Traits>& in,
      T& value, const CharT* sptr, std::size_t len) {
   if (len == 0) return false;
   auto& f = std::use_facet<std::codecvt<T, CharT, std::mbstate_t>>(
      in.getloc());
   std::mbstate_t state{};
   const CharT* from_next = sptr;
   T* to_next;
   auto result = f.out(state,
      /* from */ sptr, sptr + len, from_next,
      /* to */ &value, &value + 1, to_next);
   return result = std::codecvt_base::ok;
}

/* copy captures for floating point values
   this is based on std::num_get but we support also
   NaN and Inf notations much like strtod */
template<typename CharT, typename Traits, typename T>
typename std::enable_if<std::is_floating_point<T>::value, bool>::type
copy_capture(std::basic_istream<CharT, Traits>& in,
      T& value, const CharT* sptr, std::size_t len) {
   if (len == 0) return false;
   destructible_num_get<CharT> f;
   std::ios_base::iostate err = std::ios_base::goodbit;
   f.get(sptr, sptr + len, in, err, value);
   if (err == std::ios_base::goodbit || err == std::ios_base::eofbit) {
      /* eof is ok and quite natural as f.get accesses the
	 captured part only */
      return true;
   }
   /* support NaN and Inf which are not supported by std::num_get */
   if (len < 3) return false;
   T sign(1.0);
   if (*sptr == '+' || *sptr == '-') {
      if (*sptr == '-') sign = -sign;
      ++sptr; --len;
      if (len < 3) return false;
   }
   auto loc = in.getloc();
   if (compare_lcstring(loc, sptr, 3, "inf")) {
      value = std::numeric_limits<T>::infinity() * sign;
      return true;
   } else if (compare_lcstring(loc, sptr, 3, "nan")) {
      if (sign > 0) {
	 value = get_quiet_nan<T>()();
      } else {
	 value = -get_quiet_nan<T>()();
      }
      return true;
   } else {
      return false;
   }
}

/* formatted input of integral values but not characters,
   this is done using std::num_get */
template<typename CharT, typename Traits, typename T>
typename std::enable_if<
   std::is_integral<T>::value &&
   !is_char<T>::value &&
   /* int & short are not supported by std::num_get */
   !std::is_same<T, int>::value &&
   !std::is_same<T, short>::value,
bool>::type
copy_capture(std::basic_istream<CharT, Traits>& in,
      T& value, const CharT* sptr, std::size_t len) {
   if (len == 0) return false;
   destructible_num_get<CharT> f;
   std::ios_base::iostate err = std::ios_base::goodbit;
   f.get(sptr, sptr + len, in, err, value);
   return err == std::ios_base::goodbit || err == std::ios_base::eofbit;
}

/* formatted input of int & short which are
   not supported by std::num_get directly */
template<typename CharT, typename Traits, typename T>
typename std::enable_if<
   std::is_same<T, int>::value || std::is_same<T, short>::value,
bool>::type
copy_capture(std::basic_istream<CharT, Traits>& in,
      T& value, const CharT* sptr, std::size_t len) {
   if (len == 0) return false;
   destructible_num_get<CharT> f;
   std::ios_base::iostate err = std::ios_base::goodbit;
   long int long_value;
   f.get(sptr, sptr + len, in, err, long_value);
   if ((err == std::ios_base::goodbit || err == std::ios_base::eofbit) &&
	 long_value >= std::numeric_limits<T>::min() &&
	 long_value <= std::numeric_limits<T>::max()) {
      value = static_cast<T>(long_value);
      return true;
   } else {
      return false;
   }
}

template<typename CharT, typename Traits>
struct process_capture_f {
   process_capture_f(std::basic_istream<CharT, Traits>& in,
	 const CharT* sptr, std::size_t len) : in(in), sptr(sptr), len(len) {
   }
   template<typename Variable>
   bool operator()(Variable& var) {
      return copy_capture(in, var, sptr, len);
   }
   std::basic_istream<CharT, Traits>& in;
   const CharT* sptr;
   std::size_t len;
};

template<typename Tuple, typename CharT, typename Traits>
inline bool process_capture(std::basic_istream<CharT, Traits>& in,
      const Tuple& tuple, integer index, const CharT* sptr, std::size_t len) {
   if (index >= 0 &&
	 index < static_cast<integer>(std::tuple_size<Tuple>::value)) {
      return apply(tuple, index, process_capture_f<CharT, Traits>(in,
	 sptr, len));
   } else {
      return false;
   }
}

/* general case: process_captures with at least one variable */
template<typename CharT, typename Traits, typename... Variable>
integer process_captures(std::basic_istream<CharT, Traits>& in,
      match_result<CharT>& mr, const CharT* buf, Variable&... variables) {
   std::tuple<Variable&...> tuple(variables...);
   integer captures = static_cast<integer>(mr.size()) - 1;
   constexpr integer nof_vars = static_cast<integer>(sizeof...(Variable));
   if (captures > nof_vars) {
      throw std::invalid_argument(
	 "number of captures exceeds the number of variables");
   }
   std::size_t begin_offset;
   std::size_t end_offset;
   for (integer i = 1; i <= captures; ++i) {
      std::tie(begin_offset, end_offset) = mr(i);
      if (!process_capture(in, tuple, i-1, buf + begin_offset,
	    end_offset - begin_offset)) {
	 return i-1;
      }
   }
   return captures;
}

/* special case: process_captures with no variables */
template<typename CharT, typename Traits, typename... Variable>
integer process_captures(std::basic_istream<CharT, Traits>& in,
      match_result<CharT>& mr, const CharT* buf) {
   integer captures = static_cast<integer>(mr.size()) - 1;
   if (captures > 0) {
      throw std::invalid_argument(
	 "number of captures exceeds the number of variables");
   }
   return 0;
}

/* scan routine for fmt::scan and fmt::scan_with_callouts
   which is used as a fallback by scan for unbuffered streams --
   this is inherently inefficient as we have to operate character
   by character which leads to a complexity of O(n^2) instead of O(n) */
template<typename CharT, typename Traits, typename ProcessCaptures>
integer scan_from_unbuffered_stream(std::basic_istream<CharT, Traits>& in,
      match_result<CharT>& mr,
      ProcessCaptures&& process_captures,
      std::shared_ptr<std::basic_string<CharT, Traits>> inputbufptr) {
   if (inputbufptr) {
      inputbufptr->erase();
   } else {
      inputbufptr = std::make_shared<std::basic_string<CharT, Traits>>();
   }
   std::basic_string<CharT, Traits>& inputbuf(*inputbufptr);

   bool bol = true; bool eof = in.eof();
   if (in.fail()) return -1;
   /* try to see if the last character was a line separator */
   in.unget();
   if (in.fail()) {
      in.clear(); /* reset bad bit */
   } else {
      /* ok, retrieve last character and check if it was
	 a line terminator */
      auto ch = in.get();
      bol = (ch == '\n' || ch == '\r');
   }
   for(;;) {
      auto ch = in.get();
      if (ch == Traits::eof()) {
	 eof = true;
      } else {
	 inputbuf.push_back(ch);
      }
      auto len = inputbuf.size();
      auto res = mr.match(&inputbuf[0], len, bol, eof);
      if (res >= 0) {
	 integer result = process_captures(&inputbuf[0]);
	 auto consumed = mr.length();
	 while (len > consumed && !in.fail()) {
	    in.unget(); --len;
	 }
	 return result;
      }
      if (res != partial_match) {
	 /* try to unget the input we have seen */
	 while (len > 0 && !in.fail()) {
	    in.unget(); --len;
	 }
	 return -1;
      }
      if (mr.reset_callouts_handler()) {
	 mr.reset_callouts_handler()();
      }
   }
}

/* common scan routine which is used by fmt::scan and fmt::scan_with_callouts;
   if inputbufptr is non-null it is used whenever an additional input buffer
   is required; all pointers passed to process_captures point either into
   the life buffer or into the input buffer; if no input buffer is passed,
   all captures need to be copied if they subsequently needed */
template<typename CharT, typename Traits, typename ProcessCaptures>
integer scan(std::basic_istream<CharT, Traits>& in,
      match_result<CharT>& mr,
      ProcessCaptures&& process_captures,
      std::shared_ptr<std::basic_string<CharT, Traits>> inputbufptr = nullptr) {
   CharT* back; CharT* begin; CharT* end; std::streamsize showmanyc;
   std::tie(back, begin, end, showmanyc) = get_streambuf_pointers(in);
   if (back == nullptr || begin == nullptr || end == nullptr) {
      return scan_from_unbuffered_stream(in, mr, process_captures, inputbufptr);
   }
   if (begin == end && !in.eof()) {
      in.rdbuf()->sgetc();
      std::tie(back, begin, end, showmanyc) = get_streambuf_pointers(in);
   }
   bool bol = true;
   if (back < begin) {
      /* if we can check the character preceding the current position,
         we use that to determine whether we are at the beginning of
	 a line or not */
      CharT ch = begin[-1];
      bol = ch == '\n' || ch == '\r';
   }
   bool eof = in.eof() || showmanyc < -1;
   auto res = mr.match(begin, end - begin, bol, eof);
   if (res >= 0) {
      integer result = process_captures(begin);
      auto len = mr.length();
      in.rdbuf()->pubseekoff(len, std::ios_base::cur, std::ios_base::in);
      return result;
   }
   if (res != partial_match) return -1;
   /* remember relative offset; if matching fails
      we try to go back if possible */
   std::ptrdiff_t seek_offset = 0;
   /* if we get a partial match, we need to save the input buffer,
      and refill it; from now on we work on the saved input buffer
      and not longer that of the stream as this provides us just
      its tail; unfortunately, pcre2 does not yet support matches on
      a series of input buffers */
   if (inputbufptr) {
      inputbufptr->erase();
      inputbufptr->append(begin, end - begin);
   } else {
      inputbufptr = std::make_shared<std::basic_string<CharT, Traits>>
	 (begin, end - begin);
   }
   std::basic_string<CharT, Traits>& inputbuf(*inputbufptr);
   for(;;) {
      /* reset callout handlers for a partial match */
      if (mr.reset_callouts_handler()) {
	 mr.reset_callouts_handler()();
      }
      /* update bol, i.e. check if we are at the beginning of a line */
      CharT lastch = inputbuf.back();
      bol = lastch == '\n' || lastch == '\r';
      /* skip the current input buffer */
      in.rdbuf()->pubseekoff(end - begin, std::ios_base::cur, std::ios_base::in);
      seek_offset += end - begin;
      /* trigger an underflow operation */
      in.rdbuf()->sgetc();
      /* update our streambuf pointers */
      std::tie(back, begin, end, showmanyc) = get_streambuf_pointers(in);
      /* even in case of eof we must rerun match as its behaviour
         is different once we have recognized end of input */
      eof = begin == end || showmanyc < 0;
      std::size_t oldlen = inputbuf.length();
      /* append the entire stream buffer contents to inputbuf */
      inputbuf.append(begin, end - begin);
      res = mr.match(inputbuf.c_str(), inputbuf.length(), bol, eof);
      if (res >= 0) {
	 /* we are done, no more input is required */
	 auto len = mr.length();
	 if (len >= oldlen) {
	    in.rdbuf()->pubseekoff(len - oldlen, std::ios_base::cur,
	       std::ios_base::in);
	 }
	 return process_captures(inputbuf.c_str());
      }
      if (res != partial_match) {
	 /* try to go back to original input position, if possible */
	 in.rdbuf()->pubseekoff(-seek_offset, std::ios_base::cur,
	    std::ios_base::in);
	 return -1;
      }
   }
}

/* simplified scan routine that works on a fix-sized buffer;
   this is useful for nested input handling where scan_from_buf
   is invoked from within a callout handler */
template<typename CharT, typename ProcessCaptures>
integer scan_from_buf(const CharT* buf, std::size_t len,
      match_result<CharT>& mr,
      ProcessCaptures&& process_captures) {
   auto res = mr.match(buf, len, /* bol = */ true, /* eof = */ true);
   if (res >= 0) {
      return process_captures(buf);
   } else {
      return -1;
   }
}

} // namespace scan_impl

/* import regex & regex_error into the fmt namespace from fmt::scan_impl */
template<typename CharT>
using regex = scan_impl::regex<CharT>;
using regex_error = scan_impl::regex_error;

/* regular scan function which expects a regex object;
   note that by default JIT compilation will be enabled */
template<typename CharT, typename Traits, typename... Variable>
inline std::ptrdiff_t scan(std::basic_istream<CharT, Traits>& in,
      const regex<CharT>& re, Variable&... variables) {
   scan_impl::match_result<CharT> mr(re);
   return scan_impl::scan(in, mr, [&](const CharT* buf) {
      return scan_impl::process_captures(in, mr, buf, variables...);
   });
}

/* the following two functions allow a regular expression
   to be passed as a string; this is intended for quick shots
   where a JIT compilation possibly generates too much overhead */
template<typename CharT, typename Traits, typename... Variable>
inline std::ptrdiff_t scan(std::basic_istream<CharT, Traits>& in,
      const std::basic_string<CharT, Traits>& pattern, Variable&... variables) {
   fmt::regex<CharT> re(pattern); re.disable_jit();
   scan_impl::match_result<CharT> mr(re);
   return scan_impl::scan(in, mr, [&](const CharT* buf) {
      return scan_impl::process_captures(in, mr, buf, variables...);
   });
}

template<typename CharT, typename Traits, typename... Variable>
inline std::ptrdiff_t scan(std::basic_istream<CharT, Traits>& in,
      const CharT* pattern, Variable&... variables) {
   std::size_t len = 0; while (pattern[len]) ++len;
   fmt::regex<CharT> re(pattern, len); re.disable_jit();
   scan_impl::match_result<CharT> mr(re);
   return scan_impl::scan(in, mr, [&](const CharT* buf) {
      return scan_impl::process_captures(in, mr, buf, variables...);
   });
}

template<typename CharT, typename... Variable>
inline std::ptrdiff_t scan_from_buf(const CharT* buf, std::size_t len,
      const regex<CharT>& re, Variable&... variables) {
   scan_impl::match_result<CharT> mr(re);
   std::basic_istream<CharT> in(nullptr);
   return scan_impl::scan_from_buf(buf, len, mr, [&](const CharT* buf) {
      return scan_impl::process_captures(in, mr, buf, variables...);
   });
}

template<typename CharT>
struct capture {
   capture() : s(nullptr), len(0),
      callout_number(0), callout_string(nullptr) {
   }
   capture(const CharT* captured, std::size_t captured_len,
      std::uint32_t callout_number, const CharT* callout_string) :
	 s(captured), len(captured_len),
	 callout_number(callout_number), callout_string(callout_string) {
   }
   const CharT* s;
   std::size_t len;
   std::uint32_t callout_number;
   const CharT* callout_string;
};

template<typename CharT, typename Traits, typename Callout>
inline std::ptrdiff_t scan_with_callouts(std::basic_istream<CharT, Traits>& in,
      const regex<CharT>& re, Callout&& callout) {
   scan_impl::match_result<CharT> mr(re);
   /* we must not invoke the callout handler immediately as
      this might be premature in case of partial matches;
      hence we put all captures in a linear list which is
      emptied whenever the match engine is restarted
   */
   std::list<capture<CharT>> captures;
   mr.reset_callouts_handler() = [&captures]() { captures.clear(); };
   mr.set_callout([&](typename scan_impl::pcre_callout_block<CharT>::type*
	 callout_block) -> int {
      const CharT* captured = nullptr;
      std::size_t captured_len = 0;
      if (callout_block->capture_last > 0) {
	 auto index = callout_block->capture_last * 2;
	 auto begin_offset = callout_block->offset_vector[index];
	 auto end_offset = callout_block->offset_vector[index+1];
	 captured = reinterpret_cast<const CharT*>(callout_block->subject) +
	    begin_offset;
	 captured_len = reinterpret_cast<std::size_t>
	    (end_offset - begin_offset);
	 captures.emplace(captures.end(), captured, captured_len,
	    callout_block->callout_number,
	    reinterpret_cast<const CharT*>(callout_block->callout_string));
      }
      return 0;
   });
   /* we pass an input buffer here as we must make sure that the
      buffer is still accessible after scan_impl::scan is finished */
   auto inputbufptr = std::make_shared<std::basic_string<CharT, Traits>>();
   /* we ignore all captures here as they are supposed to be handled
      by the callout handler */
   auto res = scan_impl::scan(in, mr, [](const CharT* buf) { return 0; },
      inputbufptr);
   if (res < 0) return res;
   /* invoke the callout function for all captures */
   std::size_t count = 0;
   for (auto& capture: captures) {
      if (!callout(capture)) break;
      ++count;
   }
   return count;
}

} // namespace fmt

#endif // of #if __cplusplus < 201103L #else ...
#endif // of #ifndef FMT_SCAN_HPP
