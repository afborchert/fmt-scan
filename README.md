# fmt::scan
fmt::scan allows to process formatted input from a stream
using regular expressions

## Summary

This header-only C++11 package provides `fmt::scan` which
allows to process formatted input from a stream using
regular expressions. To compile it, you need a compiler
for C++11 and the pcre2 library from http://www.pcre.org/.

A regular expression specifies how much input is to be
read. If not specified, the regular expressions are
greedy but `".*"` does not match a newline, `"\n"` has to
be given explicitly. See
   http://www.pcre.org/current/doc/html/pcre2pattern.html
for a comprehensive manual page covering the syntax of
support regular expressions which is quite similar to
those supported by Perl. Internally, following options
of pcre2 are used:

 * `PCRE2_ANCHORED`, i.e. scanning stops as soon as the
   input is not matched by the pattern. To be skipped
   input must be specified by the pattern.

 * `PCRE2_MULTILINE`, i.e. `"^"` and `"$"` match the beginning
   and end of a line. This library assumes the begin of
   the current input to be at the beginning of a line.
   However, if the character preceding it is still available
   from the input buffer, a beginning of a line is assumed
   if and only if it is either a linefeed or carriage return.

 * `PCRE2_BSR_ANYCRLF`, i.e. `\R` matches only CR, LF, or CRLF.
   This behaviour can be overwritten within the pattern,
   i.e. `"(*ANY)"` includes also all Unicode newline sequences.

Subpatterns that are enclosed in parentheses are considered
as captures where the corresponding matched substring is
used as formatted input for the corresponding variable in
the parameter list. If the capturing functionality is not
wanted, "?:" can be inserted behind the opening "(" to
suppress this feature. Example: `"(?:fred)*"` scans arbitrary
sequences of "fred" strings without capturing them.

Following example shows how an input lines can be read
from the stream _in_ without capturing the line terminator:

```C++
   fmt::regex<char> regex(R"((.*)\n)");
   std::string line;
   while (fmt::scan(in, regex, line) > 0) {
      // process line
   }
```

fmt::scan returns the number of successful captures.
Multiple captures are supported. Following example
expects an input line to consist of two colon-separated
fields where the first is a string and the second an
integer. Leading and trailing whitespaces are to be
skipped:

```C++
   fmt::regex<char> regex(R"(\s*(.*?)\s*:\s*(\d+)\s*\n)");
   std::string s; int val;
   while (fmt::scan(in, regex, s, val) == 2) {
      // process s and val
   }
```

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

   `(?C17)`          callout capture with number 17
   `(?C"val")`       callout with name "val"

For callouts function objects are to be used which
accept as parameter a capture object with following
components:

```C++
   struct capture {
      const CharT* s;
      std::size_t len;
      std::uint32_t callout_number;
      const CharT* callout_string;
   };
```

_s_ points to the captured object with _len_
bytes into the input buffer. `callout_number` or `callout_string`
are set to the corresponding marks when used. Otherwise, they
are initialized to 0 and nullptr, respectively.

Callouts are not working as intuitively as the other approach
as they may be invoked within the course of backtracking.
Assume you want to extract all lines of the input without
the line terminators. This can be done as follows:

```C++
   std::list<std::string> lines;
   fmt::regex<char> regex(R"((?:(.*)\n(?C))*)");
   std::ptrdiff_t res;
   while ((res = fmt::scan_with_callouts(std::cin, regex,
	       [&](const fmt::capture<char>& cap) -> bool {
	    lines.emplace(cap.s, cap.len);
	    return true;
	 })) > 0) {
   }
```

Note that `(?C)` has been inserted behind the line terminator.
Alternatively, you could have tried to insert `(?C)` immediately
behind the the to-be-captured line contents:

```C++
   fmt::regex<char> regex(R"((?:(.*)(?C)\n)*)");
```

This may cause the callout to be invoked behind the last line
as `(.*)` may match the empty string and `(?C)` was given in
front of \n which is not found.

The return value of the function object determines whether
pattern matching continues or is to be aborted with
`fmt::scan_with_callouts` returning -1.

## Limitations

Whenever possible, the implementation works directly on the
`std::basic_streambuf` object of the input stream. This is
efficient as no extra copy is made before the input is
processed by the regular expression engine. Also, if possible,
conversions from captured substrings are done directly from
the stream buffer.

However, this does not work out for the standard input
stream, i.e. `std::cin`, if it is still synchronized with
the _stdio_. Such cases are supported by the library but
come with significant overhead. This can be avoided by
invoking

```C++
   std::ios::sync_with_stdio(false);
```

right at the beginning of _main_.

## License

This package is available under the terms of
the [MIT License](https://opensource.org/licenses/MIT).

## Files

To use fmt::scan, you will need just to drop
[scan.hpp](https://github.com/afborchert/fmt-scan/blob/master/scan.hpp)
within your project and `#include` it.

The source file `test_suite.cpp` is a test suite
for `fmt::scan` and the Makefile helps to compile it.
