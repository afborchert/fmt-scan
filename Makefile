Header := scan.hpp
Tests := test_suite.cpp
Targets := $(patsubst %.cpp,%,$(Tests))

CXX :=		g++ -std=c++11
CXXFLAGS :=	-Wall -g -Ofast
LDLIBS :=	-lpcre2-8 -lpcre2-16 -lpcre2-32

.PHONY:		all clean
all:		$(Targets)
clean:
		rm -f $(Targets)

$(Targets):	%: %.cpp $(Header)
		$(CXX) $(LDFLAGS) -o $@ $(CXXFLAGS) $< $(LDLIBS)
