# Makefile for main
# Y. Liow
CXX      = g++
CXXFLAGS = -g -Wall -std=c++11
LINK     = g++
LINKFLAGS =
OBJS     = main.o lexer.o parser.o
EXE      = main.exe

$(EXE): $(OBJS)
	$(LINK) $(LINKFLAGS) $(OBJS) -o $(EXE)

main.o: main.cpp parser.h lexer.h token.h
	$(CXX) $(CXXFLAGS) main.cpp -c -o main.o

lexer.o: lexer.cpp lexer.h token.h
	$(CXX) $(CXXFLAGS) lexer.cpp -c -o lexer.o

parser.o: parser.cpp parser.h lexer.h token.h
	$(CXX) $(CXXFLAGS) parser.cpp -c -o parser.o

clean:
	rm $(OBJS) $(EXE)

c:
	rm $(OBJS) $(EXE)

run:
	./$(EXE)

r:
	./$(EXE)
