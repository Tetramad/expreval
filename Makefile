HC=ghc

TARGET = libexpreval.a
SRC = ${wildcard *.hs}
ITF = ${SRC:.hs=.hi}
OBJ = ${SRC:.hs=.o}

.PHONEY: all
all: $(TARGET)

$(TARGET): $(OBJ)
	touch $(TARGET)

.PHONEY: clean
clean:
	$(RM) $(TARGET) $(OBJ) $(ITF)

%.o %.hi: %.hs
	$(HC) -c -o $@ $^
