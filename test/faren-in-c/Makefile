CC = clang
FARENC = ./farenc

BIN = bin

EXEC = $(BIN)/exec

$(EXEC): $(BIN)/libsquare-faren.a $(BIN)/main.o
	$(CC) $(BIN)/main.o -L bin -lsquare-faren -o $@

$(BIN)/main.o:
	$(CC) -c main.c -o $@

$(BIN)/libsquare-faren.a: $(BIN)
	$(FARENC) -s square.faren -o $@

$(BIN):
	mkdir -p $(BIN)

clean:
	rm -rf $(BIN)
