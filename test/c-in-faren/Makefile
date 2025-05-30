CC = clang
FARENC = ./farenc

BIN = bin

EXEC = $(BIN)/exec

$(EXEC): $(BIN)/libsquare-c.a $(BIN)/main.o
	$(FARENC) -s $(BIN)/main.o --link-archive $(BIN)/libsquare-c.a -o $@

$(BIN)/main.o:
	$(FARENC) -s main.faren -o $@

$(BIN)/libsquare-c.a: $(BIN)/square-c.o 
	ar r $@ $(BIN)/square-c.o

$(BIN)/square-c.o: $(BIN)
	$(CC) -c square.c -o $@

$(BIN):
	mkdir -p $(BIN)

clean:
	rm -rf $(BIN)
