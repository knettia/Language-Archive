FARENC = ./farenc

SOURCES = main square

BIN = bin

EXEC = $(BIN)/exec

OBJECTS = $(addprefix $(BIN)/, $(addsuffix .o,$(SOURCES)))
OBJECTS_AS_FLAGS = $(addprefix -s , $(OBJECTS)) # each object requires a -s before it

$(EXEC): $(OBJECTS)
	$(FARENC) $(OBJECTS_AS_FLAGS) -o $@

$(BIN)/%.o: %.faren $(BIN)
	$(FARENC) -s $< -o $@

$(BIN):
	mkdir -p $(BIN)

clean:
	rm -rf $(BIN)
