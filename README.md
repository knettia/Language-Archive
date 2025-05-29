# farenc

"farenc" is a test project for a small, compiled programming language called "Faren"

# Goals
- [ ] Advance language
  - [x] Create basic variables
  - [ ] Create `if` and `else` control flows
  - [ ] Create function declarations
  - [ ] Create a module system
- [ ] Compile freestanding binaries
  - [ ] Binaries that do not rely on `libc` or `clang`
  - [ ] A custom ABI distinct from C ABI 
- [ ] Better compiling
  - [ ] Allow multiple objects to link to eachother
- [ ] Create Ruby tests for compiling using `RSpec`

# Test

### To compile the executable:

```sh
cargo build
```

### To test:

Interpreting:

```sh
cargo run -- interpret source.script
```

Compiling:
```sh
cargo run -- compile source.script -o object.o
cargo run -- compile object.o -x exec
```

You can also output artifacts when building objects:

```sh
cargo run -- compile source.script -o object.o --artifacts
```
