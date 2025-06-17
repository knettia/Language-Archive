# farenc

"farenc" is a personal test project for a small, compiled programming language called "Faren". Its purpose is to test compiler design from scratch.

# Goals
- [ ] Advance language
  - [x] Create basic variables
  - [ ] Create `if` and `else` control flows
  - [ ] Create `while` and `loop` control flows
  - [x] Create function declarations
  - [ ] Create a module system
- [ ] Compile freestanding binaries
  - [ ] Binaries that do not rely on `libc` or `clang`
  - [ ] A custom ABI distinct from C ABI 
- [x] Better compiling
  - [x] Allow multiple objects to link to eachother
- [ ] Create Ruby tests for compiling using `RSpec`


# Requirements:
- [Rust](https://www.rust-lang.org/)
- [`cargo`](https://doc.rust-lang.org/cargo/getting-started/installation.html)
- [`faren-canon`](https://codeberg.org/knettia/faren-canon)
- LLVM 16 or later
- [Inkwell](https://github.com/TheDan64/inkwell)

# Compiling `farenc`:

Before building, you must ensure you have the [`faren-canon`](https://codeberg.org/knettia/faren-canon) crate cloned in the parent directory (or you could modify its path in `Cargo.toml`)

```sh
cd ..
git clone https://codeberg.org/knettia/faren-canon
```

Build `farenc` like the following:

```sh
cargo build --release
```

# Setup:

Before using `farenc`, you must export its required environment variables. You will have to run `set-export.sh`:

Making `set-export.sh` executable:

```sh
chmod +x set-export.sh
```

Running `set-export.sh`:

```sh
source ./set-export.sh
```

Alternatively:

```sh
. ./set-export.sh
```

# Using `farenc`:


Compiling `.faren` source files into relocatable object files:

```sh
farenc -s source.faren -o source.o
```

Linking Faren relocatable object files into an executable binary:

```sh
farenc -s source.o -o source
```

You can also expose artefacts during compilation with the following flags:

```sh
farenc -s source.faren -o source.o \
	--dump-ir source.ll \
	--dump-asm source.S
```
