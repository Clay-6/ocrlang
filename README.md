# OCRLang

An interpreter for OCR's [exam reference language](https://ocr.org.uk/Images/572953-j277-programming-techniques-python.docx)
used in the [j277 course](https://ocr.org.uk/qualifications/gcse/computer-science-j277-from-2020/)

It implements everything from [the spec](https://www.ocr.org.uk/Images/558027-specification-gcse-computer-science-j277.pdf)
(in section 3c), but liberties have been taking as the spec is hardly very specific on everything
other than syntax (and even then it isn't the best).

## Installation

### Prebuilt binaries

Go to the [releases page](https://github.com/Clay-6/ocrlang/releases) & download the archive
for your platform, then extract it wherever you like.

### From source

You'll need an up to date [Rust toolchain](https://rustup.rs). Development happens on the latest
stable version, so no guarantees that older versions will work.

Clone the repo, `cd` into its root directory & run

```shell
cargo build
```

to get a debug build, or

```shell
cargo build --release
```

for a release build.

```shell
cargo run [--release]
```

will run the program, and

```shell
cargo install --path crates/ocrlang
```

will install it to `~/.cargo/bin`

## Usage

Running

```shell
ocrlang
```

will open a REPL where you can execute lines of code. Run the command

```shell
ocrlang <FILE>
```

to run `<FILE>`.
