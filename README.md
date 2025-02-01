# Synox [![Build Status](https://github.com/anishathalye/synox/actions/workflows/ci.yml/badge.svg)](https://github.com/anishathalye/synox/actions/workflows/ci.yml) [![Crates.io](https://img.shields.io/crates/v/synox.svg)](https://crates.io/crates/synox) [![Documentation](https://docs.rs/synox/badge.svg)](https://docs.rs/synox)

Synox implements program synthesis of string transformations from input-output
examples. Perhaps the most well-known use of string program synthesis in
end-user programs is the [Flash
Fill](https://support.microsoft.com/en-us/office/using-flash-fill-in-excel-3f9bcf1e-db93-4890-94a0-1578341f73f7)
feature in Excel. These string transformations are learned from input-output
examples.

Synox currently implements [BlinkFill (Singh '16, in Proc. VLDB)][blinkfill], an algorithm similar to Flash Fill.

## Usage

Add this to your `Cargo.toml`:

```toml
[dependencies]
synox = "0.1"
```

## Example

Consider the following table, with a missing entry in an output column.

| Name | Graduation Year | Output |
|---|---|---|
| Alyssa P. Hacker | 1985 | A. Hacker '85 |
| Ben Bitdiddle | 2002 | B. Bitdiddle '02 |
| Cy D. Fect | 2017 | ? |

Synox can infer a program that automatically fills in the missing entry with
"C. Fect '17".

```rust
use synox::StringProgram;
use synox::blinkfill;

let unpaired: &[Vec<&str>] = &[];
let examples = &[(vec!["Alyssa P. Hacker", "1985"], "A. Hacker '85"   ),
                 (vec!["Ben Bitdiddle",    "2002"], "B. Bitdiddle '02")];

let prog = blinkfill::learn(unpaired, examples)?;

let result = prog.run(&["Cy D. Fect", "2017"])?;
assert_eq!(result, "C. Fect '17");
```

## License

Copyright (c) Anish Athalye. Released under the MIT License. See
[LICENSE.md](LICENSE.md) for details.

[blinkfill]: http://www.vldb.org/pvldb/vol9/p816-singh.pdf
