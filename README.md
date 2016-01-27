# prototype - protobuf 3 for Haskell

This package provides an implementation of [Protocol Buffers](https://github.com/google/protobuf/) in Haskell. It includes a parser for protobuf files and a mapping of the protobuf data types to Haskell types, which we use to generate parsers for the wire format. No code compilation is necessary, prototype takes in the `my.proto` files as input.

This package exists because the other protobuf implementations were not comaptible with proto3. We also wanted automatic parser generation from `.proto` files at run time.

## Implementation Roadmap

- protobuf file parser <-- we are here
- data types mapping
- generate wire fmt parsers
- test with protobuf examples

## Contribute

Help us make this the best protobuf package for Haskell! Dive into any open issues, and feel free to ask questions.

## License

Prototype is licensed under the BSD 3-Clause License.
