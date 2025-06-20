# Changelog

## [0.2] - 2025-06-20

### Added

- `sscanf` and `sprintf`.
- `unitL`, the unique lead for the unit type.
- `NullL` type synonym for leads and other synonyms for higher arity.
- Support for lifting isos and prisms from [lens][lens] to leads.
- Examples to documentation.
- Flakification.
- Direnv support.

### Changed

- Types accumulate in the correct order when composing cassettes.
- `K7` has two type parameters, down from four. The parser and printer
  types are kept in sync.
- `K7` generalized to arbitrary profunctors.
- Make `K7` an instance of `Data.Monoid`. `mempty` signifies failure
  and `(<>)` is for alternation, instead of `(<|>)`.
- Swapped the order of the fields in `K7`.
- `many1` is now called `some` to be consistent with
  `Control.Applicative`.
- Change parameter order in `UnL`, `BinL` to match the order in
  [lens][lens].
- Switched to Apache-2.0 license.

### Fixed

- `choice` works when applied to the empty list.
- `nilL`, `nothingL` now correctly fail when the input is not the
  expected constructor.
- Argument to failure continuation in `consL`.

### Removed

- Chaining combinators `catanal`, `catanar` and `chainl`, `chainr`.

## [0.1] - 2012-03-19

Initial release.

[lens]: https://hackage.haskell.org/package/lens
[0.1]: https://github.com/mboes/cassette/releases/tag/v0.1
[0.2]: https://github.com/mboes/cassette/releases/tag/v0.2
