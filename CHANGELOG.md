# Change Log
All notable changes to this project will be documented in this file. This change log follows the conventions of [keepachangelog.com](http://keepachangelog.com/).

## [Unreleased]

## [0.1.3]
### Fixed
- Fixed transition to `:script-data-escaped-dash-dash` state.

### Removed
- Remove unused dependencies.
- Remove `tag` sugar.

## [0.1.2]
### Added 
- Add syntactic sugar which makes it easier to work with tokens.

### Changed
- Destructure name and value out of attribute.
- Start using `:data` key for `:DOCTYPE` token.

### Fixed
- Tokenize states that flush buffer as character codes correctly.

## [0.1.1]
### Changed
- Make `tokenize` implementation use `lazy-seq`.

## [0.1.0]
### Added
- Add initial implementation for `Tokenizable` protocol.

[Unreleased]: https://github.com/gfjalar/data.html/compare/v0.1.3...HEAD
[0.1.3]: https://github.com/gfjalar/data.html/releases/tag/v0.1.3
[0.1.2]: https://github.com/gfjalar/data.html/releases/tag/v0.1.2
[0.1.1]: https://github.com/gfjalar/data.html/releases/tag/v0.1.1
[0.1.0]: https://github.com/gfjalar/data.html/releases/tag/v0.1.0
