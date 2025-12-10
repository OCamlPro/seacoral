# Changelog

## Next release

### Added
- Comment with test outcomes in generated testsuites [#44](https://github.com/ocamlpro/seacoral/pull/44)
- On the fly reporting and interpretation of CBMC results [#22](https://github.com/ocamlpro/seacoral/pull/22)

### Fixed
- Rights management when creating the `diver` user in the Docker image [#46](https://github.com/OCamlPro/seacoral/pull/46) (fix for [Issue #45](https://github.com/OCamlPro/seacoral/issues/45))
- Default maximum number of inputs in CBMC automatically increased [#36](https://github.com/OCamlPro/seacoral/pull/36)

## 1.0.0 ( 2025-10-30 )

### Added
- Recent version of Z3 in Docker image [#17](https://github.com/ocamlpro/seacoral/pull/17)
- New Boolean option `run.verbose_validation` (CLI arg: `--verbose-validation`) to help with debugging of validator behaviors [#7](https://github.com/ocamlpro/seacoral/pull/7)

### Fixed
- Decoder warnings due to spurious double-frees in validation code [#24](https://github.com/OCamlPro/seacoral/pull/24)
- Usability of Docker images, where `/workdir` is no longer created and the default working directory [#23](https://github.com/OCamlPro/seacoral/pull/23)
- Internal handling and messaging about malformed CBMC output or unexpected properties in counter-examples [#16](https://github.com/OCamlPro/seacoral/pull/16)
- Name of the internal directory where testsuites are exported [#12](https://github.com/ocamlpro/seacoral/pull/12)
- Detection by the validator of buffer overflows on empty arrays [#6](https://github.com/ocamlpro/seacoral/pull/6)
- Unnecessary generation by Klee, of tests that violate pre-conditions (`sc_assume`) [#5](https://github.com/ocamlpro/seacoral/pull/5)
- Handling of NaNs and floating-point literals in test suites [#2](https://github.com/ocamlpro/seacoral/pull/2)

## 0.1.0 ( 2025-10-14 )

* Initial version
