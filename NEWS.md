# qlcheckr 0.1.5

## Bug fixes

* Fixed hints on incorrect answers not being displayed.

# qlcheckr 0.1.4

## Improvements

* Removed additional line break from feedback.

# qlcheckr 0.1.3

## Bug fixes

* Fixed issue with `search_ast()` for calls without arguments.

# qlcheckr 0.1.2

## Bug fixes

* Fixed scoping issue with ql_*() evaluate results helper functions.

# qlcheckr 0.1.1

## New features

* Added `ql_src()` and `ql_ast()` for accessing user code and parsed ASTs.

# qlcheckr 0.1.0

## Initial release

**Core Functionality**:
- Added `apply_checks()` for user code validation in Quarto Live exercises.
  - Provides hints for each test that doesn't pass
  - Supports a debug view to see the state of all available check variables.
  
**Code checking functions**:
- Added `search_ast()` for analysing parsed user code:
  - Search for function usage (`.fn`).
  - Detect function arguments (`...`).
  - Identify specific expressions (`.expr`).
  
**Result checking utilities**:
- Added `exists_in()` to evaluate the presence of specific conditions,
  results, or errors across multiple outputs.
- Added helper functions for extracting useful evaluation results:
  - `ql_results()` for successful evaluation results.
  - `ql_errors()` for error messages.
  - `ql_warnings()` for warning messages.
  - `ql_messages()` for messages output.
  - `ql_outputs()` for custom filtering of results.
