# qlcheckr (development version)

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
