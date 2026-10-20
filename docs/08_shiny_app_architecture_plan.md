# Shiny App Architecture Memo (Version 1)

## 1. Recommended App Structure
Build version 1 as a single `app.R` file.

That is the right choice for this stage because the app has a clear, limited scope:
- five fixed test inputs
- one main modeling workflow
- a small number of tabs
- local history storage and CSV import/export
- a teaching goal, where code readability matters more than abstraction

A single file will make it easier to:
- understand how UI and server connect
- trace reactivity from inputs to outputs
- debug early logic
- teach from the code without forcing the reader to jump across files

For version 1, the main risk is not that the app becomes too large. The main risk is that it becomes confusing too early because of unnecessary modularization. A well-organized single `app.R` with strong section comments is the best balance of simplicity and structure.

## 2. Major Architectural Sections
The `app.R` file should be organized in this order:

**A. Package imports**  
Load only the packages needed for version 1.

**B. Global constants and app settings**  
Store values that should not change during runtime.

**C. Helper functions**  
Put all non-reactive utility functions here.

**D. UI definition**  
Define the full page layout.

**E. Server definition**  
Handle reactive data, validation, modeling, outputs, and history.

**F. App launch**  
Call `shinyApp(ui, server)`.

## 3. UI Architecture
Use a top-level `navbarPage()` layout.

Tabs:
- Current Test Entry
- Historical Comparison
- Training Targets
- Testing Protocol
- Import / Export

Keep layout simple and readable.

## 4. Server Architecture
Five responsibility areas:
- Input capture and validation
- Profile estimation and calculations
- Output rendering
- History management
- User messaging and safeguards

## 5. Core Reactive Objects
- `current_inputs()`
- `validated_tests()`
- `current_profile()`
- `profile_curve_data()`
- `current_summary_metrics()`
- `training_targets()`
- `history_data()`
- `selected_history_profiles()`
- `comparison_data()`

## 6. Helper Functions
- `build_test_dataframe()`
- `validate_test_values()`
- `fit_power_duration_model()`
- `predict_power_curve()`
- `estimate_secondary_metrics()`
- `build_training_targets()`
- `make_profile_record()`
- `append_history_record()`
- `validate_history_file()`
- `read_history_csv()`
- `write_history_csv()`
- `format_duration_label()`

## 7. Data Flow
User input → validation → modeling → outputs → optional save to history → CSV import/export.

## 8. File Import/Export Architecture
Use simple CSV:
- Export full history
- Import replaces in-session history
- Use long format for data

## 9. Refactoring Plan
- Phase 1: clean single file
- Phase 2: split helper files
- Phase 3: introduce modules if needed

## 10. Common Architecture Mistakes to Avoid
- Too much logic in render functions
- Mixing raw and validated data
- Overcomplicating modeling
- Inconsistent history structure
- Too many unclear reactives
- Unnecessary recalculation
- Overclaiming physiology
- Overengineering persistence
- Neglecting protocol page
- Designing for future complexity instead of current clarity
