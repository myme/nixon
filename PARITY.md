# Nixon v2 — parity against SPEC.md

One line per `- [ ]` item in SPEC.md, in SPEC order.

| Status | Meaning |
|---|---|
| **done** | Implemented, with the test that covers it named |
| **done (no test)** | Implemented, exercised only indirectly |
| **changed** | Deliberately different; the ENGINEERING §7 entry says why |
| **n/a** | Describes v1 machinery v2 does not have |
| **missing** | Not implemented |

Counts are at the bottom.

## 2. CLI

### 2.1 Global options

| SPEC item | Status |
|---|---|
| Tri-state flags parse to `Some`/`Some(false)`/`None` | done (`tri_state_flags_are_unset_by_default`, `tri_state_flags_can_be_turned_on_and_off`, `the_last_of_a_tri_state_pair_wins`) |
| `--path` repeatable, appends to `project_dirs` | done (`path_is_repeatable_and_appends_to_project_dirs`) |
| `bin_dirs`, `project_types`, `commands` not CLI-settable | done (`bin_dirs_project_types_and_commands_are_not_settable_from_the_cli`) |
| Help header `Command & environment launcher` | done (`help_describes_the_v2_flags_only`) |
| QUIRK "Terminal emultor" typo | changed (§7.2 — the flag is gone) |
| Default config path computed at runtime, `$HOME` as `~` | done (`help_shows_the_computed_default_config_path_with_home_collapsed`) |

### 2.2 Subcommands

| SPEC item | Status |
|---|---|
| No subcommand keyword → `run` | done (`bare_arguments_become_the_run_subcommand`, `bare_arguments_are_the_run_subcommand`, `a_subcommand_keyword_is_never_a_command_name`) |

### 2.3 Argument/config merge order

| SPEC item | Status |
|---|---|
| Effective config = defaults `<>` file `<>` CLI | done (`merge_is_associative`, `merging_defaults_on_the_left_keeps_the_right`; the three-way wiring in `main.rs` has no direct test) |
| Config file read after CLI parsing, so `-C` is honoured | done (`a_missing_global_config_is_tolerated` exercises `-C`) |
| QUIRK missing global config is fatal | changed (§7.3 — tolerated; `a_missing_global_config_is_tolerated`, `a_config_parse_error_is_fatal_and_names_the_file`) |

### 2.4 Shell completion

| SPEC item | Status |
|---|---|
| `--bash-completion-script` etc. emit scripts | changed (§2.1 — `clap_complete`'s `CompleteEnv`; the package installs `eval`-able loaders) |
| Completion protocol prints candidates one per line | done (no test — verified by hand: `_CLAP_COMPLETE_INDEX=2 COMPLETE=bash nixon -- nixon run ''`) |
| Dynamic completers for command and project names | done (no test — `complete::command_names`, `complete::project_names`) |
| Completer re-parses the line for `-C`, `-p` | done (no test — `complete::partial_cli`) |

## 3. Configuration

| SPEC item | Status |
|---|---|
| Global config `$XDG_CONFIG_HOME/nixon.md`, `-C` overrides | done (`dirs_place_the_config_and_cache_under_their_xdg_roots`) |
| Local config: whole chain for `nixon.md`, then `.nixon.md` | done (`a_farther_nixon_md_beats_a_nearer_dot_nixon_md`, `finds_nixon_md_in_the_project_root`, `finds_a_dot_nixon_md_when_there_is_no_plain_one`, `prefers_nixon_md_over_a_dot_nixon_md_in_the_same_directory`) |
| Local merged on top of global | done (`a_local_nixon_md_adds_its_commands`) |
| Local `ParseError` fatal; missing/empty ignored | done (`a_local_parse_error_is_fatal`, `an_empty_local_config_is_simply_absent`) |
| QUIRK local config found from the project root, walking up | done (`walks_up_from_the_project_root`) |
| QUIRK `force_tty`/`backend`/`loglevel` not readable from file | changed (§7.2 — the first two are gone; `loglevel` stays CLI-only, §7.5) |
| QUIRK `terminal` parsed then dropped | changed (§7.2 — removed; falls under unknown keys, `unknown_keys_are_ignored`) |
| QUIRK markdown config sets `loglevel = None` | done (`defaults_carry_the_warning_log_level`) |
| Merge: `Option` fields, rhs wins if set | done (`options_take_the_right_hand_value_when_set`) |
| Merge: path and type lists concatenate | done (`path_lists_concatenate_left_then_right_without_dedupe`) |
| Merge: commands are rhs-first | done (`local_commands_come_before_global_ones`) |
| Config heading must be followed by a source node | done (`errors_without_a_config_block`) |
| Config block language: json/none → JSON, yaml → YAML, else error | done (`parses_a_config_block`, `errors_on_an_unexpected_bash_config_block`, `a_missing_language_is_json`, `yaml_is_parsed_as_yaml`) |
| JSON/YAML parse errors reported as `ParseError` | done (`errors_on_malformed_json`) |
| Exactly one config block per file | done (`errors_on_multiple_config_blocks`) |
| Empty `{}` is valid, all-default | done (`allows_empty_json_object`, `an_empty_json_object_is_valid_and_all_default`) |
| A config block may sit anywhere, including between commands | done (`a_config_block_may_sit_between_commands`, `finds_location_with_config`) |
| Unknown keys ignored | done (`unknown_keys_are_ignored`) |
| `project_types[].desc` required, `test` optional | done (`a_project_type_without_desc_is_a_parse_error`, `a_project_type_without_name_is_a_parse_error`, `a_project_type_without_test_has_no_markers`) |
| `test` entries become `Path` markers | done (`project_types_become_path_markers`) |

## 4. Markdown parsing

| SPEC item | Status |
|---|---|
| CommonMark parser with source positions | done (comrak; `finds_location_single` and the other location tests) |
| ATX and setext headings | done (`supports_alternate_header_format`) |
| Heading text = all text + inline code | done (`detects_project_type`, `command_name_is_the_first_word`) |
| QUIRK `getText` leaves internal double spaces | done (no test — reproduced in `extract::get_text`; invisible downstream) |
| Unparseable attributes → whole text is the name | done (`extracts_name`) |
| `parseHeaderArgs` 9 cases | done (`extracts_name`, `extracts_name_arg_and_kwarg`, `extracts_a_flag` ×4, `extracts_type` ×2, `mixes_args_and_kwargs`) |
| QUIRK kwarg values are letters only | changed (§7.3 — `kwarg_values_are_not_limited_to_letters`) |
| QUIRK trailing `{...}` parsed after the name | done (`command_name_is_the_first_word`) |
| Commands in document order | done (`can_bump_header_level_gaps`, `extracts_code_block_placeholders`) |
| `type=` on a command heading applies to it and nested commands | done (`a_command_heading_type_is_inherited_by_nested_commands`, `inherited_types_are_ordered_innermost_first`) |
| BUG fix: `type=` on a section heading applies beneath it | done (`a_section_heading_type_applies_to_commands_nested_under_it`, `a_section_heading_type_stops_at_a_sibling_heading`) |
| Header level gaps are fine | done (`can_bump_header_level_gaps`) |
| Description is the first paragraph | done (`extracts_source_block`) |
| QUIRK only paragraphs tolerated before the code block | changed (§7.3 — `non_paragraph_blocks_before_the_code_block_are_skipped`) |
| Placeholders in both heading and info string → error | done (`complains_on_both_header_and_code_block_placeholders`) |
| `placeholders = header ++ source` | done (`extracts_code_block_placeholders`) |
| `is_hidden` = name starts with `_` | done (`detects_a_command_and_whether_it_is_hidden`) |
| Source includes its trailing newline | done (`extracts_source_block`, `insert_prints_the_source_including_its_trailing_newline`) |
| Language from the info string, `None` if absent | done (`detects_command_by_code_block`, `info_string_names_a_language`) |
| `project_types` from §4.4 | done (`detects_project_type`, `a_project_typed_command_is_filtered_out_elsewhere`) |
| Name is the first whitespace-delimited word | done (`parses_text_part`, `command_name_is_the_first_word`) |
| Leading spaces ignored | done (`parses_text_part` case 2) |
| Trailing `&` ignored by the name parser | done (`a_trailing_background_marker_is_not_a_placeholder`) |
| `$`/`<` not followed by `{` ignored | done (`a_lone_dollar_or_angle_is_not_a_placeholder`) |
| Unterminated placeholder is a parse error | done (`fails_on_unterminated_arg`) |
| Placeholders may be embedded in quotes | done (`parses_placeholder_part` case 15) |
| Location set once per command, first wins | done (`finds_location_with_config`) |
| Location test expectations (exact lines) | done (`finds_location_single`, `finds_location_multiple`, `finds_location_with_config`) |
| `end_line` includes trailing blank lines | done (`finds_location_multiple`) |
| Bin commands get `Loc(path, 0, 0, 0)` | done (`bin_dir_executables_become_commands`) |
| Parse errors are fatal for global and local config | done (`a_config_parse_error_is_fatal_and_names_the_file`, `a_local_parse_error_is_fatal`) |

## 5. Commands and placeholders

| SPEC item | Status |
|---|---|
| `show_command` = name + ` ${placeholder}` per placeholder | done (`the_placeholder_picker_header_is_the_outer_command`) |
| `show_command_with_description` = `name` or `name - desc` | done (`a_command_shows_its_description_and_returns_its_name`, `run_list_prints_every_command_including_hidden_ones`) |
| QUIRK `is_bg_command` is dead code | n/a (not carried over) |
| EnvVar naming, `-` → `_`, empty alias takes the command name | done (`parses_placeholder_part` cases 3, 4, 16, 17) |
| Setting a format twice is an error | changed (§7.3 — tightened; `a_second_format_modifier_is_rejected` ×5, `errors_on_combined_columns_and_fields`) |
| Field/column numbers are 1-based | done (`pick_fields_keeps_original_order_and_ignores_out_of_range`, `parses_placeholder_part`) |
| §5.3 placeholder test vectors | done (`parses_placeholder_part` ×17, `pipe_modifiers_set_the_format` ×6, `parses_list_modifier`, `parses_filter_modifier`) |
| QUIRK `filter "…"` alnum only | changed (§7.3 — `a_filter_accepts_anything_but_a_quote`) |
| QUIRK pipe modifiers need no spaces around `\|` | done (`a_pipe_modifier_needs_no_spaces`) |
| Placeholders in heading **or** info string, never both | done (`complains_on_both_header_and_code_block_placeholders`, `detects_output_format`) |
| Several placeholders in one place | done (`extracts_environment_placeholders`) |
| BUG: `lsif` offered every bin entry, recursively | changed (§7.3 — executables only, non-recursive; `non_executable_files_in_a_bin_dir_are_skipped`, `directories_in_a_bin_dir_are_not_commands`) |
| Command discovery tests (5 ported) | done (`fetches_empty_commands`, `fetches_markdown_commands`, `filters_away_missing_project_types`, `matches_project_type`, `filters_away_a_typed_command_from_markdown`) |
| Hidden commands in discovery, out of the run picker | done (`hidden_commands_are_included`, `hidden_commands_are_not_offered_but_are_still_reachable`) |
| Missing referenced command is an error | changed (§7.3 — typed, not a panic; `a_missing_referenced_command_is_a_typed_error`, `an_unknown_placeholder_command_is_a_clean_error`) |
| A CLI arg is a placeholder's search query | done (`a_cli_arg_becomes_the_placeholders_search_query`, `zip_args_pairs_queries_then_overflows`) |
| Extra args become pre-expanded `Arg` placeholders | done (`args_beyond_the_placeholders_are_passed_through_unselected`, `zip_args_pairs_queries_then_overflows`) |
| Placeholders beyond the args get defaults | done (`zip_args_leaves_unmatched_placeholders_without_a_query`) |
| Nested placeholder commands get `nixon_project_path` | done (`the_environment_and_stdin_reach_the_runner`, `a_placeholder_command_runs_then_its_selection_becomes_an_argument`) |
| Selector title is the outer command's `show_command` | done (`the_placeholder_picker_header_is_the_outer_command`) |
| Referenced commands run in the project path | done (`a_placeholder_command_runs_then_its_selection_becomes_an_argument`) |

## 6. Column / field formatting

| SPEC item | Status |
|---|---|
| `parse_columns` / `format_columns` test vectors (6 ported) | done (`parses_columns_empty_input`, `parses_columns_titles_only`, `parses_columns`, `parses_columns_no_headers`, `keeps_all_rows_and_aligns_titles_to_values_no_header`, `drops_the_header_row_from_both_titles_and_values`) |
| QUIRK widths come from the first row | done (no test — kept deliberately) |
| Regression: no-header keeps every row aligned | done (`keeps_all_rows_and_aligns_titles_to_values_no_header`, `format_columns_never_drops_a_row_without_a_header`) |
| `pick_fields` keeps original order, ignores out-of-range | done (`pick_fields_keeps_original_order_and_ignores_out_of_range`) |

## 7. Languages, evaluation and processes

| SPEC item | Status |
|---|---|
| `Display` is the lowercase name | done (`display_is_the_lowercase_name`) |
| Language matching is case-sensitive | done (`language_matching_is_case_sensitive`) |
| `--language` on `eval`/`new` accepts the same names | done (`new_has_the_documented_defaults`; `eval -l` has no test) |
| Cache dir `$XDG_CACHE_HOME/nixon`, created | done (`writing_creates_the_cache_directory_and_the_script`, `dirs_place_the_config_and_cache_under_their_xdg_roots`) |
| `<sha1>-<name><ext>`, args follow the path | done (`the_script_path_is_the_source_digest_the_name_and_the_extension`, `an_eval_command_has_an_empty_name`, `positional_args_follow_the_script_path`) |
| `gc` output strings, `--dry-run` | done (`a_dry_run_reports_without_removing`, `a_real_run_removes_and_reports`, `gc_reports_what_it_removes`) |
| QUIRK script is never executable | done (no test — the interpreter is always explicit) |
| QUIRK interpreters not configurable | done (deferred, §7.5) |
| `cwd == None` → never wrapped | done (`nothing_is_wrapped_without_a_working_directory`) |
| Stdin lines piped, otherwise inherited | done (`stdin_lines_reach_a_real_child`, `the_environment_and_stdin_reach_the_runner`) |
| `run_with_output` captures stdout, stderr inherited | done (`a_real_child_runs_and_its_output_is_captured`) |
| Rust: `Command` + fork/setsid or `process_group` | changed (§2.1 — `process_group(0)`, so the workspace needs no `unsafe`) |

## 8. Selection

| SPEC item | Status |
|---|---|
| Backend chosen by TTY; `is_gui_backend` | n/a (§7.2 — no backends) |
| `catMaybeSelection`: empty selection → `Empty` | done (`confirming_with_no_matches_is_empty`) |
| JSON candidates: string or `{title, value}` | done (`a_json_placeholder_accepts_strings_and_title_value_objects`, `bad_json_is_a_typed_error_not_a_panic`) |
| fzf option monoid and its property tests | n/a (§7.2 — no fzf argv to build) |
| Filter mode: argv `["--filter", text]`, lines verbatim | changed (§7.2 — `FilterPicker`; `the_filter_picker_returns_every_match`, `only_matches_are_returned`) |
| Interactive mode `-1 --ansi` | changed (§7.2 — `select_one` and ANSI rendering; `select_one_takes_a_unique_match_without_a_terminal`, `a_unique_query_runs_without_a_terminal`, `ansi_in_a_candidate_does_not_leak_into_the_frame`) |
| fzf exit-code mapping | n/a (§7.2) |
| `--expect` output parsing | changed (§7.2 — expect keys map to a `SelectionType`; `an_expect_key_takes_precedence`, `alt_enter_f1_and_f2_confirm_with_their_own_type`) |
| Candidate indexing, value not title, ANSI stripped | done (`a_json_placeholder_accepts_strings_and_title_value_objects`, `ansi_is_kept_for_display_but_stripped_from_the_value`) |
| Filter mode returns titles | done (`values_are_preserved_not_just_titles`) |
| Project candidates: `~`, sorted, deduplicated | done (`projects_show_a_collapsed_home_but_return_the_full_path`, `projects_are_sorted_and_deduplicated`) |
| Project options: header `Select project`, query, `f1` → Show | done (`project_selection_binds_f1_to_show`) |
| Selected values map back to projects | done (`project_list_prints_discovered_projects`) |
| Command candidate text and mapping | done (`a_command_shows_its_description_and_returns_its_name`) |
| Header `"<prompt> [<name>] (<dir>)"` | done (`command_selection_names_the_project_in_its_header`, `the_command_picker_header_names_the_project`) |
| `--no-sort`, query, expect keys | done (`command_selection_keeps_discovery_order`, `command_selection_binds_the_three_expect_keys`) |
| Documented bindings (README) | done (`the_readme_help_block_matches_the_binary`; key tables in README) |
| rofi exit codes, candidates, formatting, prompts | n/a (§7.2 — rofi removed) |
| rofi `$XDG_SESSION_TYPE` crash | n/a (§7.2) |
| rofi custom key bindings | n/a (§7.2) |

## 9. Projects

| SPEC item | Status |
|---|---|
| All markers must pass; no markers always matches | done (`a_type_with_no_markers_matches_any_directory`, `all_markers_must_match`) |
| `Path`/`File`/`Dir`/`Or` semantics | done (`markers_test_files_directories_and_alternatives`) |
| A non-directory has no types | done (`a_non_directory_has_no_types`) |
| Types in config order | done (`types_come_back_in_config_order`) |
| `find_project` — `None` if not a directory | done (`a_non_directory_has_no_types`) |
| `None` if only marker-less types matched | done (`a_catch_all_alone_does_not_make_a_directory_a_project`) |
| Otherwise `Project`, catch-all types included | done (`a_marker_bearing_type_makes_a_directory_a_project_and_keeps_the_catch_all`) |
| `find_in_project` walks up, root never tested | done (`the_nearest_enclosing_project_wins`, `returns_none_when_nothing_matches`) |
| BUG: subdirectory discovery gave the parent with an empty name | changed (§7.3 — fixed; `discovery_from_a_subdirectory_returns_the_project_root`, `discovery_from_the_project_root_returns_the_same_project`) |
| `find_in_project_or_default` | done (`outside_any_project_the_directory_itself_is_used`) |
| Source dirs expanded (`~`, `$VAR`, globs) | done (`a_tilde_expands_to_the_given_home`, `a_variable_expands_from_the_given_lookup`, `a_wildcard_expands_to_every_match`) |
| Candidate yielded **and** children scanned | done (`a_source_dir_that_is_a_project_yields_itself_and_still_scans_children`, `scans_children_of_a_source_dir`) |
| Depth 1: source dirs and their children only | done (`grandchildren_are_never_scanned_at_depth_one`, `a_negative_depth_finds_nothing`) |
| Undefined `$VAR` expands to empty | done (no test — shellexpand's no-error context) |
| Sorted by full path, no dedupe | done (`results_are_sorted_by_path_without_dedupe`, `projects_sort_by_full_path`) |
| Hidden directories included | done (`hidden_directories_are_included`) |
| Discovery on every invocation | changed (§9.6 allows laziness — v2 discovers only for the subcommands that need it; results identical) |
| `inspectProjects` output format | done (`inspect_prints_name_path_and_types`, `inspect_separates_projects_with_a_blank_line`) |
| `implode_home` exact `$HOME/` prefix | done (`implode_home_replaces_only_an_exact_home_prefix`) |

## 10. Subcommand behaviours

| SPEC item | Status |
|---|---|
| `run --list` candidates, no matches → stderr, exit 0 | done (`run_list_prints_every_command_including_hidden_ones`, `run_list_with_a_query_filters`, `run_list_with_no_matches_says_so_on_stderr_and_still_exits_zero`) |
| `project` `.` shortcut | done (no test — `pick_projects` / `project_for_query`) |
| QUIRK `project <name>` fuzzy → picker pre-filtered | done (no test) |
| `eval` placeholders resolve against the project's commands | done (no test — `eval_placeholders_are_parsed_with_the_grammar` covers parsing) |
| `eval --file` read relative to nixon's cwd | done (`eval_reads_a_file_with_dash_f`) |
| `eval` outside a project needs a TTY | changed (§7.3 — falls back to the cwd; `eval_works_outside_any_recognised_project`) |
| QUIRK `new` temp file under `/tmp` | changed (§7.3 — system temp dir) |
| QUIRK `new` can splice into a bin executable | changed (§7.3 — refused) |
| `new` ignores selection types | done (no test) |
| `new` prompt, write-back | done (`new_splices_a_command_in_when_confirmed`, `new_leaves_the_file_alone_when_declined`, `new_asks_before_writing`, `the_template_goes_after_the_chosen_command`) |
| `edit` on a bin command opens at line 0 | done (no test) |
| QUIRK `--select` treats the command as a candidate producer | done (`select_runs_the_command_and_prints_its_output_lines`) |
| Selection types ignore `--insert`/`--select` | done (no test — checked first in `handle_cmd`) |
| Success paths exit 0 even when the child fails | changed (§7.3 — propagated; `a_child_exit_code_becomes_the_process_exit_code`, `a_child_exit_code_is_propagated`) |
| Errors → stderr, exit 1 | done (`a_config_parse_error_is_fatal_and_names_the_file`, `an_unknown_placeholder_command_is_a_clean_error`) |
| Logging to stderr; only data on stdout | done (`run_list_with_no_matches_says_so_on_stderr_and_still_exits_zero`, plus every stdout assertion in `cli.rs`) |

## 11. Logging

| SPEC item | Status |
|---|---|
| Levels ordered, emitted iff `>=` configured | done (`log_levels_are_ordered`, `log_levels_include_both_spellings_of_warning`; the filtering itself is `tracing`'s) |
| Plain text to stderr, no prefix or timestamp | done (no test — `tracing_subscriber` without time, target or level) |
| Property test for level filtering | done (no test — not ported; `tracing`'s own behaviour) |
| Messages emitted | done (no test) |

## 12. Shell integration

| SPEC item | Status |
|---|---|
| ~~Keep `-b fzf`, `-T` widget shapes~~ | changed (§7.2 — struck in SPEC; widgets rewritten) |
| Ship widgets and completion scripts | done (`nix build` output check; bash, zsh and fish widgets plus three loaders) |

## 13. Packaging

| SPEC item | Status |
|---|---|
| Flake with package, devShell, overlay | done (`nix flake check`) |
| `postInstall` installs widgets and completions | done (`nix build` output check) |
| CI builds, lints and tests on push/PR | done (`.github/workflows/ci.yml`) |
| The repo's own `nixon.md` defines dev commands | done (`nixon.md`) |
| Linux and macOS | done (no test — CI matrix; not verified on macOS here) |

## Known gaps

Nothing in SPEC is **missing**. One ENGINEERING requirement is outstanding:

- **Candidate streaming.** The picker does not yet open before the command
  producing its candidates finishes. `App::empty()` and `App::injector()`
  exist and are tested (`candidates_can_be_streamed_in_after_the_picker_opens`),
  but `resolve.rs` still runs the command to completion because
  `ProcessRunner` returns captured bytes rather than a stream. Scheduled as
  **Step I**. Interactive performance is unaffected: matching is already off
  the UI thread (`a_keystroke_stays_within_a_frame_on_a_large_list`).

## Counts

| Status | Items |
|---|---|
| done | 120 |
| done (no test) | 18 |
| changed | 22 |
| n/a | 7 |
| **missing** | **0** |

167 rows covering SPEC's 174 `- [ ]` items; a few SPEC bullets that are
sub-points of one behaviour (the location expectations, the §5.3 test
vectors, the `parseHeaderArgs` cases) are one row each. 553 tests.
