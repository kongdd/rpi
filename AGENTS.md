# AGENTS.md

## What this package is

`piagent` is an **ellmer-style R interface to the `pi` coding agent CLI**. It does
not talk to a model API directly: every turn is executed by a `pi` subprocess in a
working directory, and the package parses the resulting JSON event stream.

Public surface (keep it small):

| Object | Role | ellmer analogue |
| --- | --- | --- |
| `pi_client()` | how to start `pi`: binary, dir, model, tools, session, args | `Provider` |
| `chat_pi()` / `PiChat` | one conversation | `Chat` |
| `$chat()`, `$stream()`, `$chat_async()`, `$stream_async()` | send a prompt | same |
| `$get_events()`, `$get_turns()`, `$get_tokens()`, `$get_cost()` | inspect the run | same |
| `models_pi()` | `pi --list-models` as a data.table | `models_*()` |

Internal (not exported): `pi_cli_args()`, `pi_run()`, `pi_run_async()`,
`pi_start()`, `pi_parse_event()`, `pi_turns()`, `pi_text()`, `pi_check_events()`.

## File layout

- `R/client.R` — `pi_client()` and `models_pi()`; a client is a plain S3 list.
- `R/chat.R` — the `PiChat` R6 class and `chat_pi()`.
- `R/utils.R` — argument building, process launching, JSONL parsing, event→table.
- `R/piagent-package.R` — package docs and NAMESPACE imports.
- `tests/testthat/` — pure tests always run; live tests need `options(piagent.test = TRUE)`
  (use `NOT_CRAN=true testthat::test_local()`).

## Workflow

1. Change code, then `Rscript -e 'roxygen2::roxygenise()'` (NAMESPACE and `man/` are generated — never edit by hand).
2. `Rscript -e 'testthat::test_local()'` for the fast suite.
3. `Rscript -e 'devtools::check(args = c("--no-manual", "--no-build-vignettes", "--no-tests"))'` before finishing. The target is 0 errors / 0 warnings / 0 notes.
4. Dependencies stay at `processx`, `jsonlite`, `R6`, `promises`, `later`, `data.table`. Prefer base R over adding a dependency; there is deliberately no `cli` or `rlang`.

## pi CLI facts this package depends on

These were verified against pi 1.3.0 and are easy to get wrong:

- **One process per prompt.** `pi --mode json` runs the prompt, streams JSONL events and exits. There is no long-lived process.
- **The prompt travels on the command line**, separated by `--`, so that `@file` references resolve. Do not move it to stdin, or `@file` stops working.
- **stdin is an empty, closed pipe** (`stdin = "|"` then `close(p$get_input_connection())`). `stdin = "null"` is a *filename*, not a device, and fails on Windows.
- **Sessions** are `--session-id <id>` (restores or creates) or `--no-session`. Session files are grouped per working directory, so `client$dir` matters.
- **`--system-prompt` replaces** the built-in prompt, which also removes the tool descriptions. A chat-level `system_prompt` therefore maps to `--append-system-prompt` (repeatable); only `pi_client(system_prompt = )` replaces.
- **Exit status is not the whole story.** A failed assistant response still exits 0 in JSON mode; read `stopReason`/`errorMessage` from the last `message_end` (`pi_check_events()` does this).
- **Text lives in `message_end`**, streaming deltas in `message_update$assistantMessageEvent` (`text_delta`, `thinking_delta`). Echo deltas, but return the authoritative `message_end` text.
- **Usage/cost** is on the assistant `message_end` as `usage$input`, `usage$output`, `usage$cacheRead`, `usage$cacheWrite`, `usage$cost$total`.
- Only some flags are repeatable; keep `client$args` last so users can override.

## Change discipline

- One intent per change: a new `PiChat` method, an argument mapping, an event-parser fix, or docs.
- New user-visible behaviour goes through `pi_client()` or a `PiChat` method, never through package options (`piagent.bin` is the only option, plus `piagent.echo`/`piagent.test`).
- Keep `R/*.R` files under ~300 lines; `chat.R` holds one class, `utils.R` only helpers.
- Return `data.table` for tabular results, `invisible(text)` when echoing, and a `promise` for anything `*_async()`.
- Live tests call a real model and cost money: keep them to a handful of tiny prompts, and always behind `skip_if_no_pi()`.

## Testing notes

- Pure tests should build a client with `pi_client(bin = "pi", dir = ".")` so they run without the binary installed.
- Assert on the message table and tokens, not on model wording — wording drifts. Match case-insensitively and on a single distinctive token.
- `chat_async()` tests must drain the event loop: `while (is.null(value)) later::run_now(0.1)`.
