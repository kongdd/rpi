# A fake `pi` used by the ellmer tests: it replays a fixed JSONL stream, so
# `chat_pi_ellmer()` can be tested without a model, a network or an install.
if (identical(Sys.getenv("FAKE_PI_ERROR"), "1")) {
  cat('{"type":"message_end","message":{"role":"assistant","content":[],',
      '"stopReason":"error","errorMessage":"boom"}}\n', sep = "")
  quit(status = 0)
}

emit <- function(...) cat(..., "\n", sep = "")
emit('{"type":"message_update","assistantMessageEvent":{"type":"text_delta","delta":"Hello"}}')
emit('{"type":"message_update","assistantMessageEvent":{"type":"text_delta","delta":" from pi"}}')
emit('{"type":"message_end","message":{"role":"assistant",',
     '"content":[{"type":"text","text":"Hello from pi"}],',
     '"usage":{"input":100,"output":5,"cacheRead":20,"cacheWrite":0,',
     '"totalTokens":105,"cost":{"total":0.25}},"durationMs":500,',
     '"stopReason":"end_turn"}}')
