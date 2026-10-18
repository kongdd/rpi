# %% 
# 先在 pi 中执行 /login openai-codex；这里只复用授权，不启动 pi。
library(piagent)
devtools::load_all(".")

chat <- chat_pi_codex(
  model = "gpt-6-luna",
  system_prompt = "使用中文，回答简短。",
  echo = "output"
)
chat$chat("用一句话介绍你自己。")

# %% 
# 工具由 R 执行，不经过 pi。
chat$register_tool(ellmer::tool(
  function(x, y) x + y,
  description = "计算两数之和",
  arguments = list(x = ellmer::type_number(), y = ellmer::type_number()),
  name = "add"
))

# %% 
r = chat$chat("调用 add 计算 17 + 25。")
chat$get_tokens()
# %% 
