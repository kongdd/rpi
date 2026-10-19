# %%
devtools::load_all(".")
# library(piagent)
m <- models_pi()

# %%
# 筛选某个 provider，并把模型写进客户端
client <- pi_client(
  dir = ".", model = "MiniMax-M3", thinking = "medium",
  tools = "read,bash,edit,write,codemode",
  args = c(
    "--no-extensions",
    "--extension", "builtin:codemode", # 显式启用 codemode
    "--no-skills",
    "--no-context-files"
  )
)

chat <- chat_pi("R语言tidyverse专家", client = client, echo = "all")

# 实时显示工具名称、参数及模型返回的 thinking，返回完整回答
chat$chat("你好")
# chat$chat("介绍本项目，使用codemode")
# chat$chat("你知道codemode吗")
# chat$chat("你刚才在读取文件时，是不是没有调用codemode")

data <- chat$get_turns()
show_status(data)

s = summary(chat)

# %%
chat$chat("用一句话总结上面的内容。")

chat$chat("R/client.R 文件定义了哪些导出函数")
