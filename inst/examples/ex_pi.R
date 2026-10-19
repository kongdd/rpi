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
# chat$chat("你好")
chat$chat("介绍本项目，使用codemode")
# chat$chat("你知道codemode吗")
# chat$chat("你刚才在读取文件时，是不是没有调用codemode")

data <- chat$get_turns()
data

# %%
# 直接流式打印，同时返回完整文本，无需手动循环
chat$chat("用一句话总结上面的内容。")

# pi 的模型会在回答前自行调用 read/grep 等工具
chat$chat("R/client.R 文件定义了哪些导出函数")

# %%
chat$get_tokens() 
chat$get_cost()          # 累计花费（美元）
data <- chat$get_turns() # 完整对话消息表
summary(data)
