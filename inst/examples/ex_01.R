# %% 
devtools::load_all(".")
# library(piagent)
m <- models_pi()

# %% 
# 筛选某个 provider，并把模型写进客户端
client <- pi_client(dir = ".", model = "MiniMax-M3", thinking = "medium")

chat <- chat_pi("回答保持简洁，使用中文。", client = client, echo = "text")

# 阻塞式提问，返回最终回答字符串（echo = "text" 时同时打印）
chat$chat("介绍这个项目")
data <- chat$get_turns()

# %%
# 直接流式打印，同时返回完整文本，无需手动循环
chat$chat("用一句话总结上面的内容。")

# pi 的模型会在回答前自行调用 read/grep 等工具
chat$chat("R/client.R 文件定义了哪些导出函数")

# %% 
chat$get_tokens()

# 累计花费（美元）
chat$get_cost()

data <- chat$get_turns()
# 完整对话消息表
summary(data)
