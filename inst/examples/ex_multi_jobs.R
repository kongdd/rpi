# 异步：并发提问，collect() 等全部完成后取回结果 ---------------------
# %%
library(rpi)

# label 区分并发 chat 的输出：每行前加 [A] [B] [C] 标签
chats <- lapply(LETTERS[1:3], \(x) chat_pi(client = pi_client(dir = "."), label = x))
ps <- lapply(chats, \(chat) chat$chat_async("你是什么模型"))
unlist(collect(ps))

chat = chats[[1]]
