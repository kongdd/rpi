# 异步：发多个提问，等 promise 完成后取回结果 ---------------------------
# %% 
library(piagent)


chats <- lapply(1:3, function(i) chat_pi(client = pi_client(dir = ".")))
ps <- lapply(chats, function(chat) chat$chat_async("用一句话介绍你自己"))

# promises 轮询事件循环直到全部完成
while (any(!promises::is_resolved(ps))) later::run_now(0.1)

vapply(ps, function(p) promises::extract(p), character(1))
