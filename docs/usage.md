# 使用说明

[返回项目介绍](../README.md)

`rpi` 把 [pi](https://github.com/earendil-works/pi) 编码代理接入 R，接口风格对齐
[ellmer](https://ellmer.tidyverse.org)：先用 `pi_client()` 描述"怎么启动 pi"，
再用 `chat_pi()` 建立一次会话。

与直接调用大模型 API 的 ellmer 不同，pi 是一个带工具的代理进程：它能读文件、跑 shell、
改代码。`rpi` 把每一轮对话交给 `pi` 执行，并解析其 JSON 事件流，返回最终回答、
完整事件、token 与费用统计。

## 安装

```r
# 需要 PATH 上可执行 `pi`（Windows: pi.exe）
pak::pak("kongdd/rpi")
```

## 基本用法

```r
library(rpi)

chat <- chat_pi(
  system_prompt = "回答尽量简短。",
  client = pi_client(dir = "~/projects/demo", model = "sonnet", thinking = "low")
)

chat$chat("这个包里哪些函数没有被引用？")
chat$get_turns()     # Turn 对象列表：文本、token、费用
chat$get_cost()      # 累计费用（美元）
```

`pi` 的会话保存在磁盘上，同一 chat 的后续调用沿用历史；
`chat_pi()` 等价于 `chat_pi_ellmer()`。详见下节。

## 流式与异步

```r
text <- chat$chat("总结 DESCRIPTION")      # 实时打印，并返回完整文本
chat$chat("总结 DESCRIPTION", echo = "all") # 同时打印思考与工具调用

promise <- chat$chat_async("统计 R/ 目录的行数")  # 返回 promise
```

统一使用 `$chat()` 实时输出，`$stream()` 已移除。
`$stream_async()` 仅供需要异步逐块处理的程序使用。

## 作为 ellmer 的 Chat 使用

对于 pi 支持的订阅/OAuth provider，登录 pi 后即可使用，无需另配 API key。
`chat_pi_ellmer()` 继承 `ellmer::Chat`，常用对话接口直接驱动 pi 的模型、工具与扩展：

```r
chat <- chat_pi_ellmer(model = "sonnet")   # 继承 ellmer::Chat
chat$chat("总结 DESCRIPTION")
chat$get_tokens()                          # ellmer 的 token / cost 表示
chat$chat_async("列出 R/ 里的函数")
```

`$get_turns()` 从 pi 会话的当前分支读取消息，包含工具请求与结果；
`$set_turns()` 会建立包含替换历史的新会话，下一次请求真实使用该历史，原文件不变。
`$get_events()` 可查看原始会话记录（包括系统、扩展、压缩信息）。恢复已有会话用
`session_id`，临时会话用 `pi_client(session = FALSE)`，结束后 `$close()` 清理临时文件。
同一会话不要同时由多个进程写入。

```r
chat$chat_structured("提取包名", type = ellmer::type_string())
chat$chat("描述图片", ellmer::content_image_file("plot.png", resize = "none"))
```

结构化输出通过提示词约束 JSON，并复用 ellmer 的类型转换，不是 provider 的强制 schema。
图片可直接传入；PDF 等其他附件请交给 pi 的 `@file` 与工具处理。
`$register_tool()` 明确报错：工具仍通过 `pi_client(tools = )` 或 pi extension 配置。

## 仅复用 pi 授权，直接调用 ellmer

`chat_codex()` 读取 pi 的 Codex OAuth 授权，由 ellmer 直接发送 HTTP 请求，
**不启动 pi，也不使用 pi 的工具或会话**，可以注册 R 工具：

```r
chat <- chat_codex(model = "gpt-6-luna", system_prompt = "回答简短。")
chat$chat("你好")
chat$register_tool(ellmer::tool(
  function(x, y) x + y, "两数相加",
  arguments = list(x = ellmer::type_number(), y = ellmer::type_number()), name = "add"
))
```

授权默认来自 `~/.pi/agent/auth.json`，也遵循 `PI_CODING_AGENT_DIR`。
文件只读，每次请求重新读取；过期时需在 pi 中刷新或重新登录。
仅支持 `openai-codex` OAuth；模型需在账号权限内，参数需符合 Codex 接口限制。
不支持 OpenAI 的文件上传及批处理接口。

## 客户端配置

`pi_client()` 覆盖 `pi` 的命令行开关：

```r
pi_client(
  dir = ".",                          # 代理的工作目录
  model = "sonnet",                   # 模型或 provider/model:thinking
  thinking = "high",                  # 思考等级
  tools = "read,grep,find,ls",        # 工具白名单
  exclude_tools = "write,bash",
  append_system_prompt = "用中文回答",
  session = FALSE,                    # 不落盘，每次调用相互独立
  args = "-a",                        # 传给 pi 的其它参数
  timeout = 600                       # 单次调用超时（秒）
)
```

`system_prompt`（替换内置提示，会同时丢掉工具说明，慎用）与
`append_system_prompt`（追加指令）的区别，见 `?pi_client`。

## 事件与 token

```r
events <- chat$get_events()   # pi JSON 事件流的原始列表
chat$get_tokens()             # input / output / cached_input / cost
```

`chat$get_command()` 打印下一次调用实际使用的命令行，便于排查问题。

## 与 ellmer 的对应关系

| ellmer            | rpi            |
| ----------------- | ------------------ |
| `chat_openai()`   | `chat_pi()` / `chat_pi_ellmer()`、`chat_codex()` |
| `Provider`        | `pi_client()`      |
| `$chat()`         | `$chat()`（阻塞，同时流式打印） |
| `$stream()`       | —（控制台输出用 `$chat()`） |
| `$chat_async()`   | `$chat_async()`    |
| `$get_turns()`    | `$get_turns()`     |
| `$get_tokens()`   | `$get_tokens()`    |
| `$register_tool()` | —（工具由 `pi_client(tools = )` 选择） |
