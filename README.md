# piagent

<!-- badges: start -->
<!-- badges: end -->

`piagent` 把 [pi](https://github.com/earendil-works/pi) 编码代理接入 R，接口风格对齐
[ellmer](https://ellmer.tidyverse.org)：先用 `pi_client()` 描述"怎么启动 pi"，
再用 `chat_pi()` 建立一次会话。

与直接调用大模型 API 的 ellmer 不同，pi 是一个带工具的代理进程：它能读文件、跑 shell、
改代码。`piagent` 把每一轮对话交给 `pi` 执行，并解析其 JSON 事件流，返回最终回答、
完整事件、token 与费用统计。

## 安装

```r
# 需要 PATH 上可执行 `pi`（Windows: pi.exe）
pak::pak("rpkgs/piagent")
```

## 基本用法

```r
library(piagent)

chat <- chat_pi(
  system_prompt = "回答尽量简短。",
  client = pi_client(dir = "~/projects/demo", model = "sonnet", thinking = "low")
)

chat$chat("这个包里哪些函数没有被引用？")
chat$get_turns()     # 逐条消息：角色、文本、token、费用
chat$get_cost()      # 累计费用（美元）
```

`pi` 的会话（session）保存在磁盘上，同一个 `PiChat` 的后续调用会沿用历史，
因此第二轮能记住第一轮的内容。

## 流式与异步

```r
chat$chat("总结 DESCRIPTION")               # 边生成边打印
chat$chat("总结 DESCRIPTION", echo = "all") # 同时打印思考与工具调用

promise <- chat$chat_async("统计 R/ 目录的行数")  # 返回 promise
```

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

| ellmer            | piagent            |
| ----------------- | ------------------ |
| `chat_openai()`   | `chat_pi()`        |
| `Provider`        | `pi_client()`      |
| `$chat()`         | `$chat()`（阻塞，同时流式打印） |
| `$stream()`       | —（`$chat()` 已流式） |
| `$chat_async()`   | `$chat_async()`    |
| `$get_turns()`    | `$get_turns()`（data.table） |
| `$get_tokens()`   | `$get_tokens()`    |
| `$register_tool()` | —（工具由 `pi_client(tools = )` 选择） |

## License

GPL-3
