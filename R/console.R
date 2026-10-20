# 共享控制台通道 -----------------------------------------------------------
#
# 多个 chat 并发输出时，一行只属于一个 chat，换 chat 自动断行；
# 带 label 的 chat，每个非空行加上 `[label]` 标签，便于区分来源。

pi_console <- new.env(parent = emptyenv())
pi_console$owner <- NULL # 当前未结束行的归属；NULL 表示行首

# 每个请求一个唯一编号，作为行的归属标识
pi_console_id <- local({
  n <- 0L
  function() {
    n <<- n + 1L
    n
  }
})

# 写一段文本：行首加标签，行内换行后同样加标签
pi_console_write <- function(id, tag = "", chunk, color = identity) {
  if (!nzchar(chunk)) return(invisible())
  same <- identical(pi_console$owner, id)
  if (!is.null(pi_console$owner) && !same) cat("\n")

  # 追加换行再拆分，保留原文末尾的空行。
  parts <- strsplit(paste0(chunk, "\n"), "\n", fixed = TRUE)[[1]]
  for (i in seq_along(parts)) {
    if (!nzchar(parts[[i]])) next
    prefix <- if (i > 1L || !same) tag else ""
    parts[[i]] <- paste0(prefix, color(parts[[i]]))
  }
  cat(paste(parts, collapse = "\n"), sep = "")
  pi_console$owner <- if (endsWith(chunk, "\n")) NULL else id
  utils::flush.console()
  invisible()
}

# 结束本 chat 打开的行；他人正在写的行不动
pi_console_newline <- function(id) {
  if (!identical(pi_console$owner, id)) return(FALSE)
  cat("\n")
  pi_console$owner <- NULL
  TRUE
}

# 复位通道状态（测试用）
pi_console_reset <- function() {
  pi_console$owner <- NULL
  invisible()
}

# 行标签
pi_tag <- function(label) {
  if (is.null(label) || !nzchar(label)) return("")
  blue(paste0("[", label, "] "))
}
