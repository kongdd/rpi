# %% 
pacman::p_load(Ipaper, data.table, dplyr, lubridate, ellmer, rpi)

chat <- chat_pi("R语言tidyverse专家", client = pi_mini(), echo = "all")

# %% 结构化输出：并发提取姓名和年龄，保留字段类型 ---------------------
type <- ellmer::type_object(
  name = ellmer::type_string(description = "姓名"),
  year = ellmer::type_integer(description = "出生年份"), 
  age = ellmer::type_integer(description = "年龄")
)

chat$chat_structured(
  "张三他今年30岁。",
  type = type
)
