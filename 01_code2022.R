library(tidyverse)

# day 01 ----------

df <- read_csv("data/2022/day01.csv",
               col_names = "a",
               skip_empty_rows = FALSE)

df |>
  mutate(b = is.na(lag(a)),
         c = cumsum(b)) |>
  group_by(c) |>
  summarise(a = sum(a, na.rm = TRUE)) |>
  arrange(-a)

## 68802

## part 2 --------

df |>
  mutate(b = is.na(lag(a)),
         c = cumsum(b)) |>
  group_by(c) |>
  summarise(a = sum(a, na.rm = TRUE)) |>
  arrange(-a) |>
  slice(1:3) |>
  summarise(a = sum(a))

## 205370
