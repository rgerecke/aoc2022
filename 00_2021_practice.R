library(readr)
library(dplyr)

# day 01-1 ------
df <- read_csv("data/day01.csv", col_names = "a")
df |>
  mutate(b = a > lag(a)) |>
  count(b)

## 1766

# day 01-2 ------
df |>
  mutate(b = a + lag(a) + lag(a, 2),
         c = b > lag(b)) |>
  count(c)

## 1797

# day 02-1 -----

