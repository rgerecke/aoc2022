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


# day 2 ---------

df <- read_csv("data/2022/day02.csv",
               col_names = "a")

rps <- function(opp,player) {
  pt1 <- case_when(
    opp == "A" & player == "X" ~ 3,
    opp == "B" & player == "Y" ~ 3,
    opp == "C" & player == "Z" ~ 3,
    opp == "A" & player == "Y" ~ 6,
    opp == "B" & player == "Z" ~ 6,
    opp == "C" & player == "X" ~ 6,
    TRUE ~ 0
  )

  pt2 <- case_when(
    player == "X" ~ 1,
    player == "Y" ~ 2,
    player == "Z" ~ 3
  )

  return(pt1 + pt2)
}


df |>
  separate(a, c("a", "b"), sep = " ") |>
  mutate(
    c = map2_dbl(a, b, rps)
  ) |>
  summarise(sum(c))

## 13565

## part 2 -------------

rps2 <- function(opp, out) {
  opp_num <- match(opp, LETTERS) - 1

  play_num <- case_when(
    out == "X" ~ (opp_num - 1) %% 3,
    out == "Y" ~ opp_num,
    out == "Z" ~ (opp_num + 1) %% 3,
  )

  play <- LETTERS[play_num + 24]

  return(rps(opp, play))
}


df |>
  separate(a, c("a", "b"), sep = " ") |>
  mutate(c = map2_dbl(a, b, rps2)) |>
  summarise(sum(c))

## 12424
