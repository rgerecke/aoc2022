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


# day 03 -------------

df <- read_csv("data/2022/day03.csv", col_names = "a")

find_match <- function(list1, list2) {

}

df |>
  mutate(
    b = str_sub(a, end = str_length(a) / 2) |>
      str_split(boundary("character")),
    c = str_sub(a, start = (str_length(a) / 2) + 1) |>
      str_split(boundary("character"))
  ) |>
  unnest(b) |>
  unnest(c) |>
  filter(b == c) |>
  distinct(a, b, c) |>
  mutate(d = match(c, c(letters, LETTERS))) |>
  summarise(sum(d))

## 7737

## part 2 -------------

df |>
  mutate(
    a = str_split(a, boundary("character")) |>
      map(unique),
    b = cumsum(row_number() %% 3 == 1)
  ) |>
  unnest(a) |>
  count(b, a) |>
  filter(n == 3) |>
  mutate(d = match(a, c(letters, LETTERS))) |>
  summarise(sum(d))

## 2697


# day 04 ------------

df <- read_csv("data/2022/day04.csv",
               col_names = c("a", "b"))

comp <- function(a, b) {
  amin <- as.numeric(str_extract(a, ".*(?=\\-)"))
  amax <- as.numeric(str_extract(a, "(?<=\\-).*"))

  bmin <- as.numeric(str_extract(b, ".*(?=\\-)"))
  bmax <- as.numeric(str_extract(b, "(?<=\\-).*"))

  if (amin == bmin | amax == bmax) {
    return(TRUE)
  } else if (amin > bmin) {
      return(amax < bmax)
  } else if (amin < bmin) {
      return(amax > bmax)
  }
}

df |>
  mutate(c = map2_lgl(a, b, comp)) |>
  summarise(sum(c))

## 605


## part 2 ------------------

comp2 <- function(a, b) {
  amin <- as.numeric(str_extract(a, ".*(?=\\-)"))
  amax <- as.numeric(str_extract(a, "(?<=\\-).*"))

  bmin <- as.numeric(str_extract(b, ".*(?=\\-)"))
  bmax <- as.numeric(str_extract(b, "(?<=\\-).*"))

  if (amin == bmin | amax == bmax |
      amin == bmax | amax == bmin) {
    return(TRUE)
  } else if (amin > bmin) {
    return(amin < bmax)
  } else if (amin < bmin) {
    return(amax > bmin)
  }
}

df |>
  mutate(c = map2_lgl(a, b, comp2)) |>
  summarise(sum(c))

## 914
