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


# day 05 -------------

df <- read_csv("data/2022/day05.csv",
               skip = 10,
               col_names = "a")


df_unglue <- unglue(df$a, "move {x} from {from} to {to}",
                    convert = TRUE)



#         [J]         [B]     [T]
#         [M] [L]     [Q] [L] [R]
#         [G] [Q]     [W] [S] [B] [L]
# [D]     [D] [T]     [M] [G] [V] [P]
# [T]     [N] [N] [N] [D] [J] [G] [N]
# [W] [H] [H] [S] [C] [N] [R] [W] [D]
# [N] [P] [P] [W] [H] [H] [B] [N] [G]
# [L] [C] [W] [C] [P] [T] [M] [Z] [W]
# 1   2   3   4   5   6   7   8   9

test <- list(
  c("L", "N", "W", "T", "D"),
  c("C", "P", "H"),
  c("W", "P", "H", "N", "D", "G", "M", "J"),
  c("C", "W", "S", "N", "T", "Q", "L"),
  c("P", "H", "C", "N"),
  c('T', 'H', 'N', 'D', 'M', 'W', 'Q', 'B'),
  c('M', 'B', 'R', 'J', 'G', 'S', 'L'),
  c('Z', 'N', 'W', 'G', 'V', 'B', 'R', 'T'),
  c('W', 'G', 'D', 'N', 'P', 'L')
)

for (row in df_unglue) {

  from_return <- head(test[[row$from]], -row$x)
  to_return <- c(test[[row$to]], rev(tail(test[[row$from]], row$x)))

  test[[row$from]] <- from_return
  test[[row$to]] <- to_return
}

map_chr(test, tail, 1) |>
  str_c(collapse = "")

## TWSGQHNHL

## part 2 ---------------

test2 <- list(
  c("L", "N", "W", "T", "D"),
  c("C", "P", "H"),
  c("W", "P", "H", "N", "D", "G", "M", "J"),
  c("C", "W", "S", "N", "T", "Q", "L"),
  c("P", "H", "C", "N"),
  c('T', 'H', 'N', 'D', 'M', 'W', 'Q', 'B'),
  c('M', 'B', 'R', 'J', 'G', 'S', 'L'),
  c('Z', 'N', 'W', 'G', 'V', 'B', 'R', 'T'),
  c('W', 'G', 'D', 'N', 'P', 'L')
)

for (row in df_unglue) {

  from_return <- head(test2[[row$from]], -row$x)
  to_return <- c(test2[[row$to]], tail(test2[[row$from]], row$x))

  test2[[row$from]] <- from_return
  test2[[row$to]] <- to_return
}

map_chr(test2, tail, 1) |>
  str_c(collapse = "")

## JNRSCDWPP
