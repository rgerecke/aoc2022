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

# day 06 ---------------
library(tidytext)

df <- read_csv("data/2022/day06.csv",
               col_names = "a")

df |>
  mutate(a = str_split(a, boundary("character")) |>
           map_chr(~str_c(.,collapse = " "))) |>
  unnest_tokens(b, a, token = "ngrams", n = 4) |>
  mutate(
    b = str_split(b, " "),
    c = map_dbl(b, ~length(unique(.))),
    d = row_number() + 3) |>
  filter(c == 4)

## 1238

## part 2 ------------------

df |>
  mutate(a = str_split(a, boundary("character")) |>
           map_chr(~str_c(.,collapse = " "))) |>
  unnest_tokens(b, a, token = "ngrams", n = 14) |>
  mutate(
    b = str_split(b, " "),
    c = map_dbl(b, ~length(unique(.))),
    d = row_number() + 13) |>
  filter(c == 14)

## 3037

# day 07 ------------------

## with great debt owed to
## https://github.com/KT421/advent-of-code/blob/main/2022/dec07.R
## thrilled that my code microbenchmarks faster though

df <- read_csv("data/2022/day07.csv",
               col_names = "a")

current_dir <- "main"

file_sizes <- tribble(~folder, ~size)

for (x in df$a) {
  # skip ls & dir lines
  if (x == "$ cd /") next
  if (x == "$ ls") next
  if (str_detect(x, "^dir ")) next

  # if terminal file, register size
  if (str_detect(x, "[[:digit:]]")) {
    file_sizes <- add_row(
      file_sizes,
      folder = current_dir,
      size = parse_number(x)
    )
    next
  }

  # if change to subdirectory, update dirs
  if (str_detect(x, "\\$ cd [[:alpha:]]")) {
    current_dir <- str_c(
      current_dir,
      str_extract(x, "(?<=\\$ cd ).*"),
      sep = " "
    )
    next
  }

  # if go back a directory, update dirs
  if (x == "$ cd ..") {
    current_dir <- str_remove(
      current_dir,
      "( [[:alpha:]]+)$"
    )
    next
  }
}

all_folders <- file_sizes |>
  separate(folder, LETTERS[1:10], fill = "right")

all_sizes <- bind_rows(
  group_by(all_folders, A) |>
    summarise(size = sum(size)),
  group_by(all_folders, across(A:B)) |>
    summarise(size = sum(size), .groups = "drop") %>%
    filter(complete.cases(.)),
  group_by(all_folders, across(A:C)) |>
    summarise(size = sum(size), .groups = "drop") %>%
    filter(complete.cases(.)),
  group_by(all_folders, across(A:D)) |>
    summarise(size = sum(size), .groups = "drop") %>%
    filter(complete.cases(.)),
  group_by(all_folders, across(A:E)) |>
    summarise(size = sum(size), .groups = "drop") %>%
    filter(complete.cases(.)),
  group_by(all_folders, across(A:F)) |>
    summarise(size = sum(size), .groups = "drop") %>%
    filter(complete.cases(.)),
  group_by(all_folders, across(A:G)) |>
    summarise(size = sum(size), .groups = "drop") %>%
    filter(complete.cases(.)),
  group_by(all_folders, across(A:H)) |>
    summarise(size = sum(size), .groups = "drop") %>%
    filter(complete.cases(.)),
  group_by(all_folders, across(A:I)) |>
    summarise(size = sum(size), .groups = "drop") %>%
    filter(complete.cases(.)),
  group_by(all_folders, across(A:J)) |>
    summarise(size = sum(size), .groups = "drop") %>%
    filter(complete.cases(.))
)

all_sizes |>
  filter(size <= 100000) |>
  summarise(size = sum(size))

## 1391690

## part 2 -----------------------------------

## disk space = 70000000
## main dir   = 44795677
## space remaining = 25204323
## space needed    = 30000000
## space to clear  = 4795677

all_sizes |>
  filter(size >= 4795677) |>
  arrange(size)

## 5469168


# day 08 --------------------

df <- read_csv("data/2022/day08.csv",
               col_names = "a")

trees <- df |>
  mutate(a = str_split(a, boundary("character"))) |>
  pull(a) |>
  reduce(c) |>
  as.numeric() |>
  matrix(nrow = nrow(df), byrow = TRUE)

visible <- c()

for (row in c(1:99)) {
  for (col in c(1:99)) {
    if (row == 1 | row == 99 | col == 1 | col == 99) {
      visible <- c(visible, T)
    } else {
      tree <- trees[row, col]

      above <- all(trees[1:(row-1), col:col] < tree)
      below <- all(trees[(row+1):99, col:col] < tree)
      left  <- all(trees[row:row, 1:(col-1)] < tree)
      right <- all(trees[row:row, (col+1):99] < tree)

      res <- any(above, below, left, right)

      visible <- c(visible, res)
    }
  }
}

sum(visible)

## 1698

# part 2 --------------------

get_score <- function (tree, dir) {
  if (all(dir < tree)) {
    return(length(dir))
  } else {
    return(min(which(dir >= tree)))
  }
}

high_score <- 0

for (row in c(1:99)) {
  for (col in c(1:99)) {
    if (row == 1 | row == 99 | col == 1 | col == 99) {
      next
    } else {
      tree <- trees[row, col]

      above <- get_score(tree, rev(trees[1:(row-1), col:col]))
      below <- get_score(tree, trees[(row+1):99, col:col])
      left  <- get_score(tree, rev(trees[row:row, 1:(col-1)]))
      right <- get_score(tree, trees[row:row, (col+1):99])

      res <- above * below * left * right

      if (res > high_score) {
        high_score <- res
        next
      } else {next}
    }
  }
}

## 672280


# day 09 ------------------

df <- read_csv("data/2022/day09.csv",
               col_names = "a")

h_pos <- c(0,0)
t_pos <- c(0,0)
t_hist <- list(c(0,0))

move_h <- function(pos, dir) {
  if (dir == "L") {
    return(pos + c(-1, 0))
  } else if (dir == "R") {
    return(pos + c(1, 0))
  } else if (dir == "U") {
    return(pos + c(0, 1))
  } else if (dir == "D") {
    return(pos + c(0, -1))
  }
}

for (x in df$a) {
  dir <- str_sub(x, 1, 1)
  num <- parse_number(x)

  while (num > 0) {
    # move h
    h_pos <- move_h(h_pos, dir)

    # if t within range of h, next movement
    gap <- h_pos - t_pos
    if (all(abs(gap) <= 1)) {
      num <- num - 1
      next
    }

    # if t out of range, move t
    move_t <- sign(gap)
    t_pos <- t_pos + move_t
    t_hist <- append(t_hist, list(t_pos))
    num <- num - 1
  }
}

unique(t_hist) |>
  length()

## 6376

## part 2 -----------------

h_pos <- c(0,0)
t1_pos <- c(0,0)
t2_pos <- c(0,0)
t3_pos <- c(0,0)
t4_pos <- c(0,0)
t5_pos <- c(0,0)
t6_pos <- c(0,0)
t7_pos <- c(0,0)
t8_pos <- c(0,0)
t9_pos <- c(0,0)
t9_hist <- list(c(0,0))

move_h <- function(pos, dir) {
  if (dir == "L") {
    return(pos + c(-1, 0))
  } else if (dir == "R") {
    return(pos + c(1, 0))
  } else if (dir == "U") {
    return(pos + c(0, 1))
  } else if (dir == "D") {
    return(pos + c(0, -1))
  }
}

for (x in df$a) {
  dir <- str_sub(x, 1, 1)
  num <- parse_number(x)

  while (num > 0) {
    # move h
    h_pos <- move_h(h_pos, dir)

    # if t1 within range of h, next movement
    gap1 <- h_pos - t1_pos
    if (all(abs(gap1) <= 1)) {
      num <- num - 1
      next
    }

    # if t1 out of range, move t1
    # compare to t2
    move_t1 <- sign(gap1)
    t1_pos <- t1_pos + move_t1
    gap2 <- t1_pos - t2_pos
    if (all(abs(gap2) <= 1)) {
      num <- num - 1
      next
    }

    # if t2 out of range, move t2
    # compare t3
    move_t2 <- sign(gap2)
    t2_pos <- t2_pos + move_t2
    gap3 <- t2_pos - t3_pos
    if (all(abs(gap3) <= 1)) {
      num <- num - 1
      next
    }

    # if t3 out of range, move t3
    # compare t4
    move_t3 <- sign(gap3)
    t3_pos <- t3_pos + move_t3
    gap4 <- t3_pos - t4_pos
    if (all(abs(gap4) <= 1)) {
      num <- num - 1
      next
    }

    # if t4 out of range, move t4
    # compare t5
    move_t4 <- sign(gap4)
    t4_pos <- t4_pos + move_t4
    gap5 <- t4_pos - t5_pos
    if (all(abs(gap5) <= 1)) {
      num <- num - 1
      next
    }

    # if t5 out of range, move t5
    # compare t6
    move_t5 <- sign(gap5)
    t5_pos <- t5_pos + move_t5
    gap6 <- t5_pos - t6_pos
    if (all(abs(gap6) <= 1)) {
      num <- num - 1
      next
    }

    # if t6 out of range, move t6
    # compare t7
    move_t6 <- sign(gap6)
    t6_pos <- t6_pos + move_t6
    gap7 <- t6_pos - t7_pos
    if (all(abs(gap7) <= 1)) {
      num <- num - 1
      next
    }

    # if t7 out of range, move t7
    # compare t8
    move_t7 <- sign(gap7)
    t7_pos <- t7_pos + move_t7
    gap8 <- t7_pos - t8_pos
    if (all(abs(gap8) <= 1)) {
      num <- num - 1
      next
    }

    # if t8 out of range, move t8
    # compare t9
    move_t8 <- sign(gap8)
    t8_pos <- t8_pos + move_t8
    gap9 <- t8_pos - t9_pos
    if (all(abs(gap9) <= 1)) {
      num <- num - 1
      next
    }

    move_t9 <- sign(gap9)
    t9_pos <- t9_pos + move_t9
    t9_hist <- append(t9_hist, list(t9_pos))
    num <- num - 1
  }
}

unique(t9_hist) |>
  length()

## 2607

