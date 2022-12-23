library(tidyverse)

# day 11 --------------------

monkeys <- list(
  monk0 = list(
    items = c(98, 70, 75, 80, 84, 89, 55, 98),
    fun   = function(item) {
      a <- item * 2
      b <- floor(a / 3)
      if (b %% 11 == 0) {
        return(c(b, 1))
      } else {
        return(c(b, 4))
      }
    },
    n_inspect = 0
  ),
  monk1 = list(
    items = c(59),
    fun   = function(item) {
      a <- item * item
      b <- floor(a / 3)
      if (b %% 19 == 0) {
        return(c(b, 7))
      } else {
        return(c(b, 3))
      }
    },
    n_inspect = 0
  ),
  monk2 = list(
    items = c(77, 95, 54, 65, 89),
    fun   = function(item) {
      a <- item + 6
      b <- floor(a / 3)
      if (b %% 7 == 0) {
        return(c(b, 0))
      } else {
        return(c(b, 5))
      }
    },
    n_inspect = 0
  ),
  monk3 = list(
    items = c(71, 64, 75),
    fun   = function(item) {
      a <- item + 2
      b <- floor(a / 3)
      if (b %% 17 == 0) {
        return(c(b, 6))
      } else {
        return(c(b, 2))
      }
    },
    n_inspect = 0
  ),
  monk4 = list(
    items = c(74, 55, 87, 98),
    fun   = function(item) {
      a <- item * 11
      b <- floor(a / 3)
      if (b %% 3 == 0) {
        return(c(b, 1))
      } else {
        return(c(b, 7))
      }
    },
    n_inspect = 0
  ),
  monk5 = list(
    items = c(90, 98, 85, 52, 91, 60),
    fun   = function(item) {
      a <- item + 7
      b <- floor(a / 3)
      if (b %% 5 == 0) {
        return(c(b, 0))
      } else {
        return(c(b, 4))
      }
    },
    n_inspect = 0
  ),
  monk6 = list(
    items = c(99, 51),
    fun   = function(item) {
      a <- item + 1
      b <- floor(a / 3)
      if (b %% 13 == 0) {
        return(c(b, 5))
      } else {
        return(c(b, 2))
      }
    },
    n_inspect = 0
  ),
  monk7 = list(
    items = c(98, 94, 59, 76, 51, 65, 75),
    fun   = function(item) {
      a <- item + 5
      b <- floor(a / 3)
      if (b %% 2 == 0) {
        return(c(b, 3))
      } else {
        return(c(b, 6))
      }
    },
    n_inspect = 0
  )
)

for (monkey in names(monkeys)) {
  for (item in monkeys[[monkey]][["items"]]) {
    # increase inspection number by 1
    monkeys[[monkey]][["n_inspect"]] <- monkeys[[monkey]][["n_inspect"]] + 1

    #
    res <- monkeys[[monkey]][["fun"]](item)
    monkeys[[res[2] + 1]][["items"]] <- c(monkeys[[res[2] + 1]][["items"]],
                                          res[1])


  }
  # clear monkey stash
  monkeys[[monkey]][["items"]] <- c()
}

## 54253

## part 2 ---------------

monkeys <- list(
  monk0 = list(
    items = c(98, 70, 75, 80, 84, 89, 55, 98),
    fun   = function(item) {
      a <- (item * 2) %% 9699690
      if (a %% 11 == 0) {
        return(c(a, 1))
      } else {
        return(c(a, 4))
      }
    },
    n_inspect = 0
  ),
  monk1 = list(
    items = c(59),
    fun   = function(item) {
      a <- (item * item) %% 9699690
      if (a %% 19 == 0) {
        return(c(a, 7))
      } else {
        return(c(a, 3))
      }
    },
    n_inspect = 0
  ),
  monk2 = list(
    items = c(77, 95, 54, 65, 89),
    fun   = function(item) {
      a <- (item + 6) %% 9699690
      if (a %% 7 == 0) {
        return(c(a, 0))
      } else {
        return(c(a, 5))
      }
    },
    n_inspect = 0
  ),
  monk3 = list(
    items = c(71, 64, 75),
    fun   = function(item) {
      a <- (item + 2) %% 9699690
      if (a %% 17 == 0) {
        return(c(a, 6))
      } else {
        return(c(a, 2))
      }
    },
    n_inspect = 0
  ),
  monk4 = list(
    items = c(74, 55, 87, 98),
    fun   = function(item) {
      a <- (item * 11) %% 9699690
      if (a %% 3 == 0) {
        return(c(a, 1))
      } else {
        return(c(a, 7))
      }
    },
    n_inspect = 0
  ),
  monk5 = list(
    items = c(90, 98, 85, 52, 91, 60),
    fun   = function(item) {
      a <- (item + 7) %% 9699690
      if (a %% 5 == 0) {
        return(c(a, 0))
      } else {
        return(c(a, 4))
      }
    },
    n_inspect = 0
  ),
  monk6 = list(
    items = c(99, 51),
    fun   = function(item) {
      a <- (item + 1) %% 9699690
      if (a %% 13 == 0) {
        return(c(a, 5))
      } else {
        return(c(a, 2))
      }
    },
    n_inspect = 0
  ),
  monk7 = list(
    items = c(98, 94, 59, 76, 51, 65, 75),
    fun   = function(item) {
      a <- (item + 5) %% 9699690
      if (a %% 2 == 0) {
        return(c(a, 3))
      } else {
        return(c(a, 6))
      }
    },
    n_inspect = 0
  )
)


run_monkeys <- function (monkeys, ...) {
  for (monkey in names(monkeys)) {
    for (item in monkeys[[monkey]][["items"]]) {
      # increase inspection number by 1
      monkeys[[monkey]][["n_inspect"]] <- monkeys[[monkey]][["n_inspect"]] + 1

      # add new worry item to next monkey
      res <- monkeys[[monkey]][["fun"]](item)
      monkeys[[res[2] + 1]][["items"]] <- c(monkeys[[res[2] + 1]][["items"]],
                                            res[1])
    }
    # clear monkey stash
    monkeys[[monkey]][["items"]] <- c()
  }
  return(monkeys)
}



out <- reduce(1:10000, run_monkeys, .init = monkeys)
map_dbl(out, pluck, "n_inspect") |>
  enframe() |>
  slice_max(value, n = 2)

## 13119526120


# day 12 ----------------

df <- read_csv("data/2022/day12.csv",
               col_names = "a") |>
  pull(a) |>
  str_split(boundary("character"), simplify = TRUE) |>
  matrix(nrow = 41)


df_res <- expand_grid(r = 1:41, c = 1:173) |>
  mutate(init = map2_chr(r,c,~df[.x,.y]),
         dist = ifelse(init == "S", 0, Inf),
         insp = ifelse(init == "E", "end", "Not Inspected"),
         init = str_replace(init, "S", "a") |>
           str_replace("E", "z"),
         elev = match(init, letters))

to_inspect <- filter(df_res, r == 21, c == 1)
df_iterate <- filter(df_res, r != 21 | c != 1)

explore_neighbors <- function (point_from) {
  neighbors_loc <- filter(
    df_iterate,
    (abs(point_from$r - r) == 1 & point_from$c == c)
    | (abs(point_from$c - c) == 1 & point_from$r == r),
    insp != "Inspected",
    elev <= point_from$elev + 1
    ) |>
    mutate(dist = point_from$dist + 1)

  return(neighbors_loc)
}

tictoc::tic()

while (!("end" %in% to_inspect$insp)) {

  # set up to iterate through next points of inspection
  inspections <- to_inspect |>
    mutate(insp = "Inspected")

  to_inspect <- map_dfr(group_split(inspections, row_number()),
                        explore_neighbors) |>
    distinct()

  df_iterate <- anti_join(df_iterate, to_inspect, by = c("r", "c")) |>
    bind_rows(inspections)

  next
}

tictoc::toc()

to_inspect |>
  filter(insp == "end")

## 490

## part 2 ----------------------------

# probably want to go backwards from the center until you arrive at the first a

df_res2 <- expand_grid(r = 1:41, c = 1:173) |>
  mutate(init = map2_chr(r,c,~df[.x,.y]),
         dist = ifelse(init == "E", 0, Inf),
         insp = ifelse(init == "E", "end", "Not Inspected"),
         init = str_replace(init, "S", "a") |>
           str_replace("E", "z"),
         elev = match(init, letters))

to_inspect <- filter(df_res2, r == 21, c == 149)
df_iterate <- filter(df_res2, r != 21 | c != 149)

explore_neighbors <- function (point_from) {
  neighbors_loc <- filter(
    df_iterate,
    (abs(point_from$r - r) == 1 & point_from$c == c)
    | (abs(point_from$c - c) == 1 & point_from$r == r),
    insp != "Inspected",
    elev >= point_from$elev - 1
  ) |>
    mutate(dist = point_from$dist + 1)

  return(neighbors_loc)
}

tictoc::tic()

while (!("a" %in% to_inspect$init)) {

  # set up to iterate through next points of inspection
  inspections <- to_inspect |>
    mutate(insp = "Inspected")

  to_inspect <- map_dfr(group_split(inspections, row_number()),
                        explore_neighbors) |>
    distinct()

  df_iterate <- anti_join(df_iterate, to_inspect, by = c("r", "c")) |>
    bind_rows(inspections)

  next
}

tictoc::toc()

to_inspect |>
  filter(init == "a")

## 488
