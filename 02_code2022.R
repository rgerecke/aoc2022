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
