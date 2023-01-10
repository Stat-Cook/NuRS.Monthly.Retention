make.default.functions <- function() {
  #' Produce a list of default functions
  list(
    "Mean" = function(i) mean(i, na.rm = T),
    "Na Ratio" = function(i) sum(is.na(i)) / length(i),
    "Var" = function(i) var(i, na.rm = T),
    ".N." = length
  )
}

make.folders <- function() {
  #' Create project folder structure if needed
  #' @export

  dir.create(pkg.env$processed_folder)
  dir.create(pkg.env$result_folder)
}

process.across.f <- function(data, date.col,
                             .cols = everything(),
                             .fn = default.fn) {
  #' Function factory for lagging data
  #'
  #' Produce a template function designed for applying a lagging transform
  #' at different windows
  #'
  #' @param data A data frame to be operated on
  #' @param date.col A field of 'data' that will be lagged
  #' @param .cols The columns of 'data' to be summarized
  #' @param .fn The summary functions to be applied [named list]
  #'
  #' @return A function expecting a laging time window (months) as an argument.
  function(window = 3:4) {
    lagged.group(data, date.col, window) %>%
      dplyr::summarize(
        across(
          {{ .cols }},
          .fn,
          .names = "Lag {min(window)}-{max(window)} {.col} {.fn}"
        )
      )
  }
}

lagged.process <- function(process.func) {
  #' Apply a function to sets of lag windows
  #'
  #' @param process.func A function expecting to receive a lag-window to produce data.
  #'

  lags <- list(
    "Lag 3-4" = 3:4,
    "Lag 5-6" = 5:6,
    "Lag 7-12" = 7:12
  )
  lapply(lags, function(window) process.func(window))
}


backwards.nearest.f <- function(value, vector) {
  diff <- value - vector
  diff[diff < 0] <- NA
  .min <- min(diff, na.rm = T)
  vector[which(diff == .min)[1]]
}

backwards.nearest.v <- Vectorize(backwards.nearest.f,
  vectorize.args = "value",
  SIMPLIFY = FALSE
)

backwards.nearest <- function(values, vector) {
  do.call("c", backwards.nearest.v(values, vector))
}


merge.on.last <- function(data.frame, ref.frame, data.column, ref.column) {
  ref.column.string <- substitute(ref.column)

  bn <- backwards.nearest(
    data.frame[[data.column]],
    ref.frame[[ref.column.string]]
  )

  data.frame %>%
    mutate(
      {{ ref.column.string }} := bn
    ) %>%
    left_join(ref.frame)
}


is.int <- function(value) class(value) == "integer64"

int.to.double <- function(data) {
  #' Cast all int to double
  #'
  #' @param data A data frame

  mutate(
    data,
    across(
      where(is.int),
      as.double
    )
  )
}



merge.lists.by.name <- function(...) {
  #' Combines a series of lists based on named elements
  #'
  #' Takes a series of lists and produces a single list, combining based on
  #'   names.
  #'
  #' @param ... A series of Lists with shared names
  #'
  #' @importFrom purrr reduce
  l <- list(...)
  l.names <- lapply(l, names)
  unique.names <- unique(unlist(l.names))

  bool <- all(sapply(
    l.names,
    function(i) all(unique.names %in% i)
  ))

  if (!bool) {
    stop("Not all names found in every list")
  }

  results <- list()
  for (name in unique.names) {
    data <- lapply(l, function(i) i[[name]])
    results[[name]] <- reduce(data, merge)
  }

  results
}



all.ward.month.merge <- function(data) {
  #' Add exhaustive set of Wards and Dates to a data set
  #'
  #' Produces a data frame of all combinations of Wards and Dates in 'data',
  #' and merges to 'data'.  Results in 'na' values where the Ward and Date
  #' combination wasn't in 'data'
  #'
  #' @param data A data frame with Ward and Date fields.
  #'
  #' @return A data frame

  month.ward.template <- expand.grid(
    Ward = unique(data$Ward),
    Date = pkg.env$all.months
  ) %>% data.frame()

  left_join(
    month.ward.template,
    data
  )
}

gap.expand <- function(data, ...) {
  if (!all(c(...) %in% colnames(data))) {
    stop("Not all columns found in `data`.  Check spelling.")
  }

  uniques <- lapply(
    c(...),
    function(i) unique(data[[i]])
  )

  grid <- do.call(expand.grid, uniques)

  colnames(grid) <- c(...)

  expanded <- left_join(grid, data, by = c(...))

  expanded[is.na(expanded)] <- 0

  expanded
}

nurs.join <- function(x, y) {
  #' Join data sets for NuRS study.
  #'
  #' Join data frames based on Ward, Year, and Month
  #'
  #' @param x,y data frames with fields: 'Ward', 'Year' and 'Month'
  #'
  #' :@export
  left_join(x, y, by = c("Ward", "Year", "Month"))
}
