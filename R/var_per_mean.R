var.over.mean.f <- function(var, mean) {
  #' Calculate var ratio
  #'
  #' Calculate ratio of variance and mean - allowing for singular variables.
  #' 
  #' @param var Varaince of the data
  #' @param mean Mean of the data
  if (is.na(var)) {
    return(NA)
  }
  if (var == 0) {
    return(0)
  }

  var / mean
}

var.over.mean <- Vectorize(var.over.mean.f)

actual.within.shift.var.per.mean <- function(data, window, staff = "Registered", type = "Count") {
  #' Convert within shift variance for actual staffing levels.
  #'
  #' @param data Data frame
  #' @param window A lagging period present in 'data'
  #' @param staff A staff type in 'data'
  #' @param type A staffing metric in 'data'
  #'
  var.col.string <- glue("Lag {window} Actual {staff} {type} per Bed Within Shift Var")
  mean.col.string <- glue("Lag {window} Actual {staff} {type} per Bed Mean")
  data %>%
    mutate(
      "Lag {window} {staff} {type} per Bed Within Shift Var per Mean" :=
        var.over.mean(
          .data[[var.col.string]],
          .data[[mean.col.string]]
        )
    )
}

actual.within.shift.var.per.mean.loop <- function(data, grid) {
  #' Convert all within shift actual staffing variances
  #'
  #' Converts from variance to variance per mean.
  #'
  #' @param data A data frame
  #' @param grid A frame with headings `Window`, `Staff`, `Type`.
  #'
  #' @export
  for (i in 1:nrow(grid)) {
    data <- actual.within.shift.var.per.mean(
      data,
      grid$Window[i],
      grid$Staff[i],
      grid$Type[i]
    )
  }

  data
}


planned.between.shift.var.per.mean <- function(data, window, staff = "Registered", type = "Count") {
  #' Convert between shift variance for planned staffing levels.
  #'
  #' @param data Data frame
  #' @param window A lagging period present in 'data'
  #' @param staff A staff type in 'data'
  #' @param type A staffing metric in 'data'
  #'
  var.col.string <- glue("Lag {window} Planned {staff} {type} per Bed Between Shift Var")
  mean.col.string <- glue("Lag {window} Planned {staff} {type} per Bed Mean")
  data %>%
    mutate(
      "Lag {window} {staff} {type} per Bed Between Shift Var per Mean" :=
        var.over.mean(
          .data[[var.col.string]],
          .data[[mean.col.string]]
        )
    )
}

planned.between.shift.var.per.mean.loop <- function(data, grid) {
  #' Convert all between shift planned staffing variances
  #'
  #' Converts from varaiance to variance per mean.
  #'
  #' @param data A data frame
  #' @param grid A frame with headings `Window`, `Staff`, `Type`.
  #'
  #' @export
  for (i in 1:nrow(grid)) {
    data <- planned.between.shift.var.per.mean(
      data,
      grid$Window[i],
      grid$Staff[i],
      grid$Type[i]
    )
  }

  data
}


acuity.var.features.f <- function(data, window, feature) {
  #' Convert all raw staffing variances
  #'
  #' Converts from variance to variance per mean.
  #'
  #' @param data A data frame
  #' @param window The lagged window of interest [str]
  #' @param feature The type of variable to be used [str]
  #'
  #' @export
  var.col.string <- glue("Lag {window} {feature} per Bed Var")
  mean.col.string <- glue("Lag {window} {feature} per Bed Mean")
  data %>%
    mutate(
      "Lag {window} {feature} per Bed Var per Mean" := var.over.mean(
        .data[[var.col.string]],
        .data[[mean.col.string]]
      )
    ) %>%
    select(-all_of(var.col.string))
}

acuity.var.features <- function(data, grid) {
  #' Convert all raw planned staffing variances
  #'
  #' Converts from variance to variance per mean.
  #'
  #' @param data A data frame
  #' @param grid A frame with fields `Window` and `Features`
  #'
  #' @export
  for (i in 1:nrow(grid)) {
    data <- acuity.var.features.f(data, grid$Window[i], grid$Features[i])
  }

  data
}
