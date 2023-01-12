# Tag:Inclusion
# Tag:Exclusion

inc_exc_criteria <- function(data) {
  #' Apply exclusion criteria to data set
  #'
  #' @param data A data frame with columns 'Year' and 'Ward'
  #'
  #' @export
  data %>%
    filter(.data$Year < 2021) %>%
    filter(.data$Year < 2020 | .data$Month < 6) %>%
    filter(.data$Year > 2015) %>%
    filter(.data$Year > 2016 | .data$Month >= 8) %>%
    filter(!str_detect(.data$Ward, "Ward 4")) %>%
    filter(!str_detect(.data$Ward, "Discharge")) %>%
    filter(!str_detect(.data$Ward, "CDS"))
}

# No voluntary leavers on Ward 4 - following up with Tim
# No Ulysses events assigned to CDS, do have one for "
#    Central Delivery Service - NNUH"
# No leavers from discharge lounge
