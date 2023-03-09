# Tag:Inclusion
# Tag:Exclusion

inc_exc_criteria <- function(data) {
  #' Apply exclusion criteria to data set
  #'
  #' @param data A data frame with columns 'Year' and 'Ward'
  #'
  #' @export
  Year <- Month <- Ward <- NULL
  data %>%
    filter(Year < 2021) %>%
    filter(Year < 2020 | Month < 6) %>%
    filter(Year > 2015) %>%
    filter(Year > 2016 | Month >= 8) %>%
    filter(!str_detect(Ward, "Ward 4")) %>%
    filter(!str_detect(Ward, "Discharge")) %>%
    filter(!str_detect(Ward, "CDS"))
}

# No voluntary leavers on Ward 4 - following up with Tim
# No Ulysses events assigned to CDS, do have one for "
#    Central Delivery Service - NNUH"
# No leavers from discharge lounge
