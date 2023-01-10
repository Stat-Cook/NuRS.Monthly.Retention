#' Mappings to Allocate Ward names
#' Tag:Lookup
#' Tag:Allocate
#' Tag:Ulysses
#' Tag:ESR 
#' Tag:PAS

ulys_to_allocate_list <- list(
  'Charnwood Suite' = 'Charnwood',
  'Ward 5' = 'Ward 5',
  'Ward 15' = 'Ward 15',
  'Ward 1' = 'Ward 1',
  "Acute Cardiac Unit" = 'ACU',
  
  "ZZZZ OLD Ward 9 (Pre 27.07.20)" = 'Ward 9',
  "Ward 9" = 'Ward 9',
  
  "ZZZZ OLD Ward 6 DO NOT USE" = 'Ward 6', 
  "Ward 6" = 'Ward 6',
  
  "Ward 22 (Dec 17)" = 'Ward 22',
  "ZZZZ OLD Ward 22  (DO NOT USE)" = 'Ward 22',
  
  "ZZZZ OLD Ward 12 (DO NOT USE)" = 'Ward 12', 
  "Ward 12" = 'Ward 12',
  "Ward 12 - Infusion Unit" = 'Ward 12',
  
  "Discharge Lounge"= "Discharge Lounge",
  "Discharge Planning Team"= "Discharge Lounge",
  "Discharge Hub" = "Discharge Lounge",
  
  "Ward 16 (Short Stay)" = "Ward 16 (Short Stay)",
  
  "Ward 3" = 'Ward 3',
  "Ward 3 (Apr 17)" = 'Ward 3',
  
  'Ward 2' = 'Ward 2',
  
  "Ward 10b" = 'Ward 10',
  "Ward 10" = 'Ward 10',
  "Ward 10 - Adolescent Unit" = 'Ward 10',
  
  'Ward 18' = 'Ward 18',
  "ITU / HDU"  = 'ICU/HDU',
  "Maternity" = "Ward 11",
  
  "ZZZZ OLD Ward 4 (DO NOT USE)" = 'Ward 4',
  "Ward 4 - Surgery" = 'Ward 4',
  "Ward 4" = 'Ward 4',
  
  "ZZZZ OLD Ward 7 (DO NOT USE)" = "Ward 7",
  "ZZZZ OLD Ward 7 Dec 17 (DO NOT USE)"  = "Ward 7",
  "Ward 7" = "Ward 7",
  
  "Ward 17" = "Ward 17",  
  "ZZZZ OLD Ward 7 Dec 17 (DO NOT USE)"  = "Ward 17"
)

esr_to_allocate_list <- list(
  '177 Charnwood Level 5' = 'Charnwood',
  '177 Ward 5 - Male Surgical Level 5' = 'Ward 5',
  '177 Ward 15 Level 5' = 'Ward 15',
  '177 Ward 1 Level 5' = 'Ward 1',
  '177 Acute Cardiac Unit Level 5' = 'ACU',
  '177 Ward 9 - Surgery Level 5' = 'Ward 9',
  '177 Ward 6 - Orthopaedic Trauma Level 5' = 'Ward 6',
  '177 Ward 22 - Orthopaedic Elective Level 5' = 'Ward 22',
  '177 Ward 12 Emergency Level 5' = 'Ward 12',
  "177 Discharge Lounge Level 5" = "Discharge Lounge",
  '177 Ward 16 (SSMU) Level 5' = 'Ward 16 (Short Stay)',
  '177 Ward 3 Level 5' = 'Ward 3',
  '177 Ward 2 Level 5' = 'Ward 2',
  '177 Ward 10 - Paediatrics Level 5' = 'Ward 10',
  '177 Ward 18 Level 5' = 'Ward 18',
  '177 Intensive Care Unit Level 5' = 'ICU/HDU',
  '177 Ward 11 - Maternity Level 5' = 'Ward 11',
  "177 Ward 4 - Medicine" = 'Ward 4',
  '177 Central Delivery Suite Level 5' = 'CDS',
  "177 Ward 7 - Orthopaedic Elective Level 5" = "Ward 7",
  "177 Ward 7 Respiratory Level 5"  = "Ward 7",
  "177 Ward 17 Day Clinic Level 5" = "Ward 17",
  "177 Ward 17 Level 5"= "Ward 17"
)


pas_to_allocate_list <- list(
  'JPH Charnwood Suite' = 'Charnwood',
  'JPH Ward 5' = 'Ward 5',
  'JPH Ward 15' = 'Ward 15',
  'JPH Ward 1 Stroke Unit' = 'Ward 1',
  'JPH ACU' = 'ACU',
  'JPH Ward 9' = 'Ward 9',
  'JPH Ward 6' = 'Ward 6',
  'JPH Ward 22' = 'Ward 22',
  
  'JPH Ward 12' = 'Ward 12',
  'JPH Ward 12 Infusion Service' = 'Ward 12',
  
  "JPH Discharge Lounge" = "Discharge Lounge",
  'JPH Ward 16' = 'Ward 16 (Short Stay)',
  'JPH Ward 16 (Short Stay)' = 'Ward 16 (Short Stay)',
  'JPH Ward 16 (Cohort)' = 'Ward 16 (Short Stay)',
  "JPH Short Stay Medical Unit (Ward3)" = 'Ward 3',
  "JPH Ward 3" = 'Ward 3',
  'JPH Ward 2' = 'Ward 2',
  
  "JPH Ward 10 CHD" = 'Ward 10',
  "JPH Ward 10" = 'Ward 10',
  "JPH Ward 10B" = 'Ward 10',
  "JPH Ward 10 PAU" = 'Ward 10',
  
  'JPH Ward 18' = 'Ward 18',
  
  'JPH ICU' = 'ICU/HDU',
  'JPH HDU' = 'ICU/HDU',
  
  "JPH Ward 11" = 'Ward 11',
  "JPH Ward 11 Transitional Care"= 'Ward 11',
  
  "JPH Ward 4" = 'Ward 4',
  "JPH Central Delivery Suite" = 'CDS',
  "JPH Ward 7" = "Ward 7",
  "JPH Ward 17" = "Ward 17"
)


list_lookup <- function(i, .list){
  #' Map a value to an entity in a list (returns "Unknown" is missing)
  #' @param i value to be mapped
  #' @param .list Lookup list
  
  if (i %in% names(.list)){
    return(.list[[i]])
  }
  return("Unknown")
}

esr_to_allocate <- function(values){
  #' Map ESR locations to Allocate Wards
  #' 
  #' @param values A vector of ESR locations
  f <- function(i) list_lookup(i, esr_to_allocate_list)
  
  sapply(
    values, 
    f
  )
}

pas_to_allocate <- function(values){
  #' Map PAS locations to Allocate Wards
  #' 
  #' @param values A vector of PAS locations
  f <- function(i) list_lookup(i, pas_to_allocate_list)
  
  sapply(
    values, 
    f
  )
}

ulys_to_allocate <- function(values){
  #' Map Ulysses locations to Allocate Wards
  #' 
  #' @param values A vector of Ulysses locations
  f <- function(i) list_lookup(i, ulys_to_allocate_list)
  
  sapply(
    values, 
    f
  )
}