best <- function(state, outcome) {
  ## --- Read outcome data as character to control conversion ---
  df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## --- Validate state ---
  if (!state %in% unique(df$State)) stop("invalid state")
  
  ## --- Map friendly outcome names -> actual column names in the file ---
  outcome_map <- c(
    "heart attack"  = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
    "heart failure" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
    "pneumonia"     = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  )
  
  key <- tolower(outcome)
  if (!key %in% names(outcome_map)) stop("invalid outcome")
  col <- outcome_map[[key]]
  
  ## --- Filter to the requested state and keep only hospital + outcome columns ---
  sub <- df[df$State == state, c("Hospital.Name", col)]
  names(sub) <- c("hospital", "rate")
  
  ## --- Convert "rate" to numeric; drop rows without data ("Not Available" -> NA) ---
  suppressWarnings(sub$rate <- as.numeric(sub$rate))
  sub <- sub[!is.na(sub$rate), , drop = FALSE]
  
  ## --- If no data after filtering, return NA_character_ (rare, but safe) ---
  if (nrow(sub) == 0L) return(NA_character_)
  
  ## --- Rank by lowest mortality; break ties alphabetically by hospital name ---
  ord <- order(sub$rate, sub$hospital)  # primary: smaller rate; secondary: A→Z hospital
  sub$hospital[ord][1]
}
