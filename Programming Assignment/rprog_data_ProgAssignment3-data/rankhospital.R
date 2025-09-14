rankhospital <- function(state, outcome, num = "best") {
  ## Read data
  df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Validate state
  if (!state %in% unique(df$State)) stop("invalid state")
  
  ## Map outcome -> exact column name
  outcome_map <- c(
    "heart attack"  = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
    "heart failure" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
    "pneumonia"     = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  )
  key <- tolower(outcome)
  if (!key %in% names(outcome_map)) stop("invalid outcome")
  col <- outcome_map[[key]]
  
  ## Keep only hospitals in the requested state
  sub <- df[df$State == state, c("Hospital.Name", col)]
  names(sub) <- c("hospital", "rate")
  
  ## Convert to numeric and drop missing outcome rows
  suppressWarnings(sub$rate <- as.numeric(sub$rate))
  sub <- sub[!is.na(sub$rate), , drop = FALSE]
  if (nrow(sub) == 0L) return(NA_character_)  # no data for that outcome in this state
  
  ## Order by lowest rate, tie-break by hospital name (alphabetical)
  ranked <- sub[order(sub$rate, sub$hospital), , drop = FALSE]
  
  ## Resolve the requested rank
  if (identical(num, "best")) {
    idx <- 1L
  } else if (identical(num, "worst")) {
    idx <- nrow(ranked)
  } else if (is.numeric(num)) {
    if (num < 1L || num > nrow(ranked)) return(NA_character_)
    idx <- as.integer(num)
  } else {
    ## optional guard; the assignment doesn't require this, but it's safer
    stop("invalid num")
  }
  
  ranked$hospital[idx]
}
