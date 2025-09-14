rankall <- function(outcome, num = "best") {
  ## Read data
  df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Map friendly outcome -> column name
  outcome_map <- c(
    "heart attack"  = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
    "heart failure" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
    "pneumonia"     = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  )
  key <- tolower(outcome)
  if (!key %in% names(outcome_map)) stop("invalid outcome")
  col <- outcome_map[[key]]
  
  ## States must be returned in alphabetical order
  states <- sort(unique(df$State))
  
  pick_for_state <- function(st) {
    sub <- df[df$State == st, c("Hospital.Name", col)]
    names(sub) <- c("hospital", "rate")
    suppressWarnings(sub$rate <- as.numeric(sub$rate))
    sub <- sub[!is.na(sub$rate), , drop = FALSE]
    if (nrow(sub) == 0L) return(NA_character_)
    
    ## Order by outcome ascending, then hospital name (tie-break)
    ranked <- sub[order(sub$rate, sub$hospital), , drop = FALSE]
    
    ## Resolve num
    if (identical(num, "best")) {
      idx <- 1L
    } else if (identical(num, "worst")) {
      idx <- nrow(ranked)
    } else if (is.numeric(num)) {
      if (num > nrow(ranked)) return(NA_character_)
      idx <- as.integer(num)
    } else {
      stop("invalid num")
    }
    
    ranked$hospital[idx]
  }
  
  hospitals <- sapply(states, pick_for_state, USE.NAMES = FALSE)
  
  data.frame(
    hospital = hospitals,
    state    = states,
    row.names = states,
    stringsAsFactors = FALSE
  )
}

