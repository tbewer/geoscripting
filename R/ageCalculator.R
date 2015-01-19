# John Doe
# December 2014

ageCalculator <- function(x) {
  # Function to calculate age from birth year
  # x (numeric) is the year you were born
  if(!is.numeric(x)) {
    stop("x must be of class numeric")
  } else { # x is numeric
    # Get today's date
    date <- Sys.Date()
    # extract year from date and substract
    year <- as.numeric(format(date, "%Y"))
    if(year <= x) {
      stop("You aren't born yet")
    }
    age <- year - x
  }
  return(age)
}
