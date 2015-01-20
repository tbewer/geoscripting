is.leap <- function(year) {
  if (year - 2000 == 4) {
    leap <- T
  } 
  else {
    leap <- sprintf('%s is out of the valid range', year)
  }
  return (leap)
}

is.leap(2004)
