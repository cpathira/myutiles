

#' Title
#'
#' @param x a numeric
#' @param na.rm something
#'
#' @return
#' @export
#'
#' @examples
numeric_summary <- function(x, na.rm=TRUE){

  min = min(x, na.rm=na.rm)
  max = max(x, na.rm=na.rm)
  mean = mean(x, na.rm=na.rm)
  sd = sd(x, na.rm=na.rm)
  length = length(x)
  Nmiss = sum(is.na(x))

  c(min=min, max=max, mean=mean, sd=sd, length=length, Nmiss=Nmiss)

}


#' This function creates summary of character vector
#'
#' @param  x a character vector
#' @param  na.rm an optional logical parameter for TRUE by default
#' @return A named character with six values
#' @export
#'
#' @examples
#' \dontrun{
#'  y<-c("MA","IL","ME","CA")
#'  char_summary(y)
#'  }
char_summary <- function(x, na.rm=TRUE){

  length = length(x)
  Nmiss = sum(is.na(x))
  Nunique = length(unique(x))

  c(length = length,
    Nmiss = Nmiss,
    Nunique = Nunique )

}
