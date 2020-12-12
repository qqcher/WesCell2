#' An intuitive way to add missing information to analysed values.
#'
#' Will fill a variable in a data frame when either one secondary variable matches a value in said row or when said row matches two separate secondary values.
#' The function only asks for a data frame, the function will then prompt for additional questions.
#'
#' @param df A data frame
#' @return A new data frame with the newly filled-in information.
#' @examples
#' \dontrun{
#' new.df <- fill.variable(df)
#' }

fill.variable <- function(df) {

var <- readline(prompt="Which variable are you trying to fill (i.e. cell.line)?    ")

prpt <- paste("Would you like to fill",var,"with the same data for the entire data frame? Yes/No   ")

yn <- readline(prompt=prpt)

if (yn == "Yes") {

  prpt <- paste("What value would you like to fill",var,"with?   ")

  print <- readline(prompt=prpt)

  for (i in 1:nrow(df)) {

    df[i,var] <- print

  }

  return(df)

}

prpt <- paste("Would you like to fill",var,"only on rows matching a specific secondary variable? Yes/No   ")

yn <- readline(prompt=prpt)

if (yn == "Yes") {

  prpt <- paste("What is the name of the secondary variable you will use to fill", var, "with more specificity?   ")

  var.2 <- readline(prompt=prpt)

  prpt <- paste("What is the value of", var.2, "you want to match to specifically fill", var, "?   ")

  var.2.val <- readline(prompt=prpt)

  prpt <- paste("What is the value you want", var, "filled with when", var.2, "match", var.2.val, "?   ")

  var.val <- readline(prompt=prpt)

  for (i in 1:nrow(df)) {

    if (df[i,var.2] == var.2.val)

    df[i,var] <- var.val

  }

  return(df)

}

return(print("Something went wrong, please check that your input is correct"))

}
