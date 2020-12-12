#' Remove rows in a data frame.
#'
#' Will remove rows in a data frame when either one variable matches a value in said row or when said row matches two separate values.
#' Easy method to remove replicate associated with a specif Primer and/or Sample # in a master file.
#' The function only asks for a data frame, the function will then prompt for additional questions.
#'
#' @param df A data frame
#' @return A new data frame without the dropped rows
#' @examples
#' \dontrun{
#' new.df <- clean.data(df)
#' }
#' @import stats

clean.data <- function(df) {

x <- readline(prompt="Do you want do delete all the data associated with one variable? (i.e. sample) Yes/No  ")

if (x == "Yes") {

  xx <- readline(prompt="What is the name of the column containing said variable? (i.e. Sample #)   ")

  xxx <- readline(prompt="What is the value in to column entered above whose associated data needs to be dropped? (i.e. 34)   ")

  y <- paste("all the data associated with", xx, xxx, "will be dropped, confirm? Yes/No   ")

  yy <- readline(prompt=y)

  if (yy == "Yes") {

    for (i in 1:nrow(df)) {

      if (df[i,xx] == xxx) {

        df[i,xx] <- NA

      }

    }

    df <- stats::na.omit(df)

    return(df)

  } else {

    return(print("function aborted"))

  }

  rm(xx, xxx, y, yy, i)

}

if (x == "No") {

  x <- readline(prompt="Do you want do delete all the data associated with two variable at once? (i.e. sample) Yes/No  ")

  if (x == "Yes") {

    xx <- readline(prompt="What is the name of the column containing the first variable? (i.e. Sample #)   ")

    n.xx <- readline(prompt="What is the value in to column entered above whose associated data needs to be dropped? (i.e. 34)   ")

    xxx <- readline(prompt="What is the name of the column containing the second variable? (i.e. Primer)   ")

    n.xxx <- readline(prompt="What is the value in to column entered above whose associated data needs to be dropped? (i.e. TNF)   ")

    y <- paste("all the data associated with", xx, n.xx, "and", xxx, n.xxx, "will be dropped, confirm? Yes/No   ")

    yy <- readline(prompt=y)

    if (yy == "Yes") {

      for (i in 1:nrow(df)) {

        if (df[i,xx] == n.xx & df[i,xxx] == n.xxx) {

          df[i,xx] <- NA

          df[i,xxx] <- NA

        }

      }

      df <- stats::na.omit(df)

      return(df)

    } else {

      return(print("function aborted"))

    }

  } else {return("this function can only drop data associate with ONE or TWO variable(s), function aborted")}

}

rm(xx, xxx, n.xx, n.xxx, y, yy, i)

}
