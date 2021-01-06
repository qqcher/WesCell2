#' Append one empty columns to an existing data frame.
#'
#' The function will prompt for the name of the new column
#'
#' @param df A data frame
#' @return A new data frame with newly generated empty columns
#' @examples
#' \dontrun{
#' new.df <- add.variable(df)
#' }

add_variable_one <- function(df) {

x <- readline(prompt="What is the name of the new variable?   ")

add.on <- data.frame(matrix(ncol = 1, nrow = nrow(df)))

colnames(add.on) <- c(x)

df <- cbind(df, add.on)

return(df)

}
