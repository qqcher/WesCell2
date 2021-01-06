#' Append empty columns to an existing data frame.
#'
#' @param df A data frame
#' @param l A list of the empty columns to be appended
#' @return A new data frame with newly generated empty columns
#' @export
#' @examples
#' \dontrun{
#' new.df <- add.variable.bulk(df,c("cell.line","cell.replicate","date.performed"))
#' }

add.variable.bulk <- function(df,l) {

  add.on <- data.frame(matrix(ncol = length(l), nrow = nrow(df)))

  colnames(add.on) <- l

  df <- cbind(df, add.on)

  return(df)

}
