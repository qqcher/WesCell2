#' Perform a serie of Wilcoxon test based on two subsetting variable.
#'
#' A time saver when working with  large number of Primer and experimental condition.
#'
#' @param df A data frame
#' @param subset1.var.name Name of the first subsetting variable (e.g. Primer). Has to match a column name in the data frame.
#' @param subset2.var.name Name of the second subsetting variable (e.g. Condition). Has to match a column name in the data frame.
#' @param t.test.var1 Name of the column containing the data. Has to match a column name in the data frame.
#' @param t.test.var2 Name of the grouping variable for the Wilcoxon test. Has to match a column name in the data frame.
#' @return A new data frame with a list of p-value for all grouping combinations.
#' @export
#' @examples
#' \dontrun{
#' new.df <- df.wilcoxon.test(df,"Primer","inflammatory.factor","exp.data","cell.line")
#' }
#' #' @import stats

df.wilcoxon.test <- function(df,subset1.var.name,subset2.var.name,t.test.var1,t.test.var2) {

  x <- unique(df[subset1.var.name])

  subset1.var.list <- c()

  for (i in 1:nrow(x)){

    fill <- as.character(x[i,1])

    subset1.var.list[i] <- fill

  }

  x <- unique(df[subset2.var.name])

  subset2.var.list <- c()

  for (i in 1:nrow(x)){

    fill <- as.character(x[i,1])

    subset2.var.list[i] <- fill

  }

  final <- data.frame(matrix(ncol = 5, nrow = 1))

  colnames(final) <- c("data.used", "test.by", "subset.var.1", "subset.var.2", "p.value")

  k <- 1

  for (i in 1:length(subset1.var.list)){

    subset1 <- subset(df, df[subset1.var.name] == subset1.var.list[i])

    for (j in 1:length(subset2.var.list)) {

      subset2 <- subset(subset1, subset1[subset2.var.name] == subset2.var.list[j])

      result <- stats::wilcox.test(subset2[[t.test.var1]]~subset2[[t.test.var2]], paired = F, exact = F)

      p <- result$p.value

      final[k,"data.used"] <-  t.test.var1

      final[k,"test.by"] <- t.test.var2

      final[k,"subset.var.1"] <- subset1.var.list[i]

      final[k,"subset.var.2"] <- subset2.var.list[j]

      final[k,"p.value"] <- p

      k <- k + 1

    }

  }

  return(final)

}


