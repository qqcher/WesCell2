#' Average technical replicates into biological replicates.
#'
#' Will average technical replicates into biological replicates.
#'
#' @param df A data frame
#' @param subset1.var.name Name of the first subsetting variable (e.g. Primer). Has to match a column name in the data frame.
#' @param subset2.var.name Name of the second subsetting variable (e.g. Condition). Has to match a column name in the data frame.
#' @param subset3.var.name Name of the third subsetting variable (e.g. Sample). Has to match a column name in the data frame.
#' @param data Name of the column containing the values to average. Has to match a column name in the data frame.
#' @param data.add Additional variable you wish to carry over to the new data frame. Has to match a column name in the data frame.
#' @return A new data frame with average of technical replicates.
#' @export
#' @examples
#' \dontrun{
#' new.df <- df.3sub.unique.av(df,"Primer","inflammatory.factor","Sample","exp.data","cell.line")
#' }

df.3sub.unique.av <- function(df,subset1.var.name,subset2.var.name,subset3.var.name,data,data.add) {

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

final <- data.frame()

r <- 1

for (i in 1:length(subset1.var.list)){

  subset1 <- subset(df, df[subset1.var.name] == subset1.var.list[i])

  for (j in 1:length(subset2.var.list)) {

    subset2 <- subset(subset1, subset1[subset2.var.name] == subset2.var.list[j])

    x <- unique(subset2[subset3.var.name])

    subset3.var.list <- c()

    for (k in 1:nrow(x)){

      fill <- as.character(x[k,1])

      subset3.var.list[k] <- fill

    }

    for (l in 1:length(subset3.var.list)) {

      subset3 <- subset(subset2, subset2[subset3.var.name] == subset3.var.list[l])

      final[r,subset1.var.name] <- subset1.var.list[i]

      final[r,subset2.var.name] <- subset2.var.list[j]

      final[r,subset3.var.name] <- subset3.var.list[l]

      final[r,data.add] <- subset3[1,data.add]

      final[r,"n"] <- nrow(subset3[data])

      mean.calc <- c()

      for (m in 1:nrow(subset3[data])){

        fill <- as.numeric(subset3[m,data])

        mean.calc[m] <- fill

      }

      z <- summary(mean.calc)

      col <- paste0("average.of.",data)

      final[r,col] <- z["Mean"]

      final[r,"spread"] <- abs(z["Min."] - z["Max."])

      for (q in 1:length(mean.calc)){

        fill <- mean.calc[q]

        colname <- paste0("value#",q)

        final[r,colname] <- fill

      }

      r <- r + 1

    }

  }

}

return(final)

}

