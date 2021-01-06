#' analyze.qPCR()
#'
#' Will analyze a raw RT-qPCR .csv output using the standard 2^(−ddCt) Method, a classict calculation of the fold change.
#' There is not input needed but once started the function will prompt for information about the plate map used.
#' The .csv file used need to have column names on the fist row and the associated raw data in the subsequent rows.
#' Designed for 96 wells plates, will work for other plate size depending on raw file format.
#'
#' @return A list with the plate map at each step of the analysis process.
#' @export
#' @examples
#' \dontrun{
#' result <- analyze.qPCR()
#' }
#' @import utils


analyze.qPCR <- function() {

# Get the list of samples

splnbr <- readline(prompt="Enter how many samples you have on this plate:   ")

firstspl <- readline(prompt="Enter the first sample's name:   ")

spl <- c(paste(firstspl))

for (i in 2:splnbr){

  ispl <- readline(prompt="Enter the next sample's name:   ")

  spl[i] <- ispl

}

rm(i, splnbr, firstspl, ispl)

# Get the list of primers

prmnbr <- readline(prompt="Enter how many primers you have on this plate:   ")

firstprm <- readline(prompt="Enter the first primer's name:   ")

prm <- c(paste(firstprm))

for (i in 2:prmnbr){

  iprm <- readline(prompt="Enter the next primer's name:   ")

  prm[i] <- iprm

}

rm(i, prmnbr, firstprm, iprm)

# Find the duplicate orientation

x <- readline(prompt="Are the duplicates within the rows (A1/A2)? Yes/No   ")

if (x == "Yes") {

  dup.orientatio <- "R"

} else {

  x <- readline(prompt="Are the duplicates within the columns (A1/B1)? Yes/No   ")

  if (x == "Yes") {

    dup.orientatio <- "C"

  } else {

    print("This function needs duplicate next to each others either within rows or columns, the question will be asked once again before the function stops")

    x <- readline(prompt="Are the duplicates within the rows (A1/A2)? Yes/No"   )

    if (x == "Yes") {

      dup.orientatio <- "R"

    } else {

      x <- readline(prompt="Are the duplicates within the columns (A1/B1)? Yes/No"   )

      if (x == "Yes") {

        dup.orientatio <- "C"

      } else {

        return(print("wrong input, qPCR analysis failed"))

      }

    }

  }

}


# Find the orientation of the plate (sample/primer

x <- readline(prompt="Are the samples organised along the rows (A/B/C...etc - print R) or along the columns (1/2/3..etc- print C)?   ")

if (x == "R") {

  row.col <- "samp.prim"

} else {

  if (x == "C") {

    row.col <- "prim.samp"

  } else {

    print("Please enter either 'Rows' or 'Columns', the question will be asked once again before the function stops")

    x <- readline(prompt="Are the samples organised along the rows (A/B/C...etc) or along the columns (1/2/3..etc)? Rows/Columns"   )

    if (x == "R") {

      row.col <- "samp.prim"

    } else {

      if (x == "C") {

        x <- readline(prompt="Are the duplicates on the columns (A1/B1)? Yes/No"   )

      } else {

        return(print("wrong input, qPCR analysis failed"))

      }

    }

  }

}

rm(x)

# Create the empty custom plate

if (row.col == "samp.prim"){

  if (dup.orientatio == "R"){

    r <- length(spl)

    c <- length(prm) * 2

  } else {

    if (dup.orientatio == "C"){

      r <- length(spl) * 2

      c <- length(prm)

    }

  }

}

if (row.col == "prim.samp"){

  if (dup.orientatio == "R"){

    r <- length(prm)

    c <- length(spl) * 2

  } else {

    if (dup.orientatio == "C"){

      r <- length(prm) * 2

      c <- length(spl)

    }

  }

}

template <- data.frame(matrix(ncol = c, nrow = r))

if (row.col == "samp.prim" & dup.orientatio == "R"){

  rownames(template) <- spl

  for (i in 1:length(prm)){

    colnames(template)[i*2] <- prm[i]

    colnames(template)[i*2-1] <- prm[i]

  }

}

if (row.col == "prim.samp" & dup.orientatio == "R"){

  rownames(template) <- prm

  for (i in 1:length(spl)){

    colnames(template)[i*2] <- spl[i]

    colnames(template)[i*2-1] <- spl[i]

  }

}

if (row.col == "samp.prim" & dup.orientatio == "C"){

  colnames(template) <- prm

  for (i in 1:length(spl)){

    rownames(template)[i*2] <- paste(spl[i],".rep", sep = "")

    rownames(template)[i*2-1] <- spl[i]

  }

}

if (row.col == "prim.samp" & dup.orientatio == "C"){

  colnames(template) <- spl

  for (i in 1:length(prm)){

    rownames(template)[i*2] <- paste(prm[i],".rep", sep = "")

    rownames(template)[i*2-1] <- prm[i]

  }

}

rm(c,i,r)

# Fill the empty custom plate (96w only)

wn <- readline(prompt="What is the name of the column including the well numbers on the raw data file?   ")

var <- readline(prompt="What is the name of the column including the Cq values on the raw data file?   ")

rm <- readline(prompt="You will now need to select a .csv file containing your qPCR data, type anything to continue   ")

rm(rm)

file.path <- file.choose()

xx <- utils::read.csv(file.path)

blankplate1 <- function() {
  rnme <- c("A","B","C","D","E","F","G","H")
  cnme <- c("01","02","03","04","05","06","07","08","09","10","11","12")
  nstemplate <- data.frame(matrix(ncol = 12, nrow = 8))
  colnames(nstemplate) <- cnme
  rownames(nstemplate) <- rnme
  return(nstemplate)
}

file.to.plate <- function(x,well.name,ratio.marker.1) {

  template <- blankplate1()

  cnme <- c("Well.rw","Well.cl")
  nstemplate <- data.frame(matrix(ncol = 2, nrow = (nrow(x))))
  colnames(nstemplate) <- cnme

  nstemplate[, "Well.rw"] <- substr(x[, well.name],1,1)

  nstemplate[, "Well.cl"] <- substr(x[, well.name],2,3)

  for (i in 1:nrow(x)){
    a <- nstemplate[i, paste("Well.rw")]
    b <- nstemplate[i, paste("Well.cl")]
    c <- round(x[i, ratio.marker.1],digits = 3)
    template[paste(a),paste(b)] <- c
  }

  return(template)

}

input.plate <- file.to.plate(xx, wn, var)

rm(xx, file.path, var, wn, blankplate1, file.to.plate)

for (i in 1:nrow(template)){

  for (j in 1:ncol(template)){

    template[i,j] <- input.plate[i,j]

  }

}

rm(i, j, input.plate)

# Average technical duplicates

if (row.col == "prim.samp" & dup.orientatio == "R"){

  l <- ncol(template) / 2

  avCt.plate <- data.frame(matrix(ncol = l, nrow = nrow(template)))
  colnames(avCt.plate) <- spl
  rownames(avCt.plate) <- prm

  for (i in 1:nrow(template)){

    for (j in 1:l){

      jj <- j * 2

      jjj <- j * 2 -1

      avCt.plate[i,j] <- mean(c(template[i,jj],template[i,jjj]), na.rm = T)

    }

  }

  rm(i, j, jj, jjj, l)

}

if (row.col == "samp.prim" & dup.orientatio == "R"){

  l <- ncol(template) / 2

  avCt.plate <- data.frame(matrix(ncol = l, nrow = nrow(template)))
  colnames(avCt.plate) <- prm
  rownames(avCt.plate) <- spl

  for (i in 1:nrow(template)){

    for (j in 1:l){

      jj <- j * 2

      jjj <- j * 2 -1

      avCt.plate[i,j] <- mean(c(template[i,jj],template[i,jjj]), na.rm = T)

    }

  }

  rm(i, j, jj, jjj, l)

}

if (row.col == "prim.samp" & dup.orientatio == "C"){

  l <- nrow(template) / 2

  avCt.plate <- data.frame(matrix(ncol = ncol(template), nrow = l))
  colnames(avCt.plate) <- spl
  rownames(avCt.plate) <- prm

  for (i in 1:ncol(template)){

    for (j in 1:l){

      jj <- j * 2

      jjj <- j * 2 -1

      avCt.plate[j,i] <- mean(c(template[jj,i],template[jjj,i]), na.rm = T)

    }

  }

  rm(i, j, jj, jjj, l)

}

if (row.col == "samp.prim" & dup.orientatio == "C"){

  l <- nrow(template) / 2

  avCt.plate <- data.frame(matrix(ncol = ncol(template), nrow = l))
  colnames(avCt.plate) <- prm
  rownames(avCt.plate) <- spl

  for (i in 1:ncol(template)){

    for (j in 1:l){

      jj <- j * 2

      jjj <- j * 2 -1

      avCt.plate[j,i] <- mean(c(template[jj,i],template[jjj,i]), na.rm = T)

    }

  }

  rm(i, j, jj, jjj, l)

}

# Calculate dCt (d to the housekeeping gene)

hk <- readline(prompt="Which primer is your reference gene?   ")

dCt.plate <- data.frame(matrix(ncol = ncol(avCt.plate), nrow = nrow(avCt.plate)))

if (row.col == "prim.samp"){

  colnames(dCt.plate) <- spl
  rownames(dCt.plate) <- prm

  hk.gene <- avCt.plate[hk,]

  for (i in 1:nrow(avCt.plate)){

    for (j in 1:ncol(avCt.plate)){

      dCt.plate[i,j] <- ( avCt.plate[i,j] - hk.gene[,j] )

    }
  }

  rm(i, j, hk.gene)

}

if (row.col == "samp.prim"){

  colnames(dCt.plate) <- prm
  rownames(dCt.plate) <- spl

  hk.gene <- avCt.plate[,hk]

  for (i in 1:ncol(avCt.plate)){

    for (j in 1:nrow(avCt.plate)){

      dCt.plate[j,i] <- ( avCt.plate[j,i] - hk.gene[j] )

    }
  }

  rm(i, j, hk.gene)

}

# Calculate ddCt (d to the reference sample)

ctrl <- readline(prompt="Which sample is your control?   ")

ddCt.plate <- data.frame(matrix(ncol = ncol(avCt.plate), nrow = nrow(avCt.plate)))

if (row.col == "prim.samp"){

  colnames(ddCt.plate) <- spl
  rownames(ddCt.plate) <- prm

  ctrl.spl <- dCt.plate[,ctrl]

  for (i in 1:ncol(dCt.plate)){

    for (j in 1:nrow(dCt.plate)){

      ddCt.plate[j,i] <- ( dCt.plate[j,i] - ctrl.spl[j] )

    }
  }

  rm(i, j, ctrl.spl)

}

if (row.col == "samp.prim"){

  colnames(ddCt.plate) <- prm
  rownames(ddCt.plate) <- spl

  ctrl.spl <- dCt.plate[ctrl,]

  for (i in 1:nrow(dCt.plate)){

    for (j in 1:ncol(dCt.plate)){

      ddCt.plate[i,j] <- ( dCt.plate[i,j] - ctrl.spl[,j] )

    }
  }

  rm(i, j, ctrl.spl)

}

# Calculate the exponential of ddCt

exp.plate <- ddCt.plate

for (i in 1:ncol(ddCt.plate)){

  for (j in 1:nrow(ddCt.plate)){

    exp.plate[j,i] <- round((2^(-(ddCt.plate[j,i]))),3)

  }
}

rm (i, j)

datalist = list()

datalist[[1]] <- template

datalist[[2]] <- avCt.plate

datalist[[3]] <- dCt.plate

datalist[[4]] <- ddCt.plate

datalist[[5]] <- exp.plate

names(datalist) <- c("full.data","Ct.data","dCt.data","ddCt.data","exp.data")

return(datalist)

}
