readcolnames <- function() {
library(stringr)
txtlines <- readLines("german.doc.txt")
cn <- 0
attnames <- c()
tablecolnames <-c()
for (i in 1:length(txtlines)) {
  line0 <- txtlines[i]
  attline = grep("Attribute [0-9]", line0)
  if (length(attline)) {
    cn = cn + 1
    attnames[cn] <- str_trim(txtlines[i+1]) 
    words <- strsplit(attnames[cn], ' ')
    acolname <- c()
    iword <- 0
    if (length(words[[1]]) == 1) {
      tablecolnames[cn] <- words[[1]]
    }
    else{
      for (j in 1:length(words[[1]])) {
        word1 = words[[1]][j]
        if (str_length(word1) >= 2) {
          iword = iword + 1
          acolname[iword] <- str_to_upper(str_sub(word1,1, 1))
        }
      }
      tablecolnames[cn] <- paste(acolname, collapse = "")
    }
  }
}
return(list(attnames, tablecolnames))
}

