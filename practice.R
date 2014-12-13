f_wd <- function() { ## Working directory be�ll�t�sa a megf. mapp�ra
  setwd("C:/Users/felhasz/Coursera/02R/Practice")
}
  
f_dl <- function() { ## Adatok bet�lt�se
  dataset_url <- "http://s3.amazonaws.com/practice_assignment/diet_data.zip"
  download.file(dataset_url, "diet_data.zip")
  unzip("diet_data.zip", exdir = "diet_data")
}

f_read_loc <- function() { ## Adatokf�jlok list�ja - p�ld�ban "files"
  list.files("diet_data")
}
  
f_read_full <- function() { # Adatokf�jlok list�ja el�r�si �ttal - p�ld�ban "files_full"
  list.files("diet_data", full.names=TRUE)
}

f_conc <- function(x) { ## Adatf�jlok egyes�t�se - saj�t megold�s
  conc <- read.csv(x[1])
  for(i in 2:length(x)) {
    conc <- rbind(conc, read.csv(x[i]))
  }
  return(conc)
}

f_concb <- function(x) { ## Adatf�jlok egyes�t�se - p�ld�ban "dat"
  conc <- data.frame()
  for(i in 1:length(x)) {
    conc <- rbind(conc, read.csv(x[i]))
  }
  return(conc)
}

f_dayweight <- function(data, day) {
  median(data[which(data[, "Day"] == day), 3], na.rm = TRUE)
}

f_myscript <- function() { ## �s akkor az �sszes eddigit oldjuk meg egy scripttel
  f_wd()
  files_full <- f_read_full()
  dat <- f_concb(files_full)
  str(dat)
}