f_wd <- function() { ## Working directory beállítása a megf. mappára
  setwd("C:/Users/felhasz/Coursera/02R/Practice")
}
  
f_dl <- function() { ## Adatok betöltése
  dataset_url <- "http://s3.amazonaws.com/practice_assignment/diet_data.zip"
  download.file(dataset_url, "diet_data.zip")
  unzip("diet_data.zip", exdir = "diet_data")
}

f_read_loc <- function() { ## Adatokfájlok listája - példában "files"
  list.files("diet_data")
}
  
f_read_full <- function() { # Adatokfájlok listája elérési úttal - példában "files_full"
  list.files("diet_data", full.names=TRUE)
}

f_conc <- function(x) { ## Adatfájlok egyesítése - saját megoldás
  conc <- read.csv(x[1])
  for(i in 2:length(x)) {
    conc <- rbind(conc, read.csv(x[i]))
  }
  return(conc)
}

f_concb <- function(x) { ## Adatfájlok egyesítése - példában "dat"
  conc <- data.frame()
  for(i in 1:length(x)) {
    conc <- rbind(conc, read.csv(x[i]))
  }
  return(conc)
}

f_dayweight <- function(data, day) {
  median(data[which(data[, "Day"] == day), 3], na.rm = TRUE)
}

f_myscript <- function() { ## És akkor az összes eddigit oldjuk meg egy scripttel
  f_wd()
  files_full <- f_read_full()
  dat <- f_concb(files_full)
  str(dat)
}