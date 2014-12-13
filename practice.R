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
  
f_read_full <- function(directory) { # Adatokfájlok listája elérési úttal - példában "files_full"
  list.files(directory, full.names=TRUE) ## A példában directory = "diet_data"
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

f_conc2 <- function(x) { # Adatfájlok egyesítése jobb megoldással :)
  sorok <- 0
  for(i in 1:length(x)) sorok <- sorok + nrow(read.csv(x[i])) # Hány sor van összesen?
  oszlopok <- ncol(read.csv(x[1])) # Az oszlopok száma szerencsére azonos
  dat <- data.frame(matrix(NA, nrow = sorok, ncol = oszlopok)) # Üres data.frame létrehozása
  names(dat) <- names(read.csv(x[1])) # Oszlopok elnevezése
  sor <- 1
  kezd <- vector("numeric", length = length(x)) # A résztáblák elsõ sorának tárolására
  for(i in 1:length(x)) { # For az egyes résztáblákra
    resztabla <- read.csv(x[i]) # Résztábla beolvasása
    kezd[i] <- sor # i-ik résztábla elsõ sora
    for(j in 1:nrow(read.csv(x[i]))) { # For a résztábla sorainak beolvasására
      dat[sor, 1] <- as.character(resztabla[j, 1]) # Valamiért factor helyett csupa egyest emel át...
      for(k in 2:4) dat[sor, k] <- resztabla[j, k] # A számokkal viszont nincs baj
      sor <- sor +1
    }
  }
  dat[, 1] <- factor(dat[ ,1], levels = c(dat[kezd[1], 1], dat[kezd[2], 1], dat[kezd[3], 1], dat[kezd[4], 1], dat[kezd[5], 1])) # Faktor az 1. oszlopból
  str(dat) ## Tadamm, ugyanaz az eredmény, mint a manuálisan összerakott
  return(dat)
}

f_conc3 <- function(x) { ## Ez meg a segédlet megoldása
  tmp <- vector(mode = "list", length = length(x))
  for (i in seq_along(x)) { ## Lista 5 db. data.frame-el => gyakorlatilag a lapply()
    tmp[[i]] <- read.csv(x[[i]])
  }
  output <- do.call(rbind, tmp)
}

f_conc4 <- function(x) { ## Ez meg a segédlet megoldása lapply-al
  tmp <- vector(mode = "list", length = length(x))
  tmp <- lapply(x, read.csv)
  output <- do.call(rbind, tmp)
}

f_dayweight <- function(data, day) { ## Adott napi átlagos súly
  median(data[which(data[, "Day"] == day), 3], na.rm = TRUE)
  ## Az na.rm fontos, mert alapértelmezésben false érték, ami nem szûri az NA-kat!
}

f_weightmedian <- function(directory, day) { ## A példa megoldása
  files_list <- list.files(directory, full.names = TRUE) #creates a list of files
  dat <- data.frame() #creates an empty data frame
  for (i in 1:5) { # loops through the files, rbinding them together
    dat <- rbind(dat, read.csv(files_list[i]))
  }
  dat_subset <- dat[which(dat[, "Day"] == day), ] #subsets the rows that match the 'day' argument
  median(dat_subset[, "Weight"], na.rm = TRUE) #identifies the median weight
  # while stripping out the NAs
}

f_myscript <- function(nap, directory) { ## És akkor az összes eddigit oldjuk meg egy scripttel
  files_full <- f_read_full(directory)
  dat <- f_concb(files_full)
  daymedian <- f_dayweight(dat, nap)
  return(daymedian)
}