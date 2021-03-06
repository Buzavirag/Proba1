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
  
f_read_full <- function(directory) { # Adatokf�jlok list�ja el�r�si �ttal - p�ld�ban "files_full"
  list.files(directory, full.names=TRUE) ## A p�ld�ban directory = "diet_data"
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

f_conc2 <- function(x) { # Adatf�jlok egyes�t�se jobb megold�ssal :)
  sorok <- 0
  for(i in 1:length(x)) sorok <- sorok + nrow(read.csv(x[i])) # H�ny sor van �sszesen?
  oszlopok <- ncol(read.csv(x[1])) # Az oszlopok sz�ma szerencs�re azonos
  dat <- data.frame(matrix(NA, nrow = sorok, ncol = oszlopok)) # �res data.frame l�trehoz�sa
  names(dat) <- names(read.csv(x[1])) # Oszlopok elnevez�se
  sor <- 1
  kezd <- vector("numeric", length = length(x)) # A r�szt�bl�k els� sor�nak t�rol�s�ra
  for(i in 1:length(x)) { # For az egyes r�szt�bl�kra
    resztabla <- read.csv(x[i]) # R�szt�bla beolvas�sa
    kezd[i] <- sor # i-ik r�szt�bla els� sora
    for(j in 1:nrow(read.csv(x[i]))) { # For a r�szt�bla sorainak beolvas�s�ra
      dat[sor, 1] <- as.character(resztabla[j, 1]) # Valami�rt factor helyett csupa egyest emel �t...
      for(k in 2:4) dat[sor, k] <- resztabla[j, k] # A sz�mokkal viszont nincs baj
      sor <- sor +1
    }
  }
  dat[, 1] <- factor(dat[ ,1], levels = c(dat[kezd[1], 1], dat[kezd[2], 1], dat[kezd[3], 1], dat[kezd[4], 1], dat[kezd[5], 1])) # Faktor az 1. oszlopb�l
  str(dat) ## Tadamm, ugyanaz az eredm�ny, mint a manu�lisan �sszerakott
  return(dat)
}

f_conc3 <- function(x) { ## Ez meg a seg�dlet megold�sa
  tmp <- vector(mode = "list", length = length(x))
  for (i in seq_along(x)) { ## Lista 5 db. data.frame-el => gyakorlatilag a lapply()
    tmp[[i]] <- read.csv(x[[i]])
  }
  output <- do.call(rbind, tmp)
}

f_conc4 <- function(x) { ## Ez meg a seg�dlet megold�sa lapply-al
  tmp <- vector(mode = "list", length = length(x))
  tmp <- lapply(x, read.csv)
  output <- do.call(rbind, tmp)
}

f_dayweight <- function(data, day) { ## Adott napi �tlagos s�ly
  median(data[which(data[, "Day"] == day), 3], na.rm = TRUE)
  ## Az na.rm fontos, mert alap�rtelmez�sben false �rt�k, ami nem sz�ri az NA-kat!
}

f_weightmedian <- function(directory, day) { ## A p�lda megold�sa
  files_list <- list.files(directory, full.names = TRUE) #creates a list of files
  dat <- data.frame() #creates an empty data frame
  for (i in 1:5) { # loops through the files, rbinding them together
    dat <- rbind(dat, read.csv(files_list[i]))
  }
  dat_subset <- dat[which(dat[, "Day"] == day), ] #subsets the rows that match the 'day' argument
  median(dat_subset[, "Weight"], na.rm = TRUE) #identifies the median weight
  # while stripping out the NAs
}

f_myscript <- function(nap, directory) { ## �s akkor az �sszes eddigit oldjuk meg egy scripttel
  files_full <- f_read_full(directory)
  dat <- f_concb(files_full)
  daymedian <- f_dayweight(dat, nap)
  return(daymedian)
}