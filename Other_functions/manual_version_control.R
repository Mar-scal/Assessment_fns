### To facilitate checking of files between Assessment_fns folders on local and Y:/Offshore/Assessment/Assessment_fns
### FK June 2022

folders <- c("Ageing", "Contour", "Fishery", "Maps", "Model", "Other_functions", "RUNME", "Survey_and_OSAC", "Survey_design", "Testing")

mar <- list.files("C:/Users/keyserf/Documents/GitHub/Mar-scal/Assessment_fns/", recursive = T)
sky <- list.files("Y:/Offshore/Assessment/Assessment_fns/", recursive = T)

fk.info <- NULL
for(i in 1:length(folders)){
  fk <- list.files(paste0("C:/Users/keyserf/Documents/GitHub/Assessment_fns/", folders[i]), recursive=T)
  for(j in 1:length(fk)){
    fk1 <- as.data.frame(file.info(paste0("C:/Users/keyserf/Documents/GitHub/Assessment_fns/", folders[i], "/", fk[j])))
    fk1 <- cbind(data.frame(file=paste0(folders[i], "/", fk[j])), fk1)
    row.names(fk1) <- NULL
    fk.info <- rbind(fk.info, fk1)
  }
}
fk.info$loc <- "fk"

ms.info <- NULL
for(i in 1:length(folders)){
  mar <- list.files(paste0("C:/Users/keyserf/Documents/GitHub/Mar-scal/Assessment_fns/", folders[i]), recursive=T)
  for(j in 1:length(mar)){
    ms1 <- as.data.frame(file.info(paste0("C:/Users/keyserf/Documents/GitHub/Mar-scal/Assessment_fns/", folders[i], "/", mar[j])))
    ms1 <- cbind(data.frame(file=paste0(folders[i], "/", mar[j])), ms1)
    row.names(ms1) <- NULL
    ms.info <- rbind(ms.info, ms1)
  }
}
ms.info$loc <- "ms"

sky.info <- NULL
for(i in 1:length(folders)){
  sky <- list.files(paste0("Y:/Offshore/Assessment/Assessment_fns/", folders[i]), recursive=T)
  for(j in 1:length(sky)){
    s1 <- as.data.frame(file.info(paste0("Y:/Offshore/Assessment/Assessment_fns/", folders[i], "/", sky[j])))
    s1 <- cbind(data.frame(file=paste0(folders[i], "/", sky[j])), s1)
    row.names(s1) <- NULL
    sky.info <- rbind(sky.info, s1)
  }
}
sky.info$loc <- "sky"

check <- NULL
for(i in 1:nrow(fk.info)){
  print(i)
  if(fk.info$file[i] %in% ms.info$file)
  {
    if(!fk.info$size[i] == ms.info[which(ms.info$file == fk.info$file[i]),]$size) {
      chk <- data.frame(file=fk.info$file[i], FKsize = fk.info$size[i], MSsize=ms.info[which(ms.info$file == fk.info$file[i]),]$size)
      check <- rbind(check, chk)
    }
  }
  if(!fk.info$file[i] %in% ms.info$file) {
    chk <- data.frame(file=fk.info$file[i], FKsize = fk.info$size[i], MSsize= NA)
    check <- rbind(check, chk)
  }
}

check

check2 <- NULL
for(i in 1:nrow(ms.info)){
  print(i)
  if(ms.info$file[i] %in% sky.info$file)
  {
    if(!ms.info$size[i] == sky.info[which(sky.info$file == ms.info$file[i]),]$size) {
      chk <- data.frame(file=ms.info$file[i], MSsize = ms.info$size[i], SKYsize=sky.info[which(sky.info$file == ms.info$file[i]),]$size)
      check2 <- rbind(check2, chk)
    }
  }
  if(!ms.info$file[i] %in% sky.info$file) {
    chk <- data.frame(file=ms.info$file[i], MSsize = ms.info$size[i], SKYsize= NA)
    check2 <- rbind(check2, chk)
  }
}

check2

check3 <- NULL
for(i in 1:nrow(sky.info)){
  print(i)
  if(sky.info$file[i] %in% ms.info$file)
  {
    if(!sky.info$size[i] == ms.info[which(ms.info$file == sky.info$file[i]),]$size) {
      chk <- data.frame(file=sky.info$file[i], SKYsize = sky.info$size[i], MSsize=ms.info[which(ms.info$file == sky.info$file[i]),]$size)
      check3 <- rbind(check3, chk)
    }
  }
  if(!sky.info$file[i] %in% ms.info$file) {
    chk <- data.frame(file=sky.info$file[i], SKYsize = sky.info$size[i], MSsize= NA)
    check3 <- rbind(check3, chk)
  }
}


write.csv(check3, "C:/Users/keyserf/Documents/check3.csv")
