#' Reads in carcass length data
#' @description Reads in and formats carcass length data for use in get_length_samples. 
#' @param dir The address of the directory where the carcass length data is stored.
#' @author Matt Tyres and Jordy Bernard
#' @export

prepare_length_data <- function(dir){
  lengthdata <- read.csv(dir, header=T)
  lengthdata$Sex[lengthdata$Sex %in% c("--","UNK","unknown")] <- NA
  lengthdata$Sex <- as.factor(as.character(lengthdata$Sex))
  lengthdata$Length <- suppressWarnings(as.numeric(as.character(lengthdata$Length)))
  lengthdata$Length[lengthdata$Length<200] <- NA
  lengthdata$sexfac <- as.numeric(as.factor(lengthdata$Sex))
  lengthdata$yearfac <- as.numeric(as.factor(lengthdata$year))
  lengthdata$riverfac <- as.numeric(as.factor(lengthdata$river))
  lengthdata$speciesfac <- as.numeric(as.factor(lengthdata$species))
  lengthdata$cat <- NA
  lengthdata$cat[lengthdata$river=="Chena" & lengthdata$species=="Chinook" & lengthdata$Sex=="male"] <- 1
  lengthdata$cat[lengthdata$river=="Chena" & lengthdata$species=="Chinook" & lengthdata$Sex=="female"] <- 2
  lengthdata$cat[lengthdata$river=="Chena" & lengthdata$species=="Chum" & lengthdata$Sex=="male"] <- 3
  lengthdata$cat[lengthdata$river=="Chena" & lengthdata$species=="Chum" & lengthdata$Sex=="female"] <- 4
  lengthdata$cat[lengthdata$river=="Salcha" & lengthdata$species=="Chinook" & lengthdata$Sex=="male"] <- 5
  lengthdata$cat[lengthdata$river=="Salcha" & lengthdata$species=="Chinook" & lengthdata$Sex=="female"] <- 6
  lengthdata$cat[lengthdata$river=="Salcha" & lengthdata$species=="Chum" & lengthdata$Sex=="male"] <- 7
  lengthdata$cat[lengthdata$river=="Salcha" & lengthdata$species=="Chum" & lengthdata$Sex=="female"] <- 8
  lengthdata <- subset(lengthdata, !is.na(cat) & !is.na(year) & !is.na(Length))
  length_data <- list(y=lengthdata$Length, year=lengthdata$yearfac, cat=lengthdata$cat,
                      N=nrow(lengthdata), Ncat=max(lengthdata$cat), Nyear= max(lengthdata$yearfac))
  return(length_data)
}
