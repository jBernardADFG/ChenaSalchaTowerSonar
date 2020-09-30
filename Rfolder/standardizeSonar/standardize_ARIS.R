#' Converts ARIS data to a standardized format.
#' @description Takes in the output of \code{\link{ProcessFiles_ARIS}} and returns a data frame containing river, station, sonar type, date, hour, minute, second, fractional day, length, shift, and hour within shift ASSOCIATED WITH EACH RECORDED SONAR TARGET. Rows with NA for length correspond to sonar files with no fish recorded.  This is where the "zero" counts will come from in the expansion.
#' @param x data.frame returned from \code{\link{ProcessFiles_ARIS}}.
#' @param river Name of the river (e.g. "Chena").
#' @param station Name of sonar station (e.g. "Chena North").
#' @author Matt Tyers and Jordy Bernard.
#' @export

standardize_ARIS <- function(x, river, station, year) {
  
  # --------------------------
  # 2019 data fixages
  if (year==2019){
    if(grepl("-", x$Date[1])){
      d <- as.POSIXct(x$Date)
      x$Date <- as.character(d, format="%m/%d/%Y")
    }
    if(length(names(x)[names(x)=="Date.1"])>0){
      names(x)[names(x)=="Date.1"] <- "Date1"
      x$Date1 <- x$Date
      x$Date1 <- as.Date(x$Date1, format="%m/%d/%Y")
    }
    if(length(names(x)[names(x)=="Length"])>0){
      names(x)[names(x)=="Length"] <- "L_cm"
    }
    if(nchar(x$StartTime[1])<8){
      x$StartTime <- paste0(x$StartTime,":00")
    }
  }
  # --------------------------
  
  date <- as.Date(x$Date1)  # was Date2
  
  Time_spl1 <- strsplit(as.character(x$Time), split=":")
  hour1 <- as.numeric(sapply(Time_spl1, "[", 1))
  min1 <- as.numeric(sapply(Time_spl1, "[", 2))
  sec1 <- as.numeric(sapply(Time_spl1, "[", 3))
  
  Time_spl2 <- strsplit(as.character(x$StartTime), split=":")
  hour2 <- as.numeric(sapply(Time_spl2, "[", 1))
  min2 <- as.numeric(sapply(Time_spl2, "[", 2))
  sec2 <- as.numeric(sapply(Time_spl2, "[", 3))
  
  hour <- ifelse(!is.na(hour1), hour1, hour2)
  min <- ifelse(!is.na(min1), min1, min2)
  sec <- ifelse(!is.na(sec1), sec1, sec2)
  
  return(data.frame(river=river, 
                    station=station,
                    sonartype="ARIS",
                    date=date,
                    hour=hour,
                    min=min,
                    sec=sec,
                    timefrac = hour/24 + min/(24*60) + sec/(24*60*60),
                    length=as.numeric(as.character(x$L_cm))*10,
                    shift = floor(hour/8) + 1,
                    shift_hr = hour %% 8 + 1,
                    min20=floor(min/20)+1,
                    block=paste(date,floor(hour/8) + 1,hour %% 8 + 1,floor(min/20)+1,sep="_"),
                    stringsAsFactors=F))
}
