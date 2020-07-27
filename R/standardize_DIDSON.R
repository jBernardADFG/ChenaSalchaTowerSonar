#' Converts DIDSON data to a standardized format.
#' @description Takes in the output of \code{\link{ProcessFiles_DIDSON}} and returns a data frame containing river, station, sonar type, date, hour, minute, second, fractional day, length, shift, and hour within shift ASSOCIATED WITH EACH RECORDED SONAR TARGET.  Rows with NA for length correspond to sonar files with no fish recorded. This is where "zero" counts will come from in the expansion.
#' @param x data.frame returned from \code{\link{ProcessFiles_DIDSON}}.
#' @param river Name of the river (e.g. "Chena").
#' @param station Name of sonar station (e.g. "Chena North").
#' @author Matt Tyers.
#' @export

standardize_DIDSON <- function(x, river, station) {
  if(grepl("-", x$Date[1])){
    d <- as.POSIXct(x$Date)
    x$Date <- as.character(d, format="%m/%d/%Y")
  }
  date <- as.Date(as.character(x$Date), format="%m/%d/%Y")
  StartTime_spl <- strsplit(as.character(x$StartTime), split=":")
  hour <- as.numeric(sapply(StartTime_spl, "[", 1))
  min <- as.numeric(sapply(StartTime_spl, "[", 2))
  sec <- as.numeric(sapply(StartTime_spl, "[", 3))
  x$Time <- ifelse(is.na(x$Time), 0, x$Time)
  min1 <- min + floor(x$Time)
  sec1 <- round(sec + (x$Time-floor(x$Time))*60)
  min2 <- min1 + (sec1>=60)
  sec2 <- sec1 %% 60
  hour1 <- hour + (min2>=60)
  min3 <- min2 %% 60
  date1 <- date + (hour1>=24)
  hour2 <- hour1 %% 24
  return(data.frame(river=river, 
                    station=station,
                    sonartype="DIDSON",
                    date=date1,
                    hour=hour2,
                    min=min3,
                    sec=sec2,
                    timefrac = hour2/24 + min3/(24*60) + sec2/(24*60*60),
                    length=x$Length*1000,
                    shift = floor(hour2/8) + 1,
                    shift_hr = hour2 %% 8 + 1,
                    min20=floor(min3/20)+1,
                    block=paste(date1,floor(hour2/8) + 1,hour2 %% 8 + 1,floor(min3/20)+1,sep="_"),
                    stringsAsFactors=F))
}
