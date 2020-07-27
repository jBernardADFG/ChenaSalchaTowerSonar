#' Reads ARIS files.
#' @description Called internally within ProcessFiles_ARIS. Returns a data frame containing the rows from the file columns for date, start time, the end time, and duration.
#' @param filename File to be processed.
#' @author Allison Matter and Carl Pfisterer.
#' @importFrom stringr str_extract

ReadFile_ARIS=function(filename="tempfile.txt"){
  Numfish<-as.numeric(scan(filename,nline=1, quiet=T, what=character())[4])
  if (Numfish == 0 ) 
  {            
    StartTime =(str_extract(filename, "([0-9]{6})"))                
    StartTime<-as.character(as.factor(StartTime))
    hr<-substr(StartTime, 1, 2)
    min<-substr(StartTime, 3, 4)
    sec<-substr(StartTime, 5, 6)
    time <- paste(hr, min, sec, sep= ':')
    StartTime<-time
    EndTime = scan(filename,skip=(32+Numfish),nlines=1,quiet=TRUE, what=character())[4]
    
    # Date=(stringr::str_extract(filename, "(2016)[-](10)[-]([0-9]{2})"))
    Date=(stringr::str_extract(filename, "([0-9]{4})[-]([0-9]{2})[-]([0-9]{2})"))
    Temp=rep(scan(filename, skip=18, nlines=1, quiet=TRUE, what=character()))[4]
    Duration = as.integer(round(as.POSIXlt(strptime(EndTime,format="%H:%M:%S"))-
                                  as.POSIXlt(strptime(StartTime,format="%H:%M:%S"))))
    Data = data.frame(Date=Date,Temp=Temp,StartTime=StartTime,EndTime=EndTime,Duration=Duration, FileName=filename,File=1, Total=NA,
                      Frame=NA, Dir=NA, R.m=NA, Theta=NA, L.cm=NA, dR.cm=NA, L.dR=NA, Aspect=NA, Time=NA, Date=NA,Pan=NA, Tilt=NA,Roll=NA, 
                      Species=NA, Motion=NA, Motion2=NA, Q=NA, N=NA)
    colnames(Data) = c("Date1","Temp","StartTime","EndTime","Duration", "FileName", "File","Total","Frame#", "Dir", "R_m", "Theta", "L_cm", "dR_cm", "L_dR", "Aspect", "Time", "Date2", 
                       "Pan", "Tilt", "Roll", "Species", "Motion", "Motion2", "Q", "N" )
  } else {
    Data<-read.table(filename, skip= 25, fill=T, header=F) ;
    fishtable=Data[1:Numfish,-(13:22)];
    colnames(fishtable) = c("File","Total","Frame#", "Dir", "R(m)", "Theta", "L(cm)", "dR(cm)", "L/dR", "Aspect", "Time", "Date", 
                            "Pan", "Tilt", "Roll", "Species", "Motion", "Motion2", "Q", "N" )
    StartTime =(str_extract(filename, "([0-9]{6})")) 
    StartTime<-as.character(as.factor(StartTime))
    hr<-substr(StartTime, 1, 2)
    min<-substr(StartTime, 3, 4)
    sec<-substr(StartTime, 5, 6)
    time <- paste(hr, min, sec, sep= ':')
    StartTime<-time
    EndTime = rep(scan(filename,skip=(32+Numfish),nlines=1,quiet=TRUE, what=character())[4], Numfish, ncol=1)
    
    Duration = as.integer(round(as.POSIXlt(strptime(EndTime,format="%H:%M:%S"))-
                                  as.POSIXlt(strptime(StartTime,format="%H:%M:%S"))))
    # Date=rep((stringr::str_extract(filename, "(2016)[-](10)[-]([0-9]{2})")), Numfish, ncol=1)
    Date=rep((stringr::str_extract(filename, "([0-9]{4})[-]([0-9]{2})[-]([0-9]{2})")), Numfish, ncol=1)
    Temp=rep(scan(filename, skip=18, nlines=1, quiet=TRUE, what=character()))[4]
    Data<- data.frame(Date=Date, Temp=Temp,StartTime=StartTime,EndTime=EndTime,Duration=Duration, FileName=filename, fishtable)
    colnames(Data) = c("Date1","Temp","StartTime","EndTime","Duration", "FileName", "File","Total","Frame#", "Dir", "R_m", "Theta", "L_cm", "dR_cm", "L_dR", "Aspect", "Time", "Date2", 
                       "Pan", "Tilt", "Roll", "Species", "Motion", "Motion2", "Q", "N" )
  } 
  Data
}