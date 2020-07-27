#' Reads DIDSON files.
#' @description Called internally within \code{\link{ProcessFiles_DIDSON}}. Returns a data frame containing the rows from the file columns for date, start time, the end time, and duration.
#' @param filename File to be processed.
#' @author Carl Pfisterer.
ReadFile_DIDSON = function(filename="tempfile.txt"){
  StartTime = scan(filename,skip=5,nlines=1,quiet=TRUE,what=character())[4];  			#Keep as text
  Duration = round(as.double(scan(filename,skip=3, nlines=1,quiet=TRUE,what=character())[4]),digits=0)	#Duration in minutes
  EndTime = as.POSIXlt(strptime(StartTime,format="%H:%M:%S")+round(Duration*60));  #Round otherwise it uses floor
  if(is.na(EndTime)){cat("\n\nProblem with file:",filename,"\n\n")}
  if(EndTime$hour==0 & EndTime$min==0 & EndTime$sec==0) {
    #cat("\n\nFile:",filename,"\n\n");
    EndTime = "00:00:00";
  }
  else{
    EndTime = paste(EndTime$hour,":",EndTime$min,":",EndTime$sec,sep='');
  }
  Date = as.Date(scan(filename,skip=4,nlines=1,quiet=TRUE,what=character())[3],format="%m/%d/%Y");
  Date = DateToText(Date);
  Data = read.table(filename,skip=7,header=TRUE,sep='\t');
  if(length(Data[,1])>0){
    Data$Date = Date;
    Data$StartTime = StartTime;
    Data$EndTime = EndTime;
    Data$Duration = Duration;
    Data$FileName = filename;
  }
  else{Data = data.frame(Sample=NA,Ping=NA,Time=NA,Range=NA,Amplitude=NA,XAngle=NA,YAngle=NA,Direction=NA,Length=NA,Area=NA,Operator=NA,Date=Date,StartTime=StartTime,EndTime=EndTime,Duration=Duration,FileName=filename);}
  Data = Data[,c("FileName","Date","StartTime","EndTime","Duration","Sample","Ping","Time","Range","Amplitude","XAngle","YAngle","Direction","Length","Area","Operator")]
  Data
}