#' Processes all DIDSON files within a specified directory.
#' @description Reads all the files in the passed directory and appends them into one data frame.  This is the only function called by the user to read DIDSON data.  The function calls the ReadFile_DIDSON function internally to read individual sonar files. Sometimes the function will error out because of a bad text file - just remove the offending file (look at the text file first, to make sure that it is empty) and the function should run after that.
#' @param dir Full address of the directory to be processed.
#' @author Carl Pfisterer
#' @export
ProcessFiles_DIDSON = function(dir){
  cat(paste("\nProcessing files in directory:",dir,"\n"));
  files = list.files(dir,pattern=".txt$",full.names=TRUE);  #Only read files ending in .txt
  if(length(files) > 0){
    progress = txtProgressBar(style=3,min=0,max=length(files),initial=0);
    cnt = 0;
    for(file in files){
      #cat("File: ",file,"\n");
      Data = ReadFile_DIDSON(file);
      if(cnt==0){
        AllData = list(Data);
      }
      else{
        AllData = append(AllData,list(Data));   #Append to list of data
      }
      cnt = cnt+1;
      if((cnt%%20) == 0) setTxtProgressBar(progress,value=cnt); #Update every 20 files
    }
    AllData = do.call(rbind,AllData);   #Merge data from all files into one data frame
  }
  else{
    AllData = NA;         #Return NA if there are no files in the directory
  }
  AllData
}