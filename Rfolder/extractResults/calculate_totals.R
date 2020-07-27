#' Calculate totals, according to a user-input date range.
#' @param final_results The output of \code{\link{extract_final_results}}.
#' @param from The start date (e.g. "2019-06-23"). 
#' @param to The end date (e.g. "2019-08-10"). 
#' @author Matt Tyres and Jordy Bernard.
#' @export

calculate_totals <- function(final_results, from, to){
  maketot <- function(x, from, to) {
    daterange <- (as.Date(rownames(x)) >= as.Date(from)) & (as.Date(rownames(x)) <= as.Date(to))
    return(c(sum(x$DailyEst[daterange]), sqrt(sum(x$DailyVar[daterange]))))
  }
  tots <- rbind(maketot(x=final_results$Cchin_final, from=from, to=to),
                maketot(x=final_results$Cchum_final, from=from, to=to),
                maketot(x=final_results$Schin_final, from=from, to=to),
                maketot(x=final_results$Schum_final, from=from, to=to))
  tots1 <- data.frame(River=c("Chena","Chena","Salcha","Salcha"),
                      Species=c("Chinook","Chum","Chinook","Chum"),
                      Estimate=tots[,1],
                      SE=tots[,2],
                      StartDate=from,
                      EndDate=to)
  return(list(totals=tots1))
}

