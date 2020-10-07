#' Plot sonar + hamachan estimates for 2020 analysis
#' @param df data.frame with columns named est, sd, method 
#' @param y_up (numeric) upper bound of y-axis
#' @param main (character) plot title
#' @author Matt Tyres and Jordy Bernard.
#' @export

allison_plot <- function(df, y_up, main) {
  
  h_days <- ceiling(sum(df$method=="Sonar")*0.2)
  df <- df[(which(df$method=="Sonar")[1]-h_days):(which(df$method=="Sonar")[length(which(df$method=="Sonar"))]+h_days),]
  
  start <- which(df$method=="Sonar")[1]
  end <- which(df$method=="Sonar")[length(which(df$method=="Sonar"))]
  
  s_l <- s_m <- s_h <- rep(NA, length(start:end)) 
  s_l <- df$est[df$method=="Sonar"] - 2*df$se[df$method=="Sonar"]
  s_m <- df$est[df$method=="Sonar"]
  s_h <- df$est[df$method=="Sonar"] + 2*df$se[df$method=="Sonar"]
  s_x <- as.Date(rownames(df)[start:end])
  for (i in 1:length(s_l)){
    s_l[i] <- max(s_l[i], 0)
  }
  
  h1_l <- h1_m <- h1_h <- rep(NA, length(1:(start-1)))
  h1_l <- df$est[1:(start-1)] - 2*df$se[1:(start-1)]
  h1_m <- df$est[1:(start-1)]
  h1_h <- df$est[1:(start-1)] + 2*df$se[1:(start-1)]
  h1_x <- as.Date(rownames(df)[1:(start-1)])
  for (i in 1:length(h1_l)){
    h1_l[i] <- max(h1_l[i], 0)
  }
  
  h2_l <- h2_m <- h2_h <- rep(NA, length((end+1):nrow(df)))
  h2_l <- df$est[(end+1):nrow(df)] - 2*df$se[(end+1):nrow(df)]
  h2_m <- df$est[(end+1):nrow(df)]
  h2_h <- df$est[(end+1):nrow(df)] + 2*df$se[(end+1):nrow(df)]
  h2_x <- as.Date(rownames(df)[(end+1):nrow(df)])
  for (i in 1:length(h2_l)){
    h2_l[i] <- max(h2_l[i], 0)
  }
  
  d <- as.Date(row.names(df))
  plot(d, rep(0, length(d)), ty="n", xlim=c(min(d), max(d)), ylim=c(0,y_up), main=main, xlab="", xaxt="n", ylab="escapement")
  axis(1, at=d[], as.character(d, format="%m/%d"))
  
  polygon(c(s_x, s_x[length(s_x):1]), c(s_l, s_h[length(s_h):1]), col=rgb(1,0,0,0.3))
  polygon(c(h1_x, h1_x[length(h1_x):1]), c(h1_l,  h1_h[length(h1_h):1]), col=rgb(0,0,1,0.3))
  polygon(c(h2_x, h2_x[length(h2_x):1]), c(h2_l, h2_h[length(h2_h):1]), col=rgb(0,0,1,0.3))
  
  lines(s_x, s_m, col="red", lwd=2)
  lines(h1_x, h1_m, col="blue", lwd=2)
  lines(h2_x, h2_m, col="blue", lwd=2)
  
  legend("topright", fill= c(rgb(1,0,0,0.3), rgb(0,0,1,0.3)),legend=c("Sonar Mixture Model","Historical Hierarchical"))
}
