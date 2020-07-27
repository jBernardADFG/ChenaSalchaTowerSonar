#' Plot expanded visual and sonar counts.
#' @param x Output of \code{\link{compile_estimates}}.
#' @author Matt Tyres and Jordy Bernard.
#' @export

visual_sonar_plot <- function(x, ...) {
  vis95lo <- x$vis_count_expanded - 2*sqrt(x$vis_var_expansion)
  vis95hi <- x$vis_count_expanded + 2*sqrt(x$vis_var_expansion)
  son95lo <- x$sonar_count_expanded - 2*sqrt(x$sonar_var_total)
  son95hi <- x$sonar_count_expanded + 2*sqrt(x$sonar_var_total)
  
  datex <- as.Date(rownames(x))
  plot(as.Date(rownames(x)), x$vis_count_expanded, ylim=c(0, max(vis95hi,son95hi,na.rm=T)), type='l', col=4, lwd=2, xlab="",ylab="",...=...)
  # lines(as.Date(rownames(x)), vis95hi, col=4, lwd=1)
  # lines(as.Date(rownames(x)), vis95lo, col=4, lwd=1)
  for(i in 2:length(datex)) polygon(c(datex[c(i-1,i)],rev(datex[c(i-1,i)])), c(vis95lo[c(i-1,i)],rev(vis95hi[c(i-1,i)])), border=NA, col=adjustcolor(4, alpha.f=.2))
  lines(as.Date(rownames(x)), x$sonar_count_expanded, col=2, lwd=2)
  # lines(as.Date(rownames(x)), son95hi, col=2, lwd=1)
  # lines(as.Date(rownames(x)), son95lo, col=2, lwd=1)
  for(i in 2:length(datex)) polygon(c(datex[c(i-1,i)],rev(datex[c(i-1,i)])), c(son95lo[c(i-1,i)],rev(son95hi[c(i-1,i)])), border=NA, col=adjustcolor(2, alpha.f=.2))

  # lines(as.Date(rownames(x)), son95hi, col=2, lwd=1)
  # lines(as.Date(rownames(x)), son95lo, col=2, lwd=1)
  for(i in 2:length(datex)) polygon(c(datex[c(i-1,i)],rev(datex[c(i-1,i)])), border=NA, col=adjustcolor(6, alpha.f=.2))
  legend("topleft",fill=adjustcolor(c(4,2,6),alpha.f=.3),legend=c("visual","sonar"))
}
