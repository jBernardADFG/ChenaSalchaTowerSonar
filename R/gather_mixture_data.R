#' Gather and format data for mixture model
#' @param sonar_fish data.frame containing the sonar data
#' @param length_priors Output of extract_length_priors
#' @param runtiming_priors Output of extract_runtiming_priors
#' @author Matt Tyres and Jordy Bernard
#' @export

gather_mixture_data <- function(sonar_fish, length_priors, runtiming_priors){
  
  days1.205 <- sonar_fish$date - as.numeric(as.Date("2018-12-31",format="%Y-%m-%d")) - 205
  days1.205 <- as.numeric(days1.205)
  
  ## sonar-measured lengths and actual lengths from the tethered-fish experiment 
  ## This was once used directly in the JAGS model but now runs in an lm() outside to simplify 
  dl <- c(632,602,1049,664,768,663,1025,685,681,957,953,747,646,666,627,531,584)
  al <- c(740,600,1170,710,950,650,1020,665,760,1040,970,840,700,700,620,620,690)
  
  mixl_logitd_3.data <- list(n.fish=nrow(sonar_fish),alpha.sex.chin=c(5,5),alpha.sex.chum=c(5,5),day=days1.205,
                             L.mm.D=sonar_fish$length,sonar=as.numeric(as.factor(sonar_fish$sonartype)),
                             river=as.numeric(as.factor(sonar_fish$river)),
                             nchena=sum(sonar_fish$river=="Chena"),nsalcha=sum(sonar_fish$river=="Salcha"))
  
  mixl_logitd_3.data$chena_chin_m_mn <- length_priors$prior_mn[1]
  mixl_logitd_3.data$chena_chin_f_mn <- length_priors$prior_mn[2]
  mixl_logitd_3.data$chena_chum_m_mn <- length_priors$prior_mn[3]
  mixl_logitd_3.data$chena_chum_f_mn <- length_priors$prior_mn[4]
  mixl_logitd_3.data$salcha_chin_m_mn <- length_priors$prior_mn[5]
  mixl_logitd_3.data$salcha_chin_f_mn <- length_priors$prior_mn[6]
  mixl_logitd_3.data$salcha_chum_m_mn <- length_priors$prior_mn[7]
  mixl_logitd_3.data$salcha_chum_f_mn <- length_priors$prior_mn[8]
  mixl_logitd_3.data$chena_chin_m_sd_mn <- length_priors$prior_sd_mn[1]
  mixl_logitd_3.data$chena_chin_f_sd_mn <- length_priors$prior_sd_mn[2]
  mixl_logitd_3.data$chena_chum_m_sd_mn <- length_priors$prior_sd_mn[3]
  mixl_logitd_3.data$chena_chum_f_sd_mn <- length_priors$prior_sd_mn[4]
  mixl_logitd_3.data$salcha_chin_m_sd_mn <- length_priors$prior_sd_mn[5]
  mixl_logitd_3.data$salcha_chin_f_sd_mn <- length_priors$prior_sd_mn[6]
  mixl_logitd_3.data$salcha_chum_m_sd_mn <- length_priors$prior_sd_mn[7]
  mixl_logitd_3.data$salcha_chum_f_sd_mn <- length_priors$prior_sd_mn[8]
  mixl_logitd_3.data$chena_chin_m_se <- length_priors$prior_se[1]
  mixl_logitd_3.data$chena_chin_f_se <- length_priors$prior_se[2]
  mixl_logitd_3.data$chena_chum_m_se <- length_priors$prior_se[3]
  mixl_logitd_3.data$chena_chum_f_se <- length_priors$prior_se[4]
  mixl_logitd_3.data$salcha_chin_m_se <- length_priors$prior_se[5]
  mixl_logitd_3.data$salcha_chin_f_se <- length_priors$prior_se[6]
  mixl_logitd_3.data$salcha_chum_m_se <- length_priors$prior_se[7]
  mixl_logitd_3.data$salcha_chum_f_se <- length_priors$prior_se[8]
  mixl_logitd_3.data$chena_chin_m_sd_sd <- length_priors$prior_sd_sd[1]
  mixl_logitd_3.data$chena_chin_f_sd_sd <- length_priors$prior_sd_sd[2]
  mixl_logitd_3.data$chena_chum_m_sd_sd <- length_priors$prior_sd_sd[3]
  mixl_logitd_3.data$chena_chum_f_sd_sd <- length_priors$prior_sd_sd[4]
  mixl_logitd_3.data$salcha_chin_m_sd_sd <- length_priors$prior_sd_sd[5]
  mixl_logitd_3.data$salcha_chin_f_sd_sd <- length_priors$prior_sd_sd[6]
  mixl_logitd_3.data$salcha_chum_m_sd_sd <- length_priors$prior_sd_sd[7]
  mixl_logitd_3.data$salcha_chum_f_sd_sd <- length_priors$prior_sd_sd[8]
  
  ## measured length vs true length regression ests (don't think this is currently used)
  mixl_logitd_3.data$betaD0_mn <- 44.81988
  mixl_logitd_3.data$betaD1_mn <- 0.87156
  mixl_logitd_3.data$betaD0_prec <- 63.36818^(-2)
  mixl_logitd_3.data$betaD1_prec <- 0.07835^(-2)
  
  ## Recentering(ish) the length data!  This gets messy but helps convergence.
  mn_length <- mean(sonar_fish$length)
  mixl_logitd_3.data$chena_chin_m_mn <- mixl_logitd_3.data$chena_chin_m_mn - mn_length
  mixl_logitd_3.data$chena_chin_f_mn <- mixl_logitd_3.data$chena_chin_f_mn  - mn_length
  mixl_logitd_3.data$chena_chum_m_mn <- mixl_logitd_3.data$chena_chum_m_mn  - mn_length
  mixl_logitd_3.data$chena_chum_f_mn <- mixl_logitd_3.data$chena_chum_f_mn  - mn_length
  mixl_logitd_3.data$salcha_chin_m_mn <- mixl_logitd_3.data$salcha_chin_m_mn  - mn_length
  mixl_logitd_3.data$salcha_chin_f_mn <- mixl_logitd_3.data$salcha_chin_f_mn  - mn_length
  mixl_logitd_3.data$salcha_chum_m_mn <- mixl_logitd_3.data$salcha_chum_m_mn  - mn_length
  mixl_logitd_3.data$salcha_chum_f_mn <- mixl_logitd_3.data$salcha_chum_f_mn - mn_length
  
  ## updating the measured vs true length regression with centered lengths
  al_c <- al - mn_length
  c_mod <- lm(dl~al_c)
  c_mod_coef <- summary(c_mod)$coefficients
  mixl_logitd_3.data$betaD0_mn <- c_mod_coef[1,1]
  mixl_logitd_3.data$betaD1_mn <- c_mod_coef[2,1]
  mixl_logitd_3.data$betaD0_prec <- c_mod_coef[1,2]^(-2)
  mixl_logitd_3.data$betaD1_prec <- c_mod_coef[2,2]^(-2)
  
  # differential run-timing priors
  mixl_logitd_3.data$mu.b0 <- runtiming_priors$a0[nrow(runtiming_priors$a0),]
  mixl_logitd_3.data$mu.b1 <- runtiming_priors$a1[nrow(runtiming_priors$a1),]
  
  return(mixl_logitd_3.data)
}
