library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(doParallel)

install.packages("devtools")
library(devtools)

install_github("LimpopoLab/hydrostats")
library(hydrostats)



#function for converting hydrological years
hy <- function(d) {
     y <- lubridate::year(d)
     m <- lubridate::month(d)
     if (m >= 10) {
          y <- y + 1
     }
     return(y)
}

# pearson type 3 from hydrostats
pt3 <- function(c,Fx) {
     k <- c/6
     z <- qnorm(Fx)
     K_T <- z + ((z^2) - 1) * k + ((z^3) -(6*z)) * (k^2) / 3 - ((z^2) - 1) * (k^3) + z * (k^4) + (k^5)/3
     pt3 <- K_T
     return(pt3)
}

#skew function from hydrostats
skew <- function(x) {
     m <- mean(x, na.rm = TRUE)
     nas <- which(is.na(x))
     n <- length(x) - length(nas)
     s <- (sum((x-m)^2, na.rm = TRUE)/(n-1))^0.5
     m3 <- (sum((x-m)^3, na.rm = TRUE)/(n-1))
     skew <- m3/(s^3)
     return(skew)
}

# standard deviation from hydrostats
stdev <- function(x, biascorr=TRUE) {
     m <- mean(x, na.rm = TRUE)
     nas <- which(is.na(x))
     n <- length(x) - length(nas)
     if (biascorr) {
          stdev <- (sum((x-m)^2, na.rm = TRUE)/(n-1))^0.5
     } else {
          stdev <- (sum((x-m)^2, na.rm = TRUE)/(n))^0.5
     }
     return(stdev)
}





# load in flow data
x <- read_table("little_pine_cr.txt", skip = 31, col_names = FALSE)

y <- x %>%
     mutate(dt = force_tz(ymd_hms(paste(X3, X4, X5)), tzone = "US/Eastern")) %>%
     mutate(Q = X6 * 0.0283168) %>% #convert cfs to m^3/s
     select(dt, Q)

# figuring out the peak flows
y$hydroY <- NA
for (i in 1:nrow(y)) {
     y$hydroY[i] <- hy(y$dt[i])
}

registerDoParallel(detectCores())
z <- foreach (i = 1:nrow(y), .combine = 'rbind') %dopar% {
     out <- array(NA, dim = 3)
     out[1] <- y$dt[i]
     out[2] <- y$Q[i]
     out[3] <- hy(y$dt[i])
     return(out)
}

znew <- data.frame(z[,1], z[,3], z[,2]) %>%
     mutate(dt = as_datetime(`z...1.`)) %>%
     rename(hydroYear = `z...3.`, Q = `z...2.`) %>%
     select(dt, hydroYear, Q)

peak <- znew %>%
     group_by(hydroYear) %>%
     summarize(peakQ = max(Q))

peak <- peak %>%
     mutate(LogPeak = log(peakQ, 10))

m <- mean(peak$LogPeak)
s <- stdev(peak$LogPeak)
c <- skew(peak$LogPeak)

Fx <- 0.99 # Starting with f(x) = .99, the !% or 100-year flood
tR <- 10 # return period
Fx <- 1 - (1 / tR)

p <- pt3(c, Fx)
Qflood <- 10^(m + (s * p)) 

# use ggplot histogram tools to display this data
# vline(yintercept = [flood level]) + 

ggplot(peak) + 
     geom_histogram(binwidth = 10, color = 'black', fill = "blue", aes(x = peakQ))

