# load basic libraries.
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(doParallel)
library(latex2exp)
# install and load library hydrostats. specific for hydrology analysis
install.packages("devtools")
library(devtools)
install_github("LimpopoLab/hydrostats")
library(hydrostats)

# HERE: define necessary functions.
# converting hydrological years - utilizes lubridate library.
hy <- function(d) {
     y <- lubridate::year(d)
     m <- lubridate::month(d)
     if (m >= 10) {
          y <- y + 1
     }
     return(y)
}

# pearson type 3 analysis from hydrostats
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

# load in flow data for Allegheny River Natrona station
x2 <- read_table("allegheny_natrona.txt", skip = 31, col_names = FALSE)

# modify the timezone to US/Eastern and convert cfs to cubic meters per second
y2 <- x2 %>%
     mutate(dt = force_tz(ymd_hms(paste(X3, X4, X5)), tzone = "US/Eastern")) %>%
     mutate(Q = X6 * 0.0283168) %>% #convert cfs to m^3/s
     select(dt, Q)

# figuring out the peak flows. this process can take a while depending on computer/connection

y2$hydroY <- NA
for (i in 1:nrow(y2)) {
     y2$hydroY[i] <- hy(y2$dt[i])
}

# parallelize the process.
registerDoParallel(detectCores())
z2 <- foreach (i = 1:nrow(y2), .combine = 'rbind') %dopar% {
     out <- array(NA, dim = 3)
     out[1] <- y2$dt[i]
     out[2] <- y2$Q[i]
     out[3] <- hy(y2$dt[i])
     return(out)
}

znew2 <- data.frame(z2[,1], z2[,3], z2[,2]) %>%
     mutate(dt = as_datetime(`z2...1.`)) %>%
     rename(hydroYear = `z2...3.`, Q = `z2...2.`) %>%
     select(dt, hydroYear, Q)

# create variable/data frame for peak flow each year
peak2 <- znew2 %>%
     group_by(hydroYear) %>%
     summarize(peakQ = max(Q))

# log of peak flow
peak2 <- peak2 %>%
     mutate(LogPeak = log(peakQ, 10))

# using log of peak flow, we can display/organize these data to evaluate in the context of floods
r <- 1:10
m <- mean(peak2$LogPeak)
s <- stdev(peak2$LogPeak)
c <- skew(peak2$LogPeak)

Fx <- 0.99 # Starting with f(x) = .99, the !% or 100-year flood
tR <- 2 # return period
Fx <- 1 - (1 / tR)

p2 <- pt3(c, Fx)
Qflood2 <- 10^(m + (s * p2)) 



# use ggplot histogram tools to display this data
ggplot(peak2) + 
     geom_histogram(binwidth = 50, color = 'black', fill = "blue", aes(x = peakQ)) +
     geom_vline(xintercept = Qflood2, color = "red") +
     geom_label(label=paste(tR, "year flood"), x=40, y=10) +
     labs(title = "Peak Flow (Q) of Allegheny at Natrona", x = "Peak Flow (m^3/s)", 
          y = "Number of Years")

# now for more data. analyze flood level changes over ten year increments
r <- 16:25
m <- mean(peak2$LogPeak[r])
s <- stdev(peak2$LogPeak[r])
c <- skew(peak2$LogPeak[r])
p2 <- pt3(c, Fx)
Qflood2 <- 10^(m + (s * p2)) 
print(peak2$hydroYear[r])

# csv file for ten year increments
f2 <- read_csv("flood10year.csv")
m1_2 <- lm(flood10 ~ mid, data = f2)

# This works >>>>>>>
# analyzing days above the 5 year flood value. number of distinct floods over a value
ex2 <- znew2 %>%
     mutate(da = as_date(dt)) %>%
     group_by(da) %>%
     summarize(Qday = max(Q, na.rm = TRUE), hy = floor(mean(hydroYear))) %>%
     group_by(hy) %>%
     summarize(n_days = n(), daysOver5 = sum(Qday > Qflood2))

ggplot(ex2, aes(hy, daysOver5)) +
     geom_point(color = 'red') +
     ggtitle("Days Over 5-year Flood Value per year") +
     xlab("Hydrological Year") +
     ylab("Days Over 5-year Flood Value")


# analysis of independent floods
# number of independent floods over a certain level per year

# Find decadal flood levels 
n2 <- floor(nrow(peak2)/5) # determine number of starting points, every 5 years for 10 years
beg <- array(NA, dim = n2)
end <- array(NA, dim = n2)
dur <- array(NA, dim = n2)
mQ <- array(NA, dim = n2)
md <- array(NA, dim = n2)
sd <- array(NA, dim = n2)
cd <- array(NA, dim = n2)
pd <- array(NA, dim = n2)
Qd <- array(NA, dim = n2)
tR <- 10 #return period
Fxd <- 1 - (1/tR)


for (i in 1:n2) {
     beg[i] <- (5 * (i - 1)) + 1 # starting point in peak array
     end[i] <- beg[i] + 9 # ending point
     if (end[i] > nrow(peak2)) {
          end[i] <- nrow(peak2) # to avoid going over
     }
     dur[i] <- end[i] - beg[i] + 1
     dat2 <- peak2$LogPeak[beg[i]:end[i]]
     md[i] <- mean(dat2)
     mQ[i] <- mean(peak2$peakQ[beg[i]:end[i]])
     sd[i] <- stdev(dat2)
     cd[i] <- skew(dat2)
     pd[i] <- pt3(cd, Fxd)
     Qd[i] <- 10^(md[i] + (sd[i] * pd[i]))
}

#beginning year, end yr, duration in yrs, mean flow, mean log, 
# stdev of the log, skewness of log, p-stat of log-PT3, tR/Fx flood level
floods2 <- data.frame(beg, end, dur, mQ, md, sd, cd, pd, Qd)

# analyze significance of data over time..
# keep in mind allegheny is a highly controlled river so significance is not expected to be 
# large
mod <- glm(Qd~end, gaussian(link = "log"), data = floods2)

#plot mean flow and ten-year flood level (mQ and Q

plot(floods2$end, floods2$Qd)

ggplot(floods2, aes(end, Qd)) +
     geom_point(color = 'red') +
     xlab('End Year') + ylab('10-Year Flood Level')

flood_long2 <- floods2 %>%
     mutate(mid = floor((beg + end) / 2)) %>%
     select(mid, mQ, Qd) %>%
     rename(`Mean Flow` = mQ, `10% Flood` = Qd) %>%
     pivot_longer(c(`Mean Flow`, `10% Flood`), names_to = "Legend", values_to = "value")

ggplot(flood_long2, aes(x = mid, y = value, color = Legend)) +
     geom_line(size = 1.5) +
     xlab('Hydrological Year') +
     ylab(TeX('Q ($m^3/s$)'))

