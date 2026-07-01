# load basic libraries.
library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(doParallel)

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


# load in flow data for little pine creek
x <- read_table("little_pine_cr.txt", skip = 31, col_names = FALSE)

# modify the timezone to US/Eastern and convert cfs to cubic meters per second
y <- x %>%
     mutate(dt = force_tz(ymd_hms(paste(X3, X4, X5)), tzone = "US/Eastern")) %>%
     mutate(Q = X6 * 0.0283168) %>% #convert cfs to m^3/s
     select(dt, Q)

# figuring out the peak flows. this process can take a while depending on computer/connection

y$hydroY <- NA
for (i in 1:nrow(y)) {
     y$hydroY[i] <- hy(y$dt[i])
}

# parallelize the process.
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

# create variable/data frame for peak flow each year
peak <- znew %>%
     group_by(hydroYear) %>%
     summarize(peakQ = max(Q))

# log of peak flow
peak <- peak %>%
     mutate(LogPeak = log(peakQ, 10))

# using log of peak flow, we can display/organize these data to evaluate in the context of floods
#r <- 1:10

#Example
m <- mean(peak$LogPeak)
s <- stdev(peak$LogPeak)
c <- skew(peak$LogPeak)

Fx <- 0.99 # Starting with f(x) = .99, the 1% or 100-year flood
tR <- 2 # return period
Fx <- 1 - (1 / tR)

p <- pt3(c, Fx)
Qflood <- 10^(m + (s * p)) 

# use ggplot histogram tools to display this data
ggplot(peak) + 
     geom_histogram(binwidth = 10, color = 'black', fill = "blue", aes(x = peakQ)) +
     geom_vline(xintercept = Qflood, color = "red") +
     geom_label(label=paste(tR, "year flood"), x=40, y=10) +
     labs(title = "Peak Flow (Q) of Little Pine Creek", x = "Peak Flow (m^3/s)", 
          y = "Number of Years")


# analyzing days above the 5 year flood value. number of distinct floods over a value
# days per year over a threshold
ex <- znew %>%
     mutate(da = as_date(dt)) %>%
     group_by(da) %>%
     summarize(Qday = max(Q, na.rm = TRUE), hy = floor(mean(hydroYear))) %>%
     group_by(hy) %>%
     summarize(n_days = n(), daysOver5 = sum(Qday > Qflood))

ggplot(ex, aes(hy, daysOver5)) +
     geom_point(color = 'red') +
     ggtitle("Days Over 5-year Flood Value per year") +
     xlab("Hydrological Year") +
     ylab("Days Over 5-year Flood Value")





# now for more data. analyze flood level changes over ten year increments
r <- 16:25
m <- mean(peak$LogPeak[r])
s <- stdev(peak$LogPeak[r])
c <- skew(peak$LogPeak[r])
p <- pt3(c, Fx)
Qflood <- 10^(m + (s * p)) 
print(peak$hydroYear[r])

# csv file for ten year increments
f <- read_csv("flood10year.csv")
m1 <- lm(flood10 ~ mid, data = f)

# Find decadal flood levels
n <- floor(nrow(peak)/5) # determine number of starting points, every 5 years for 10 years
beg <- array(NA, dim = n)
end <- array(NA, dim = n)
dur <- array(NA, dim = n)
mQ <- array(NA, dim = n)
md <- array(NA, dim = n)
sd <- array(NA, dim = n)
cd <- array(NA, dim = n)
pd <- array(NA, dim = n)
Qd <- array(NA, dim = n)
Fxd <- 0.9 #test 10yr flood

for (i in 1:n) {
     beg[i] <- (5 * (i - 1)) + 1 # starting point in peak array
     end[i] <- beg[i] + 9 # ending point
     if (end[i] > nrow(peak)) {
          end[i] <- nrow(peak) # to avoid going over
     }
     dur[i] <- end[i] - beg[i] + 1
     dat <- peak$LogPeak[beg[i]:end[i]]
     md[i] <- mean(dat)
     mQ[i] <- mean(peak$peakQ[beg[i]:end[i]])
     sd[i] <- stdev(dat)
     cd[i] <- skew(dat)
     pd[i] <- pt3(cd, Fxd)
     Qd[i] <- 10^(md[i] + (sd[i] * pd[i]))
}

floods <- data.frame(beg, end, dur, mQ, md, sd, cd, pd, Qd)

mod <- glm(Qd~end, gaussian(link = "log"), data = floods)

#plot mean flow and ten-year flood level (mQ and Q

plot(floods$end, floods$Qd)

ggplot(floods, aes(end, Qd)) +
     geom_point(color = 'red') +
     xlab('End Year') + ylab('10-Year Flood Level')

flood_long <- floods %>%
     mutate(mid = floor((beg + end) / 2)) %>%
     select(mid, mQ, Qd) %>%
     rename(`Mean Flow` = mQ, `10% Flood` = Qd) %>%
     pivot_longer(c(`Mean Flow`, `10% Flood`), names_to = "Legend", values_to = "value")

ggplot(flood_long, aes(x = mid, y = value, color = Legend)) +
     geom_line(size = 1.5) +
     xlab('Hydrological Year') +
     ylab(TeX('Q ($m^3/s$)'))


