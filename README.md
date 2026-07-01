I.	Introduction
The purpose of this data project was to analyze flow data from two different locations in Pittsburgh, PA, specifically Little Pine Creek and the Allegheny River at Natrona. 
Using publicly available data from https://streamstats.usgs.gov/ss/ , we queried for discharge data spanning back nearly three decades in each location. These data were analyzed
using R scripts contained within this repository where we have generated summary statistics for flow data over time. Additionally, we were able to examine data trends for 5-year 
flood periods and understand the differences between these two locations.

II.	Data and Methods
Using RStudio, I imported discharge data from StreamStats, creating basic data frames to work from. Using functions from lubridate, I converted the date/time data to US/Eastern 
for further data analysis in R. The discharge units also needed to be converted from CFS to cubic meters per second, which was achieved using dplyr with simple equations.
Next, I parallelized a process to calculate peak flow per hydro year. The R library hydrostats contains a function to easily convert date time to hydrological year, which was 
achieved using the hy function. Since these data frames are quite large, it was necessary to parallelize to generate the results efficiently. After achieving this, a new data frame
summarizing peak flow in each hydrological year was generated. 
Additional statistical functions including mean, standard deviation, skew, and Pearson type 3 analysis were performed on the peakflow/hY data to accurately represent it. Then, I 
used ggplot2 to display peak flow (Q) and the number of recorded years at each value as a histogram. To examine days in the dataset above the 5 year flood value, I mutated a new 
data frame to represent these values. Using ggplot2, I made a point graph displaying how many days per hydrological year exceeded the 5-year flood value. In completion of the data 
analysis, I wanted to examine decadal flood levels:
