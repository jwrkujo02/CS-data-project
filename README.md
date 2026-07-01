I.	Introduction.

The purpose of this data project was to analyze flow data from two different locations in Pittsburgh, PA, specifically Little Pine Creek and the Allegheny River at Natrona. 
Using publicly available data from https://streamstats.usgs.gov/ss/ , we queried for discharge data spanning back nearly three decades in each location. These data were analyzed
using R scripts contained within this repository where we have generated summary statistics for flow data over time. Additionally, we were able to examine data trends for 5-year 
flood periods and understand the differences between these two locations.

II.	Data and Methods.

Using RStudio, I imported discharge data from StreamStats, creating basic data frames to work from. Using functions from lubridate, I converted the date/time data to US/Eastern 
for further data analysis in R. The discharge units also needed to be converted from CFS to cubic meters per second, which was achieved using dplyr with simple equations.
Next, I parallelized a process to calculate peak flow per hydro year. The R library hydrostats contains a function to easily convert date time to hydrological year, which was 
achieved using the hy function. Since these data frames are quite large, it was necessary to parallelize to generate the results efficiently. After achieving this, a new data frame
summarizing peak flow in each hydrological year was generated. 

Additional statistical functions including mean, standard deviation, skew, and Pearson type 3 analysis were performed on the peakflow/hY data to accurately represent it. Then, I 
used ggplot2 to display peak flow (Q) and the number of recorded years at each value as a histogram. To examine days in the dataset above the 5 year flood value, I mutated a new 
data frame to represent these values. Using ggplot2, I made a point graph displaying how many days per hydrological year exceeded the 5-year flood value. In completion of the data 
analysis, I wanted to examine decadal flood levels by the mean peak flow (Q) values per hydrological year. To do this, we created yet another data frame with statistical values to
generate a graph representing mean flow and 10-year flood trends.

Once all code was written and graphs generated, we proceeded to further examine the findings.

III. Results and Discussion.

Peak flow values differ greatly between Little Pine Creek and Allegheny River at Natrona. According to our data, most years at LPC had Q values around a mean of 10 cubic meters per second 
(Figure 1). In comparison to Allegheny River at Natrona, this location had much more variation and consistently higher Q values, with the mean localizing around 2800 cubic meters 
per second (Figure 2). Considering that these bodies of water are fundamentally different (i.e., creek versus river), this was to be expected. 

In the context of days over the 5-year flood value in both locations, the Allegheny River has significantly more days than that of Little Pine Creek (Figure 3, 4). The number of days 
per year do not appear to correlate between the two locations.

Lastly, our examination of mean flow and 10-year flood values per hydrological year returned interesting findings. Here, we observe that the Allegheny River has less significant 
difference in mean flow and flooding (Figure 6). Based on the status of the river, this data is understandable-- the Allegheny is tightly controlled by local regulation in terms of flow and 
its ability to flood. On the contrary, Little Pine Creek is not specifically regulated to prevent flooding, hence more significant differences are found in these datasets. Reports on 
significance are contained in the data table below.

|River             |p-value |2.5%   |97.5% |
|------------------|--------|-------|------|
|Allegheny River   |.824    |-0.006 |.0079 |
|Little Pine Creek |0.003   |0.038  |0.072 |

![Figure 1. Little Pine Creek Peak Flow (Q).]("PeakFlowQ_LPC.eps")

![Figure 2. Allegheny River at Natrona Peak Flow (Q).]("Peak Flow Q Allegheny.jpeg")

![Figure 3. Days over 5-year flood at LPC.]("LPC_5yearflood_days.eps")

![Figure 4. Days over 5-year flood at Allegheny Natrona.]("Days over 5YFV Natrona.jpeg")

![Figure 5. Mean Flow and 10-year Flood Values for LPC.]("mQ and 10yr Flood LPC.jpeg")

![Figure 6. Mean Flow and 10-year Flood Values for Allegheny at Natrona.]("mQ and 10-flood Allegheny.jpeg")

IV. Conclusions.

The overall goal of this data project aimed to examine discharge data from two geographically different bodies of water, Little Pine Creek and the Allegheny River at Natrona. Here,
we have determined trends in flood and flow data consistent with the historical status of these rivers. Additionally, our process of data analysis and utilizing R coding to achieve 
these results presents a useful framework for analyzing flow data at other locations. Public data from StreamStats does contain some graphical presentations of data, but creating a
concise workflow like that of this data project can greatly assist in understanding and presentation.

V. Citations.

[1]. https://streamstats.usgs.gov/ss/?information-portal=regionalInformation&region=PA



