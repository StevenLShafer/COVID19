# Daily Modeling and Update of the COVID-19 Pandemic

## Overview:
This is my analysis, not Stanford’s. My plots and regressions are intended to understand the trajectory of COVID. It is not confidential and can be freely shared. The R program code is available in this repository. The daily updates can be accessed at https://1drv.ms/u/s!AuOyHP_aTIy7rowrt2AjGpWm_frnEQ?e=KBcNbh. 

Please contact me at steven.shafer@Stanford.edu if you would like to be added or removed from the recipient list. Suggestions are most welcome! You are welcome to use the R code on GitHub for any purpose.

I am attempting to keep the analysis and commentary apolitical. I am now including partisan lean as a metric to help understand the epidemic. I occasionally point out misrepresentations by government officials. I occasionally point out where government recommendations have placed Americans at increasing risk.

I try to provide a daily update in the morning, except Sundays. My analysis my be delayed by my clinical responsibilities as a Stanford anesthesiologist.

There is a lot of information on the figures. If something isn’t clear, please see the explanation on slide 2.

## Data sources:

* USA Case Data: https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv
* USA Death Data: https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv
* USA Testing and Hospitalization Data: https://raw.githubusercontent.com/COVID19Tracking/covid-tracking-data/master/data/states_daily_4pm_et.csv
* Global Case Data: https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv
* Global Death Data: https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv
* Global Testing Data: https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv
* Mobility Data: https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv
* Partisan Lean: MIT Election Data and Science Lab: https://doi.org/10.7910/DVN/VOQCHQ/HEIJCQ
* Ensemble Model: https://github.com/reichlab/covid19-forecast-hub/raw/master/data-processed/COVIDhub-ensemble/2020-xx-xx-COVIDhub-ensemble.csv

## Models:
Future projections of case numbers are based on the Gompertz function (https://en.wikipedia.org/wiki/Gompertz_function): log⁡(𝑐𝑢𝑚𝑢𝑙𝑎𝑡𝑖𝑣𝑒 𝑐𝑎𝑠𝑒𝑠)=𝑐𝑢𝑟𝑟𝑒𝑛𝑡 𝑐𝑎𝑠𝑒𝑠+(𝑚𝑎𝑥𝑖𝑚𝑢𝑚 𝑐𝑎𝑠𝑒𝑠 −𝑐𝑢𝑟𝑟𝑒𝑛𝑡 𝑐𝑎𝑠𝑒𝑠)(1−𝑒^(−𝑘 𝑡) ). This is a naïve asymptotic model. k is the rate constant, such that log(2) / k = time to 50% rise. t is the number of days. Wikipedia The Gompertz function is estimated from the last 3 weeks of data for cumulative cases (red dots in the figures). Deaths are predicted from a log linear regression of deaths over the past 21 days. For the US, and individual states, I am also including the 98% prediction interval from the COVID-19 Forecast Hub (https://covid19forecasthub.org/). 
The rate of change in daily cases and deaths is the slope of delta cases / day over the last 14 days, divided by the average number of cases.

## Locations
The locations for the modeling are where Pamela and I have family and friends, locations of interest to friends and colleagues, or countries in the news (e.g., China, South Korea, Sweden, Brazil) or with significant economic impact on the United States (e.g., Japan, Canada, Mexico). Locations are easy to add.

---
Stay safe, well, resilient, and kind.

Steve Shafer
steven.shafer@stanford.edu