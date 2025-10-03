
This file contains descriptions of the data files provided with the code. Only data explicitly required for running all the models have been provided. These data come from the long-term individual-based study of great tits in Wytham Woods, Oxfordshire, UK.

Following are the datasets used for analyses

(1) wytham_gretichicks_natalinfo.csv

id:unique identifier for each individual chick, broodid:unique identifier for the brood an individual belongs to, birthyear: year in which the chick hatched, natalsection:region of woodland where individual was born, birthlaydate: date when first egg of clutch was laid, april_birthlaydate: lay date in april days (number of days since april 1st), birthhatchdate: date when chick hatched, april_birthhatchdate: hatch date in april days, clutch_size: number of eggs laid in clutch, num_chicks: number of hatched chicks in brood, mean_chick_weight (not used here), mother: unique identifier for mother, father: unique identifier for father, sex (if known) - not used here, natalbox: the nestbox in which individual was born, chickweight: weight of individual at 15 days old

(2) tempdata_1965to2024.csv

Daily temperature data from Met Office Hadley Centre datasets for central England (https://www.metoffice.gov.uk/hadobs/)

Contains, year, month, day, max (maximum temp), min (minimum temp) from 1965 to 2024

(3) raindata_1965to2024.csv

Daily rainfall data from Met Office Hadley Centre datasets for central England (https://www.metoffice.gov.uk/hadobs/)

Contains, year, month, day, rainfall from 1965 to 2024

Code file 'computing ECEs.R' uses (1), (2), and (3)

(4) chickweight_ECEdata_5perc1perc.csv

Code to compute the variables in this dataset is in the code file 'computing ECEs.R'. It contains the same columns from (1) and the following additional variables:

hot_ece1_5: Number of hot ECEs (5% threshold) in hatchling period
hot_ece2_5: Number of hot ECEs (5% threshold) in nestling period
hot_ece1_1: Number of hot ECEs (1% threshold) in hatchling period
hot_ece2_1: Number of hot ECEs (1% threshold) in nestling period
cold_ece1_5: Number of cold ECEs (5% threshold) in hatchling period
cold_ece2_5: Number of cold ECEs (5% threshold) in nestling period
cold_ece1_1: Number of cold ECEs (1% threshold) in hatchling period
cold_ece2_1: Number of cold ECEs (1% threshold) in nestling period
rain_ece1_5: Number of rain ECEs (5% threshold) in hatchling period
rain_ece2_5: Number of rain ECEs (5% threshold) in nestling period
rain_ece1_1: Number of rain ECEs (1% threshold) in hatchling period
rain_ece2_1: Number of rain ECEs (1% threshold) in nestling period
average_temperature_per1: Average temperature during hatchling period
average_temprange_per1: Average temperature range during hatchling period
average_temperature_per2: Average temperature during nestling period
average_temprange_per2: Average temperature range during nestling period
average_rainfall_per1: Average rainfall during hatchling period
average_rainfall_per2: Average rainfall during nestling period

Code file 'Fledging weight models.R' uses (4) for analysis. 

(5) gretisurvivaldata_updated_2025.csv

This contains the same columns as (4) and 3 additional columns:

mean_ld: mean laying date of the population in given year
relative_ld: difference between individual lay date and mean_ld
local_recruitment: 0 (did not recruit) or 1 (recruited), whether the individual was recorded breeding in subsequent years

Code file 'Local recruitment models.R' uses (5) for analysis. 


All models in the manuscript can be run and all figures can be reproduced from the scripts in 'Fledging weight models.R' and 'Local recruitment models.R'
------------------------------------------------------------------------------------------------------
