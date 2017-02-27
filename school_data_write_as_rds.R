# ONLY NEEDS TO BE RUN WHEN CSV DATA IS UPDATED
# Often we receive data as CSV rather than SQL query piped into R
# Apples and Pears and coordinates, write as RDS TO BE read in global.R --------
library(tidyverse)

# GET GEOLOCATION ---------------------------------------------------------
# http://www.education.gov.uk/edubase/home.xhtml
school_locations <- as_tibble(read_csv("data/2016-11-09-schools_north_east.csv")) %>%
  mutate(school_name = as.character(school_name), la = as.character(la),
         phase = as.factor(phase))

write_rds(school_locations, "data/school_locations.rds")


# APPLES, made-up variable 1 -----------------------------------------------------------------
apples_data <- as_tibble(read_csv("data/apples.csv",
                                        na = c("", "NA", "NULL"))) %>%
  na.omit()  #  remove incomplete cases

colnames(apples_data)[1] <- "urn"  #  Encoding issue, from SQL, rename it

write_rds(apples_data, "data/apples_data.rds")

# PEARS, made-up variable 2 --------------------------------------------------------------------
pears_data <- as_tibble(read_csv("data/pears.csv",
                                  na = c("", "NA", "NULL"))) %>%
  na.omit()  #  remove incomplete cases

colnames(pears_data)[1] <- "urn"  #  Encoding issue, from SQL, rename it


write_rds(pears_data, "data/pears_data.rds")
