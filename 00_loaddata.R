# source: Stanford's Big Local News twitter data
# https://biglocalnews.org/

library(tidyverse)
library(janitor)
library(lubridate)

#download the latest file from BLN site, then process it here
data <- read_csv("archived_data/governors_20200330.csv",
                 col_types = cols(.default = "c"))


#convert date/time column
data <- data %>% 
  mutate(
    created_at = ymd_hms(created_at)
  )

glimpse(data)



#any missing parties?
data %>% 
  filter(is.na(party_affiliation)) %>% 
  count(state)

## This clearly is a shortcoming in the data - we'll have to fix this
# Will turn to:
# https://ballotpedia.org/Partisan_composition_of_governors

# Import processed governors table from a saved Gsheet:
governorslist <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vR612UAkJ68pjn6Wh-ndohpUiugtT1SyTkqw8PBegzwuphh9Efyp711frrmdD7nLOnxnw778iTY3mpw/pub?gid=0&single=true&output=csv")

#join
joined <- left_join(data, governorslist, by = c("abbreviation" = "state_abbrev"))

#make sure no parties switched
joined %>% 
  filter(party_affiliation.x != party_affiliation.y)


#clean up table and create governor identification field ####

#create party abbreviation and speaker column for identification
joined <- joined %>% 
  mutate(
    party_abbrev = str_sub(party_affiliation.y, 1L, 1L),
    speaker = paste0(governor, " (", abbreviation, "-", party_abbrev, ")")
  )

#cull unneeded columns
names(joined)

joined <- joined %>% 
  select(
    speaker,
    everything(),
    -date_assumed_office.x,
    -party_affiliation.x,
    -office_title,
    date_assumed_office = date_assumed_office.y,
    party_affiliation = party_affiliation.y,
    party_abbrev
  )



#save result for next step
saveRDS(joined, "archived_data/twitterdata_governors.rds")
write_csv(joined, "archived_data/twitterdata_governors.csv")
