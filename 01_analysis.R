# source: Stanford's Big Local News twitter data
# https://biglocalnews.org/

#pulls data directly from the Stanford BLS site 
#an alternative if need be is archived verion stored in archived_data directory

library(tidyverse)
library(janitor)
library(lubridate)

raw <- read_csv("https://storage.googleapis.com/bln_prod/project/850c9bfc-7c02-4d48-b631-898a81ff4144/governors_20200328.csv?X-Goog-Algorithm=GOOG4-RSA-SHA256&X-Goog-Credential=bln-storage%40big-local-news-267923.iam.gserviceaccount.com%2F20200330%2Fauto%2Fstorage%2Fgoog4_request&X-Goog-Date=20200330T211143Z&X-Goog-Expires=86400&X-Goog-SignedHeaders=host&X-Goog-Signature=32f102ae691018635cdcee442b2b6aee8cc7e1fe73f3ea00fbf7dd96ee23c3fc807f1622fbd38f1ffa047474a521bd80938f7118333e891f957b5cbe4acc045287ad3b30ab0a85816225972912010fdf6fb18760ae6f017ba6b53de342c31420eafc56c66290dfac748f75e367b3e7e258b083ab85a8000a856765831f064292703db47d742ad876bf5affabb8f164dedded32197e303728b65525c8bc6464272bc4dadfe344c9795cdf2b48de75d58cfa49834f1fceadc15cb104dc99dee5049784baab37e8cff7a7bc5ae832ce31244d5f404c3698a864569d017e6f02a08ced9680f1f7be57dd0eeefc0bc04957e561d4f4a91487adeba80aa2a3f11e16d1",
                 col_types = cols(.default = "c"))

#convert date/time column

twdata_all <- raw %>% 
  mutate(
    created_at = ymd_hms(created_at)
  )

glimpse(twdata_all)


#some quick gropuings explore

twdata_all %>% 
  count(party_affiliation)

twdata_all %>% 
  count(state)

#date ranges
twdata_all %>% 
  arrange(created_at) %>% 
  head(10)

twdata_all %>% 
  arrange(desc(created_at)) %>% 
  head(10)
  





  
  






