# Load libraries
library(readr)
library(pdftools)
## Load Libraries
#for data tidying
library(tibble)
library(tidyr)
#for data wrangling
library(dplyr)
library(stringr)
#for functional programming
library(purrr)
#other
library(here)
#for the pipe operator
library(magrittr) 
#dates
#library(lubridate)
#for data vis
#library(ggplot2)
#for adding labels in all charts in a facet wrap
#library(lemon)

# Data Import 
pdf_mozambique <- pdf_text(here("data", "raw_data", "mozambique.pdf"))
spsd <- read_csv(here("data/raw_data", "SPSD.csv"), col_names = TRUE, name_repair="universal")

imstart = pdf_mozambique %>% str_which(pattern = "IMPLEMENTING MECHANISM SUMMARY")
imend = pdf_mozambique %>% str_which(pattern = "KEY ISSUE, SPSD, and PROGRAM SUMMARY")

mozambique1 <- pdf_mozambique[1:(imstart-1)]
mozambique2 <- pdf_mozambique[imstart:(imend-1)]
mozambique3 <- pdf_mozambique[imend:length(pdf_mozambique)]

mozambique2 %<>% str_c(collapse = "")
mozambiqueIM <- mozambique2 %>% str_split(pattern = "IM ", simplify = TRUE)
mozambiqueIM <- as_tibble(mozambiqueIM[-1])

mozambiqueIM %<>%
  separate(value,c("IM.info","IM.narrative","IM.funding"), sep="IMPLEMENTING MECHANISM NARRATIVE|FUNDING SUMMARY") %<>%
  mutate(youth = str_detect(IM.narrative,"youth"),
                         fp.rh = str_detect(IM.funding,"HL.7.")
                         ) %<>%
  separate(IM.info,c("IM.heading","IM.details"),sep="Mechanism Number") %<>%
  separate(IM.heading,c("IM.number","IM.name"), sep = ":", extra = "merge", convert = TRUE) %<>%
  #extra=merge will split only twice so any semicolons in the IM name will not cause a split
  #convert = true will detect column classes
  mutate(IM.name= str_trim(IM.name),
         IM.details= str_trim(IM.details),
         IM.narrative= str_squish(IM.narrative), #unlike trim, squish will remove spaces inbetween as well as leading and trailing
         IM.funding=str_trim(IM.funding)) %<>%
  write_csv(file = here("data/wrangled_data", "mozambiqueIM.csv"))

mozambiqueIM[2]
