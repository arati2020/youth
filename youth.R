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
pdf_mozambique <- pdftools::pdf_text(here("data", "raw_data", "mozambique.pdf"))
imstart = pdf_mozambique %>% str_which(pattern = "IMPLEMENTING MECHANISM SUMMARY")
imend = pdf_mozambique %>% str_which(pattern = "KEY ISSUE, SPSD, and PROGRAM SUMMARY")

mozambique1 <- pdf_mozambique[1:(imstart-1)]
mozambique2 <- pdf_mozambique[imstart:(imend-1)]
mozambique3 <- pdf_mozambique[imend:length(pdf_mozambique)]
length(mozambique3)

mozambique2 %<>% str_c(collapse = "")
mozambiqueIM <- mozambique2 %>% str_split(pattern = "IM ", simplify = TRUE)
