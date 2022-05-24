# Load libraries
library(readr)
#library(pdftools)
#library(docxtractr)
#library(officer)
library(textreadr)
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

path = here("data", "raw_data", "Mozambique.docx")

#using textreadr
doc_mozambique <- textreadr::read_docx(path)
imstart = doc_mozambique %>% str_which(pattern = "IMPLEMENTING MECHANISM SUMMARY")
imend = doc_mozambique %>% str_which(pattern = "KEY ISSUE, SPSD, and PROGRAM SUMMARY")

mozambique1 <- doc_mozambique[1:(imstart-1)]
mozambique2 <- doc_mozambique[imstart:(imend-1)]
mozambique3 <- doc_mozambique[imend:length(doc_mozambique)]

mozambique2 %<>% str_c(collapse = "\\n")

mozambiqueIM <- mozambique2 %>% str_split(pattern = "IM ", simplify = TRUE)
mozambiqueIM <- as_tibble(mozambiqueIM[-1])

##########################################################################################

spsd <- read_csv(here("data/raw_data", "SPSD.csv"), col_names = TRUE, name_repair="universal")

mozambiqueIM %<>%
  separate(value,c("IM.info","IM.narrative","IM.funding"), sep="IMPLEMENTING MECHANISM NARRATIVE|FUNDING SUMMARY") %<>%
  mutate(youth = str_detect(IM.narrative,"youth"),
         fp.rh = str_detect(IM.funding,"HL.7."),
         peace.security = str_detect(IM.funding,"PS."),
         democracy.humanrights.gov = str_detect(IM.funding,"DR."),
         health = str_detect(IM.funding,"HL."),
         edu.socialservices = str_detect(IM.funding,"ES."),
         eco.growth = str_detect(IM.funding,"EG."),
         human.ass = str_detect(IM.funding, "HA."),
         prog.dev.oversight = str_detect(IM.funding,"PO.")
        ) %<>%
  separate(IM.info,c("IM.heading","IM.details"),sep="Mechanism Number") %<>%
  separate(IM.heading,c("IM.number","IM.name"), sep = ":", extra = "merge", convert = TRUE) %<>%
  #extra=merge will split only twice so any semicolons in the IM name will not cause a split
  #convert = true will detect column classes
  mutate(IM.name= str_trim(IM.name),
         IM.details= str_trim(IM.details),
         IM.narrative= str_squish(IM.narrative), #unlike trim, squish will remove spaces in between as well as leading and trailing
         IM.funding=str_trim(IM.funding)) %<>%
  write_csv(file = here("data/wrangled_data", "mozambiqueIM.csv"))


####################################################################################
#using officer to read in docx file
doc_mozambique<- officer::read_docx(path)
docx_summary(doc_mozambique)[1]

#using doxtractr ot convert to pdf
docxtractr::convert_to_pdf(path, pdf_file = sub("[.]docx", ".pdf", path))

#using docxtractr read in the docx file
doc_mozambique <- docxtractr::read_docx(path)
test <- docxtractr::docx_extract_tbl(doc_mozambique, tbl_number = 25)

# Data Import using pdf tools
pdf_mozambique <- pdf_text(here("data", "raw_data", "mozambique.pdf"))

imstart = pdf_mozambique %>% str_which(pattern = "IMPLEMENTING MECHANISM SUMMARY")
imend = pdf_mozambique %>% str_which(pattern = "KEY ISSUE, SPSD, and PROGRAM SUMMARY")

mozambique1 <- pdf_mozambique[1:(imstart-1)]
mozambique2 <- pdf_mozambique[imstart:(imend-1)]
mozambique3 <- pdf_mozambique[imend:length(pdf_mozambique)]
