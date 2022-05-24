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

#country.name= "Mozambique"
countries = list("Mozambique","India")
  
func_country = function(country.name){
              
              #using textreadr
              doc_country <- textreadr::read_docx(dir(here("data","raw_data"),full.names = T, pattern = str_c("^",country.name)))
              imstart = doc_country %>% str_which(pattern = "IMPLEMENTING MECHANISM SUMMARY")
              imend = doc_country %>% str_which(pattern = "KEY ISSUE, SPSD, and PROGRAM SUMMARY")
              
              country1 <- doc_country[1:(imstart-1)]
              country2 <- doc_country[imstart:(imend-1)]
              country3 <- doc_country[imend:length(doc_country)]
              
              country2 %<>% str_c(collapse = regex("\\n"))
              
              countryIM <- country2 %>% str_split(pattern = "IM ", simplify = TRUE)
              countryIM <- as_tibble(countryIM[-1])
              
              ##########################################################################################
              
              spsd <- read_csv(here("data/raw_data", "SPSD.csv"), col_names = TRUE, name_repair="universal")
              
              countryIM %<>%
                separate(value,c("IM.info","IM.narrative","IM.funding"), sep="IMPLEMENTING MECHANISM NARRATIVE|FUNDING SUMMARY") %<>%
                mutate(country= country.name,
                       youth = str_detect(IM.narrative,"youth"),
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
                write_csv(file = here("data/wrangled_data", str_c(country.name,"IM.csv")))
          #    return(countryIM)
              }

walk(countries,func_country)

#write_csv(allIM, file = here("data/wrangled_data", "allIM.csv"))

#Create list of countries??
#read from google drive folder 
#Create one file of all countries.

####################################################################################
#using officer to read in docx file

path = here("data", "raw_data", "Mozambique.docx")

doc_country<- officer::read_docx(path)
docx_summary(doc_country)[1]

#using doxtractr ot convert to pdf
docxtractr::convert_to_pdf(path, pdf_file = sub("[.]docx", ".pdf", path))

#using docxtractr read in the docx file
doc_country <- docxtractr::read_docx(path)
test <- docxtractr::docx_extract_tbl(doc_country, tbl_number = 25)

# Data Import using pdf tools
pdf_country <- pdf_text(here("data", "raw_data", "mozambique.pdf"))

imstart = pdf_country %>% str_which(pattern = "IMPLEMENTING MECHANISM SUMMARY")
imend = pdf_country %>% str_which(pattern = "KEY ISSUE, SPSD, and PROGRAM SUMMARY")

country1 <- pdf_country[1:(imstart-1)]
country2 <- pdf_country[imstart:(imend-1)]
country3 <- pdf_country[imend:length(pdf_country)]
