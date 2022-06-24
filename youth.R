# Load libraries
library(googledrive)
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

OP.folder = "1r8TysHgddUGQmj7iTWdYL9ajFbQIF2_p" #id of approved OP folder

# for every regionID in drive_ls(as_id(OP.folder))["id"] except for the folder with name USAID functional Bureaus , run a function that lists all files
#then for every file get the name using drive_get

regionIDs <- drive_ls(as_id(OP.folder)) %>% filter(name != "USAID Functional Bureaus") %>% select("id") %>% pull()  #list of regionID drives within the approved OP folder except bureaus

#all filenames within a folder
filelist <- function(folderID){drive_ls(as_id(folderID))}

countryfilenames <- map_dfr(regionIDs, filelist) %>% select("name") %>% pull() #list name of all files in all the regionID
countryfileids <- map_dfr(regionIDs, filelist) %>% select("id") %>% pull()

walk(.x= countryfileids, ~{drive_download(as_id(.x))} )

##################################
#manual updates to nepal and asia regional files
#############################################

spsd <- read_csv(here("data/raw_data", "SPSD.csv"), col_names = TRUE, name_repair="universal")

<<<<<<< HEAD
func_country = function(countryfilename){
              
              #using textreadr
              doc_country <- textreadr::read_docx(countryfilename)
              imstart = doc_country %>% str_which(pattern = "IMPLEMENTING MECHANISM SUMMARY")
              imend = doc_country %>% str_which(pattern = "KEY ISSUE, SPSD, and PROGRAM SUMMARY")
              
              country1 <- doc_country[1:(imstart-1)]
              country2 <- doc_country[imstart:(imend-1)]
              country3 <- doc_country[imend:length(doc_country)]
              
              country2 %<>% str_c(collapse = "@")
              
              countryIM <- country2 %>% str_split(pattern = "IM [:digit:]{5,6}:", simplify = TRUE)
              countryIM <- as_tibble(countryIM[-1])
              
              ##########################################################################################
              
             countryIM %<>%
                
                separate(value,c("IM.info","IM.narrative","IM.funding"), 
                         sep="IMPLEMENTING MECHANISM NARRATIVE|FUNDING SUMMARY") %<>%
                
                separate(IM.info,c("IM.heading","IM.table"),sep="Mechanism Number") %<>%
                
                select(-IM.heading) %<>%
                
                separate(IM.table,c("IM.number","IM.Name","Prime.Partner","Award.Number","Implementing.Mechanism.Type","Source.Agency","Implementing.Agency","Planned.Funding","Start.Date","End.Date","Total.Estimated.Cost"),
                         sep = "Implementing Mechanism Name|Prime Partner|Award Number|Implementing Mechanism Type|Source Agency|Implementing Agency|Planned Funding|Start Date|End Date|Total Estimated Cost", 
                         convert = TRUE) %<>%
                #split IM.funding here
                
                mutate(across(.cols = IM.number:IM.funding,
                              stringr::str_remove_all, "@|:@")
                       ) %<>%
                
                mutate(IM.narrative = str_squish(IM.narrative)) %<>%
              
                mutate(country= str_remove(countryfilename," Full OP Report Approved.docx"),
                       youth = ifelse(str_detect(IM.narrative,"youth"),"Youth",NA),
                       fp.rh = ifelse(str_detect(IM.funding,"HL\\.7\\."),"FP.RH",NA),
                       peace.security = ifelse(str_detect(IM.funding,"PS\\."),"Peace and security",NA),
                       democracy.humanrights.gov = ifelse(str_detect(IM.funding,"DR\\."),"Democracy and human rights",NA),
                       health = ifelse(str_detect(IM.funding,"HL\\."),"Health",NA),
                       edu.socialservices = ifelse(str_detect(IM.funding,"ES\\."),"Education and social services",NA),
                       eco.growth = ifelse(str_detect(IM.funding,"EG\\."),"Economic Growth",NA),
                       human.ass = ifelse(str_detect(IM.funding, "HA\\."),"Humanitarian Assistance",NA),
                       prog.dev.oversight = ifelse(str_detect(IM.funding,"PO\\."),"Program Dev & Oversight",NA)
                      ) %<>%
                
                unite("Program.Type",youth:fp.rh, sep="/", na.rm=TRUE, remove=FALSE) %<>%
                unite("sectors",peace.security:prog.dev.oversight, sep="/", na.rm=TRUE, remove=FALSE) %<>%
                
                separate(country,c("country","year"),sep="(?=FY)") %<>%
                separate(country,c("country","date"),sep="(?=\\d{1,2}_\\d{1,2}_\\d{4})") %<>%
          #TODO date not captured properly      
                write_csv(file = here("data/wrangled_data", str_c(countryfilename,"IM.csv")))
                
              return(countryIM)
              }

#countryfilenames="Mozambique 11_16_2021 FY 2021 Full OP Report Approved.docx"
allIM <- map_dfr(.x = countryfilenames, .f = func_country)

write_csv(allIM, file = here("data/wrangled_data", "allIM.csv"))

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
=======
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
         IM.narrative= str_squish(IM.narrative), #unlike trim, squish will remove spaces inbetween as well as leading and trailing
         IM.funding=str_trim(IM.funding)) %<>%
  write_csv(file = here("data/wrangled_data", "mozambiqueIM.csv"))

mozambiqueIM[2]
>>>>>>> ef4af8c58add0419bc306b45f8b26c89b003cde7
