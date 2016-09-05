### Convert HCPCS (or CPT) procedure codes to CCS codes for further analysis. 
### 
# 
# Janos Perge, 09/05/2016
# 
# To run this analysis:  
# -Clone this github repository on your hard drive (git@github.com:jperge/CMS_procedures_per_provider.git)
# 
# -All analyzis is written in R. If you are using RStudio, make sure to manually set the working directory to this source file location
# (under Session/Set Working Directory/To Source File Location
# 
# To convert HCPCS (or CPT) codes 
# to CCS codes I use a conversion table available by HCUP (https://www.hcup-us.ahrq.gov). 
# This table is already downloaded in this repository (2016_ccs_services_procedures.csv), 
# but is also available online with further information on the conversion:  
# https://www.hcup-us.ahrq.gov/toolssoftware/ccs_svcsproc/ccssvcproc.jsp#info
# 

rm(list=ls())

##Set working directory:
##If running this script in RStudio:
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

##if running this in base R, try:
# File <- "procedures_by_provider.R"
# Files <- list.files(path=file.path("~"),recursive=T,include.dirs=T)
# Path.file <- names(unlist(sapply(Files,grep,pattern=File))[1])
# Dir.wd <- dirname(Path.file)
# setwd(Dir.wd)

packageList = c("data.table","stringr",'plyr')

is_installed <- function(mypkg) is.element(mypkg, installed.packages()[,1])

load_or_install<-function(package_names)
{
  for(package_name in package_names)
  {
    if(!is_installed(package_name))
    {
      install.packages(package_name,repos="http://lib.stat.cmu.edu/R/CRAN")
    }
    options(java.parameters = "-Xmx8g")
    library(package_name,character.only=TRUE,quietly=TRUE,verbose=FALSE)
  }
}

load_or_install(packageList)

cms_filename = "Medicare_Provider_Util_Payment_PUF_CY2012.txt" #data is also available on CMS for years 2013 and 2014
my_data_file = "procedures2012.RData"

start = Sys.time()
#open data from tabular or Rdata file saved on HD:
if(file.exists(my_data_file) && !exists("physician_data")){
  load(my_data_file)
} else if(!file.exists(my_data_file)) {
  physician_data = data.frame(fread(cms_filename)) #This second way of reading data is ~5 times faster!
  physician_data = physician_data[2:nrow(physician_data),]
  colnames(physician_data) = tolower(colnames(physician_data))

  # save(physician_data, file=my_data_file)
}
Sys.time()-start

head(physician_data)

colnames(physician_data)

#physician data is large and clogs memory. Therefore I take what I need and clear the rest from the workspace:
descriptor_vars = c("npi", "nppes_provider_last_org_name", "nppes_provider_first_name", "nppes_provider_mi", 
                    "nppes_credentials", "nppes_provider_gender", "nppes_entity_code", "nppes_provider_street1", 
                    "nppes_provider_street2", "nppes_provider_city", "nppes_provider_zip", "nppes_provider_state", 
                    "nppes_provider_country", "provider_type", "medicare_participation_indicator", "place_of_service")

physician_info   = physician_data[, descriptor_vars]
doctor_procedure = physician_data[, c('npi', 'hcpcs_code','line_srvc_cnt',"bene_unique_cnt", "bene_day_srvc_cnt", 
                                      "average_medicare_allowed_amt", "stdev_medicare_allowed_amt", 
                                      "average_submitted_chrg_amt", "stdev_submitted_chrg_amt", "average_medicare_payment_amt",
                                      "stdev_medicare_payment_amt")]
conversion_table = physician_data[, c('hcpcs_code', 'hcpcs_description')]
rm(physician_data)

#-----------------------------------------------------------------------------
## Spreadsheet #1: Doctor's parameters such as NPI, Name, state, gender, etc...
#-----------------------------------------------------------------------------
#bring in physician years of expertise:
npi_file = 'physician_grad_year.csv'
npi_frame = data.frame(fread(npi_file))
colnames(npi_frame) = tolower(colnames(npi_frame))

physician_info = data.table(physician_info)
setkey(physician_info, npi)
physician_info = unique(physician_info)
physician_info = merge(physician_info, npi_frame, all.x=TRUE) #left outer join

#-----------------------------------------------------------------------------
# Spreadsheet #2: HCPCS/CPT code to CCS conversion¶
#-----------------------------------------------------------------------------
conversion_table = conversion_table[!duplicated(conversion_table$hcpcs_code),]
conversion_table = data.table(conversion_table)
setkey(conversion_table, hcpcs_code)

ccs_file = '2016_ccs_services_procedures.csv'
ccs_table = data.table(read.csv(ccs_file))
ccs_table$Code.Range <- as.character(ccs_table$Code.Range)
setkey(ccs_table,Code.Range,CCS,CCS.Label)
head(ccs_table)

# combine the above two tables

#create an incremental sequence of CSS codes from Code.Range:
get_code_range <- function(inp,ccscode,ccsdesc){
    
    code_range <- vector(mode="numeric", length=0) #empty 
    
    aaa = unlist(strsplit(inp, "-", fixed = TRUE))    
    aaa = sub("\'", "", aaa)
    
    if (!grepl("[a-zA-Z]", aaa[1])){  #if code does not contain letters
        aaa = as.numeric(aaa)
        code_range = seq.int(aaa[1],aaa[2])
        code_range = sprintf("%05d", code_range) # fixed width of five characters with leading zeros
        code_range = as.character(code_range)
        
    } else #if hcpcs code is alphanumeric, with the numeric part as an incremental sequence
    {
        bbb = substring(aaa[1], seq(1,nchar(aaa[1])), seq(1,nchar(aaa[1]),1)) #break up string to individual characters
        letterPos = grep("[a-zA-Z]", bbb, value = FALSE)
        letterChar = grep("[a-zA-Z]", bbb, value = TRUE)

        numericPart1 = grep("[0-9]", bbb, value = TRUE)
        numericPart1 = as.numeric(paste(numericPart1, collapse=""))

        bbb = substring(aaa[2], seq(1,nchar(aaa[2])), seq(1,nchar(aaa[2]),1)) 
        numericPart2 = grep("[0-9]", bbb, value = TRUE)
        numericPart2 = as.numeric(paste(numericPart2, collapse=""))

        cr  = seq.int(numericPart1,numericPart2)
        
        if (letterPos==1){
            code_range = sprintf("%s%04d", letterChar, cr) # fixed width of four characters with leading zeros
        } else
        {
            code_range = sprintf("%04d%s", cr, letterChar)
        }            
    }
    out = data.frame(hcpcs_code=code_range, ccs_code = ccscode, css_desc = ccsdesc)
}

# Expand CCS table, i.e. list every HCPCS code specified within the Code.Range
expanded_ccs = with(ccs_table, Map(get_code_range, Code.Range, CCS, CCS.Label))
expanded_ccs = rbind.fill(expanded_ccs)
expanded_ccs = data.table(expanded_ccs) 
setkey(expanded_ccs,hcpcs_code)
conversion_table = merge(conversion_table,expanded_ccs, by='hcpcs_code')

head(conversion_table)

#-----------------------------------------------------------------------------
## Spreadsheet #3: Provider vs Procedure count.  
#-----------------------------------------------------------------------------
#cells in the final matrix (npi_vs_css) correspond to total procedure counts for a given provider and given CCS category

# Merge CCS codes into physician data frame:
toAppend = conversion_table[, .(hcpcs_code ,ccs_code)]
setkey(toAppend,hcpcs_code)

head(toAppend)

doctor_procedure = data.table(doctor_procedure)
setkey(doctor_procedure, "hcpcs_code")
doctor_procedure = merge(doctor_procedure, toAppend, by='hcpcs_code')

head(doctor_procedure)

#pool similar procedures (with identical ccs codes) per provider
setkey(doctor_procedure, "npi","ccs_code")
doctor_procedure = doctor_procedure[,proc_per_patient := bene_day_srvc_cnt/bene_unique_cnt]
npi_vs_ccs = doctor_procedure[, .(proc_cnt=sum(line_srvc_cnt, na.rm=T), 
                                  uniq_cnt=sum(bene_unique_cnt, na.rm=T),
                                  day_srvc_cnt=sum(bene_day_srvc_cnt, na.rm=T),
                                  bene_cnt=sum(bene_unique_cnt, na.rm=T),
                                  med_proc_per_patient=median(proc_per_patient, na.rm=T), 
                                  est_allo_amt=sum(average_medicare_allowed_amt*line_srvc_cnt, na.rm=T), 
                                  est_pay_amt=sum(average_medicare_payment_amt*line_srvc_cnt, na.rm=T)), 
                              by = .(npi,ccs_code)] 

tail(npi_vs_ccs)

#These procedure codes will be used to estimate the average length of outpatient office visits:
sub = conversion_table[hcpcs_code %in% c('99211', '99212', '99213', '99214', '99215')]
sub

#calculate the average duration of office visits per provider and merge it into physician_info table
#use hcpcs codes 99211-215 to estimate office visit durations
#Half of the providers (~400,000) have these office visit codes
setkey(doctor_procedure,hcpcs_code)
office_visits = doctor_procedure[hcpcs_code %in% c('99211', '99212', '99213', '99214', '99215')]
setkey(office_visits, hcpcs_code)
office_visits = office_visits[,cpt := as.numeric(as.character(hcpcs_code))]

setkey(office_visits, cpt)
office_visits = office_visits[cpt==99211,visit_dur := 5]
office_visits = office_visits[cpt==99212,visit_dur := 10]
office_visits = office_visits[cpt==99213,visit_dur := 15]
office_visits = office_visits[cpt==99214,visit_dur := 25]
office_visits = office_visits[cpt==99215,visit_dur := 40]

office_visits = office_visits[, tot_office_mins:= visit_dur*line_srvc_cnt]
setkey(office_visits, npi)
office_per_doc = office_visits[,.(office_mins = sum(tot_office_mins,na.rm=T)/sum(line_srvc_cnt,na.rm=T)), by=npi]

#merge this into physician info table:
physician_info = merge(physician_info, office_per_doc, all.x=TRUE)


#-------------------------------------------------------------------------
## Save the results
#-------------------------------------------------------------------------
start = Sys.time()
# write.csv(physician_info, file = "physician_info.csv", row.names=FALSE, na="")
# write.csv(conversion_table, file = "CPT_to_CCS_conversion.csv", row.names=FALSE, na="")
# write.csv(npi_vs_ccs, file = "provider_vs_procedures2012.csv", row.names=FALSE, na="")

#Note that write.csv saves numeric hcpcs codes as an arabic number e.g. '102', omiting zero characters in the beginning.
#This would cause mismatches later when mapping fixed-length character arrays such as '00102'. 
#However, R's native data format saves the codes with leading zeros.

#R.data file:
save(physician_info, conversion_table, npi_vs_ccs, file='provider_vs_procedures_2_2012.RData')

Sys.time()-start

rm(list=ls())

