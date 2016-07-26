
# Medical procedures by physician, Medicare data (2012)
# Janos Perge, 07/25/2016
# 
# Purpose:
#   1) Access Medicare data on the number of performed medical procedures per physician (as a proxi for quality of care).
#   2) Convert HCPCS (or CPT) procedure codes to CCS codes for further analysis
#   3) Save data in three spreadsheets: #1-Provider information, #2-CPT-to-CCS conversion table and #3-Number of performed procedures per provider, broken down to different CCS categories (244 different procedure types)
#   
# Before running this code:
#   -Get the data. This is a public use file downloadable from CMS: 
#   https://www.cms.gov/apps/ama/license.asp?file=http://download.cms.gov/Research-Statistics-Data-and-Systems/Statistics-Trends-and-Reports/Medicare-Provider-Charge-Data/Downloads/Medicare_Provider_Util_Payment_PUF_CY2012_update.zip Visit the url above, accept CMS disclaimer, download and unzip file (2GB) and place it within the same directory as this script.
#   -I assume you run this script in RStudio. If running base-R, you should manually set the working directory to the data-containing folder, or uncomment lines 27-31.
#   -Note that most of the runtime is spent on opening 2GB of data and dumping it into RAM (~5min on my Win10 machine with core i7 and 8GB RAM) 
#   or writing the results to disc (~1.5 min). The actual processing time of the data on my machine took ~40sec. 
#   If you need to run this code repeatedly, uncomment line 62, "save(physician_data, file=my_data_file)", to save the entire CMS spreadsheet into an .RData file. This is smaller and faster to read than the original data file.
#
# Data is described in detail in 'Medicare-Physician-and-Other-Supplier-PUF-Methodology.PDF'.
# Further information on the Code conversion can be found here:
# https://www.hcup-us.ahrq.gov/toolssoftware/ccs_svcsproc/ccssvcproc.jsp#info

rm(list=ls())  
cms_filename = "Medicare_Provider_Util_Payment_PUF_CY2012.txt" #available data for years 2012,2013,2014
my_data_file = "procedures2012.RData"

# Set working directory:
# If running this script in RStudio:
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#if running this in base-R, try:
# File <- "procedures_by_provider.R"
# Files <- list.files(path=file.path("~"),recursive=T,include.dirs=T)
# Path.file <- names(unlist(sapply(Files,grep,pattern=File))[1])
# Dir.wd <- dirname(Path.file)
# setwd(Dir.wd)

#Load packages:
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

start = Sys.time()
#open data from tabular file saved on HD or from Rdatafile:
if(file.exists(my_data_file) && !exists("physician_data")){
  load(my_data_file)
} else if(!file.exists(my_data_file)) {
  #physician_data = read.delim(cms_filename, stringsAsFactors=FALSE)
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

coreStart = Sys.time()

physician_info   = physician_data[, descriptor_vars]
doctor_procedure = physician_data[, c('npi', 'hcpcs_code','line_srvc_cnt' )]
conversion_table = physician_data[, c('hcpcs_code', 'hcpcs_description')]
rm(physician_data)

#Spreadsheet #1: Doctor's parameters such as NPI, name, gender, etc..:
physician_info = physician_info[!duplicated(physician_info$npi),]  # no time saved by converting to a data.table 

#Spreadsheet #2: CHPCS/CPT code to CCS conversion:
conversion_table = conversion_table[!duplicated(conversion_table$hcpcs_code),]
conversion_table = data.table(conversion_table)
setkey(conversion_table, hcpcs_code)
head(conversion_table)

ccs_file = '2016_ccs_services_procedures.csv'
ccs_table = data.table(read.csv(ccs_file))
ccs_table$Code.Range <- as.character(ccs_table$Code.Range)
setkey(ccs_table,Code.Range,CCS,CCS.Label)
head(ccs_table)

# combine the above two tables:

#create an incremental sequence of CSS codes from Code.Range. 
#E.g. an input of 'T0020-T0022' would result: ['T0020', 'T0021', 'T0022']:
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

##Spreadsheet #3: Provider vs Procedure count:
##cells in the final matrix (npi_vs_css) correspond to total procedure counts for a given provider and given CCS category
##Rows: provider (npi as an ID), columns: CCS code.

# Merge CCS codes into physician data frame:
toAppend = conversion_table[, .(hcpcs_code ,ccs_code)]
setkey(toAppend,hcpcs_code)
head(toAppend)

doctor_procedure = data.table(doctor_procedure)
setkey(doctor_procedure, "hcpcs_code")
doctor_procedure = merge(doctor_procedure, toAppend, by='hcpcs_code')

setkey(doctor_procedure, "npi","ccs_code")
npi_vs_css = doctor_procedure[, .(proc_count = sum(line_srvc_cnt)), by = .(npi,ccs_code)] #pool similar procedures (with identical ccs codes)
setkey(npi_vs_css, "npi","ccs_code")
npi_vs_css= dcast(npi_vs_css, npi ~ ccs_code) #convert data.table from long to wide format (i.e. npi vs. ccs table)
head(npi_vs_css)

coreProcessingT = Sys.time()-coreStart
coreProcessingT

## Save the three spreadsheets:
start = Sys.time()
#csv files:
# write.csv(physician_info, file = "physician_info.csv", row.names=FALSE, na="")
# write.csv(conversion_table, file = "CPT_to_CCS_conversion.csv", row.names=FALSE, na="")
# write.csv(npi_vs_css, file = "provider_vs_procedures2012.csv", row.names=FALSE, na="")

#Note that write.csv saves numeric hcpcs codes as an arabic number e.g. '102', omiting zero characters in the beginning.
#This would cause mismatches later when mapping fixed-length character arrays such as '00102'. 
#However, R's native data format saves the codes with leading zeros.

#R.data file:
save(physician_info, conversion_table, npi_vs_css, file='provider_vs_procedures2012.RData')

savetime = Sys.time()-start
savetime
