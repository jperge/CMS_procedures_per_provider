# Medical procedures by physician, Medicare data (2012)
# Janos Perge, 07/21/2016
# Purpose:
# 1) Access Medicare data on the number of performed medical procedures per physician (as a proxi for quality of care).
# 2) Convert HCPCS (or CPT) procedure codes to CCS codes for further analysis
# Data is a public use file downloadable from CMS: https://www.cms.gov/apps/ama/license.asp?file=http://download.cms.gov/Research-Statistics-Data-and-Systems/Statistics-Trends-and-Reports/Medicare-Provider-Charge-Data/Downloads/Medicare_Provider_Util_Payment_PUF_CY2012_update.zip
# To run this code, first visit the above url, accept CMS disclaimer, download and unzip file (2GB) and place it in CMS directory (or a location of your choice). Opening and saving file takes about 1 min on my Win10 machine with core i7 and 8GB RAM.
# packageList = c("jsonlite", "data.table", "parallel","foreach","stringr","ggplot2","reshape",'plyr','rjson')

#Obtain packages:

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

#1) Access and explore data:

setwd("C:/Users/bubuk/Documents/R/medicare-analysis")
physician_medicare = "CMS/Medicare_Provider_Util_Payment_PUF_CY2012.txt" #available data for years 2012,2013,2014
my_data_file = "procedures2012.RData"

#open data from tabular file saved on HD or from Rdatafile:
if(file.exists(my_data_file) && !exists("pm")){
  load(my_data_file)
} else if(!file.exists(my_data_file)) {
  pm = read.delim(physician_medicare, stringsAsFactors=FALSE)
  pm = pm[2:nrow(pm),]
  colnames(pm) = tolower(colnames(pm))

  save(pm, file=my_data_file)
}

descriptor_vars = c("npi", "nppes_provider_last_org_name", "nppes_provider_first_name", "nppes_provider_mi", 
                    "nppes_credentials", "nppes_provider_gender", "nppes_entity_code", "nppes_provider_street1", 
                    "nppes_provider_street2", "nppes_provider_city", "nppes_provider_zip", "nppes_provider_state", 
                    "nppes_provider_country", "provider_type", "medicare_participation_indicator", "place_of_service")

payment_vars = c("npi", "hcpcs_code", "line_srvc_cnt", "bene_unique_cnt", "bene_day_srvc_cnt", 
                "average_medicare_allowed_amt", "stdev_medicare_allowed_amt", "average_submitted_chrg_amt", 
                "stdev_submitted_chrg_amt", "average_medicare_payment_amt", "stdev_medicare_payment_amt")

sel_pm = pm[, payment_vars]
sel_pm = data.table(sel_pm)
setkey(sel_pm, "npi") 

#coarse measures on payment and patient numbers:
phys_summ = sel_pm[
  , 
  list(
    service_total=sum(line_srvc_cnt),
    ben_total=sum(bene_unique_cnt),
    payment=sum(average_medicare_payment_amt * line_srvc_cnt),
    charged=sum(average_submitted_chrg_amt * line_srvc_cnt),
    allowed=sum(average_medicare_allowed_amt * line_srvc_cnt),
    unique_services_per_patient=sum(bene_day_srvc_cnt)/sum(bene_unique_cnt),
    duplicates_per_service=sum(line_srvc_cnt)/sum(bene_day_srvc_cnt),
    services_per_patient=sum(line_srvc_cnt)/sum(bene_unique_cnt)
  ),
  by="npi"
  ]

# 2) Convert HCPCS/CPT codes into CCS codes:

df = pm[, c('hcpcs_code', 'hcpcs_description')]
df = df[!duplicated(df$hcpcs_code),]
# write.csv(df, file = "cptlist12.csv", row.names=FALSE, na="")
# df = read.csv("cptlist12.csv")
df$ccs_code = 0
df$ccs_desc = 'none'
df = arrange(df, hcpcs_code)

table_file = '2016_ccs_services_procedures.csv'
ccs_table = read.csv(table_file)
ccs_table$Code.Range <- as.character(ccs_table$Code.Range)

#create an incremental sequence of CSS codes using Code.Range:
get_code_range <- function(inp){    
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
    code_range
}

#find corresponding css code and description for each hcpcs code:
for (i in 1:nrow(ccs_table)){
    codeRange = get_code_range(ccs_table$Code.Range[i])
    if(length(codeRange) != 0) {
        df$ccs_code[(df$hcpcs_code %in% codeRange * 1)>0] = ccs_table$CCS[i]
        df$ccs_desc[(df$hcpcs_code %in% codeRange * 1)>0] = as.character(ccs_table$CCS.Label[i])
    }
}
df = arrange(df, hcpcs_code)
df$hcpcs_code = as.character(df$hcpcs_code)

#matching efficiency:
sum(df$ccs_code>0)/nrow(df)

#save conversion table as a csv file:
df$hcpcs_code = sprintf("%05s", df$hcpcs_code) # fixed width with leading zeros
write.csv(df, file = "CPT_to_CCS_conversion.csv", row.names=FALSE, na="")
#potential error in future analysis: .csv file saves numeric hcpcs codes as an arabic number e.g. '102', ommiting zero characters in the beginning.
#This would cause mismatches later when mapping fixed-length character arrays such as '00102'. 
#Need to fix this: what are other ways of forcing write.csv to save as a fixed-length character array?

#Currently not as pressing, since R's native data format saves the codes correctly: 
save(df, file='CPT_to_CSS_Conv_Table.RData')

##These HCPCS codes don't have a match in the CCS table:
subset(df, ccs_code==0)

# Merge CCS codes into physician data frame:
toAppend = df
toAppend = toAppend[,-2] ## drop hcpcs description (redundant with other dataframe)

#pm = merge(pm, toAppend, by='hcpcs_code') #runs out of memory, either if used on pm or on a subset of pm. Only works with data.Table!

pm = data.table(pm)
setkey(pm, "hcpcs_code") 
pm = merge(pm, toAppend, by='hcpcs_code')

my_data_file2 = "procedures2012_with_CCS.RData"
save(pm, file=my_data_file2)


