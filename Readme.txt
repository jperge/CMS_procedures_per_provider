Explore medical procedures and costs in healthcare claims data (Medicare, 2012)

Janos Perge, 08/11/2016

Purpose:  

1) Access data on the number and cost of medical procedures performed by Medicare providers.
2) Convert HCPCS (or CPT) procedure codes to CCS codes for further analysis.
3) Save data in three spreadsheets: #1-Provider information, #2-CPT-to-CCS conversion table and #3-Number of performed procedures per provider, broken down to different CCS categories (244 different procedure types).
4) Explore and visualize this saved data set by running 'visualize_procedures.ipynb'.

To look at the source code, data.frames and the figures:
- Open 'procedures_by_provider2.ipynb' and 'visualize_procedures2.ipynb' within Github.

To repeat the analyses:
-Clone this github repository on your hard drive (git@github.com:jperge/CMS_procedures_per_provider.git)
-Get the Medicare-provider-charge data. This is a publicly available file on medical procedures and the associated cost performed by Medicare providers during year 2012. The data is downloadable from CMS: https://www.cms.gov/apps/ama/license.asp?file=http://download.cms.gov/Research-Statistics-Data-and-Systems/Statistics-Trends-and-Reports/Medicare-Provider-Charge-Data/Downloads/Medicare_Provider_Util_Payment_PUF_CY2012_update.zip Visit the url above, accept the CMS disclaimer, download and unzip file (2GB) and place it within the same directory as this script. The data is described in detail in 'Medicare-Physician-and-Other-Supplier-PUF-Methodology.PDF', included in this repository.
-All analyzis is written in R. If you are using RStudio, make sure to manually set the working directory to this source file location (under Session/Set Working Directory/To Source File Location
-Note that most of the runtime is spent on opening 2GB of data and dumping it into RAM (~1min on my Win10 machine with core i7 and 8GB RAM) or writing the results to disc (~1 min). The actual processing time of the data on my machine took ~40sec. If you need to run this code repeatedly, uncomment line "save(physician_data, file=my_data_file)", which will save the entire CMS spreadsheet into an .RData file. This file is smaller and faster to read than the original data file.
-Run 'procedures_by_provider2.r' to open, rearrange and save data.
-Run '
 
To simplify (or compress) the roughly 9000 procedure codes into 244 categories, I convert HCPCS (or CPT) codes to CCS codes using a conversion table available by HCUP (https://www.hcup-us.ahrq.gov). This table is already downloaded in this repository (2016_ccs_services_procedures.csv), but is also available online with further information on the conversion:
https://www.hcup-us.ahrq.gov/toolssoftware/ccs_svcsproc/ccssvcproc.jsp#info

To obtain information on the provider's graduation year, I use a different data source from CMS (https://data.medicare.gov/data/physician-compare) and merge it into this data set using NPI as a cross link. The extracted graduation years are already included in this repository (physician_grad_year.csv). However, if you wish to repeat the process, the source code is in 'obtain_gradyear.ipynb'.

Big thanks to Vik Paruchuri for providing example code which got me started on this CMS data set! 
(http://www.vikparuchuri.com/blog/exploring-us-healthcare-data/). 
Further analyses on this data set can be also found on Propublica (https://www.propublica.org/series/examining-medicare).