## ----configure_knitr, eval = TRUE, echo=FALSE---------------------------------
knitr::opts_chunk$set(echo = TRUE, collapse = TRUE, comment = '#>')
knitr::opts_chunk$set(echo = TRUE,out.width=700, fig.align='center') #figure out.width=700px
options(rmarkdown.html_vignette.check_title = FALSE)

# Clear memory
rm(list=ls()) 

# Useful function
`%!in%`<- Negate(`%in%`)

## ----runchunks, eval = TRUE---------------------------------------------------
# Set whether or not the following chunks will be executed (run):
run.simulations <- FALSE # Change FALSE to TRUE here to make the vignette work
make.plots <- FALSE # change to TRUE to make the plots
run.simulations.2 <- FALSE #for second part of vignette
make.plots.2 <- FALSE # change to TRUE to make the plots

## ----setup, eval = TRUE-------------------------------------------------------
# Set working directory
loc.wd <- "C:/Users/jwambaug/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/Research Projects/DermalTK/vingette"
#loc.wd <- "/Users/ameade/Library/CloudStorage/GoogleDrive-aemeade7@gmail.com/My Drive/EPA/HTTK-Dermal/HTTK-Dermal-Vignette"

#loc.wd = "C:/Users/cschacht/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/CCTE_2023_2024/dermal/thisone"

# Set location of httk package
#loc.httk <- "C:/Users/jwambaug/git/httk-dev/httk"


# Load packages
packages <- c("httk","dplyr",
              "tidyverse","xlsx","Metrics", #for data manipulation
              "ggplot2","ggforce","ggpubr","ggrepel","viridis", #for plotting
              "knitr", "ggpubr","grid","ggh4x","readr","ggforce","knitr","tidyr",
              "stringr") #to make pretty visual tables

sapply(packages, require, character.only=TRUE)

# Make sure we're using a clean version of the httk chemical library:
#reset_httk()

## ----1_load_data, eval = run.simulations--------------------------------------
# # Load CvT table database
# load(paste0(loc.wd,"/R_CvT_Database/CvT_all_tables_2022-08-17.Rdata")) #Necessary file that is not included yet
#   # Load CvT data that is not yet in CvT database (Annabel did this)
#   load(paste0(loc.wd,"/R_CvT_Database/CvT_all_tables_2023-07-05_noSQL.Rdata")) #Necessary file that is not included yet
# 
#   #Add cumulative_amount column in SQL DB_series (we don't use any cumulative amounts from SQL data in final results)
#   DB_series <- DB_series %>% mutate(cumulative_amount=NA)
# 
# # Bind data sets from database and noSQL collection.
# DB_documents_noSQL<-DB_documents_noSQL[colnames(DB_documents_noSQL) %in% colnames(DB_documents)] #Keep only columns from DB_documents_noSQL that are also found in DB_documents
# DB_documents <- rbind(DB_documents,DB_documents_noSQL)
# DB_studies_noSQL<-DB_studies_noSQL[colnames(DB_studies_noSQL) %in% colnames(DB_studies)]
# DB_studies <- rbind(DB_studies,DB_studies_noSQL)
# DB_chemicals <- rbind(DB_chemicals,DB_chemicals_noSQL)
# DB_subjects <- rbind(DB_subjects,DB_subjects_noSQL)
# DB_series <- rbind(DB_series,DB_series_noSQL)
# DB_conc_time_values_noSQL<-DB_conc_time_values_noSQL[colnames(DB_conc_time_values_noSQL) %in% colnames(DB_conc_time_values)]
# DB_conc_time_values <- rbind(DB_conc_time_values,DB_conc_time_values_noSQL)

## ----1_filter_data, eval = run.simulations------------------------------------
# # Join necessary DBs: conc_time_values, series, studies, subjects
# df.full <- DB_conc_time_values %>%
#   left_join(DB_series,by=c("fk_series_id"="id")) %>%
#   left_join(DB_studies,by=c("fk_study_id"="id")) %>%
#   left_join(DB_subjects,by=c("fk_subject_id"="id")) %>%
#   left_join(DB_documents[,c("id","pmid")],by=c("fk_extraction_document_id" = "id")) %>%
#   dplyr::rename(extraction_document_pmid=pmid) %>%
#   select(!(ends_with(".x") | ends_with(".y"))) %>%
#   filter(!is.na(test_substance_dtxsid)) %>% #remove series w/o defined chemicals (Changed from study to series AG)
#   filter( !is.na(conc)) %>% #filter out NA data
#   filter(!is.na(conc_medium_normalized)) %>% #remove series w/o defined medium (i.e., plasma, blood, urine, etc.)
#   filter(test_substance_dtxsid==analyte_dtxsid | is.na(analyte_dtxsid)) #analyte (measured substance) and test substance must be the same
# 
# df.full <- distinct(df.full)
# 
# # Remove empty cells
# df.full <- type_convert(df.full)
# df.full <- df.full %>% filter(!is.na(conc))
# 
# # Get list of chemicals that have dermal data
# df.dermal <- df.full %>% filter(administration_route_normalized=="dermal")
# dermal_dtxsid <- unique(df.dermal$test_substance_dtxsid)
# 
# # Only keep data for chemicals that have dermal data
# df <- df.full %>% filter(test_substance_dtxsid %in% dermal_dtxsid)

## ----summarize_cvt, eval=run.simulations--------------------------------------
# table <- df %>% mutate(Chemical = get_chem_id(dtxsid=test_substance_dtxsid)$chem.name) %>% dplyr::count(Chemical,test_substance_dtxsid,extraction_document_pmid,sort=F) %>% dplyr::rename(Number.of.Data.Points = n)
# row.names(table) <- 1:nrow(table)
# knitr::kable(table, row.names=TRUE,caption="List of chemicals with dermal data before cleaning data.")
# write.table(table,
#             file=paste0(loc.wd,"/tables/ChemicalswDermalData.txt"),
#             col.names=TRUE,
#             row.names=FALSE,
#             quote=FALSE,
#             sep="\t")

## ----1_supptable, eval=run.simulations----------------------------------------
# # Load chemical info
# supptab1_meade2023 <- read.xlsx(file=paste0(loc.wd,"/CCD-Batch-Search_2022-11-29_07_06_35.xlsx"),
#                                 sheetName="Main Data") #A separate download was needed for this file AG 12/6/23
# 
# # Rename chemical properties and change units
# supptab1_meade2023 <- supptab1_meade2023 %>% mutate(Water.Solubility.mg.L = WATER_SOLUBILITY_MOL.L_OPERA_PRED*AVERAGE_MASS*1000,
#                                                     Vapor.Pressure.mmHg = VAPOR_PRESSURE_MMHG_OPERA_PRED,
#                                                     Boiling.Point.C = BOILING_POINT_DEGC_OPERA_PRED,
#                                                     logP = OCTANOL_WATER_PARTITION_LOGP_OPERA_PRED,
#                                                     MW = AVERAGE_MASS,
# ) %>%
#   select(c(PREFERRED_NAME,DTXSID,Water.Solubility.mg.L,Vapor.Pressure.mmHg,Boiling.Point.C,logP,MW))
# 
# # Add column for "Volatility
# supptab1_meade2023 <- supptab1_meade2023 %>% mutate(Volatility = ifelse(Boiling.Point.C<=75,"Very Volatile",
#                                                                         ifelse(Boiling.Point.C>400,"Not Volatile",
#                                                                                ifelse((Boiling.Point.C>=250)&(Boiling.Point.C<=400),"Semi-Volatile","Volatile"))),
#                                                     # Rename long chemicals to nick-names
#                                                     PREFERRED_NAME = ifelse(PREFERRED_NAME=="Methyl tert-butyl ether","MBTE",PREFERRED_NAME),
#                                                     PREFERRED_NAME = ifelse(PREFERRED_NAME=="3,3',5,5'-Tetrabromobisphenol A","Bromdian",PREFERRED_NAME),
#                                                     PREFERRED_NAME = ifelse(PREFERRED_NAME=="4,4'-Sulfonyldiphenol","Bisphenol S",PREFERRED_NAME),
#                                                     PREFERRED_NAME = ifelse(PREFERRED_NAME=="Dichloromethane","DCM",PREFERRED_NAME),
#                                                     PREFERRED_NAME = ifelse(PREFERRED_NAME=="Tetrachloroethylene","PERC/TCE",PREFERRED_NAME),
#                                                     PREFERRED_NAME = ifelse(PREFERRED_NAME=="1-Methylbenzene","Toluene",PREFERRED_NAME))
# 
# supptab1_meade2023$Volatility <- factor(supptab1_meade2023$Volatility, levels=c("Not Volatile","Semi-Volatile","Volatile","Very Volatile"))
# #save chemical dataframe
# save(supptab1_meade2023,file=paste0("meade2023_",format(Sys.time(), "%b_%d_%Y"),".Rdata"))
# 
# supptab1_meade2023[,c(3:7)]=signif(supptab1_meade2023[,c(3:7)],3)
# knitr::kable(supptab1_meade2023,
#              #row.names=TRUE,
#              caption="Chemical properties",
#              floating.environment="sidewaystable",
#              align="c")
# write.table(supptab1_meade2023,
#             file=paste0(loc.wd,"/tables/ChemicalProperties.txt"),
#             col.names=TRUE,
#             row.names=FALSE,
#             quote=FALSE,
#             sep="\t")
# 

## ----1_load_QSAR, eval=run.simulations----------------------------------------
# load_dawson2021() #get QSAR data, overwrite for p-values greater than 0.05

## ----1_clean_data, eval=run.simulations---------------------------------------
# # Select route (dermal/iv/oral)
# route.ls = c("iv","dermal","oral")
# 
# # Reduce data based on available in vitro data
# # df.reduced <- df %>% filter(administration_route_normalized %in% route.ls) %>% #only iv, oral, dermal routes
# #   filter(test_substance_dtxsid %!in% c("DTXSID4021557","DTXSID4025371","DTXSID3020752")) #no metabolism data for: dibromomethane, halothane, isoflurane
# df.reduced <- df %>% filter(administration_route_normalized %in% route.ls) %>% #only iv, oral, dermal routes
#   filter(!(test_substance_dtxsid %in% c("DTXSID4021557","DTXSID4025371","DTXSID3020752"))) #no metabolism data for: dibromomethane, halothane, isoflurane
# 
# df.reduced <- type_convert(df.reduced) #change dose_level_normalized to numeric
# 
# #Join df.reduced with the Volatility/chemical info from supptab1_meade2023
# df.reduced = df.reduced %>% left_join(supptab1_meade2023,by=c("test_substance_dtxsid"="DTXSID"))
# 
# df.reduced=subset(df.reduced, !(conc_medium_normalized %in%c("feces","urine")))
# # Display which chemicals are being looked at
# table <- df.reduced %>%
#   mutate(Chemical = get_chem_id(dtxsid=test_substance_dtxsid)$chem.name) %>%
#   dplyr::count(Chemical,test_substance_dtxsid,extraction_document_pmid,conc_medium_normalized,sort=F) %>%
#   dplyr::rename("Number.of.Data.Points" = n)
# row.names(table) <- 1:nrow(table)
# knitr::kable(table,
#              row.names=TRUE,
#              caption="List of chemicals with dermal data after cleaning data.")
# write.table(table,
#             file=paste0(loc.wd,"/tables/ChemicalswDermalDataCleaned.txt"),
#             col.names=TRUE,
#             row.names=FALSE,
#             quote=FALSE,
#             sep="\t")

## ----conc_cleanup, eval=run.simulations---------------------------------------
# # Set dummy column for Concentration units
# df.reduced$conc_normalized = NA
# df.reduced$conc_units_normalized = df.reduced$conc_units_original
# 
# allset = which(df.reduced$conc_units_original %in% c("ug/mL","ug/g","mg/kg", "mcg/mL","mg/L"))
# df.reduced$conc_units_normalized[allset]="mg/L"; df.reduced$conc_normalized[allset]=df.reduced$conc_original[allset]
# 
# ngml = which(df.reduced$conc_units_original=="ng/ml");df.reduced$conc_normalized[ngml]=  df.reduced$conc_original[ngml]*0.001
# df.reduced$conc_units_normalized[ngml]="mg/L"
# 
# ngl = which(df.reduced$conc_units_original=="ng/l");df.reduced$conc_normalized[ngl]=  df.reduced$conc_original[ngl]*1e-06
# df.reduced$conc_units_normalized[ngl]="mg/L"
# 
# molunts = which(df.reduced$conc_units_original %in% c("uM","nmol/L","umol/L"))
# dtx=unique(df.reduced[molunts,c("test_substance_dtxsid","conc_units_original")])
# dtx$mw = get_physchem_param(param="MW", dtxsid=dtx$test_substance_dtxsid)
# 
# for(z in 1:dim(dtx)[1]){
#   these.rows=which(df.reduced$test_substance_dtxsid==dtx$test_substance_dtxsid[z] & df.reduced$conc_units_original==dtx$conc_units_original[z])
#   conversion_val = convert_units(input.units = dtx$conc_units_original[z],output.units = "mg/L",dtxsid=dtx$test_substance_dtxsid[z])
#   df.reduced[these.rows,"conc_normalized"] = df.reduced[these.rows,"conc_original"] * conversion_val
#   df.reduced$conc_units_normalized[these.rows] = "mg/L"
# }
# 
# perc.dose=which(df.reduced$conc_units_original %in%c("% dose", "% dose/h"))
# df.reduced[perc.dose, "conc_normalized"]=df.reduced[perc.dose, "conc_original"]*df.reduced[perc.dose, "dose_level_original"]
# #df.reduced[perc.dose,"conc_units_normalized"]=df.reduced[perc.dose,"dose_level_original_units"]
# df.reduced[perc.dose,"conc_units_normalized"]="mg/L"
# # ppbv
# ppbv = which(df.reduced$conc_units_original=="ppbv")
# df.reduced[ppbv, "conc_normalized"] = convert_units(input.units = "ppbv",output.units = "mg/L",dtxsid=unique(df.reduced$analyte_dtxsid[ppbv]),state="gas") * df.reduced[ppbv, "conc_original"]
# df.reduced$conc_units_normalized[ppbv] = "mg/L"
# 
# 

## ----1_convert_dose_units, eval=run.simulations-------------------------------
# # Convert ug/mL to mg
# df.reduced <- df.reduced %>%
#   separate(dose_volume,
#            into=c("dose_volume","dose_volume_units"),
#            sep=" ") %>% #separate dose volume into number and units, units either NA or ml/mL
#   mutate(dose_volume=as.numeric(dose_volume)) %>%
#   mutate(dose_level_new =
#            ifelse(tolower(dose_level_original_units)=="ug/ml",
#                   dose_level_original*dose_volume/1e+3, # ug >- mg, vol in mL
#                   dose_level_original), #convert ug/ml to mg
#          dose_level_new_units =
#            ifelse(tolower(dose_level_original_units)=="ug/ml",
#                   "mg",
#                   dose_level_original_units))
# 
# 
# # Convert ppm units to mg/L (assuming ppm is in air) - add MW to dataframe to do this
# df.reduced <- df.reduced %>%
#   left_join(chem.physical_and_invitro.data[,c("DTXSID","MW")],
#             by=c("test_substance_dtxsid"="DTXSID"))
# df.reduced$MW.y=NULL
# colnames(df.reduced)=gsub("MW.x","MW",colnames(df.reduced)) #fix MW
# df.reduced  = df.reduced %>% #
#   mutate(MW = as.numeric(MW)) %>%
#   group_by(test_substance_dtxsid) %>%
#   mutate(dose_level_new =
#            ifelse(dose_level_original_units=="ppm" ,
#                   dose_level_new *
#                     convert_units(input.units="ppmw", #?? whatever
#                                   output.units = "mg/l",
#                                   MW = unique(MW)),
#                   dose_level_new),
#          # dose_level_new =
#          #   ifelse(dose_level_original_units=="ppm" & administration_route_original!="dermal vapor",
#          #          dose_level_new *
#          #            convert_units(input.units="ppmw", #?? whatever
#          #                          output.units = "mg/l",
#          #                          MW = unique(MW), state="liquid"),
#          #          dose_level_new),
#          dose_level_new_units =
#            ifelse(tolower(dose_level_original_units)=="ppm",
#                   "mg/L",
#                   dose_level_new_units))
# df.reduced$dose_level_new_units[which(df.reduced$dose_level_new_units=="ppm")]="mg/L"
# 
# # Set normalized vehicle
# df.reduced <- df.reduced %>%
#   mutate(dose_vehicle_normalized =
#            ifelse(grepl("corn oil",dose_vehicle) |
#                     grepl("ethanol",dose_vehicle),"octanol",
#                   ifelse(!is.na(dose_vehicle) &
#                            !grepl("Cookie",dose_vehicle),
#                          "water",
#                          NA)))
# 
# # Remove added chemical MW column for now
# df.reduced <- df.reduced %>% select(!MW)
# 
# # Display dosing scenarios
# table <- df.reduced %>%
#   mutate(dose_level_original=signif(dose_level_original,3),
#          dose_level_new = signif(dose_level_new,3),
#          Chemical = get_chem_id(dtxsid=test_substance_dtxsid)$chem.name) %>%
#   count(Chemical,dose_level_original,
#         dose_level_original_units,
#         dose_level_new,
#         dose_level_new_units,
#         dose_volume,
#         dose_volume_units,
#         dose_vehicle,
#         dose_vehicle_normalized,
#         dermal_applied_area,
#         dermal_applied_area_units,sort=F) %>%
#   rename("Number.of.Data.Points" = n)
# row.names(table) <- 1:nrow(table)
# knitr::kable(table,
#              #row.names=TRUE,
#              caption="Dosing Scenarios in CvT Data",
#              floating.environment="sidewaystable",
#              align="c")

## ----summarize_cvt2, eval=run.simulations-------------------------------------
# table <- df %>% mutate(Chemical = get_chem_id(dtxsid=test_substance_dtxsid)$chem.name) %>% dplyr::count(Chemical,test_substance_dtxsid,extraction_document_pmid,sort=F) %>% dplyr::rename(Number.of.Data.Points = n)
# row.names(table) <- 1:nrow(table)
# knitr::kable(table, row.names=TRUE,caption="List of chemicals with dermal data before cleaning data.")

## ----1_updateparms, eval=run.simulations--------------------------------------
# df.new=df.reduced
# 
# df.new$species = tolower(df.new$species)
# # get dose duration in numbers and time units
# df.new$dose_duration_num = as.numeric(str_extract(df.new$dose_duration, "[0-9]+"))
# df.new$dose_duration_units = str_extract(df.new$dose_duration, "[aA-zZ]+")
# df.cases <- df.new %>% mutate(BW = ifelse(!is.na(weight_kg),weight_kg,
#                                                     case_when(
#                                                       species=="rat" ~ 0.4,
#                                                       species=="human" ~ 70,
#                                                       species=="mouse" ~ 0.025,
#                                                       species=="rabbit" ~ 1.6
#                                                     )),
#                                         # Set human default height to 175 cm if not listed
#                                         height_cm = ifelse(species=="human" & is.na(height_cm),175,height_cm),
#                                         totalSA = case_when(
#                                           species=="rat" ~ 9.83*(BW*1000)^(2/3), # formula from Gouma et al., 2012, 195-240grams
#                                           species=="human" ~ sqrt(height_cm * BW/3600) * 100^2,
#                                           species=="mouse" ~ 9.83*(BW*1000)^(2/3),
#                                           species=="rabbit" ~ 100*11*(BW)^(2/3)  # formula from Tadashi et al., 2018
#                                         ),
#                                         #Assume Fskin_exposed = 10% if not stated
#                                         Fskin_exposed = ifelse(!is.na(dermal_applied_area),dermal_applied_area/totalSA,0.1),
#                                         #Infinite Dose if dosing units are concentration mg/l AJG 4/13/24: Double check that mg/l are listed in the correct places in the previous chunk.
#                                         InfiniteDose = ifelse(dose_level_new_units=="mg/L",1,0), # omg
# 
#                                         Vvehicle = ifelse(tolower(dose_volume_units)=="ml",dose_volume/1e3,dose_volume),
#                                         #Standardize time units for dose duration AJG 5/7/24
#                                         dose_duration_units = case_when(
#                                           dose_duration_units %in% c("hr","h","hour","hrs","hours") ~ "hours",
#                                           dose_duration_units %in% c("min","m","minute","minutes","mins") ~ "mins",
#                                           dose_duration_units %in% c("day","d","days") ~ "days"
#                                         ),
#                               dose_duration_num2 = ifelse(dose_duration_units=="mins", dose_duration_num/60, dose_duration_num),
#                               dose_duration_units2 = ifelse(dose_duration_units=="mins", "hours", dose_duration_units)
# 
# 
# )
# # remove experiments where conc_medium_normalized = feces and set observation to collect
# df.cases = subset(df.cases, !(conc_medium_normalized %in%c("urine","feces")))
# df.cases = df.cases%>%
#   mutate(conc_medium_obs = recode(conc_medium_normalized,
#                                   "blood"="Cven",
#                                   "liver"="Cliver",
#                                   "lung"="Clung",
#                                   "kidney"="Ckidney",
#                                   "plasma"="Cplasma",
#                                   "serum"="Cplasma",
#                                   "expired air" = "Aexhaled"
#   ))
# 
# 
# # Display dosing scenarios
# table <- df.cases %>%
#   mutate(dose_level_original=signif(dose_level_original,3),
#          dose_level_new = signif(dose_level_new,3),
#          Chemical = get_chem_id(dtxsid=test_substance_dtxsid)$chem.name) %>%
#   count(Chemical, species,dose_level_original,
#         dose_level_original_units,
#         dose_level_new,
#         dose_level_new_units,
#         dose_volume,
#         dose_volume_units,
#         dose_vehicle,
#         dose_vehicle_normalized,
#         dose_duration_num2,
#         dermal_applied_area,
#         dermal_applied_area_units,
#         extraction_document_pmid,sort=F)
# row.names(table) <- 1:nrow(table)
# knitr::kable(table,
#              #row.names=TRUE,
#              caption="Dosing Scenarios in CvT Data",
#              floating.environment="sidewaystable",
#              align="c")
# write.table(table,
#             file=paste0(loc.wd,"/tables/CvTdbDosingScenarios.txt"),
#             col.names=TRUE,
#             row.names=FALSE,
#             quote=FALSE,
#             sep="\t")
# 

## ----1_mergetable, eval=run.simulations---------------------------------------
# df.cases_new = df.cases
# df.cases_new$Kvehicle2water = df.cases_new$dose_vehicle_normalized # set this parameter
# df.cases_new =  df.cases_new %>% mutate(
#   administration_route_normalized = case_when(
#     administration_route_normalized == "oral" ~ "Oral",
#     administration_route_normalized == "iv" ~ "IV",
#     administration_route_normalized == "dermal" ~ "Dermal"
#   ),
#     #Add exhalation for volatile compounds
#     Exhalation = case_when(
#       levels(Volatility)[Volatility] == "Very Volatile" ~ TRUE,
#       levels(Volatility)[Volatility] == "Volatile" ~ TRUE,
#       levels(Volatility)[Volatility] == "Semi-Volatile" ~ TRUE,
#       levels(Volatility)[Volatility] == "Not Volatile" ~ FALSE
#     ),
#     # Set default vehicle volume when it is unknown. Assume a uniform 1 mm layer of vehicle on skin. Then there is 0.0001 L vehicle per cm^2 of exposed skin. For cases with non-dermal routes of exposure, these values are NA.
#     Vvehicle = case_when(
#       administration_route_normalized=="Dermal" ~ ifelse(!is.na(Vvehicle), Vvehicle, Fskin_exposed*totalSA*0.0001),
#       administration_route_normalized=="Oral" ~ NA,
#       administration_route_normalized=="IV" ~ NA)
# 
# )
# 
# # save what we want
# concentration_data_meade2023 = df.cases_new
# cgwtools::resave(concentration_data_meade2023,file=paste0("meade2023_",format(Sys.time(), "%b_%d_%Y"),".Rdata"))
# 

## ----1_simsetup, eval=run.simulations-----------------------------------------
# 
# # Set output units for dermal model to be same as data
# output.units.original <- c( "Agut"="mg", "Agutlumen"="mg", "Aliver"="mg", "Aven"="mg", "Alung"="mg", "Arest"="mg", "Akidney"="mg", "Atubules"="mg", "Ametabolized"="mg", "Avehicle"="mg", "Ain"="mg",
#                             "Cgut"="mg/L", "Cliver"="mg/L", "Cven"="mg/L", "Clung"="mg/L", "Cart"="mg/L", "Crest"="mg/L", "Ckidney"="mg/L", "Cplasma"="mg/L", "Cvehicle"="mg/L", "Cvehicle_infinite"="mg/L")
# 
# df.simulation = concentration_data_meade2023
# 
# 
# 
# # Find experiments with unique dosing regimes/species/etc and average those that had multiple time points
# # for the same type of experiment. Rename each experiment with new_id.
# keep.cols=c("PREFERRED_NAME","test_substance_dtxsid", "conc_medium_obs", "conc_medium_normalized", "administration_route_normalized", "species", "BW", "dose_level_new", "dose_level_new_units","totalSA","Vvehicle","InfiniteDose","dose_duration_num2","dose_duration_units2","Fskin_exposed","Exhalation","dose_vehicle_normalized","Water.Solubility.mg.L", "Vapor.Pressure.mmHg","Boiling.Point.C","logP", "Volatility", "Kvehicle2water")
# df.simulation_red=data.frame(df.simulation%>%group_by(across(all_of(c("time_hr",keep.cols) )))%>%
#   summarize(conc_normalized=mean(conc_normalized),.groups="drop")%>%group_by(across(all_of(keep.cols)))%>% arrange(time_hr)%>% mutate(new_id=cur_group_id())%>%
#   ungroup())
# 
# df.simulation_red2=df.simulation_red%>%mutate(across(c(dose_duration_num2,dose_duration_units2,Vvehicle),~ifelse(is.na(.x),list(NULL),.x)))

## ----1_simulations,eval=run.simulations---------------------------------------
# df.run.sims = df.simulation_red2
# fkID = unique(df.run.sims$new_id)
# method.perm = c("Potts-Guy", "UK-Surrey")# for
# Kvehicle2water=c("octanol","water") # for
# exhalation_for_simulation = c("TRUE","FALSE")
# 
# 
# conc_predictions_meade2023LIST=list()
# 
# 
# for (include.e in exhalation_for_simulation){
#   for(i in 1:length(fkID)){
#     for(mp in 1:length(method.perm)){
#       for(k in 1:length(Kvehicle2water)){
#         sub.df = subset(df.run.sims, new_id == fkID[i])
#         specs = unique(sub.df[,c("test_substance_dtxsid","conc_medium_obs","administration_route_normalized",
#                                  "species","BW","dose_level_new","dose_level_new_units",
#                                  "totalSA","Vvehicle","InfiniteDose","dose_duration_num2","dose_duration_units2",
#                                 "Fskin_exposed","Exhalation")])
#         # check for experiments with only 1 timepoint and reset sub.df
#         if(length(sub.df$time_hr)==1){
#             sub.df=rbind(sub.df, sub.df[rep(1,1),])
#             sub.df[1,c("conc_normalized","time_hr")]=0
#         }
# 
#         parms = parameterize_dermal_pbtk(dtxsid = specs$test_substance_dtxsid,
#                                  species = specs$species,
#                                  totalSA = specs$totalSA,
#                                  default.to.human=T,
#                                  Kvehicle2water = Kvehicle2water[k],
#                                  InfiniteDose = specs$InfiniteDose,
#                                  method.permeability = method.perm[mp],
# 
#                                  model.type="dermal_1subcomp",
#                                  clint.pvalue.threshold = Inf,
#                                  suppress.messages = T
#                                  )
#         parms$BW = specs$BW
#         parms$Fskin_exposed = specs$Fskin_exposed
# 
#         # Remove exhalation or keep exhalation (except for collected air which is )
#         if(include.e == FALSE) {parms$Qalvc = 0}
# 
#         if(unique(sub.df$PREFERRED_NAME!="DCM"))
#           { # tolerance is bad for DCM, so set tolerance to default values for all other compounds.
#            rtol=1e-5; atol=1e-5
#         }else{
#            rtol=1e-4; atol=1e-4
#         }
# 
# 
#         sub.df=sub.df[order(sub.df$time_hr),]
#         timey = unique(c(0,sub.df$time_hr)) #time in hours
#         gc() # garbage collection (memory)
#         ok=data.frame(solve_dermal_pbtk(parameters = parms,
#                         times = timey/24, #convert to days
#                          route = tolower(specs$administration_route_normalized),
#                          model.type="dermal_1subcomp",
#                          plot = FALSE,
#                          output.units = output.units.original,
#                          washoff = TRUE,
#                          input.units = specs$dose_level_new_units,
#                          initial.dose=specs$dose_level_new,
#                          dose.duration = unlist(specs$dose_duration_num2),
#                          dose.duration.units = unlist(specs$dose_duration_units2),
#                          Vvehicle = unlist(specs$Vvehicle),
#                         #dosing.dermal = dosing.dermal,
#                          Kvehicle2water = Kvehicle2water[k],
#                          suppress.messages = TRUE,
#                         atol = atol, rtol= rtol,
#                         method="lsoda"
#                          ))
#          # extract only times we need
#           ok.time = subset(ok, time%in%c(ok$time[!(is.na(match(ok$time, sub.df$time_hr/24)))]))
#           sub.df$Prediction = ok.time[,c(specs$conc_medium_obs)]
#           sub.df$exhalation_for_simulation = include.e
# 
#           # specify vehicle and perm. method and vehicle if dermal
#           if(specs$administration_route_normalized != "Dermal"){
#             sub.df$method.perm = NA
#             sub.df$Kvehicle2water = NA
#             sub.df$method.name = specs$administration_route_normalized
#           }else{
#             sub.df$method.perm = method.perm[mp]
#             sub.df$Kvehicle2water = Kvehicle2water[k]
#             sub.df$method.name = paste(method.perm[mp], "- vehicle = ", Kvehicle2water[k])
#           }
# 
#         conc_predictions_meade2023LIST[[paste0(fkID[i],".",Kvehicle2water[k],".",method.perm[mp],".exh",include.e)]]= sub.df
#       }
#     }
#   }
# }
# #Bind data together to create a dataframe and organize some columns
#  conc_predictions_meade2023DF = do.call(rbind, conc_predictions_meade2023LIST)
#  rownames(conc_predictions_meade2023DF)=NULL
# 
# 
# 
#  cgwtools::resave(conc_predictions_meade2023DF, conc_predictions_meade2023LIST, file=paste0("meade2023_",format(Sys.time(), "%b_%d_%Y"),".Rdata"))
# 

## ----load_data, eval=(make.plots & !run.simulations)--------------------------
# load(paste0(loc.wd, "/meade2023_Jun_13_2025.Rdata"))

## ----1_prepareData, eval = make.plots-----------------------------------------
# conc_predictions_meade2023DF$method.perm = gsub( "UK-Surrey","Surrey", conc_predictions_meade2023DF$method.perm)
# conc_predictions_meade2023DF$method.name = gsub( "UK-Surrey","Surrey", conc_predictions_meade2023DF$method.name)
# df.sim = conc_predictions_meade2023DF
# # Convert aexhaled air from mg to mg/l (assume a room of x liters, for humans - humans
# # the only species in these exhaled experiments)
# df.sim[which(df.sim$conc_medium_obs=="Aexhaled"),"Prediction"]=df.sim[which(df.sim$conc_medium_obs=="Aexhaled"),"Prediction"]/10000
# 
# df.sim$conc_normalized[which(df.sim$conc_normalized==0)]=1e-10
# df.sim$Prediction[which(df.sim$Prediction==0)]=1e-10
# df.sim.rmsle=df.sim %>% group_by(PREFERRED_NAME, method.name, exhalation_for_simulation)%>%
#   mutate(cmax_obs = max(conc_normalized), cmax_pred=max(Prediction)) %>%
#   mutate(RMSLE = rmse(log10(conc_normalized),log10(Prediction)),
#     Cmax_RMSLE = rmse(log10(cmax_obs), log10(cmax_pred)))
# 
# 
# # Linear fit
# ddf.sim.rmsle <- df.sim.rmsle %>% group_by(method.name) %>% do(
#   {
#     Adjusted.R2 = summary(lm(log10(Prediction) ~ log10(conc_normalized),data=.))$adj.r.squared
#     Coefficient = lm(log10(Prediction) ~ log10(conc_normalized),data=.)$coefficients[2]
#     Intercept = lm(log10(Prediction) ~ log10(conc_normalized),data=.)$coefficients[1]
#     Residuals = lm(log10(Prediction) ~ log10(conc_normalized),data=.)$residuals
#     Fitted.Values = lm(log10(Prediction) ~ log10(conc_normalized),data=.)$fitted.values
#     data.frame(.,Adjusted.R2,Coefficient,Intercept,Residuals,Fitted.Values)
#   }
# )
# 

## ----1_plot3, eval = make.plots, fig.height=10, fig.width=20------------------
# # Plot RMSLE vs Boiling Point
# 
# data = subset(df.sim.rmsle, exhalation_for_simulation==TRUE)
# 
# data=data%>%group_by(PREFERRED_NAME, Boiling.Point.C,method.name)%>%
#   summarize(Chemical.cmax.RMSLE = rmse(log10(cmax_obs),log10(cmax_pred)),
#     Chemical.RMSLE=rmse(log10(conc_normalized),log10(Prediction)))
# 
# data.rect = data.frame(xmin=c(0,75,250,400),
#                        xmax=c(75,250,400,Inf),
#                        ymin=rep(-Inf,4),
#                        ymax=rep(Inf,4),
#                        Volatility=c("Very Volatile",
#                                     "Volatile",
#                                     "Semi-Volatile",
#                                     "Not Volatile"))
# data.rect$Volatility <- factor(data.rect$Volatility, levels=data.rect$Volatility)
# viridis.colors <- viridis(4)
# plot <- ggplot(data=data,aes(x=`Boiling.Point.C`,y=Chemical.RMSLE)) +
#   geom_point()+
#   geom_smooth(method="lm",formula='y~x') +
#   geom_text_repel(data=data,aes(x=`Boiling.Point.C`,y=Chemical.RMSLE,label=PREFERRED_NAME),
#                   max.overlaps=Inf)+
#   facet_wrap(~method.name,scales="free") +
#   #scale_x_log10() +
#   geom_rect(data=data.rect,
#             aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax,fill=Volatility),
#             alpha=0.5,
#             inherit.aes=FALSE) +
#   scale_fill_manual(values=c(`Not Volatile`=viridis.colors[1],
#                              `Semi-Volatile`=viridis.colors[2],
#                              `Volatile`=viridis.colors[3],
#                              `Very Volatile`=viridis.colors[4]))+
# 
#   labs(y="RMSLE",title="Boiling Point vs. RMSLE",x="Boiling Point (C)")+
#   theme_bw() +
#   theme(text=element_text(size=33),
#         plot.title=element_text(hjust=0.5),
#         legend.position="bottom")
# plot
# ggsave(paste(getwd(), "/Figures_May1/BPvsRMSLE_suppl.png",sep=""),
#        width = 20, height = 10, units = "in")
# 
# plot2 <- ggplot(data=data,aes(x=`Boiling.Point.C`,y=Chemical.cmax.RMSLE)) +
#   geom_point()+
#   geom_smooth(method="lm",formula='y~x') +
#   geom_text_repel(data=data,aes(x=`Boiling.Point.C`,y=Chemical.cmax.RMSLE,label=PREFERRED_NAME),
#                   max.overlaps=Inf)+
#   facet_wrap(~method.name,scales="free") +
#   #scale_x_log10() +
#   geom_rect(data=data.rect,
#             aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax,fill=Volatility),
#             alpha=0.5,
#             inherit.aes=FALSE) +
#   scale_fill_manual(values=c(`Not Volatile`=viridis.colors[1],
#                              `Semi-Volatile`=viridis.colors[2],
#                              `Volatile`=viridis.colors[3],
#                              `Very Volatile`=viridis.colors[4]))+
# 
#   labs(y="RMSLE",title="Boiling Point vs. Cmax RMSLE",x="Boiling Point (C)")+
#   theme_bw() +
#   theme(text=element_text(size=33),
#         plot.title=element_text(hjust=0.5),
#         legend.position="bottom")
# #annotate_figure(plot2,top=text_grob("Boiling Point vs RMSLE",size=40,face="bold"))
# ggsave(paste(getwd(), "/Figures_May1/BPvsRMSLE_cmax_suppl.png",sep=""),
#        width = 20, height = 10, units = "in")
# 
# 

## ----1_exhalation_rmsle_plot,  eval=make.plots, fig.height=10, fig.width=30----
# df.exh.compare=df.sim.rmsle
# 
# Exh = data.frame(df.exh.compare %>% group_by(PREFERRED_NAME,method.name, Boiling.Point.C)%>%
#   arrange(exhalation_for_simulation) %>%
#   summarize(Cmax=Cmax_RMSLE[exhalation_for_simulation==FALSE]-Cmax_RMSLE[exhalation_for_simulation==TRUE], Full_time_course = RMSLE[exhalation_for_simulation==FALSE]-RMSLE[exhalation_for_simulation==TRUE]))
# 
# Exh$method.name= sub(" - ", " -\n", Exh$method.name)
# 
# 
# 
# #stack data
# stackExh = data.frame(Exh %>%
#   pivot_longer(cols = c("Cmax", "Full_time_course"),
#                names_to = "ind",
#                values_to = "values"))
# 
# data.text = unique(stackExh[,c("Boiling.Point.C","values","PREFERRED_NAME","ind","method.name")])
# data.rect = data.frame(xmin=c(0,75,250,400),
#                        xmax=c(75,250,400,Inf),
#                        ymin=rep(-Inf,4),
#                        ymax=rep(Inf,4),
#                        Volatility=c("Very Volatile",
#                                     "Volatile",
#                                     "Semi-Volatile",
#                                     "Not Volatile"))
# data.rect$Volatility <- factor(data.rect$Volatility,
#                                levels=data.rect$Volatility)
# viridis.colors <- viridis(4)
# 
# # rename labels for facets
# # labeller=labeller(ind = c(Cmax="Cmax",
# #               Full_time_course = "Full time-course"))
# 
# plot <- ggplot(data=stackExh,aes(x=Boiling.Point.C,y=values)) +
#   geom_point()+ #geom_line()+
#   theme_bw()+
#   geom_smooth(method='lm') +
#   facet_grid(ind~method.name, scales="free", labeller=labeller(ind = c(Cmax="Cmax",
#               Full_time_course = "Full time-course")))+
#   geom_rect(data=data.rect,
#             aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax,fill=Volatility),
#             alpha=0.5,
#             inherit.aes=FALSE) +
#   scale_fill_manual(values=c(`Not Volatile`=viridis.colors[1],
#                              `Semi-Volatile`=viridis.colors[2],
#                              `Volatile`=viridis.colors[3],
#                              `Very Volatile`=viridis.colors[4]))+
#   geom_text_repel(data=data.text,aes(x=Boiling.Point.C,y=values,label=PREFERRED_NAME),
#                   max.overlaps=Inf)+
#   #facet_grid(cols=vars(Route.or.Method.if.Dermal), rows=vars(RMSLE.Type),scales="free_y")+
#   #scale_x_log10() +
#   geom_hline(aes(yintercept=0)) +
#   labs(y=expression(Delta~"RMSLE"),x="Boiling Point (C)",title="Change in RMSLE when Exhalation is Added")+
#   scale_color_brewer(type="qual",palette="Paired") +
#   theme(axis.text.x = element_text(angle=45,vjust=1,hjust=1),
#         plot.title=element_text(hjust=0.5),
#         text=element_text(size=30),
#         legend.position="bottom")
# plot
# 
# ggsave(paste(getwd(), "/Figures_May1/BP_RMSLE_Exhalation_main.png",sep=""),
#        width = 20, height = 10, units = "in")
# 
# # P-values for regression lines
# stackExhStats = stackExh%>% group_by(ind, method.name) %>%
#   summarize(model = list(lm(values~Boiling.Point.C, data=cur_data())),
#             .groups="drop")%>%
#   rowwise()%>%
#   mutate(msummary=list(summary(model)),
#          rsquared=msummary$r.squared,
#          p_val= pf(msummary$fstatistic[1], msummary$fstatistic[2], msummary$fstatistic[3], lower.tail=FALSE)
#         )%>%select(ind,method.name,rsquared,p_val)
# 

## ----cvt_plot_by_exh, eval =make.plots, fig.height=3, fig.width=3-------------
# df.sim2 = df.sim.rmsle
# 
# chem = unique(df.sim2$PREFERRED_NAME)
# 
# for(chx in 1:length(chem)){
#   each.chem = subset(df.sim2, PREFERRED_NAME==chem[chx])
#   #each.chem$dose = paste(each.chem$dose_level_new, each.chem$dose_level_new_units)
#   each.chem$dose=paste(each.chem$dose_level_new, each.chem$dose_level_new_units)
#   # each.chem$dose = factor(each.chem$dose,
#   #                         levels=unique(each.chem$dose)[order(unique(each.chem$dose_level_new))])
# 
#   each.chem$method.name= sub(" - ", " -\n", each.chem$method.name)
#   each.chem$subject=paste(each.chem$species, "BW",each.chem$BW, "kg")
# 
# 
#  ggplot(each.chem)+
#     geom_point(data=each.chem, aes(x=time_hr, y=conc_normalized,colour=dose))+
#     geom_line(data=each.chem, aes(x=time_hr, y=Prediction,colour=dose,linetype=exhalation_for_simulation),linewidth=1)+
#     labs(title=paste(chem[chx], "\n Log-transformed Concentrations v time"), x=" Time (hr)", y="Log Concentrations (mg/L)", colour="")+
#     facet_nested(~conc_medium_normalized+method.name,scales="free")+
#    scale_y_log10() +
#      theme(legend.position = "bottom")
# 
# 
#   ggsave(paste(getwd(), "/Figures_May1/cvt_eachchem_by_exhalation_suppl", unique(each.chem$test_substance_dtxsid),".png",sep=""),
#          width = 8, height = 6, units = "in")
# }
# 

## ----1_plot1,eval =make.plots, fig.height=3, fig.width=3----------------------
# df.new = subset(df.sim.rmsle, exhalation_for_simulation==Exhalation)
# 
# chem = unique(df.new$PREFERRED_NAME)
# 
# for(chx in 1:length(chem)){
#   each.chem = subset(df.new, PREFERRED_NAME==chem[chx])
#   #each.chem$dose = paste(each.chem$dose_level_new, each.chem$dose_level_new_units)
#   each.chem$dose=paste(each.chem$dose_level_new, each.chem$dose_level_new_units)
#   each.chem$dose = factor(each.chem$dose,
#                           levels=unique(each.chem$dose)[order(unique(each.chem$dose_level_new))])
# 
#   each.chem$method.name= sub(" - ", " -\n", each.chem$method.name)
#   each.chem$subject=paste(each.chem$species, "BW",each.chem$BW, "kg")
# 
#   ggplot(each.chem, aes(x=conc_normalized,y=Prediction))+
#     geom_point(aes(colour=dose,shape=conc_medium_normalized))+
#     labs(title=paste(chem[chx], "\n Log-transformed Concentrations"), x="Observed Concentrations (mg/L)", x="Predicted Concentrations (mg/L)", colour="", shape="")+
#     #facet_wrap(~dose,scales="free")+
#     facet_wrap(~method.name,scales="free")+
#     geom_smooth(method='lm',formula='y~x',colour="black")+
#      geom_abline(linetype=2)+
#      #geom_smooth(aes(color=method.name),method='lm',formula='y~x')+
#           scale_x_log10() +
#     scale_y_log10() +
#     theme_bw() + theme(text=element_text(size=11),
#                        strip.text=element_text(size=13))
#     #theme(legend.position = "bottom")
# 
#   ggsave(paste(getwd(), "/Figures_May1/pvo_bydose_suppl", unique(each.chem$test_substance_dtxsid),".png",sep=""),
#          width = 6, height = 6, units = "in")
# 
# 
# }
# 
# 
# # Linear fit for these.
# linear.fit.new <- df.new %>% group_by(method.name) %>% do(
#   {
#     Adjusted.R2 = summary(lm(log10(Prediction) ~ log10(conc_normalized),data=.))$adj.r.squared
#     Coefficient = lm(log10(Prediction) ~ log10(conc_normalized),data=.)$coefficients[2]
#     Intercept = lm(log10(Prediction) ~ log10(conc_normalized),data=.)$coefficients[1]
#     Residuals = lm(log10(Prediction) ~ log10(conc_normalized),data=.)$residuals
#     Fitted.Values = lm(log10(Prediction) ~ log10(conc_normalized),data=.)$fitted.values
#     data.frame(.,Adjusted.R2,Coefficient,Intercept,Residuals,Fitted.Values)
#   }
# )
# # RMSLE grouped by method type
# rmsle.all = df.new%>%group_by(method.name)%>%
#   summarize(  Adjusted.R2 = summary(lm(log10(Prediction) ~ log10(conc_normalized)))$adj.r.squared,
#               RMSLE_total =rmse(log10(Prediction),log10(conc_normalized)),
#                 CMAX_RMSLE_total =rmse(log10(cmax_pred) ,log10(cmax_obs)    )
# 
#               )
# 

## ----1_plot6, eval=make.plots, fig.height=10, fig.width=20--------------------
# df.new = subset(df.sim, exhalation_for_simulation==Exhalation)
# df.sim.rmsle2=df.new %>% group_by(PREFERRED_NAME, method.name)%>%
#   mutate(cmax_obs = max(conc_normalized), cmax_pred=max(Prediction)) %>%
#   mutate(RMSLE = rmse(log10(conc_normalized),log10(Prediction)),
#     Cmax_RMSLE = rmse(log10(cmax_obs), log10(cmax_pred)))
# 
# 
# col=c("firebrick", "indianred", "blue4","steelblue1","#2ca25f", "#addd8e")
# plot.pvo=ggplot(df.sim.rmsle2, aes(x=conc_normalized, y=Prediction))+
#   geom_point(aes( colour=method.name,shape=PREFERRED_NAME), alpha=1,size=2)+
#     labs(title=paste0("(A) Concentrations"),
#        y = "Simulated (mg/L)", x="Observed (mg/L)",
#        color="Route or\n Method",shape="Chemical")+
#     scale_shape_manual(values=1: length(unique(df.sim.rmsle2$PREFERRED_NAME)))+
#     geom_smooth(aes(colour=method.name),method="lm",se=F)+
#   #scale_color_brewer(type="qual",palette="Paired",direction=-1) +
#   scale_colour_manual(values=col)+
#   theme_bw() +
#   scale_x_log10() + scale_y_log10() +
#   geom_abline(lty="dashed") +
#   #coord_fixed(xlim=lims,ylim=lims) +
#   theme(plot.title=element_text(hjust=0.5),
#         text=element_text(size=30))
# 
# plot.cmax = ggplot(df.sim.rmsle2, aes(x=cmax_obs, y=cmax_pred))+
#   geom_point(aes( colour=method.name,shape=PREFERRED_NAME), alpha=1,size=2)+
#     labs(title=paste0("(B) Max Concentrations"),
#        y = "Simulated (mg/L)", x="Observed (mg/L)",
#        color="Route or\n Method", shape="Chemical")+
#   #scale_color_brewer(type="qual",palette="Paired",direction=-1) +
#    scale_colour_manual(values=col)+
#      scale_shape_manual(values=1: length(unique(df.sim.rmsle2$PREFERRED_NAME)))+
#   theme_bw() +
#   scale_x_log10() + scale_y_log10() +
#   geom_abline(lty="dashed") +
#   #coord_fixed(xlim=lims,ylim=lims) +
#   theme(plot.title=element_text(hjust=0.5),
#         text=element_text(size=30))
# 
# ggarrange(plot.pvo, plot.cmax, common.legend=T,legend="right")
# ggsave(paste(getwd(), "/Figures_May1/PvO_main.png",sep=""),
#        width = 20, height = 10, units = "in",dpi=300)
# 
# 
# method.rmsle.only=df.new %>% group_by(PREFERRED_NAME, method.name)%>%
#     mutate(cmax_obs = max(conc_normalized), cmax_pred=max(Prediction)) %>%
#     summarize(RMSLE = rmse(log10(conc_normalized),log10(Prediction)),
#            Cmax_RMSLE = rmse(log10(cmax_obs), log10(cmax_pred))) %>%group_by(method.name)%>%summarize(mean(Cmax_RMSLE),
#                                                                                                       mean(RMSLE))
# 
# 
# 

## ----1_exhalation_correct, eval=make.plots------------------------------------
# # Extract correct exhalation
# 
# # df = conc_predictions_meade2023DF
# # df$conc_normalized[which(df$conc_normalized==0)]=1e-10
# # df$Prediction[which(df$Prediction==0)]=1e-10
# df=df.sim
# dfmatchE =  subset(df, exhalation_for_simulation==Exhalation)
# # order chemicals by volatility
# vol.chem = unique(dfmatchE[,c("PREFERRED_NAME","Boiling.Point.C")])
# vol.chem2=vol.chem[order(vol.chem$Boiling.Point.C,decreasing=F),]
# 
# #also want to extract table to identify  experiments that actually did list the vehicle as either water or octanol (or similar)
# dfmatchE2 = data.frame(dfmatchE%>%group_by(PREFERRED_NAME,method.name,dose_vehicle_normalized,Kvehicle2water,administration_route_normalized)%>%
#       mutate(cmax_obs = max(conc_normalized), cmax_pred=max(Prediction)) %>%
#   summarize(Cmax_RMSLE = rmse(log10(cmax_obs),log10(cmax_pred)),
#          RMSLE = rmse(log10(conc_normalized),log10(Prediction))))
# 
# # Reorganize the columns so RMSLE values can be grouped by Cmax or total RMSLE and then take average RMSLE values of oral or iv values so we have one per chemical
# dfmatchE3 = data.frame(dfmatchE2 %>%
#   pivot_longer(cols = c("Cmax_RMSLE", "RMSLE"),
#                names_to = "ind",
#                values_to = "value") %>% group_by(PREFERRED_NAME,ind,administration_route_normalized) %>%
#     mutate(value = if_else(administration_route_normalized !="Dermal", mean(value[administration_route_normalized !="Dermal"]), value)
#            ))
# 
# # Make PREFERRED_NAME a factor and set the levels to match those from the volatility order.
# dfmatchE3$PREFERRED_NAME = factor(dfmatchE3$PREFERRED_NAME,
#                                               levels=vol.chem2$PREFERRED_NAME)
# 
# 
# 

## ----1_heatmap, eval=make.plots, fig.height=8, fig.width=11-------------------
# ggplot(dfmatchE3, aes(y=PREFERRED_NAME, x=method.name))+
#   geom_tile(aes(fill=value))+
#   #geom_tile(aes(fill=Chemical.Cmax.RMSLE.NE))+
#   facet_wrap(~ind)+
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
#   scale_fill_viridis()+
#   geom_text(aes(label=round(value,2)),colour="white",size=4)+
#   labs(title="",
#        x="Route or\n Method",
#        fill="RMSLE",y="Chemical")+
#   theme_bw() +
#   theme(axis.text.x=element_text(angle=45,vjust=1,hjust=1),
#         axis.text.y=element_text(angle=45,vjust=1.05,hjust=0.9),
#         plot.title=element_text(hjust=0.5),
#         text=element_text(size=20))+
#   geom_tile(data=subset(dfmatchE3, dose_vehicle_normalized==Kvehicle2water & administration_route_normalized=="Dermal"),aes(y=PREFERRED_NAME, x=method.name),
#             size=1.5,fill=NA,colour="red" )
# ggsave(paste(getwd(),"/Figures_May1/RMSLE_compare_main.png",sep=""),width=11,height=8,units="in")
# 

## ----2_load_data, eval=run.simulations.2--------------------------------------
# # Load in vitro data
# load("invitrodb_3_5_mc5.Rdata")
# 
# # Get chemicals of interest
# load_sipes2017(overwrite=F)

## ----2_load_chem_data, eval=run.simulations.2---------------------------------
# # Using list, download chem info from CompTox Dashboard for chem.list chemicals, and load.
# df.chem <- read.xlsx(file="CCD-Batch-Search_2023-03-08_12_23_25.xlsx",
#                      sheetName="Main Data")# %>%filter(!is.na(BOILING_POINT_DEGC_OPERA_PRED))
# df.chem <- type_convert(df.chem,na="N/A")
# df.chem <- type_convert(df.chem,na="N/A")

## ----2_clean_data, eval=run.simulations.2-------------------------------------
# #Get list of chemicals that can run in dermal model:
# chem.list <- get_cheminfo(info="DTXSID",species="Human",model="dermal_1subcomp")
# write.table(chem.list,
#             file=paste0(loc.wd,"/tables/ChemicalList.txt"),
#             col.names=TRUE,
#             row.names=FALSE,
#             quote=FALSE,
#             sep="\t")
# 
# # Take out volatile chemicals
# chems.from.invivo <- c("DTXSID0020232","DTXSID6022923","DTXSID8024151","DTXSID3022409","DTXSID1026081")
# df.chem <- df.chem %>% filter(!is.na(BOILING_POINT_DEGC_OPERA_PRED))%>%
#   filter((BOILING_POINT_DEGC_OPERA_PRED >= 400) | (DTXSID %in% chems.from.invivo))
# chem.list <- df.chem$DTXSID
# 
# #get sample to test code with
# set.seed(100)
# #chem.list <- sample(chem.list,50)
# toxcast.dermal <- mc5 %>% subset(dsstox_substance_id %in% chem.list)

## ----2_conc_of_interest, eval=run.simulations.2-------------------------------
# # CONCENTRATIONS OF INTEREST - https://doi.org/10.1093/bioinformatics/btw680 ----------------
# toxcast.table <- NULL
# old.time <- Sys.time()
# num.chem <- length(chem.list)
# for(k.chem in 1:num.chem){ #for each chemical
#   this.id <- chem.list[k.chem]
#   # Modulus operator
#   if(k.chem %% 100==0){
#     new.time <- difftime(Sys.time(), old.time,units="secs")
#     cat(paste0("Run ", k.chem,":",round(new.time)," secs \n"))
#   }
#   toxcast.dermal.chem <- subset(toxcast.dermal, dsstox_substance_id == this.id) #subset over chemical
#   toxcast.dermal.chem.hits <- subset(toxcast.dermal.chem, hitc==1) #only look at hits
#   if(dim(toxcast.dermal.chem.hits)[1]>0){ #if there were any hits
#     this.row <- data.frame(Chemical = as.character(toxcast.dermal.chem.hits[1,"chnm"]),
#                            DTXSID = this.id,
#                            Total.Assays = dim(toxcast.dermal.chem)[1],
#                            Unique.Assays = length(unique(toxcast.dermal.chem$aeid)),
#                            Total.Hits = dim(toxcast.dermal.chem.hits)[1],
#                            Unique.Hits = length(unique(toxcast.dermal.chem.hits$aeid)),
#                            Low.AC50 = min(toxcast.dermal.chem.hits$modl_ga),
#                            Low.AC10 = min(toxcast.dermal.chem.hits$modl_ac10),
#                            Low.ACC = min(toxcast.dermal.chem.hits$modl_acc),
#                            Q10.AC50 = quantile(toxcast.dermal.chem.hits$modl_ga,probs=0.1),
#                            Q10.AC10 = quantile(toxcast.dermal.chem.hits$modl_ac10,probs=0.1),
#                            Q10.ACC = quantile(toxcast.dermal.chem.hits$modl_acc,probs=0.1),
#                            Med.AC50 = quantile(toxcast.dermal.chem.hits$modl_ga,probs=0.5),
#                            Med.AC10 = quantile(toxcast.dermal.chem.hits$modl_ac10,probs=0.5),
#                            Med.ACC = quantile(toxcast.dermal.chem.hits$modl_acc,probs=0.5),
#                            Q90.AC50 = quantile(toxcast.dermal.chem.hits$modl_ga,probs=0.9),
#                            Q90.AC10 = quantile(toxcast.dermal.chem.hits$modl_ac10,probs=0.9),
#                            Q90.ACC = quantile(toxcast.dermal.chem.hits$modl_acc,probs=0.9)
#                            )
#     toxcast.table <- rbind(toxcast.table, this.row)
#   }
# }
# rownames(toxcast.table) <- seq(1,dim(toxcast.table)[1]) # set rownames to be sequential numbers
# 
# #View table
# knitr::kable(head(toxcast.table[,1:6]), caption = "Summarized ToxCast Data",
#              floating.environment="sidewaystable")
# write.table(toxcast.table,
#             file=paste0(loc.wd,"/tables/ToxCastSummaryTable.txt"),
#             col.names=TRUE,
#             row.names=FALSE,
#             quote=FALSE,
#             sep="\t")

## ----2_equivalant_dose, eval=run.simulations.2--------------------------------
# 
# # Get Cmax = max(Cplasma) from dermal model for each chemical in toxcast table
# old.time <- Sys.time()
# include.these=which(toxcast.table$DTXSID %in% get_cheminfo(info="dtxsid", suppress.messages=T))
# toxcast.table2 = subset(toxcast.table,DTXSID %in% toxcast.table$DTXSID[include.these])
# 
# atol <- rtol <- 1e-5
# 
# num.chem <- length(toxcast.table2$DTXSID)
# plot.each=F
# # Set Dermal Solutions
# method.ls <- c("Potts-Guy","UK-Surrey")
# toxcast.ls=list()
# for(k.chem in 1:num.chem){
#   this.id = toxcast.table2$DTXSID[k.chem]
#   suppress.messages = TRUE
# 
#     for(k.method in 1:2){
#       this.method <- method.ls[k.method];
#     #  if (this.method=="2-Compartment") this.method.input <- "NULL"
#       this.model.type <-"dermal_1subcomp"
#       end.time=0
#       parms=parameterize_dermal_pbtk(dtxsid=this.id,
#                                      model.type = this.model.type,
#                                      method.permeability = this.method,
#                                      clint.pvalue.threshold=Inf,
#                                      suppress.messages=TRUE,
#                                      Kvehicle2water = 1,
#                                      species="Human",
#                                      default.to.human = T)
#       parms$InfiniteDose=1
#       #if(end.time<2){ #make sure solver finishes
#         out=try( solve_dermal_pbtk(parameters=parms,
#                                      model.type=this.model.type,
#                                      method.permeability=this.method,
#                                      #Kvehicle2water=1, #vehicle is water
#                                      days=5,
#                                      initial.dose = 1,
#                                      input.units = "ppm",
#                                      dose.duration=8,
#                                      dose.duration.units="hr",
#                                      washoff=T,
#                                     # InfiniteDose=T,
#                                      atol = atol, rtol= rtol,
#                                      method="lsoda",
#                                      suppress.messages=suppress.messages) )#put SA for hands!!!
#          if (is(out,"try-error")){
#            Cmax=0
#            browser()
#          }
# 
#         else{end.time <- out[nrow(out),"time"]
# 
# 
#       Cmax <- max(out[,"Cplasma"])
# 
#         }
#        toxcast.ls[[paste0(k.chem,".",this.method)]]=cbind(toxcast.table2[toxcast.table2$DTXSID==this.id,],Cmax=Cmax,Permeability = this.method)
#     }
# 
# }
# 
# toxcast2.all=do.call(rbind,toxcast.ls)
# 
# #     # Calculate the EquivDose in units same as input.units (ppm)
# toxcast2.all$EquivDose = signif(10^toxcast2.all$Q10.AC50 /
#                                   toxcast2.all$Cmax,
#                                 4)
# # Edit data
# toxcast3=toxcast2.all%>%
#   rename(`Permeability Type` = Permeability,
#          `Equivalent Dose`=EquivDose)%>%
#   mutate(Chemical = ifelse(Chemical=="Methyl tert-butyl ether","MTBE",Chemical),
#          Chemical = ifelse(Chemical=="3,3',5,5'-Tetrabromobisphenol A","Bromdian",Chemical),
#          Chemical = ifelse(Chemical=="4,4'-Sulfonyldiphenol","Bisphenol S",Chemical),
#          Chemical = ifelse(Chemical=="Dichloromethane","DCM",Chemical),
#          Chemical = ifelse(Chemical=="Tetrachloroethylene","PERC/TCE",Chemical),
#          Chemical = ifelse(Chemical=="1-Methylbenzene","Toluene",Chemical))
# 
# # Remove unused columns
# ivive_meade2023 <- toxcast3 %>% select(Chemical,DTXSID,`Permeability Type`,`Equivalent Dose`,Cmax)
# 

## ----add_water_solubility, eval=run.simulations.2-----------------------------
# chem.props <- subset(chem.physical_and_invitro.data,
#                      DTXSID %in% ivive_meade2023$DTXSID)[
#                        ,c("DTXSID","MW","logWSol")]
# 
# 
# ivive_meade2023 <- merge(ivive_meade2023,
#                          all.x = TRUE,
#                          chem.props,
#                          by="DTXSID")
# ivive_meade2023 <- within(ivive_meade2023,
#                           WS.ppm <-
# signif(MW*10^logWSol*1.001142303,4))
# 
# # Save data
#  cgwtools::resave(ivive_meade2023,
#                   file=paste0(
#                     loc.wd,
#                     "/meade2023_",
#                     format(Sys.time(), "%b_%d_%Y"),
#                     ".Rdata"))

## ----2_load_without_running, eval = (make.plots.2 & !run.simulations.2), echo=F----
# # load file with ivive_meade2023
# load("meade2023_Jun_20_2025.Rdata")

## ----identify_potent, eval=make.plots.2---------------------------------------
# # Modify for plotting
# ivive_meade2023$`Permeability Type` = gsub("UK-Surrey","Surrey",ivive_meade2023$`Permeability Type`)
# ivive_meade2023[ivive_meade2023==Inf] = 1e15
# write.table(subset(ivive_meade2023,
#                    ivive_meade2023[,"Equivalent Dose"] <
#                      ivive_meade2023[,"WS.ppm"]),
#             file=paste0(loc.wd,"/tables/IVIVEAchievable.txt"),
#             col.names=TRUE,
#             row.names=FALSE,
#             quote=FALSE,
#             sep="\t")
# ivive_meade2023$`Equivalent Dose` <- log10(ivive_meade2023$`Equivalent Dose`)
# ivive_meade2023$`Permeability Type` = paste0("Permeability Method is ",ivive_meade2023$`Permeability Type`)
# write.table(ivive_meade2023,
#             file=paste0(loc.wd,"/tables/IVIVEResults.txt"),
#             col.names=TRUE,
#             row.names=FALSE,
#             quote=FALSE,
#             sep="\t")

## ----2_plot1,fig.width=20,fig.height=12, eval=make.plots.2--------------------
# # Plot Histogram
# data = ivive_meade2023 %>% group_by(`Permeability Type`) %>% mutate(Mean = mean(`Equivalent Dose`,na.rm=TRUE)) %>% mutate(Median = median(`Equivalent Dose`,na.rm=TRUE))
# AED.labels <- c("Needs Protection (AED caused by <1ppm)",
#                 "AED Possibly Acheivable",
#                 "\"Safe\" (Unachievable AED)",
#                 "No Dermal Penetration")
# 
# # Shaded rectangles with cutoff at 10,000 ppm:
# data.rect.1e4ppm = data.frame(xmin=c(-Inf,0,4,12),
#                        xmax=c(0,4,12,Inf),
#                        ymin=rep(-Inf,4),
#                        ymax=rep(Inf,4),
#                        Bioactivity=AED.labels)
# # Shaded rectangles with cutoff at 1,000,000 ppm:
# data.rect.1e6ppm = data.frame(xmin=c(-Inf,0,6,12),
#                        xmax=c(0,6,12,Inf),
#                        ymin=rep(-Inf,4),
#                        ymax=rep(Inf,4),
#                        Bioactivity=AED.labels)
# data.rect.1e4ppm$Bioactivity <- factor(data.rect.1e4ppm$Bioactivity,
#                                        levels=data.rect.1e4ppm$Bioactivity)
# data.rect.1e6ppm$Bioactivity <- factor(data.rect.1e6ppm$Bioactivity,
#                                        levels=data.rect.1e6ppm$Bioactivity)
# 
# data.med = unique(data[,c("Permeability Type","Mean","Median")]) %>% pivot_longer(cols=c("Mean","Median"),names_to="Statistic",values_to="Statistic.Value")
# viridis.colors <- viridisLite::viridis(4)
# plot <- ggplot(data,aes(x=`Equivalent Dose`)) +
#   geom_histogram(na.rm=TRUE,alpha=0.5,position="identity") +
#   geom_freqpoly(na.rm=TRUE) +
#   geom_vline(data = data.med, aes(xintercept=Statistic.Value,linetype=Statistic)) +
#   geom_rect(data=data.rect.1e4ppm,
#             aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax,fill=Bioactivity),
#             alpha=0.5,
#             inherit.aes=FALSE) +
#   #scale_x_log10() +
#   scale_fill_manual(values=setNames(viridis.colors[4:1], AED.labels))+
#   labs(title=paste0("Distribution of concentrations causing AED\nfor hands submerged for 8 hours a day\nfor five days"),
#        y = "Number of Chemicals", x="Log10 of Administrered Equivalent Doses (AEDs) ppm in water") +
#   facet_col(vars(`Permeability Type`),scales="fixed") +
#   theme_bw() +
#   theme(plot.title=element_text(hjust=0.5))
# plot
# ggsave(paste(getwd(), "/Figures_May1/histogram2.png",sep=""),
#        width = 20, height = 12, units = "cm")

## ----2_dataprep_for_plot2, fig.width=15, fig.height=10,eval=make.plots.2------
# 
# # WATER SOLUBILITY
# ivive_meade2023 <-
#   ivive_meade2023 %>%
#   mutate(WSol = case_when( ((MW*10^logWSol)>=10) ~ "Water Soluble (WSol>10g/L)",
#                            ((MW*10^logWSol)<10) ~ "Not Soluble"
#                            ))
# #ivive_meade2023ALL <- ivive_meade2023WS; ivive_meade2023ALL$WSol = "Non-Water Soluble"
# ivive_meade2023WS <- ivive_meade2023 %>% filter(WSol=="Water Soluble (WSol>10g/L)")
# ivive_meade2023NonWS <- ivive_meade2023 %>% filter(WSol=="Not Soluble")
# # Different bands for soluble/non-soluble:
# data.rect.1e4ppm$WSol <- "Not Soluble"
# data.rect.1e6ppm$WSol <- "Water Soluble (WSol>10g/L)"
# data.rect <- rbind(data.rect.1e4ppm,data.rect.1e6ppm)
# data.rect[,"Permeability Type"] <- "Permeability Method is Potts-Guy"
# data.rect2 <- data.rect
# data.rect2[,"Permeability Type"] <- "Permeability Method is Surrey"
# data.rect <- rbind(data.rect,data.rect2)
# 
# ivive_meade2023 <- rbind(ivive_meade2023NonWS,ivive_meade2023WS)
# data = ivive_meade2023 %>% group_by(`Permeability Type`,`WSol`) %>% mutate(Mean = mean(`Equivalent Dose`,na.rm=TRUE)) %>% mutate(Median = median(`Equivalent Dose`,na.rm=TRUE))
# data.med = unique(data[,c("Permeability Type","WSol","Mean","Median")]) %>% pivot_longer(cols=c("Mean","Median"),names_to="Statistic",values_to="Statistic.Value")

## ----2_plot2, fig.width=15, fig.height=10,eval=make.plots.2-------------------
# chem.to.label <-  c("DTXSID0020232",
#                   # "DTXSID6022923",
#                   # "DTXSID8024151",
#                    "DTXSID3022409",
#                    "DTXSID1026081", "DTXSID9048699",
#                    "DTXSID0022858","DTXSID1037","DTXSID0023163",
#                    "DTXSID5024845" )
# data.text = data %>% filter(DTXSID %in% chem.to.label)
# #PLOT WS
# plot <- ggplot(data) +
#   geom_rect(data=data.rect,
#             aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax,fill=Bioactivity),
#             alpha=0.5#,
#             #inherit.aes=FALSE
#             ) +
#   geom_histogram(na.rm=TRUE,alpha=0.5,position="identity",aes(x=`Equivalent Dose`)) +
#   geom_freqpoly(na.rm=TRUE,aes(x=`Equivalent Dose`)) +
#   geom_vline(data = data.med, aes(xintercept=Statistic.Value,linetype=Statistic)) +
# #  geom_point(data=data.text,aes(x=`Equivalent Dose`,y=0),size=2) +
# #  geom_text_repel(data=data.text,aes(x=`Equivalent Dose`,y=0,label=Chemical),
# #                  max.overlaps=14,
# #                  hjust = "right",nudge_x=-1,nudge_y=0.2)+
#   scale_fill_manual(values=setNames(viridis.colors[4:1], AED.labels))+
#   #ylim(0,135)+
#   #geom_text(x=) +
#   labs(title=paste0("Distribution of concentrations causing AED\nfor hands submerged for 8 hours a day\nfor five days"),
#        y = "Number of Chemicals", x="Log10 of Needed Concentration (ppm) in water") +
#   facet_wrap(vars(`Permeability Type`,`WSol`),scales="free_y",ncol=2) +
#   theme_bw() +
#   theme(plot.title=element_text(hjust=0.5),
#         text=element_text(size=20))
# plot
# ggsave(paste(getwd(), "/Figures_May1/histogram_all_main.png",sep=""),
#        width = 15, height = 10, units = "in")
# 

## ----finallist2, eval=FALSE---------------------------------------------------
# data2=data[order(data$`Equivalent Dose`),]
# data2$`Equivalent Dose`= exp(data2$`Equivalent Dose`)
# data3=data.frame(data2%>%group_by(Chemical,DTXSID, `Permeability Type`,WSol,logWSol)%>%
#   mutate(row=row_number())%>%
#   ungroup() %>%
# pivot_wider(id_cols=c(Chemical, DTXSID, row, WSol,logWSol),
#   names_from = `Permeability Type`,
#                     values_from = c(`Equivalent Dose`),
#                     names_sep=""))
# data4=data3[,-3]
# data4=data4[order(data4$Permeability.Method.is.Surrey),]
# 
# 
# 

## ----finallist, eval=FALSE----------------------------------------------------
# pmat=list()
# pm=c("UK-Surrey","Potts-Guy")
# kw=c("water","octanol")
# for(i in 1:length(supptab1_meade2023$DTXSID)){
#   for(m in 1:2){ #each method
#     for(k in 1:2){ # each vehicle
#   p=parameterize_dermal_pbtk(dtxsid=supptab1_meade2023$DTXSID[i],
#                              method.permeability =pm[m] ,species="Human",
#                              Kvehicle2water = kw[k])
#   pmat[[paste(i,".",m,".",k)]]=cbind(p$P, pm[m], kw[k],p$MW, supptab1_meade2023$DTXSID[i])
# 
#   }
#   }
# }
# 
# pmat2=do.call(rbind,pmat)
# colnames(pmat2)=c("Perm", "Method","Vehicle","MW","DTXSID")
# pmat2=data.frame(pmat2)
# pmat2$Method=gsub("UK-Surrey","Surrey",pmat2$Method)
# pmat2$Vehicle=paste("Vehicle is", pmat2$Vehicle)
# 
# pmat3=merge(pmat2,supptab1_meade2023, by="DTXSID")
# 
# 
# ggplot(pmat3,aes(x=as.numeric(MW.x),y=as.numeric(Perm)))+
#   geom_point()+
#   facet_grid(Method~Vehicle,scales="free")+
#   labs(x="MW",y="Kp - Permeability")
# ggsave(paste(getwd(), "/Figures_May1/MWvPerm.png",sep=""),
#        width = 4, height = 3.5, units = "in")
# 
# ggplot(pmat3,aes(x=as.numeric(logP),y=as.numeric(Perm)))+
#   geom_point()+
#   facet_grid(Method~Vehicle,scales="free")+
#   labs(x="log(Kow)",y="Kp- Permeability")
# ggsave(paste(getwd(), "/Figures_May1/logKowvPerm.png",sep=""),
#        width = 4, height = 3.5, units = "in")
# 
# 

## ----save_workspace, eval=make.plots.2----------------------------------------
# save.image(paste0(loc.wd,
#                       "MeadeWorkspace",
#                       Sys.Date(),
#                       ".RData"))

