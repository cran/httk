## ----include=FALSE------------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = '#>')

## ----clear_memory, eval = TRUE------------------------------------------------
rm(list=ls()) 

## ----runchunks, eval = TRUE---------------------------------------------------
# Set whether or not the following chunks will be executed (run):
execute.vignette <- FALSE

## ----load_packages, eval = execute.vignette-----------------------------------
# library("httk")
# library("data.table")

## ----subpop_specs, eval = execute.vignette------------------------------------
# nsamp<-1000
# #List subpop names
# ExpoCast.group<-list("Total",
#                      "Age.6.11",
#                      "Age.12.19",
#                      "Age.20.65",
#                      "Age.GT65",
#                      "BMIgt30",
#                      "BMIle30",
#                      "Females",
#                      "Males",
#                      "ReproAgeFemale",
#                      "Age.20.50.nonobese")
# #List subpop gender specifications
# gendernum <- c(rep(list(NULL),7),
#                list(list(Male=0, Female=1000)),
#                list(list(Male=1000, Female=0)),
#                list(list(Male=0, Female=1000)),
#                list(NULL))
# #List subpop age limits in years
# agelim<-c(list(c(0,79),
#                c(6,11),
#                c(12,19),
#                c(20,65),
#                c(66,79)),
#           rep(list(c(0,79)),4),
#           list(c(16,49)),
#           list(c(20,50)))
# #List subpop weight categories
# bmi_category <- c(rep(list(c('Underweight',
#                              'Normal',
#                              'Overweight',
#                              'Obese')),
#                       5),
#                   list('Obese', c('Underweight','Normal', 'Overweight')),
#                   rep(list(c('Underweight',
#                              'Normal',
#                              'Overweight',
#                              'Obese')),
#                       3),
#                   list(c('Underweight', 'Normal', 'Overweight')))

## ----generate_parallel, eval = execute.vignette-------------------------------
# tmpfun <- function(gendernum, agelim, bmi_category, ExpoCast_grp,
#                    nsamp, method){
#   result <- tryCatch({
#                      pops <- httk::httkpop_generate(
#                   method=method,
#                   nsamp = nsamp,
#                   gendernum = gendernum,
#                   agelim_years = agelim,
#                   weight_category = bmi_category)
# 
#                   filepart <- switch(method,
#                                      'virtual individuals' = 'vi',
#                                      'direct resampling' = 'dr')
# saveRDS(object=pops,
#           file=paste0(paste('data/httkpop',
#                       filepart, ExpoCast_grp,
#                       sep='_'),
#                       '.Rdata'))
# return(getwd())
# }, error = function(err){
#   print(paste('Error occurred:', err))
#   return(1)
# })
# }
# 
# cluster <- parallel::makeCluster(2, # Reduced from 40 to 2 cores
#                        outfile='subpopulations_parallel_out.txt')
# 
# evalout <- parallel::clusterEvalQ(cl=cluster,
#              {library(data.table)
#               library(httk)})
# parallel::clusterExport(cl = cluster,
#               varlist = 'tmpfun')
# #Set seeds on all workers for reproducibility
# parallel::clusterSetRNGStream(cluster,
#                               010180)
# out_vi <- parallel::clusterMap(cl=cluster,
#                   fun = tmpfun,
#                   gendernum=gendernum,
#                   agelim=agelim,
#                   bmi_category=bmi_category,
#                   ExpoCast_grp = ExpoCast.group,
#                   MoreArgs = list(nsamp = nsamp,
#                                   method = 'virtual individuals'))
# out_dr <- parallel::clusterMap(cl=cluster,
#                   fun = tmpfun,
#                   gendernum=gendernum,
#                   agelim=agelim,
#                   bmi_category=bmi_category,
#                   ExpoCast_grp = ExpoCast.group,
#                   MoreArgs = list(nsamp = nsamp,
#                                   method = 'direct resampling'))
# parallel::stopCluster(cluster)

