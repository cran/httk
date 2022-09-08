## ----setup_vignette, eval = TRUE----------------------------------------------
knitr::opts_chunk$set(echo = TRUE, fig.width=5, fig.height=4)

## ----install_httk, eval = FALSE-----------------------------------------------
#  install.packages("httk")

## ----clear_memory, eval = TRUE------------------------------------------------
rm(list=ls())

## ----load_httk, eval = TRUE---------------------------------------------------
library(httk)

# Check what version you are using 
packageVersion("httk")

## ----cheminfo_ex1, eval = TRUE------------------------------------------------
head(get_cheminfo())

## ----cheminfo_ex1a, eval = FALSE----------------------------------------------
#  get_cheminfo()

## ----cheminfo_ex2, eval = TRUE------------------------------------------------
head(get_cheminfo(info = "all", median.only=TRUE))

## ----cheminfo_ex3, eval = TRUE------------------------------------------------
"80-05-7" %in% get_cheminfo()

## ----cheminfo_ex4, eval = TRUE------------------------------------------------
subset(get_cheminfo(info = "all"), Compound %in% c("A","B","C"))

## ----oral_equiv_ex, eval = TRUE-----------------------------------------------
calc_mc_oral_equiv(0.1,chem.cas = "34256-82-1",species = "human")
calc_mc_oral_equiv(0.1,chem.cas = "99-71-8", species = "human")

## ----tkstats_ex, eval = TRUE--------------------------------------------------
calc_tkstats(chem.cas = "34256-82-1",species = "rat")
calc_tkstats(chem.cas = "962-58-3", species = "rat")

## ----pbtk_ex, eval = TRUE-----------------------------------------------------
head(solve_pbtk(chem.name = "bisphenol a", plots = TRUE))

## ----subset_ex, eval = TRUE---------------------------------------------------
my_data <- subset(get_cheminfo(info = "all"), Compound %in% c("A","B","C"))

## ----writetodisk_ex, eval = TRUE----------------------------------------------
write.csv(my_data, file = "my_data.csv")

## ----help1, eval = FALSE------------------------------------------------------
#  help(httk)

## ----help2, eval = FALSE------------------------------------------------------
#  help(package = httk)

## ----help3, eval = FALSE------------------------------------------------------
#  vignette(package = "httk")

## ----help4, eval = FALSE------------------------------------------------------
#  vignette("IntroToHTTK")

