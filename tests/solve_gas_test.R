# R CMD BATCH --no-timing --no-restore --no-save solve_gas_test.R solve_gas_test.Rout

# Get rid of anything in the workspace:
rm(list=ls()) 

library(httk)

# The following arguments were added: method = "lsode",mf = 10.
# Rationale: Following is required for the same results on various OS's due to
#   precision differences.
head(solve_gas_pbtk(chem.name="pyrene",times=c(0,0.1,0.05),
                    method = "lsode",mf = 10))
head(solve_gas_pbtk(chem.cas="129-00-0",times=c(0,0.1,0.05),
                    method = "lsode",mf = 10))
head(solve_gas_pbtk(
  parameters=parameterize_gas_pbtk(chem.cas="129-00-0"),
  times=c(0,0.1,0.05),
  method = "lsode",mf = 10))

parameterize_gas_pbtk(chem.name="styrene")

quit("no")