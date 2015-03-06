# This function returns the flows, volumes, and partition coefficients for the
# lumped tissues specified in tissue list
# Ktissue2plasma -- tissue to free plasma concentration partition coefficients
#                   for every tissue specified by Schmitt (2008) (the tissue.data table)
# tissuelist -- a list of character vectors, the name of each entry in the list
#               is a lumped tissue, the words in the vector are the Schmitt (2008)
#               tissues that are to be lumped, for example:
#               tissuelist<-list(Rapid=c("Brain","Kidney"))
# species specifies the flow.col and vol.col in the tissuedata.table
get_lumped_tissues<- function(Ktissue2plasmain,
                            tissuelist=NULL,
                            species="Human")
{
  PK.physiology.data <- PK.physiology.data
  tissue.data <- tissue.data
  if (!(species %in% colnames(PK.physiology.data)))
  {
    if (toupper(species) %in% toupper(colnames(PK.physiology.data)))
    {
      temp <- colnames(PK.physiology.data)[toupper(colnames(PK.physiology.data))==toupper(species)]
      warning(paste(species,"coerced to",temp,"for tissue data."))
      species <- temp
    } else stop(paste("Tissue data for",species,"not found."))
  }
  vol.col <- paste(species,"Vol (L/kg)",sep=" ")  
  flow.col <- paste(species,"Flow (mL/min/kg^(3/4))",sep=" ")

# Initialize the output lists:
	vol <- list()
	flow <- list()
	Ktissue2plasmaout <- list()
 
# The vector all.tissues indicates whether each tissue in tissue.data has been lumped yet (TRUE/FALSE)
	all.tissues <- rep(FALSE,length(tissue.data$Tissue))
	names(all.tissues) <- tissue.data$Tissue

# Blood cells only need a partioncoefficient:
  Ktissue2plasmaout[["red blood cells"]] <- Ktissue2plasmain[["red blood cells"]]	
  all.tissues["red blood cells"] <- T
 
# This loop adds up the volumes and flows for the tissues within each lumped tissue
# as well as Red blood cells
	for (this.lumped.tissue in c(names(tissuelist),"cleanup"))
	{
# Anything that has not yet been lumped is added to the lumped tissue "Rest"
		if (this.lumped.tissue == "cleanup")
		{
			this.lumped.tissue <- "rest"
# First check to see if rest has been created and create it if it is missing:
			if (!("rest" %in% names(vol)))
			{
				vol[["rest"]] <- 0
				flow[["rest"]] <- 0
        Ktissue2plasmaout[["rest"]] <- 0
			}
# Every tissue not already lumped gets added to "Rest"
			these.lumped.tissues <- tissue.data$Tissue[!all.tissues]
		}	else{
			vol[[this.lumped.tissue]] <- 0
			flow[[this.lumped.tissue]] <- 0
			Ktissue2plasmaout[[this.lumped.tissue]] <- 0
			these.lumped.tissues <- tissuelist[[this.lumped.tissue]]
		}
# Loop over every tissue that is lumped into the tissue:   
		for (this.tissue in these.lumped.tissues)
		{
			if (!(this.tissue %in% tissue.data$Tissue))
				stop(paste(this.tissue,"not in list",paste(tissue.data$Tissue)))
			if (all.tissues[[this.tissue]] & this.tissue !="rest")
				stop(paste(this.tissue,"assigned to multiple lumped tissues"))

# Mark that this tissue has been lumped:
			all.tissues[[this.tissue]] <- TRUE
# Find the row in the tissue.data table that corresponds to this tissue: 
			this.row <- tissue.data$Tissue==this.tissue
			
#Add the volume for this tissue to the lumped tissue:
			vol[[this.lumped.tissue]] <- vol[[this.lumped.tissue]] + as.numeric(tissue.data[this.row,vol.col])
#Add the flow for this tissue to the lumped tissue:                             
			flow[[this.lumped.tissue]] <- flow[[this.lumped.tissue]] + as.numeric(tissue.data[this.row,flow.col])
			 
#Add a contribution to the partition coefficient weighted by the volume of this tissue:
			Ktissue2plasmaout[[this.lumped.tissue]] <- Ktissue2plasmaout[[this.lumped.tissue]] + as.numeric(tissue.data[this.row,vol.col])*Ktissue2plasmain[[this.tissue]]
		}
#Calculate the average parition coefficient by dividing by the total volume of
#the lumped tissue:
		Ktissue2plasmaout[[this.lumped.tissue]] <- Ktissue2plasmaout[[this.lumped.tissue]]/vol[[this.lumped.tissue]]
	}

  # Must have tissue-specific flows for these tissues (even if lumped) in order
  # to calculate other quantities (e.g. rate of metabolism, renal clearance):
  for (this.tissue in c("liver","gut","kidney"))
    if (is.null(flow[[this.tissue]])) 
    {
      if (this.tissue %in% tissue.data[,"Tissue"]) flow[[this.tissue]] <- as.numeric(tissue.data[tissue.data[,"Tissue"]==this.tissue,flow.col])
      else if (paste(this.tissue,"s",sep="") %in% tissue.data[,"Tissue"]) flow[[this.tissue]] <- as.numeric(tissue.data[tissue.data[,"Tissue"]==paste(this.tissue,"s",sep=""),flow.col])
      else stop(paste("Tissue",this.tissue,"not found in tissue.data table."))            
    }

  # Must have tissue-specific volumes for these tissues (even if lumped) in order
  # to calculate other quantities (e.g. rate of metabolism):
    for (this.tissue in c("liver"))
    if (is.null(vol[[this.tissue]])) 
    {
      if (this.tissue %in% tissue.data[,"Tissue"]) vol[[this.tissue]] <- as.numeric(tissue.data[tissue.data[,"Tissue"]==this.tissue,vol.col])
      else if (paste(this.tissue,"s",sep="") %in% tissue.data[,"Tissue"]) vol[[this.tissue]] <- as.numeric(tissue.data[tissue.data[,"Tissue"]==paste(this.tissue,"s",sep=""),vol.col])
      else stop(paste("Tissue",this.tissue,"not found in tissue.data table."))
    }
    
 	return(list(Ktissue2plasma=Ktissue2plasmaout,vol=vol,flow=flow))
}