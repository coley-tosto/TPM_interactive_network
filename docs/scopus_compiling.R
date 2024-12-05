#Compiling data from Scopus for all current TPM members
##Read-in the data containing Scopus ID numbers
tpm_members <- read.csv("../data/project_data.csv", header=TRUE, 
                        stringsAsFactors=FALSE, fileEncoding="latin1")

##I could not find the Scopus ID for several members - subsetting to 
##remove the NAs
tpm_members_scopus <- tpm_members[!(is.na(tpm_members$Scopus_ID)),]

##Putting the Scopus IDs in the correct format for a mass query
##(i.e., AU-ID(000000000) OR AU-ID(111111111) OR AU-ID(222222222))
tpm_members_scopus$scopus_search <- paste("AU-ID(", 
                                          tpm_members_scopus$Scopus_ID, ")", 
                                          sep="")
searchString <- paste(tpm_members_scopus$scopus_search, 
                      collapse=" OR ")
searchString

#Paste this into Scopus and export all document to csv with Authors, 
#Document Title, Author keywords, Indexed Keywords turn off 
#'truncate to optimise for Excel'

##Compiling a list of names for Overton, need one name per line
names <- paste0(tpm_members$first_name," ",tpm_members$last_name)
write.table(cbind(names),
            'overton_names.txt',
            sep = "",
            quote = FALSE,
            row.names = FALSE,
            col.names = FALSE)
