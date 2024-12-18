# Interactive Network for Te Pūnaha Matatini
This group is comprised on individuals from a wide variety of backgrounds who are all interested in understanding **complex** systems. They bring together researchers from tertiary institutions, government institutes, provate sector organizations, and marae communities from all throughout Aotearoa New Zeland. You can read more about this incredible group [here](https://www.tepunahamatatini.ac.nz/)

This repository contains code related to a ShinyApp which attempts to visualize how this huge diversity of individuals comes together in collaboration. The current version of the ShinyApp online can be found [here]( https://c-tosto.shinyapps.io/TPM-interactive-collaboration-network/).

## Navigating this repository
In this repository there are two folders, /data and /docs. 
  1. `/data`: located here are the two datasets feeding information into the shinyApp. There are two ways that the data was fed into the ShinyApp: 1) via Google Drive through using **google sheets**, this way is currently commented out but is left in the script, 2) By using GitHub links to the data, this way appears to work better when integrating with shiny.io.
       - `project_data.csv` contains the metadata for all of the people working within TPM including details such as their corresponding institution/company, the year they joined TPM, and the project(s) they are involved within TPM.
         
       - `zotero_data.csv` contains information from a variety of deliverables produced by the members of TPM. This data can be used to give information about the collaborations within this group. The data was gathered from the [TPM group library](https://www.zotero.org/groups/4719845/tpmgrouplibrary/library).
         
       - `scopus.csv` was generated by searching all TPM member's Scopus IDs and is what is **currently** being used to inform the collaborations within the network. Only publications after 2015 were included as that is when Te Pūnaha Matatini was formed.
         
       - `overton_authors.csv` and `overton_keywords.csv`: In order to get an idea of Te Pūnaha Matatini's influence/role in policy documents, [Overton](https://www.overton.io/) was used. I performed two searches:
           1. **Searching a list of names and downloading the corresponding data** - this gives you an idea of how many times TPM members have had their work cited in policy documents and is stored in the `overton_authors.csv`
           2. **Searching keywords and downloading the corresponging data** - I used "Te Pūnaha Matatini OR Te Punaha Matatini" as the keyword search string. This gives an idea of how many times TPM is mentioned in policy documents and results are stored in `overton_keywords.csv`
              
              - When you export the data as an Excel file it returns multiple tabs in the file. The **"Matched References"** tab was saved as a .csv and is what is stored here.
         
  2. `/docs`: located in this folder are the two `.R` files necessary to run the ShinyApp.
      - `ui.R` contains the details related to the **user interface** of the application
        
      - `server.R` contains the information necessary to make the server run. Here there are custom functions and details about what data we want to use and how it should be interacting with each other.
        - Currently, the columns that the script is pulling from `scopus.csv` are "Authors", "Year", and "EID".
          
      - `scopus_compliling.R` contains information about how the Scopus search was done (i.e., pulling together all of the ScopusIDs in the correct format).
        
      - `shiny_app_token.RDS` is required if you are wanting to read in the data with google sheets   

## Running this application
To use this ShinyApp download the whole repository and use the `runApp()` function from the package `shiny` in R to execute it. If your working directory is **not** set to inside of the `/docs` folder, ensure to specify it when running the code:

```
runApp(appDir = "TPM_interactive_network/docs")
```
