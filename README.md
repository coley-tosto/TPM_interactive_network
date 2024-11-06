# Interactive Network for Te Pūnaha Matatini
This group is comprised on individuals from a wide variety of backgrounds who are all interested in understanding **complex** systems. They bring together researchers from tertiary institutions, government institutes, provate sector organizations, and marae communities from all throughout Aotearoa New Zeland. You can read more about this incredible group [here](https://www.tepunahamatatini.ac.nz/)

This repository contains code related to a ShinyApp which attempts to visualize how this huge diversity of individuals comes together in collaboration. 

## Navigating this repository
In this repository there are two folders, /data and /docs. 
  1. `/data`: located here are the two datasets feeding information into the shinyApp.
       - `project_data.csv` contains information about all of the people working within TPM including details such as their corresponding institution/company, the year they joined TPM, and the project(s) they are involved within TPM.
       - `zotero_data.csv` contains information from a variety of deliverables produced by the members of TPM. This is the data used to give information about the collaborations within this group. The data was gathered from the [TPM group library](https://www.zotero.org/groups/4719845/tpmgrouplibrary/library).
         
  2. `/docs`: located in this folder are the two `.R` files necessary to run the ShinyApp.
      - `ui.R` contains the details related to the **user interface** of the application
      - `server.R` contains the information necessary to make the server run. Here there are custom functions and details about what data we want to use and how it should be interacting with each other.

## Running this application
To use this ShinyApp download the whole repository and use the `runApp()` function from the package `shiny` in R to execute it. If your working directory is **not** set to inside of the `/docs` folder, ensure to specify it when running the code:

```
runApp(appDir = "TPM_interactive_network/docs")
```
