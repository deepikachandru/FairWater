# CSC8633_202324_Group9
# Project Structure and Execution Guide

## Project Template Overview

This project is organized with the following structure:

- **reports:**
  - `Group_9_Report.pdf`: Main report file containing the analysis details.
  - `Group_9_Report.Rmd`: Rmarkdown version of the analysis report containing all the code to generate the pdf.
  - `Group_9_Dashboard.pbix`: The dashboard of the project.
  
- **log:**
  - `Gitlog.txt`: Git log file capturing version control history.

- **munge:**
  - `01-A.R`: R script file which contains the code been used for data manipulation and visualisation.

- **data**
  - Contains the data needed for the analysis and modelling.
  
- **models**
  - Contains the code for modelling and the actual models.

## Running the Analysis

### 1. Install R and RStudio
   Ensure you have the latest versions of R and RStudio installed on your computer.

### 2. Install Libraries

- LaTex
  - If LaTeX is not installed on your system, you may need to install it before knitting the report in order to generate the       pdf version of it.

    Use the following R code within RStudio:
    ```R
    install.packages("tinytex")
    tinytex::install_tinytex()
    ```
    
- Caret
  - Use the following R code within RStudio:
    ```R
    install.packages('caret')
    ```
    
- Forecast
  - Use the following R code within RStudio:
    ```R
    install.packages('forecast')
    ```
    
- Metrics 
  - Use the following R code within RStudio:
    ```R
    install.packages('Metrics')
    ```
    
### 3. Install Git
   Go to https://git-scm.com/downloads and download Git for your system. 
   
   After installing Git, clone the repository: https://github.com/NewcastleDataScience/CSC8633_202324_Group9.git
    
### 4. Set Working Directory
   Open RStudio and set the working directory to the main folder of the project (`CSC8633_202324_Group9`).

### 5. Open and Run the Analysis Report
   - Navigate to the `reports` folder.
   - Open the `Group_9_Report.Rmd` file.
   - Press the "Knit" button in RStudio to execute the analysis.
   - The analysis report will be generated and displayed in your browser.

For more details about ProjectTemplate, see http://projecttemplate.net
