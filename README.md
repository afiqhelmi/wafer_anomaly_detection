# wafer_anomaly_detection
This repositories intended to detect anomaly on wafer through 46 sensors value using z-score.

#Prerequisite
1. R version >= 4
2. R Studio 2024.04.1+748

#Instructions
1. install all necessary libraries (RStudio -> Tools -> Install Packages)
   -shiny
   -shinydashboard
   -DT
   -RSQLite
   -readr
   -dplyr
   -tidyr
   -ggplot2
   -shinyjs

2. run upload_to_database.R to upload CSV file into database. close app upon complete
3. run anomaly_detection.R and select database & table that have created early. click 'Load Data from DB" button
4. data will shows according to filter dropdown. can adjust filter as needed

*notes: chart will shows normal & outliers from each sensors. 
*outliers is the anomaly that has been detected.
