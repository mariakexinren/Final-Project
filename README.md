# Final-Project

## Notes about the project 

#### Shiny App Link: 

#### Final_Project folder includes the shiny app files.
#### Billboard_Top_1.csv file is the finalized tidy dataset. 
#### Final Project.R is the file that has all the code.
#### Final_Project_Write_Up.Rmd is the file that has the R Markdown code.
#### Final_Project_Write_Up.pdf is the pdf output from the R Markdown code.(The Final Project.R file is only the code part of the project, the specific analysis is included in both the shiny app and the Final_Project_Write_Up.Rmd file.)


#### All the project libraries I used for the project are listed below, I have also included the install.packages() as comments in each document

library(shiny)  
library(devtools)  
library(spotifyr)  
library(tidyverse)    
library(dplyr),  
library(billboard)  
library(httr)  
library(miscTools)  
library(ggplot2)  
library(car)  
library(ggvis)  
library(plotly)  
library(knitr)  
library(kableExtra)


#### Warning: When running the Final_Project_Write_Up.Rmd file, the real time data collection uses functions get_track and get_features. Both functions works, but can be a little slow. Also, sometimes they don't work on the first try- you might have to rerun the specific code again. (They will work for sure, the Spotify API access code can be a little slow and weird.) But I already saved the correct finalized tidy dataset named Billboard_Top_1.csv, and it is saved in the same directory as both the github and the shiny app folder. So you can skip to the section labeled Analysis of dataset of the Final Project.R document if runtime of the code takes too long.



