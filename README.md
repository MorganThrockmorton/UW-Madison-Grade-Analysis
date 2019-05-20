# University of Wisconsin, Madison Grade Analysis Project

## Description

This project was a term project for a statistical computing course taught at California State University, Sacramento. We were asked to use the data given on grades from University of Wisconsin, Madison from 2006 to 2017 that can be found [here](https://www.kaggle.com/Madgrades/uw-madison-courses) on Kaggle to answer various questions. 

The project was done using R script files and presented in RMarkdown format. I encourage those who would like to see an overview of the project to check out the [HTML output of the RMarkdown file](/index.html). This file does not contain every line of code, library, or package I used; however, it goes in depth about the decisions and direction I took throughout the process of my analysis.

For an in depth overview of my project, I have provided the following information.

## Running the Code

To run any of this code, you will need to install R on your computer. I used RStudio as the IDE. For help getting started, [click here](https://www.computerworld.com/article/2497143/business-intelligence-beginner-s-guide-to-r-introduction.html?page=2).

### Dependencies

You will also need to install the following packages:

* readr
* ggplot2
* plyr
* dplyr

If there is a dependency that I missed, please let me know.

## File Breakdown

* [relational_database_map](/Relational_Database_Map.jpg) is a picture of the relational database map that I created before starting any of the coding. This shows how each of the tables relate to each other. This information was not provided in the data dictionary on Kaggle and was discovered by looking through each of the tables.

* [term_codes](/term_codes.csv) is a CSV file of the term codes for classes that can be found on [this](https://www.bussvc.wisc.edu/bursar/termcode.html) website. All I did was copy and paste this information into Excel before importing into R to clean.

* [UW_Analysis_Term_Project](/UW_Analysis_Term_Project.R) is the R script file I used to write all of the code before using markdown. All of the code is commented and is broken into steps. The code is a reference for all of the variables I used throughout the process. Note that at the bottom I have **rm(variable)** for all of the variables that I used. I did this to clean my global environment so that I could keep track of the size of the tables I was working with and merging together.

* [UWMadison](/UWMadison.Rmd) is the markdown file I used for the project. This file contains every line of code that I used for the project along with my analysis. To run this file, you will have to "knit" the document in RStudio.

* [UW_Analysis_Term_Project_View](/index.html) is the HTML output of the RMarkdown file.