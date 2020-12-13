# BST260Project_KB_LC_AR

# Project Name: Examining Policy Interventions on COVID-19 and Other Infectious Disease Incidence in Australia 
## Team members: Kristyn Beam, Luis Castelo Branco, Alex Rashedi

_Goal of the project_

COVID-19 has rocked the globe in the year 2020. With it's emergence in late 2019, quick spread, and lack of medical treatments and interventions, nonpharmaceutical interventions have been the forefront of limiting spread, disease, and death. Given the fact that non-pharmaceutical interventions (NPIs) have been the main defense against spread of COVID-19, investigation into how NPIs have not only affected the spread of COVID-19, a respiratory disease, but also other infectious diseases spread by contact or respiratory droplets was the goal of our project. 

We used several data sources to plot the COVID rates, as well as other infectious disease rates by province in Australia with an indication of when each NPI was implemented to determine the effect.

_How to navigate our files in the repo_

Instructions:
1. First look at the .RMD and HTML files: `data_cleaning.Rmd` or `data_cleaning.html`. These files give an overview of the project as well as show our data analysis including the exploratory analysis. 
2. Run the .rmd file to ensure that the policy data is up to date and the merge_dat file is available for the shiny app.
3. In the folder: x, our shiny app is located to run `app.R`. All of our data files are located in the RMD files folder, which our shiny app should point to. Running the app should allow the shiny app to work.
4. Our final statistical analysis is at the bottom of our .rmd file. This includes a t-test of the means of cases of several diseases.

Data files for our shiny app are written into the folder "RMD files":

1. `aus_total_high.csv`
2. `pol_dat_aus.csv`
3. `aus_total.csv`
4. The `merge_dat` file necessary to run our shiny app map function must be obtained by first running the .rmd file.

_Final project products_

1. Our website can be found [here](https://sites.google.com/view/kblcar-bst260-final-project/home)
2. Link to screencast can be found [here](www.here.com)  
