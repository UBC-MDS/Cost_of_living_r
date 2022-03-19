# Exploring the cost of living
Authors: Affrin Sultana, Arushi Ahuja, Jiwei Hu, Margot Vore
<br>

The Cost of Living Comparison dashboard can be accessed [here](https://cost-of-living-r.herokuapp.com/)
<br>

This r-based dashboard is designed to help new college graduates explore the cost of living in different major cities around the world.Our dashboard aims to ease the decision making for the last factor mentioned.In this project , we are using the **Cost of Living - NUMBEO Dataset** which is avaliable in [Kaggle](https://www.kaggle.com/joeypp/cost-of-living-numbeo-dataset).
More details about the usage of this dashboard can be found in the [proposal document](https://github.com/UBC-MDS/Cost_of_living_py/blob/main/doc/Proposal.md).

![](img/demo.gif)

# Usage

## Run app locally 
To install the dashboard and run it locally,follow the steps below:

Run the following command at the root directory of the project:
```bash
git clone https://github.com/UBC-MDS/Cost_of_living_r
cd Cost_of_living_r
```
Open the app.R script and change the last line from `app$run_server(host = '0.0.0.0')` to `app$run_server()` and save the changes.
<br>

**Run the dashboard via command line**
```bash
Rscript app.r
```
Copy and past the local host address given by the command to your internet browser. 
<br> 

**Run the dashboard via RStudio**

Open app.R in RStudio, set your working directory to the Cost_of_living_r folder, and hit the source bottom at the top right corner of the app pane. Copy and past the local host address given by the command to your internet browser. 



# Our Motivation
Students completing their degree spend the last 3 to 4 months of their last semester wondering: what next? Students who are looking to work after graduating especially have questions about the current job market, companies and whether they will be able to afford moving to a new country or city for a job. There should be a one stop solution where in these users(graduating students) can simply key in the desired city or region and estimated income which would give them information about the cost of living as well as the expected savings per month to help them plan better.

# Our Solution
 We have designed an interactive dashboard to give the users flexibility to compare the breakdown of monthly expense from different cities using a series of drop-down menus. We are planning on providing 3 different menus for the users to explore. The first menu allows the user to pick different global cities to compare living expenses across, with all expenses shown in USD regardless of the location. If the user is unsure what cities they are interested in but want to explore a given region of the world,they can search by global region which will display all cities within the selected area. The second drop down menu allows the user to select a subset of monthly expenses to explore from average rent prices and monthly food expenditures.  Finally, the user will be able to enter their expected salary in order to examine how their monthly wages will compare to their monthly expenditures.


# Description of the Dashboard

We have created 4 different visualizations within the dashboard. 
* The first plot is a bar chart that shows the breakdown of monthly expenses per city selected by the user. 
* The second chart shows the surplus or deficit of living in each selected city, considering all monthly expenses and the users expected salary.
*  The third plot shows the prices of buying a property 
*  The fourth plot is a geospatial representation which depicts desired living expenses in cities of interest.

Following actions can be performed on our dashboard :

* Select the expense from a range of options.
* Filter the visualizations based on a desired city/cities of interest from the dropdown.
* Filter the visualizations based on a desired region of interest from the dropdown.
* Enter the expected monthly salary in USD to get an estimate of the savings.
<br>

# Contributing
Interested in contributing? Check out the [contributing guidelines](https://github.com/UBC-MDS/Cost_of_living_r/blob/main/CONTRIBUTING.md). Please note that this project is released with a [Code of Conduct](https://github.com/UBC-MDS/Cost_of_living_r/blob/main/CODE_OF_CONDUCT.md).Any feedback or enhancements are welcome.By contributing to this project, you agree to abide by its terms.

The following software was used for the creation and deployment of this dashboard. To learn more about them, please vist:

* [Dash R User Guide](https://dash.plotly.com/r)
* [Dash interactive visualization](https://dash.plotly.com/r/interactive-graphing)
* [ggplot documentation](https://ggplot2.tidyverse.org/reference/)
* [Deploying Dash R on Heroku](https://github.com/UBC-MDS/dashr-heroku-deployment-demo)

# License
This dashboard was created by Affrin Sultana, Arushi Ahuja, Jiwei Hu, and Margot Vore. It is licensed under the terms of the MIT license.

#  Credits
The raw data for this dashboard was sourced from this [Kaggle dataset](https://www.kaggle.com/joeypp/cost-of-living-numbeo-dataset)

