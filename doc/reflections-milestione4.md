# Reflections

We thoroughly enjoyed the experience of building the app from scratch. Learnt so many new and cool features and tried implementing them to the best of our abilities.
While we were successfully able to implementing all the major plots and aspects of our dashboard that we had originally proposed, there is still room for improvement. 

* Simplicity and easy to use features were the most important factors that we kept on our minds while designing this app.
* The major difference between our DashR and DashPy app is that in DashR we have the option of choosing cities based on single Region whereas in DashPy we have the option of choosing cities based on multiple region. We wanted to make the remove multi selection feature from DashPy but the selection of dropdown menu is dynamic based on radioitems hence customising the multi selection option for dynamic dropdown was very challenging. We spent couple of hours debugging it but couldnot achiev it hence left it as is.
* Another difference between DashPy and DashR app is that the legend in DashR app onlu includes price where as in DashPy , it is based on price and cities.
* As per the TA's feedback of zooming in on the map based on the region selected we were able to achieve it on our DashR app.
* We also wanted to add interactivity between each plots using app callback but due to time constrains we decided not go ahead with it.
* We have added the feature of hoverText, html label which was newly taught to us during the lectures.This gives us more information about the values in the plot
* Earlier our multi-selection feature didnot function as intended, as the map plot was not able to filter when multiple regions were chosen but we were able to **fix** it and it works as were had earlier planned it to.
* We imporved the axis labels of few plots to make it more human interpretable
* We felt that the map didnot fit well in the dbc Card, hence we fixed the size of the plot to avoid scrolling horizontaly or vertically
* As suggested by our peers, we customised the placholders for the "Region/Cities" dropdown for better understanding of the users.
* We added an additional tab which explains "how the app works" to help new users navigate easily.
* Earlier we had the option of selecting single category of cost from the dropdown however taking the peer feedback seriouly we made alteration to allow the user 
to select multiple 'monthly cost' options, so that they can see the cumulative cost of many different expenses.
* We wanted to match the colour scheme of all the bar plots with the heat map but we couldnot do so due to limitation of the ggplotly.
* We wanted to add a generic legend to the heat map (now it is dynamic based on the selection of monthly cost categories) but it results in shrinking of the map if too many categories are selected. 
But as the build time for DashR app takes too long so we decided to leave it as it. It works fine if upto 2-3 categories are chosen.
* We wanted to add instructions for Docker, to run the app locally, but the docker app was taking 40 minutes to build and it threw an error as it could not find different dash libraries so we decided not to include it.


We are very satisfied with app taht we have created so far.Morever we had a great learning experience while working on this project and this group.


