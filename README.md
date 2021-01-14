# MIT-15.072-Bike-Rebalancing

This project was conducted during the MIT 15.072 Analytics Edge course (Fall 2020).

Executive Summary:
Deployment Strategy for e-Scooters in Boston
David Liszewski, Victor Jouault, Tristan Breyer, Aniruddh Hari

Boston is a major US city that currently has laws that ban e-Scooter travel. Upon their expected repeal, we propose having a market entrance and deployment strategy for an e-Scooter fleet based on the usage of the popular shared-bikes system in the city, Bluebikes. A region in the heart of Boston and Cambridge was identified for market entrance, as its size and demographic patterns matched the profile of e-Scooter users in other major cities. Given the dockless nature of e-Scooters, we found 50 hotspots to act as the deployment zones in this region by aggregating the flow of travel at the nearby bike stations. 

To account for the uncertainty in travel patterns in the initial market entrance, we developed four equally likely demand scenarios for the types of trips that e-Scooter users will take. Using the demand scenarios and a stochastic optimization approach, we determine the optimal number of e-Scooters and their daily distribution across the deployment zones to maximize the number of trips taken per month, subject to a break-even profitability constraint. We find that an optimal distribution leads to a 10% increase in the total number of rides over a baseline uniform distribution across the deployment zones. The optimization analysis is performed on the months leading up to the market entry target month. Linear regression on these values is utilized to predict the optimal number of e-Scooters one month in advance, leading to 500 scooters in Sept. 2019 (vs a-posteriori optimal of 544 scooters). Findings show that seasonal and yearly trends are stable, and therefore an e-Scooter company could buy supplies earlier than one month in advance to save costs. In addition, a daily e-scooter distribution machine learning model is developed to predict the demand for rides on a given day while accounting for more uncertainties, particularly weather conditions. While the final model leads to an R2 of 24%, it overpredicts the total monthly rides by a factor of 4.5 as the prediction problem is complex, with a high noise-to-signal ratio. 

The analysis of our optimization model outputs highlights that break-even and profitability scenarios are possible when entering the Boston market. However, this doesn’t exactly meet industry reality and our early results would benefit from being reviewed and stress-tested by industry experts to tune hypotheses and drive impact. In addition, there is scope for further improvement in the prediction model, which can then be used in unison with the optimization framework to converge closer to the reality of things.


