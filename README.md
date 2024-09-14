**Repository for thesis with title: Extreme Value modelling with Missing Data**

In this repository you will find the code I used for my MSc thesis on Extreme Value Theory during my MSc in Statistics at Imperial College London. My supervisors were Zak Varty (Imperial College London) and Euan McGonigle (University of Southampton).

My thesis focused on Multiple Imputation for extreme values. I designed and tested an imputation algorithm to impute data that follow the Generalised Pareto Distribution which are in a data set together with non-extreme data following different distributions.

In this repo you can find

-   The **data** folder which contains the data generating functions for exact and approximate thresholds.

-   The **src** which contains the missing data functions make_missing_mcar.R and make_missing_mar.R for MCAR and MAR data respectively, the imputation function gpd_impute.R, the pooling function pool_custom.R and the simulation functions simulate_cca.R and simulate_algorithm.R for the Complete Case Analysis (CCA) and our algorithm.

-   The **analyses** folder which contains the R code for the simulations for the CCA and our algorithm.

-   The **outputs** folder which has the figures used in the thesis in the order they were used.

-   The **report** folder which contains the final thesis pdf.

I hope you find the code present in this directory useful in understanding the main ideas of my thesis. If you have any questions don't hesitate to contact me on: [is323\@ic.ac.uk](mailto:is323@ic.ac.uk){.email}
