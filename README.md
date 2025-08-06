Here are some scripts used in a mansucript from SIUC that is about to be submitted. There is also the main dataset for the regression.

Order or scripts
1. pull data from Lotex server and basic cleaning - this script is with the Spatial Wildlife Ecology Lab
2. GPS clean script - more detailed cleaning like remove fast steps and looking at trajectory
3. BaBA script to locate road encounters and classify them
4. Gather data for baba logistic regression main analysis
        - Create HRs (KDs in this case) then use road density script to get road density per HR
        - Use NLCD proportion script to get NLCD classifications around each road encounter
        - time of day script to get time of day and season for each road encounter
5. use Df 5 to run the RMarkdown file of the top model and make some plots
6. optional use BaBA ranking script to look at road impermiability and make maps/figures
   
