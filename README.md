### Invasion_Impacts_Pollinators_Davis_et_al_2017
Code and datafiles for analysis contained in "Contrasting impacts of highly invasive plant species on flower-visiting insect communities" by Emily Davis, Ruth Kelly, Christine A. Maggs and Jane Stout. Published in 2018 in the journal Biodiversity and Conservation, Biodiversity and conservation, 27(8), 2069-2085. 

All code by Ruth Kelly, data collected by Emily Davis. Study design by Jane Stout, Emily Davis and Christine A. Maggs 

These files contain the code and datafiles for a manuscript about the impacts of three riparian invasive species, Heracleum mantegazzianum (Giant Hogweed), Impatiens glandulifera (Himalayan Balsam) and Fallopia Japonica on pollinating insects taxa (honey bees, hoverflies, bumblebees and solitary bees in the Republic of Ireland and Northern Ireland (UK)).  This paper is currently in review and I will add a link to the manuscript which contains details of the sample sites, methodologies and protocols here as soon as it is published.  In the meantime if you have any questions about the code, or interest in using our datasets on pollinating species associated with these plant species then please contact me at : kellyr44@tcd.ie.  Code is provided for replicability rather than teaching purposes and is therefore not well annotated (i.e. I'm assuming the reader already understands R), but I am happy to provide further explanation on request.  


#### The files contained are as follows: 

Note:  Throughout the filenames and code the species are marked as -> JK = Japanese Knotweed, HB = Himalayan Balsam, GH = Giant Hogweed.


##### Raw data 

Field data on pollination communities are in three .csv files, one for each invasive species and it's corresponding control sites are named: "GH_with_flower_data_vr1.csv", "HB_with_flower_data_vr1.csv" and  "JK_with_flower_data_vr1.csv"   

##### Core analyses of pollinator datasets (above)

Code files contain code for Generalised Linear Mixed Models (GLMMs) and ordination analyses (Redundancy Analysis, RDA), based on the above .csv datasheets.  Here again the code is split into 3 sections, one file per plant species. Namely: "GH_only_polly_analysis_01_08_2017.R", "HB_only_polly_analysis_01_08_2017.R" and "JK_only_polly_analysis_01_08_2017.R". 

##### Results of GLMMs 

.csv files containing the results of final GLMM's for all plant species on abundance and diversity of insect pollinators are given in: "GH_abundance_impact_results_01_08_2017.csv", "GH_diversity_impact_results_01_08_2017.csv",  "HB_abundance_impact_results_01_08_2017.csv", "HB_diversity_impact_results_01_08_2017.csv", "JK_abundance_impact_results_01_08_2017.csv" and  "JK_diversity_impact_results_01_08_2017.csv".  These are included here for use with the last code file which contains example code for replicating the type of coefficient plots of model outputs given in the main paper (e.g. Fig 3). 

##### Example code for coefficient plot (Fig. 3) based on GLMM model outputs. 

I've included this because I have recently realised how much I like coefficient plots (it's quite a lot).   This one is done using ggplot2 "pollinator_abundance_coefficient_plot_example_code_01_08_2017.R" 


### Citations

As with all things R related this analysis was made possible by the generous work of the R Core Team and R package creators.  This code uses 4 fantastic packages, For GLMMS - "glmmADMB" and "lme4", for ordination analysis "vegan" and for plotting "ggplot2"

##### glmmADMB
Fournier DA, Skaug HJ, Ancheta J, Ianelli J, Magnusson A, Maunder M, Nielsen A and Sibert J (2012). “AD Model Builder: using automatic differentiation for statistical inference of highly parameterized complex nonlinear models.” _Optim. Methods Softw._, *27*, pp. 233-249.

Skaug H, Fournier D, Bolker B, Magnusson A and Nielsen A (2016-01-19). _Generalized Linear Mixed Models using 'AD Model Builder'_. R package version 0.8.3.3.

##### lme4
Douglas Bates, Martin Maechler, Ben Bolker, Steve Walker (2015). Fitting Linear Mixed-Effects Models Using lme4. Journal of Statistical Software, 67(1), 1-48. doi:10.18637/jss.v067.i01.

##### vegan

 Jari Oksanen, F. Guillaume Blanchet, Michael Friendly, Roeland Kindt, Pierre Legendre, Dan McGlinn, Peter R. Minchin, R. B. O'Hara, Gavin L. Simpson, Peter Solymos, M. Henry H. Stevens, Eduard Szoecs and Helene Wagner (2017). vegan: Community Ecology Package. R package version 2.4-2. https://CRAN.R-project.org/package=vegan
 
##### ggplot2

H. Wickham. ggplot2: Elegant Graphics for Data Analysis. Springer-Verlag New York, 2009.

#################################################################################################################

#### Funding.. 
This project was conducted during and as a result of the CIRB program at Queen's University Belfast, whose core aim was to reduce the influence of these three invasive plant species on waterways of conservation importance in Ireland.  It was The project is part financed by the European Union's European Regional Development Fund through the INTERREG IVA Cross-border Programme, managed by the Special EU Programmes Body and part financed by the Department of the Environment (Northern Ireland) and the Department of the Environment, Community and Local Government (Ireland), see: http://www.qub.ac.uk/research-centres/cirb/
