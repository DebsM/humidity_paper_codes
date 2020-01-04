# Paper codes (mode details will be added once paper is published)

Data sources are public and can be found at:

electricity load data:
https://www.eia.gov/electricity/data/eia861/

population data:
https://www.census.gov/programs-surveys/popest/data/data-sets.html

climate data:
https://www.esrl.noaa.gov/psd/data/gridded/data.narr.html


# Processed data file: Processed_data_for_data_availability.zip

%%%%%
R version:
platform       x86_64-w64-mingw32          
arch           x86_64                      
os             mingw32                     
system         x86_64, mingw32             
status                                     
major          3                           
minor          4.4                         
year           2018                        
month          03                          
day            15                          
svn rev        74408                       
language       R                           
version.string R version 3.4.4 (2018-03-15)
nickname       Someone to Lean On      


# Download and install R:
https://www.r-project.org/
https://cran.r-project.org/doc/manuals/r-release/R-admin.html

Installation time less than 10 minutes on most computers.


# About the main package, bartMachine, including manual:
https://cran.r-project.org/web/packages/bartMachine/index.html
Special attention to updating the java package and running the initial codes in the right order, as seen in the files uploaded here.


# Instructions 

Files that start with 0 are preprocessing files. Files that start with 1 were the ones used to process the results. RData file types are also submitted to aid in processing the 1_ files. 

Attention to changing working director and machine memory parameters when running codes (available memory and cores), as in

clusterEvalQ(cl, options(java.parameters = "-Xmx4000m"))
	clusterEvalQ(cl, set_bart_machine_num_cores(4))
  


