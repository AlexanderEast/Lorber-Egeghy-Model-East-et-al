# Lorber-Egeghy-Model-East-et-al
 <pre>
Estimates aggregate route exposure for PFOA and PFOS given media specific concentration summary statistics 

Input Folder contains single input file containing individual, concentration, and exposure data. 
Output Folder contains output file containing exposure and dose predictions for PFOA and PFOS for each individual. 

R Folder contains:
  Control:   Single function which sources all other R files to return output file to workspace and output folder
  Common:    Individual functions and PK functions 
  Food:      Exposure-to-Exposure predictions for each individual 
  Media:     Concentration-to-Exposure predictions for each individual and route
  Packages:  Install and loading of required packages
  
Run by entering data into the input file without renaming sheets and columns and calling the function LEM.run() in /R/Control.R

</pre> 
  
