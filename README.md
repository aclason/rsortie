# rsortie

## Forest Stand Neighbourhood Dynamics Model

This is an R package that facilitates the use of SORTIE-ND, a C++ model, in R. 

**rsortie** allows the user to modify input files, run simulations, and process output files for this forest stand dynamics model.

We assume that the user is familiar with the program R, which can be downloaded from the web at http://www.cran.r-project.org/. 

The user must also download and be familiar with SORTIE-ND, to download the SORTIE-ND GUI and view an introduction, please visit http://www.sortie-nd.org/.

All questions regarding this code should be directed to alana.clason@bvrc.ca or sortie@bvcentre.ca

## Installation

1. Install the SORTIE-ND GUI.  

Go to http://www.sortie-nd.org/software/index.html and download the newest version.

2. Set the value of the JAVA-HOME environment variable (Optional)  

In some cases, the SORTIE GUI does not know where to locate Java even though it is installed on the user’s system. To fix this issue on a Windows computer:

Open the Windows System Properties Control Panel applet on any version of Windows.  
Choose Advanced System settings.  
Click on the Environment Variables button.  
Click on the New button under System Variables.  
Set **JAVA_HOME** as the *environment variable name*.  
Set the **location of java.exe**  as the *environment variable Value*.  
Click OK and close the JAVA_HOME environment variable editor.

3. Install `rsortie`, in R.

```r
devtools::install_github("aclason/rSORTIE")
```

## File and Folder descriptions  
(maybe don't need to have this here as it's in a vignette)  

1. **Inputs**

These input files are the default files used to run the model, they can be changed through the arguments in the package.  

**Files.txt (e.g., “InitDateCreek.csv”)**  
These files contain the list of files that you wish to use in the simulation.  
     
**Base XML file (e.g., “ICH.xml”)**  
These files should be a standard SORTIE input parameter file, with no modifications required. It is very important that  
this file contains all variables and sections that will be modified. For example, if you might be adding XML code with  
new harvest rules, a harvest section must be in the original SORTIE file. Similarly, if you are changing initial densities,  
then each size class that you might want to use should be in the original file.  

**Parameter Values File (e.g., “A1.csv”)**  
These files contain a set of new values for different parameters.  

**Variable Names (e.g., “VariableNames.csv”)**  
This is a translation file that matches the parameter names in the input file with those in the XML file and tells the general  
format of the variable. Formats are described more later. This file will only be changed to add new variables or to change the  
parameter name of a variable.  

For information on how to edit these files please refer to https://bvcentre.ca/sortie-nd.  
  
  
2. **R**

**MakeFiles.R**  
R script that contains all the code needed to run the simulation.

**Functions.R**  
R script that contains functions to do… 

**ParseXML.R**  
R script that contains functions to parse…

**ReplaceInfo.R**  
R script that contains functions to replace the base XML with the parameter values you wish to use.

**SORTIE-HelperFunctions.R**  
R script that contains functions to do… 
