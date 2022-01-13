# rsortie

## Forest Stand Neighbourhood Dynamics Model

This is an R package that facilitates the use of SORTIE-ND, a C++ model, in R. 

**rsortie** allows the user to modify input files, run simulations, and process output files for this forest stand dynamics model.

We assume that the user is familiar with the program R, which can be downloaded from the web at http://www.cran.r-project.org/. 

The user must also download and be familiar with SORTIE-ND, to learn and download the SORTIE-ND GUI, please visit http://www.sortie-nd.org/.

All questions regarding this code should be directed to alana.clason@bvcentre.ca or sortie@bvcentre.ca  

## Installation

1.\ Install the SORTIE-ND GUI.  

Go to http://www.sortie-nd.org/software/index.html and download the newest version.

2.\ Set the value of the JAVA-HOME environment variable (Optional)  

In some cases, the SORTIE GUI does not know where to locate Java even though it is installed on the user’s system. To fix this issue on a Windows 10 computer:

Search for **Environment Variables** and select **Edit the system Environment Variables**.  
Click **Environment Variables** near the bottom right.  
Under **System Variables**, click **New**.  
Set the *Variable Name* as **JAVA_HOME**.  
Set the *Variable Value* as the `C:\filepath\location\of\java.exe`  
Click OK and close the Environment Variables editor.  

Or

Open **Command Prompt** (Run as administrator).  
Run:  
```r 
setx -m JAVA_HOME "C:\filepath\location\of\java.exe"
```
Verify the Environment Variable has been added correctly by restarting Command Prompt and running:  
```r
echo %JAVA_HOME%
```
You should see the file path location of java.exe  


3.\ Install `rsortie`, in R.

```r
devtools::install_github("aclason/rsortie")
```

## File and Folder descriptions  
(maybe don't need to have this here as it's in a vignette)  

1.\ **Inputs**

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
  
  
2.\ **R**

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
