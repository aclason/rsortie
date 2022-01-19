# rsortie

### Modelling forest stand dynamics with SORTIE-ND in R

This is an R package that allows users to interact with SORTIE-ND. Specifically, users can modify input parameter files, run simulations, and process output files from SORTIE-ND.

We assume that users are familiar with R (http://www.cran.r-project.org/), and have downloaded are familiar with SORTIE-ND (http://www.sortie-nd.org/).

### Installation

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


3.\ Install `rsortie` from github.

```r
#install.packages("devtools")
library("devtools")
devtools::install_github("aclason/rsortie")
```
For information on parameterizations of SORTIE in British Columbia and research at the Bulkley Valley Research Centre, visit https://forests-bvcentre.ca.
