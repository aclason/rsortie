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

