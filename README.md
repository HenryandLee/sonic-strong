# sonic-strong
STRONG project SNA analysis, including data and pre-processing procedures. Created in summer 2024. 

This directory is organized following the guidelines suggested by Gentzkow and Shapiro (2014). Gentzkow, Matthew and Jesse M. Shapiro. 2014. Code and Data for the Social Sciences: A Practitioner’s Guide. University of Chicago mimeo,
http://faculty.chicagobooth.edu/matthew.gentzkow/research/CodeAndData.pdf. 

File directories in scripts have been renamed using here package to improve portability. 

TODO: 
1. Consider streamlining data cleaning process for each sheet. Currently we use bash to perform the cleaning one sheet at a time. May consider using Quarto interactive report later. 
2. Create rundirectory.bat for complete reproducibility. 

Naming convention in *Evan's Google drive folder*: 
1. <filename>_made_perfect: all commas and zeros have been filled for all entries
2. ignore "<submission>-corrected" folders

MatchAndSplitAmongRaters.R
1. for multiple users. split rows to 
2. for single sheet, simply split rows. 

Ignore sheet: 03_09_2021_vero #1 - Vero wasn't recognized by otter.ai transcripts

REM0407.R
1. "all" was treated as a separate node
2. JeffreyREM.R was an updated version of REM0407.R

raw sheet (from Henry's labeling folder) --> perfect sheet (as in Evan's folder) --> 
run REM (JeffreyREM.R) --> store coefficients (z-score, etc.) in a corresponding sheet --> 

match team and person id using "user names and ids" in *Simon-analysis" with scores 
"all performance scores Feb 2023", say "fluency_team_1", regress creativity scores on coefficients from coefficient sheet 

Vero type info is identifiable by condition column. Round is identifiable by file.name column. 

01_21_2021 does not have Vero type (pilot session). 


