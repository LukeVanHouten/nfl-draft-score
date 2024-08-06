# nfl-draft-score

WIP project to score each college whose players were drafted across every NFL draft beginning in 1936. Schools who had players drafted high and had more players drafted in general are scored higher. Scoring functions transformed in multiple ways to weight pick number vs number of picks by using the Tukey ladder of powers. Functions included currently are $\sum\limits^p_{i = 1} \frac{1}{n}$, $\sum\limits^p_{i = 1} \frac{1}{\sqrt{n}}$, and $\sum\limits^p_{i = 1} \frac{1}{\ln(n + \epsilon)}$, where $p$ represents each pick a college had, $n$ is the pick number, and $\epsilon$ is a number that adds bias to the log transformation (it is for now set to $e - 1$ so that the function starts at 1). I also have linear models set up to compare these draft scores with Reddit user jimbobbypaul's ranking of the top 131 FBS programs between 1983 and 2022.

In order to get these graphs locally, just download `draft.R`, `drafts.csv`, and both .csv files containing "jbp" in the filename. Then just run the script in RStudio, and adjust the function values to your liking.

Warning: Any duplicate college names were fixed manually in Excel in the drafts.csv file using the find and replace tool. Rebuilding this file using data.R will bring back the duplicates. These duplicates are:

 - West. Michigan/Western Michigan
 - East. Michigan/Eastern Michigan
 - West. Illinois/Western Illinois
 - East. Illinois/Eastern Illinois
 - SE Missouri St./Southeast Missouri St.
 - NW Missouri St./Northwest Missouri St.
 - Boston Col./Boston College
 - East. Kentucky/Eastern Kentucky
 - Md-Eastern Shore/MD-Eastern Shore
 - Baldwin Wallace/Baldwin-Wallace
 - Lenoir Rhyne/Lenoir-Rhyne
 - S.F. Austin/Stephen F. Austin
 - Angelo St./Angelo State (TX)
 - UTSA/Texas-San Antonio
 - UTEP/Texas-El Paso
 - UConn/Connecticut
 - ULM/La-Monroe
 - FIU/Florida International
 - FAU/Florida Atlantic
 - UCF/Central Florida
 - Mid. Tennessee St./Middle Tennessee
More
All instances of "State" were replaced with "St."

The code to find more similar schools is in `similar names.txt`, but the only problematic ones are the ones listed above. This code also missed a lot of the duplicates, specifically the ones under Angelo St. in the list above. This is because `agrep()` isn't able to get "Florida International" from "FIU" for example.
