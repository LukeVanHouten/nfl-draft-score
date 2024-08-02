# nfl-draft-score

WIP project to score each college whose players were drafted across every NFL draft beginning in 1936. Schools who had players drafted high and had more players drafted in general are scored higher. Scoring functions transformed in multiple ways to weight pick number vs number of picks by using the Tukey ladder of powers. Functions included currently are $\sum\limits^p_{i = 1} \frac{1}{n}$, $\sum\limits^p_{i = 1} \frac{1}{\sqrt{n}}$, and $\sum\limits^p_{i = 1} \frac{1}{\ln(n + \epsilon)}$, where $p$ represents each pick a college had, $n$ is the pick number, and $\epsilon$ is a number that adds bias to the log transformation (it is for now set to $e - 1$ so that the function starts at 1). I plan on adding a linear regression between these scores and other metrics of CFB team skill, TBD.

Warning: Any duplicate college names were fixed manually in Excel in the drafts.csv file using the find and replace tool. Rebuilding this file using data.R will bring back the duplicates. These duplicates are:

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

The code to find more similar schools is in `similar names.txt`, but the only problematic ones are the ones listed above.

