# nfl-draft-score

WIP project to score each college whose players were drafted in the NFL draft. Schools who had players drafted high and had more players drafted in general are scored higher. Scoring functions transformed in multiple ways to weight pick number vs number of picks by using the Tukey ladder of powers. Functions included currently are $\sum\limits^p_{i = 1} \frac{1}{n}$, $\sum\limits^p_{i = 1} \frac{1}{\sqrt{n}}$, and $\sum\limits^p_{i = 1} \frac{1}{\ln(n + \epsilon)}$, where $p$ represents each pick a college had, $n$ is the pick number, and $\epsilon$ is a number that adds bias to the log transformation (it is for now set to $e - 1$ so that the function starts at 1). I plan on adding a linear regression between these scores and other metrics of CFB team skill, TBD.

I should be able to build up the dataset used to include every NFL draft. The script will then plot the scores for each of the above functions.
