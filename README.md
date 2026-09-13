# DATA2902 Group Project 1 - DATA Psychics

# Documentation
## GitHub Repository: https://github.sydney.edu.au/tbis0085/DATA2902-Psychics/tree/main 
## Shiny App:  https://taskin-saadman.shinyapps.io/DATA_Psychics_App/

The shiny app investigates how different lifestyle factors are associated with academic performance, and vice versa, via Chi-Square tests, Two Sample tests, and Monte Carlo permutation tests. The relevant tabs with each test are titled Question 1, Question 2, and Question 3, respectively. The introduction and appendix tabs give context to the research process and resources used.

## QUESTION 1 TAB
Question 1 contains three tabs: Introduction, Chi-Square Independence, and CMH Chi-Square. The Chi-Square Independence and CMH Chi-Square tabs have sidebars which detail the HATPC framework.

The Introduction tab introduces the user to research Question 1 and outlines the statistical tests used. The sidebar informs the user which variables are being used to answer the question and what significance level is applied to the results. 

The second tab details the Pearson Chi-square Test of Independence with the HATPC process outlined on the sidebar. The user is able to select two categorical variables, and the test runs on whichever variables the user selects, unless they are the same. After the user selects their desired variables, the Contingency Table Display, Test Result, Chi-square Distribution, Assumption Check and Interpretation update to reflect the results of the two variables chosen. The user is able to change the Contingency Table Display (Observed Counts, Expected Counts or Row Proportions) using the display radio button.

The third tab details the Cochran-Mantel-Haenszel (CMH) Chi-square Test. The Sidebar details the HATPC process and allows the user to change how the Contingency Table is displayed (Combined, Domestic or International). The CMH Chi-square Test includes the Contingency Table by Student Status, Test Result, Chi-Square Distribution and the Adjusted Interpretation. 

A limitation of the Chi-Square Test tabs is that while the user can select different variables to run the test, the HATPC framework on the sidebar remains static and does not change to reflect the new selections. This may limit interpretability as it may not align with the results displayed.

## QUESTION 2 TAB
Question 2 conducts two sample tests. The side bar allows the user to interact with the app, as it has two drop-down menus for the user to select variables, and two selection menus for the user to pick between numerical or ordinal variables, and alternative hypotheses.

The first tab is the introduction tab. The introduction has the current research question based on the variables selected in the drop-down menus, as well as a brief overview of the three possible tests that the app can perform based on what criteria the variable data meets. 

The second tab performs assumption checking if a numerical variable is chosen. The test with ordinal variables does not require checking assumptions of normality or homogeneity of variance. Normality is checked via the q-q plot, and a visual demonstration of the variance can also be seen in the violin and box plots. For additional information on the distribution, such as the median values, the user can hover over the violin and box plots. Fulfilment of equal variance assumption, as checked in Levene’s test, is used to decide whether a Student’s t-test or Welch’s t-test is conducted. 

The third tab presents the results of the hypothesis test in the HATPC format. The user can pick in the sidebar the pair of variables and the alternative hypothesis they are interested in, and the appropriate test is automatically conducted with the following logic: if a numerical variable is picked and the groups have equal variance, a Student’s two sample t-test is performed; if a numerical variable is picked and the groups do not have equal variance, a Welch’s two sample t-test is performed; and if an ordinal variable is picked, a Wilcoxon Rank-Sum test is performed.

As some of the distributions of the numerical data are very irregular, it may be prudent to have the option to perform the Wilcoxon Rank-Sum test as well as the regular Students’ or Welch’s t-test, and compare the conclusions for both tests to arrive at a more well-considered answer.

## QUESTION 3 TAB
The introduction tab outlines the research question and provides an overview of the statistical method used. It also gives a brief explanation of how users can interact with the module and interpret its output.

The Rental Status vs Grade tab investigates whether there is a difference in mean grades between rental status groups. Users can select the unit of interest, either DATA1X01 or DATA2X02, using a drop down menu. They can also specify the number of permutations used through a numeric input, with the number constrained between 1 and 100,000 permutations and a default value of 1000. After clicking the "Run Permutation Test" button, the app performs a two-sided two-sample Monte Carlo permutation t-test and displays a null distribution showing the simulated differences in mean grades between rental status groups. The corresponding hypothesis test results are then presented using the HAPTC framework.

The Sleep Hours vs Enrolment tab follows the same procedure but examines the difference in mean sleep hours between DATA2X02 enrolment groups. The only user-controlled input on this tab is the number of permutations, which is similarly constrained as before. After the user runs the test by clicking the button, the app produces a null distribution of the difference in mean sleep hours between enrolment groups and presents the hypothesis test using the HAPTC framework.

As the program does not set a random seed, every click of the "Run Permutation Test" button will generate a new set of random permutations, potentially producing slightly different null distributions and p-values, limiting exact reproducibility of individual results. The program also limits the number of permutations past 100,000 to limit computational time.

## AI Usage Statements
- Taskin: I used GPT 5.6 Sol by OpenAI for helping me with the coding part (tabs) and suggesting me statistical approaches for the chi-squared test tabs. All of the work was reviewed by me.
- Felix: I used Opus 5 by Claude to help with formatting the written portion of the statistical tests, including symbols and sub/superscripts.
- Nadh: I used Gemini to clarify the framework of the Monte Carlo resampling test, including how to apply a HAPTC framework onto it.
- Tuhin: I used Sonnet 5 by Claude to assist with CSS bugs and Gemini Plus to help with resampling graphs
