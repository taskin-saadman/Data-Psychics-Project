# DATA2902 Group Project 1 - DATA Psychics

> **Award:** This project was awarded the **Best Project Prize for DATA2902 in 2026S2**.

# Documentation
## Repository Notice & Attribution

> **Academic Project - Repository Mirror**
>
> This repository is a personal mirror of the original [DATA2902-Psychics repository](https://github.sydney.edu.au/tbis0085/DATA2902-Psychics), developed collaboratively as part of **DATA2902 at the University of Sydney**.
>
> The mirror is maintained for **academic reference and portfolio purposes**. The project is the collective work of its student contributors, and its presence on this personal GitHub account does not imply individual ownership of the original work.
>
> **Data ownership and usage:** The dataset used in this project was provided through the University of Sydney DATA2X02 Team for educational purposes. All rights to the dataset remain with the University of Sydney.
>
> The University of Sydney does not endorse or officially maintain this personal mirror.

## Shiny App: https://fliu0227.shinyapps.io/DATA_Psychics_App/

The shiny app investigates how different lifestyle factors are associated with academic performance, and vice versa, via Chi-Square tests, Two Sample tests, and Monte Carlo permutation tests. The app is interactive and allows users to explore different variables and hypotheses.

## QUESTION 1 TAB
Question 1 contains three tabs: Introduction, Chi-Square Independence, and CMH Chi-Square. The Chi-Square Independence and CMH Chi-Square tabs have sidebars which detail the HATPC framework.

The Introduction tab introduces the user to research Question 1 and outlines the statistical tests used. The sidebar informs the user which variables are being used to answer the question and what the results mean.

The second tab details the Pearson Chi-square Test of Independence with the HATPC process outlined on the sidebar. The user is able to select two categorical variables, and the test runs on whichever variables are selected.

The third tab details the Cochran-Mantel-Haenszel (CMH) Chi-square Test. The Sidebar details the HATPC process and allows the user to change how the Contingency Table is displayed (Combined, Domestic, or International).

A limitation of the Chi-Square Test tabs is that while the user can select different variables to run the test, the HATPC framework on the sidebar remains static and does not change to reflect the selected variables.

## QUESTION 2 TAB
Question 2 conducts two sample tests. The side bar allows the user to interact with the app, as it has two drop-down menus for the user to select variables, and two selection menus for the user to select the alternative hypothesis and significance level.

The first tab is the introduction tab. The introduction has the current research question based on the variables selected in the drop-down menus, as well as a brief overview of the three possible tests.

The second tab performs assumption checking if a numerical variable is chosen. The test with ordinal variables does not require checking assumptions of normality or homogeneity of variance. Normality is assessed with histograms and Q-Q plots, while homogeneity of variance is assessed with boxplots.

The third tab presents the results of the hypothesis test in the HATPC format. The user can pick in the sidebar the pair of variables and the alternative hypothesis they are interested in, and the results will be displayed accordingly.

As some of the distributions of the numerical data are very irregular, it may be prudent to have the option to perform the Wilcoxon Rank-Sum test as well as the regular Students’ or Welch’s t-test.

## QUESTION 3 TAB
The introduction tab outlines the research question and provides an overview of the statistical method used. It also gives a brief explanation of how users can interact with the module and interpret the results.

The Rental Status vs Grade tab investigates whether there is a difference in mean grades between rental status groups. Users can select the unit of interest, either DATA1X01 or DATA2X02, using a drop-down menu.

The Sleep Hours vs Enrolment tab follows the same procedure but examines the difference in mean sleep hours between DATA2X02 enrolment groups. The only user-controlled input on this tab is the number of permutations used for the Monte Carlo test.

As the program does not set a random seed, every click of the "Run Permutation Test" button will generate a new set of random permutations, potentially producing slightly different null distributions.

## AI Usage Statements
- Taskin: I used GPT 5.6 Sol by OpenAI for helping me with the coding part (tabs) and suggesting me statistical approaches for the chi-squared test tabs. All of the work was reviewed by me.
- Felix: I used Opus 5 by Claude to help with formatting the written portion of the statistical tests, including symbols and sub/superscripts.
- Nadhira: I used Gemini to clarify the framework of the Monte Carlo resampling test, including how to apply a HAPTC framework onto it.
- Tuhin: I used Sonnet 5 by Claude to assist with CSS bugs and Gemini Plus to help with resampling graphs
