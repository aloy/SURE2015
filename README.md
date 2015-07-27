# SURE2015 

Code and files for research project with Alex Damisch, Summer 2015

## Folders
- One-Sample Bootstrap: Shiny app
    - User file input and variable choice, plots, bootstrapping for mean/median/standard deviation, normal- and percentile-based confidence intervals
- Two-Sample Bootstrap: Shiny app
    - User file input and grouping/response variable choice, plots, bootstrapping for mean/median difference and mean/median/standard deviation ratio, normal- and percentile-based confidence intervals
- Permutation Tests: Shiny app 
    - User file input and grouping/response variable choice, permutes groups, p-value options
- Regression Permutation: Shiny app
    - User file input and x/y variable choice, permutes variables, normal- and percentile-based bootstrap confidence intervals, prediction intervals for user-inputted ŷ
- ANOVA: Shiny app
    - User file input and grouping/response variable choice, plots with additional categorical options, ANOVA summary, individual/multiple comparison confidence intervals, permutation F-test
- Model Selection: Shiny app
    - User file input and response variable choice, displays model estimate summary based on user-selected variables, performs AIC/BIC backwards selection from full model and all subsets selection (if sufficiently few variables), displays plots and calculates influence based on various methods for full or user-inputted models, confidence intervals for coefficients
    - Cross-validation still a work in progress
- r
    - Raw R code for implementation in above apps
- trial ggvis
    - Basic app to display ggvis graphics, requires user inputted data set (optional in other apps) 
