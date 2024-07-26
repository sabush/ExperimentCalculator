---
title: "Using the Experiment Calculator"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Using the Experiment Calculator}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---



## Running the power calculator locally

To run the power calculator locally, run the following code:


``` r
library(ExperimentCalculator)
#ExperimentCalculator::run_app() # Uncomment to run the apps
```

If you don't want to run locally, the calculator is also available on shinyapps.io here (https://stephen-bush.shinyapps.io/ExperimentCalculator/)

## Planning an Experiment

The first two tabs of the calculator provides tools to plan an experiment. The first tab does this for a binary outcome (where the outcome is a yes/no such as a conversion rate), and the second tab does this for a normal outcome (such as a length, revenue, or a count of events per user).

There are also two modes for experiment planning, either calculating the effect size for measuring a fixed effect size, or determining the effect size that can be detected for a given sample size. The first of these is more traditional for a sample size calculator, but the second is useful when there is a constraint on the amount of sample available for an experiment. In this case you are able to calculate the smallest effect size that you will be able to detect with the desired level of power (the minimum detectable effect), which can be compared to the expected effect size of the experiment to determine whether the experiment size is reasonable.

The information needed for both types of outcome are slightly different, and we will consider these in turn. 

### Binary Outcome

If the outcome that you are planning the experiment to measure is binary (yes/no, or good/bad), then you will need to perform power calculations based on tests for proportions. 

Preparation - using existing data, or research, estimate the outcome rate in the baseline group. If you already measure the outcome, and you expect the outcome to be similar during the experiment, you can use this. 

* Enter this proportion under 'Success rate in the baseline group'

There are other experiment design factors that also need to be defined to perform calculations

* (If calculating smallest effect) The available sample size across all groups
* The proportion of traffic/users in each experiment group. If you have more than two experiment groups/variants, then right click on the table and select 'Insert rows above' to add rows to the table.
* The effect type that you want to use
  * Absolute effect - a raw difference between response rates in the baseline group and a different experiment group. So a 2% increase from a baseline of 20% will give a response rate of 22%
  * Relative effect - the relative increase between response rates in the baseline group and a different experiment group as a percentage of the baseline. So a 2% relative increase from a 20% baseline gives a response rate of 20.4% (20% * 1.02)
* (If calculating sample size) The smallest effect size that you want to be able to detect in the experiment (minimum detectable effect). Note that the smaller the effect is, the larger the required sample size.
* The level of significance - the level of risk that you are willing to accept that you get a significant result if there is no difference between groups
* The level of power - the likelihood that you will get a significant result if the true difference is the minimum detectable effect.
* If there are more than two groups, then there are two more relevant options:
  * Comparisons to make - do you want to compare all groups to the baseline group, or all pairs of groups. The latter option involves more comparisons.
  * Multiple comparisons correction - do you want to correct for multiple comparisons? Note that not correcting means that there is an increased chance of a false positive in at least one group.

INSERT IMAGE

Once this information is entered, the power calculator will determine either the required sample size or minimum detectable effect, and construct power plots for each pair of treatments. The power plot can be downloaded by selecting the camera icon when hovering over the plot.


### Normal Outcome

If the outcome that you are planning the experiment to measure is numeric, and you plan on having a sufficiently large sample size (say, over 100 in each group), then you will need to perform power calculations based on tests for means. If the sample size in each group is expected to be small, then the validity of these tests will depend on the expected distribution of the outcome. 

Preparation - using existing data, or research, estimate the mean and standard deviation of the outcome metric in the baseline group. If you already measure the outcome, and you expect the outcome to be similar during the experiment, you can use this.

There are other experiment design factors that also need to be defined to perform calculations

* (If calculating smallest effect) The available sample size across all groups
* The proportion of traffic/users in each experiment group. If you have more than two experiment groups/variants, then right click on the table and select 'Insert rows above' to add rows to the table.
* The effect type that you want to use
  * Absolute effect - a raw difference between response rates in the baseline group and a different experiment group. So a 2% increase from a baseline of 20% will give a response rate of 22%
  * Relative effect - the relative increase between response rates in the baseline group and a different experiment group as a percentage of the baseline. So a 2% relative increase from a 20% baseline gives a response rate of 20.4% (20% * 1.02)
* (If calculating sample size) The smallest effect size that you want to be able to detect in the experiment (minimum detectable effect). Note that the smaller the effect is, the larger the required sample size.
* The level of significance - the level of risk that you are willing to accept that you get a significant result if there is no difference between groups
* The level of power - the likelihood that you will get a significant result if the true difference is the minimum detectable effect.
* If there are more than two groups, then there are two more relevant options:
  * Comparisons to make - do you want to compare all groups to the baseline group, or all pairs of groups. The latter option involves more comparisons.
  * Multiple comparisons correction - do you want to correct for multiple comparisons? Note that not correcting means that there is an increased chance of a false positive in at least one group.

INSERT IMAGE

Once this information is entered, the power calculator will determine either the required sample size or minimum detectable effect, and construct power plots for each pair of treatments. The power plot can be downloaded by selecting the camera icon when hovering over the plot.


## Significance testing

Once an experiment is complete, the next step is to analyse and communicate the results. The next to tabs assist with this. Once provided with summary data from the experiment, a summary of the experiment is provided, as well as plots with group differences and with group summaries. 

As with the power calculations, the analysis approach will depend on the outcome type. There is a tab for a binary outcome and a tab for a numeric (normal) outcome metric.

### Binary Outcome

To determine whether there are significant differences between groups, we will need to know the number of observations and number of positive outcomes in each group. Enter this information into the table at the top left of the page, being sure to make the first group your baseline/control group.

There are additional variables that you will need to include to perform analysis
* The effect type that you want to use
  * Absolute effect - a raw difference between response rates in the baseline group and a different experiment group. So a 2% increase from a baseline of 20% will give a response rate of 22%
  * Relative effect - the relative increase between response rates in the baseline group and a different experiment group as a percentage of the baseline. So a 2% relative increase from a 20% baseline gives a response rate of 20.4% (20% * 1.02)
* The level of significance - the level of risk that you are willing to accept that you get a significant result if there is no difference between groups
* If there are more than two groups, then there are two more relevant options:
  * Comparisons to make - do you want to compare all groups to the first group (which should be the control/baseline), or all pairs of groups.
  * Multiple comparisons correction - do you want to correct for multiple comparisons? Note that not correcting means that there is an increased chance of a false positive in at least one group.
  
Once this information is entered, the app will produce a summary, as well as two plots. The first plot shows the difference between each pair of groups (depending on which groups you have asked to compare), and the second plot summarises each group. These plots can be downloaded by selecting the camera icon when hovering over the plot.

### Normal Outcome

To determine whether there are significant differences between groups, we will need to know the number of observations and both the mean and standard deviation of the outcome metric for observations in each group. Enter this information into the table at the top left of the page, being sure to make the first group your baseline/control group.

There are additional variables that you will need to include to perform analysis
* The effect type that you want to use
  * Absolute effect - a raw difference between response rates in the baseline group and a different experiment group. So a 2% increase from a baseline of 20% will give a response rate of 22%
  * Relative effect - the relative increase between response rates in the baseline group and a different experiment group as a percentage of the baseline. So a 2% relative increase from a 20% baseline gives a response rate of 20.4% (20% * 1.02)
* The level of significance - the level of risk that you are willing to accept that you get a significant result if there is no difference between groups
* If there are more than two groups, then there are two more relevant options:
  * Comparisons to make - do you want to compare all groups to the first group (which should be the control/baseline), or all pairs of groups.
  * Multiple comparisons correction - do you want to correct for multiple comparisons? Note that not correcting means that there is an increased chance of a false positive in at least one group.
  
Once this information is entered, the app will produce a summary, as well as two plots. The first plot shows the difference between each pair of groups (depending on which groups you have asked to compare), and the second plot summarises each group. These plots can be downloaded by selecting the camera icon when hovering over the plot.

## Sample Ratio Check

A critical quality check for an experiment is to see whether the number of users (entities) in each experiment group matches the expected proportion when designing the experiment. If these are substantially different then the experiment results are not trustworthy and there may be data or implementation quality issues, such as inconsistent tracking or poor randomisation. Kohavi, Tang and Xu (2020) recommendation a threshold of 0.001 (0.1%).  

This tab allows you to perform a sample ratio check for an experiment with two or more groups.

1. In the table of expected proportions and actual counts, create a row for each group/variant of the experiment. If you need to add or remove rows, right click on the table and select "Insert row above" or "Remove row".
2. For each group, enter the number of observations that you have in that group in the column titled "sample_size" and the proportion of observations that you expect to be in that group (based on the design) in the column titled "expected_proportion".
3. (Optional) Set the threshold for the sample ratio mismatch test. Note that by default the threshold is set to the recommendation above.

INSERT IMAGE

On the right hand panel, there is a statement about whether there is a sample ratio error as well as a p-value for the test.

