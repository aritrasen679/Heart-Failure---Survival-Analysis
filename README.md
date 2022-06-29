# Heart-Failure---Survival-Analysis

Survival Analysis is a branch of statistical modelling that is optimal for working with censored, time-to-event data.
Such data-sets are common in medical studies. Given a cohort of patients and an observation window, we would observe the event (such as death, commencement/recovery of/from disease etc) for some of the patients, whereas other patients would be censored, either because the observation period ended or because they dropped out from the study prematurely.
The challenge is to incorporate both types of outcomes (actual event vs censoring) as well as the time taken for the observation, in order to come up with a statistical distribution of the time-to-event. 
In this notebook, I explore 2 models:
1.	Kaplan-Meier Estimator
2.	Log-Rank Test
3.	Cox Proportional Hazard Model

The Kaplan-Meier estimator is a test statistic that gives us an approximation of the true survival function of a population, the approximation getting better with increasing sample size. This estimator can robustly handle censoring, and can be derived from the Hazard Function using Maximum Likelihood Estimation.
This estimator can be used for simple comparison of survival rates between groups (For instance, survival rates between smokers and non-smokers).
The Cox Proportional Hazard Model is a survival analysis model that assumes that the baseline hazard function of a population is multiplicatively influenced by the covariates. For instance, given a group of smokers and non-smokers, the baseline hazard of the population is multiplied by 2 in case of smokers.

