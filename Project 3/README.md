This study evaluates the causal effect of Payment Protection Plan (PPP) enrollment on credit card default risk using a customer-level dataset of demographic and financial variables. A key challenge is the presence of confounding, as customers who enroll in PPPs are likely already at higher risk of default. To address this, we estimate the Average Causal Effect (ACE) using two complementary approaches: Outcome Regression (OR) and Inverse Probability Weighting (IPW). A central focus of the analysis is the selection of an appropriate adjustment set of covariates. We compare traditional variable selection using LASSO with causal discovery methods, including PC, Rank PC, and Rank-based Interleaved IAMB, which aim to identify Markov Blankets around the treatment and outcome variables. To ensure robustness, a bootstrap procedure is employed to assess estimation stability and bias. 

Results show that causal discovery methods yield more parsimonious and stable models compared to LASSO, particularly in the presence of highly correlated and non-Gaussian financial variables. Across methods, PPP enrollment is consistently associated with an increase in default probability of approximately 10% to 30%, a counter-intuitive finding given the program’s intended purpose. This suggests potential structural issues in the PPP design or the presence of unobserved confounders.

To create the Report.pdf:

1. Run the plot-for-eda.Rmd file. Ensure the OUTPUT_IMAGES_EDA folder is already created.

2. Run ANALYSIS_V6, which will run the analysis and output images. This mandates A and runs conditional independence tests at a 5% significance level, as discussed throughout the report. Note this takes several hours to run due to bootstrapping. Ensure the ANALYSIS_V6 folder is already created.

3. Run the files in the ANALYSIS_TESTING_1ALPHA_2OPTA subfolder, which runs the analysis at 1% and 10% significance levels, and also tests the impact of not mandating A. ANALYSIS_V6_10 checks significance levels of 10%, ANALYSIS_V6_01 tests 1%, and ANALYSIS_V5 tests 5% but not mandating A. Note this takes several hours to run due to bootstrapping. Ensure all subfolderes are already created.

4. Run the Report.Rmd file, which will output the final report. This incorporates images output from running ANALYSIS_V6, ANALYSIS_V5, and plot-for-eda.

5. The separate Appendix.Rmd file contains images of outputs from the ANALYSIS_V6.Rmd. Several of those are output automatically, but the markov blankets are not; screenshot those appropriately to obtain the images needed to run Appendix.Rmd.