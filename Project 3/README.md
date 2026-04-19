[![Review Assignment Due Date](https://classroom.github.com/assets/deadline-readme-button-22041afd0340ce965d47ae6ef1cefeee28c7c493a6346c4f15d667ab976d596c.svg)](https://classroom.github.com/a/uyATYxY5)

To create the Report.pdf:

1. Run the plot-for-eda.Rmd file. Ensure the OUTPUT_IMAGES_EDA folder is already created.

2. Run ANALYSIS_V6, which will run the analysis and output images. This mandates A and runs conditional independence tests at a 5% significance level, as discussed throughout the report. Note this takes several hours to run due to bootstrapping. Ensure the ANALYSIS_V6 folder is already created.

3. Run the files in the ANALYSIS_TESTING_1ALPHA_2OPTA subfolder, which runs the analysis at 1% and 10% significance levels, and also tests the impact of not mandating A. ANALYSIS_V6_10 checks significance levels of 10%, ANALYSIS_V6_01 tests 1%, and ANALYSIS_V5 tests 5% but not mandating A. Note this takes several hours to run due to bootstrapping. Ensure all subfolderes are already created.

4. Run the Report.Rmd file, which will output the final report. This incorporates images output from running ANALYSIS_V6, ANALYSIS_V5, and plot-for-eda.

5. The separate Appendix.Rmd file contains images of outputs from the ANALYSIS_V6.Rmd. Several of those are output automatically, but the markov blankets are not; screenshot those appropriately to obtain the images needed to run Appendix.Rmd.