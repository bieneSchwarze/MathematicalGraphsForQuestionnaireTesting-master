# MathematicalGraphsForQuestionnaireTesting

The R-Scripts (version 4.0.3, https://cran.r-project.org/) provided supplement the paper draft "Using Mathematical Graphs for Questionnaire Testing in Large-Scale Surveys" by K. Stark (katharina.stark@lifbi.de) and S. Zinn (szinn@diw.de).

This paper introduces a flexible approach to test electronic questionnaires in different aspects by representing questionnaires as mathematical graphs. 

We illustrate the approach with a simple example and three error-scenarios for a programmed questionnaire (error-free: baseline scenario, erroneously omitted question: scenario 1, too few cases in related sample: scenario 2). The provided R-scripts contain the simulation of the data for the simple example mimicking empirical data ("1_SimpleExample_DataSimulation.R"), as well as the procedure of the graph generations and the procedure to test the questionnaires with detailed remarks of all three scenarios ("2_SimpleExample_BaselineScenario.R", "3_SimpleExample_Scenario1.R", "4_SimpleExample_Scenario2.R"). With these files the simple example in the manuscript can be reproduced. The R-file "Functions.R" contains procedures / functions that are necessary to generate the questionaire graphs and has to be loaded to R before running the example (is already part of the code).

The procedure we have created can and should also be used for other / own applications (than the one we have presented for illustrative purposes). We have created a template (in R, "Template_GraphGenerationTesting.R") for this purpose. The first part of that R source code file creates a questionnaire graph from a progression table. The second part tests the generated questionnaire graph for filter and path errors as well as for redundant questionnaire parts (based on associated data collected with the underlying questionnaire). In particular, parts in the source code that need to be supplemented with regard to your own application are marked with a 'note'. The R-file Functions.R contains procedures / functions that are necessary to generate the questionaire graphs and has to be loaded to R before the template. (As with the simple example: The R-file "Functions.R" contains procedures / functions that are necessary to generate the questionaire graphs and has to be loaded to R before running the code.)

