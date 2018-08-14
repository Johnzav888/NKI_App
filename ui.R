ipak <- function( pkg ){  # Check if the packages are installed, and if not install them. Finally load them!
  new.pkg <- pkg[!( pkg %in% installed.packages()[, "Package"] ) ]
  if ( length( new.pkg ) ) 
    install.packages(new.pkg, dependencies = TRUE )
  sapply( pkg, require, character.only = TRUE )
}

# usage
packages <- c( "shiny", "ggplot2", "dplyr", "shinyjs", "shinythemes","plotly","samplesize","fBasics", "DT", "survival")
ipak( packages )


#library(shiny)
#library(ggplot2)
#library(dplyr)
#library(DT)
#library(shinyjs)
#library(shinythemes)
#library(plotly)
#library(samplesize)
#library(fBasics)
#library(survival)
#library(crayon)

#library(shiny.router)

#source("https://raw.githubusercontent.com/daattali/advanced-shiny/master/busy-indicator/helpers.R")

#saveRDS(withBusyIndicatorUI, file = "shinyhelper1")
#saveRDS(withBusyIndicatorServer, file = "shinyhelper2")

source("Helpers.R")
#options(shiny.launch.browser = .rs.invokeShinyWindowViewer)


ui <- tagList(
  tags$head(
    tags$script("src"="app.js")
  ), fluidPage(navbarPage(theme=shinytheme("spacelab"), title = div(img(src = "images/logo-AVL.jpg", height = "40px", width = "120px"
                                                                        ))#, h4("NKI_statistics", 
                                                                         #style = "font-family: 'Lobster', cursive;
                                                                        #font-weight: 500; line-height: 1.1; 
                                                                        #color: #e8ea0f;")) 
                           , fluid = FALSE, windowTitle = "NKI_statistics", 
                           tabPanel("Design and statistical analysis of mice experiments", value = "StartPage",
                                    
                                    HTML(paste0("<br/>", "<br/>","<br/>",
                                                    "<center>This app supports researchers conducting mice experiments in the Netherlands Cancer Institute in the statistical aspects of the studies.
                                                      It provides explanations of basic statistical concepts and tests. Moreover, researchers can use this app to calculate sample size and power
                                                    when an experiment is being designed. The latter is vital and must be performed, because:</center>",
                                                    "<br/>", "<br/>","<br/>",
                                                    "<i><center><strong> More power increases the confidence in the results, whether they are significant or not </strong></center></i>",
                                                    "<br/>", "<br/>","<br/>",
                                                    "Details about sample size calculations can be found in the  ")),
                                               
                                               #tags$div(
                                              #        id = "sdf",
                                               #       tags$a(onclick = "customHref('Power')", "Power")),
                                               #tags$div(
                                                 #id = "Others",
                                                 tags$a("Statistical Power section", onclick = "customHref('Page2');customHref('tab1');")
                                                 #tags$a("ff2", onclick = "customHref('Others');")
                                              
                                      ,
                                      
                                    fluidRow(
                                      column(8, HTML(paste0("<br/>", "<br/>","<br/>",
                                      "Sample size calculation depends on the type of experiment. The most common experimental designs used for mice experiments at the NKI compare groups of mice with respect to mean/median values, survival outcomes,
                                      proportions and tumor growth. Examples of such experiments are listed below and more information can be found under the specific tabs.")))),
                          
                                    fluidRow(
                                      column(8, HTML(paste0("<br/>", "<br/>","<br/>",
                                      "<i><strong><font color='red'> Comparison of mean/medians</font></strong></i>",
                                      "<br/>", "<br/>","<br/>",
                                      "<i>Example:</i> : A scientist wants to test the hypothesis that a novel compound reduces high-density lipoprotein (HDL) cholesterol levels 
                                        in a transgenic C57Bl/6J strain of mice compared with standard treatment. Therefore she randomizes half of the mice to the standard treatment and the other half to the new treatment, in order to compare average HDL cholesterol levels between the two groups",
                                      "<br/>", "<br/>","<br/>")))),
                          
                          tags$a("Go to Mean/Median Analysis", onclick = "customHref('Page4');"),
                          
                          
                          fluidRow(
                            column(8, HTML(paste0("<br/>", "<br/>","<br/>",
                               "<i><strong><font color='red'> Survival Analysis </font></strong></i>",
                               "<br/>", "<br/>","<br/>",
                               "<i>Example:</i> To evaluate whether treatment with paclitaxel improves survival after esophageal adenocarcinoma (EAC), a scientist uses a peritoneal dissemination xenograft mouse model and injects human EAC cell lines intraperitoneally/subcutaneously into SCID mice.
                                Two weeks later, mice are randomly assigned to treatment by vehicle or paclitaxel (20mg/kg, 2 times a week for 2 weeks). Mice are followed until death or the end of the study and  the mouse survival times are compared between the 2 groups.",
                               "<br/>", "<br/>","<br/>")))),
               
                       tags$a("Go to Survival Analysis", onclick = "customHref('Page5');"),
                    
                       fluidRow(
                         column(8, HTML(paste0("<br/>", "<br/>","<br/>",
                            "<i><strong><font color='red'> Proportion Analysis</font></strong></i>",
                            "<br/>", "<br/>","<br/>",
                            "<i>Example:</i> A scientist wants to test the hypothesis that a new combination treatment is able to completely regress a specific tumor. The response to treatment is assessed using the 
                            percentage of tumor volume change (ΔVol) at the final study day (i.e., seven days after the last treatment) compared with the baseline tumor volume at Day 0.
                            The criteria for response classification is based on some pre-defined criteria.",
                            "<br/>", "<br/>","<br/>")))),
               
                      tags$a("Go to Proportion Analysis", onclick = "customHref('Page6');"),
    
                      fluidRow(
                        column(8, HTML(paste0("<br/>", "<br/>","<br/>",
                            "<i><strong><font color='red'> Growth Curve Analysis</font></strong></i>",
                          "<br/>", "<br/>","<br/>",
                          "<i>Example:</i> A scientist wants to test the hypothesis that a new treatment is able to suppress the tumor growth
                           compared with standard treatment. To assess that she conducts an experiment, where tumor cells are injected into mice and the volume of the growing tumor is measured every 2-3 days until 
                          the mouse dies or is sacrificed. When tumors reach 200mm3, she randomizes half of the mice to the standard treatment and the other half to the new treatment,
                          in order to compare the rate of tumor growth between groups.",
                          "<br/>", "<br/>","<br/>")))),
  
                      tags$a("Go to Growth Curve Analysis", onclick = "customHref('Page7');"),
  
                      HTML(paste0("<br/>", "<br/>","<br/>"))),
              
       tabPanel("Statistical Power", value = "Page2",
                                    withMathJax(),
                                      tabsetPanel(
                                        tabPanel("Interpretation", value = "tab1",
                                                 HTML("<script>$('#tab1').click(function() {
                                        tabs = $('.tabbable .nav.nav-tabs li')
                                                      tabs.each(function() {
                                                      $(this).removeClass('active')
                                                      })
                                                      $(tabs[1]).addClass('active')
                                                      
                                                      tabsContents = $('.tabbable .tab-content .tab-pane')
                                                      tabsContents.each(function() {
                                                      $(this).removeClass('active')
                                                      })
                                                      $(tabsContents[1]).addClass('active')
                                                      
                                                      $('#tab1').trigger('change').trigger('shown');
                                                      $(document).ready(function(){$(window).scrollTop(0);});
                                                      })</script>"),
                                                 
                                                 fluidRow(
                                                   column(8, HTML(paste0("<br/>","<br/>","<br/>","<br/>","<br/>","<br/>",
                                                            "An experiment is conducted to answer a particular research question, for instance, to investigate whether the outcome after a new treatment differs from the outcome after the standard treatment,
                                                            i.e. whether there is an effect of the new treatment. The results of a statistical hypothesis test are supposed to reflect the true state of nature, but they may not always do so.
                                                            A researcher can make two types of correct decisions and two types of errors, which is shown in the table below.",
                                                            "<br/>","<br/>","<br/>","<br/>","<br/>","<br/>")))),
                                                
                                                 tags$img(src = "images/HypothesisTest_table.PNG", width = "800px", height = "300px"),
                                                 
                                                 
                                                 fluidRow(
                                                   column(8, HTML(paste0(
                                                   "<br/>","<br/>","<br/>",
                                                   "The effect either exists or not in nature, while the result of the statistical analysis is either significant or non-significant. 
                                                   Therefore, based on the statistical analysis, a researcher either makes a correct inference about the effect or a false one.  
                                                   The type I error, which is denoted by the Greek letter \\(\\alpha\\) , is the probability of finding an effect when it does not exist in nature \\(\\alpha\\) is also called the significance level of a test
                                                   and the Type II error, denoted by the Greek letter \\(\\beta\\) , is the probability of not finding an effect when the effect exists in nature.  
                                                   So, the Type I error is the probability of a false positive finding, while the Type II error is the probability of a false negative finding.
                                                   Complements of the two probabilities, 1-\\(\\alpha\\)  and 1-\\(\\beta\\) , are probabilities of correctly not finding an effect (true negative finding) and correctly finding an effect (true positive finding),
                                                   respectively. The latter probability, 1-\\(\\beta\\) is called the statistical power of a test. The value of alpha is  usually fixed at 0.05. The value of beta decreases with increasing effect size
                                                   and sample size.")))),
                                                 fluidRow(
                                                   column(8, HTML(paste0("<br/>","<br/>","<br/>",
                                                  "If there is a true effect of a treatment, researchers would like to detect it with high probability. A power level of 0.8 or 0.9 is usually considered sufficient. 
                                                   For illustration, if 100 experiments are conducted with an existing true effect and each experiment has a power of 0.8 or 80%, the statistical analyses would be significant for 80 experiments
                                                   (and result in rejection of the hypothesis of no effect), while 20 experiments would result in a non-significant result of the statistical test, i.e., the true effect would be missed.
                                                   On the other hand, if none of the 100 experiments is based on a true effect, and a significance level of alpha=0.05 or 5% is used, then the statistical analysis of 5 experiments would be 
                                                   expected to be statistically significant (p<0.05), i.e., reflecting false positive (or chance) findings.",
                                                   "<br/>","<br/>","<br/>",
                                                   "Statistical power is a measure of confidence to detect an effect (i.e. significant result) if it truly exists.
                                                   The power depends on the sample size of an experiment and the magnitude of the effect. During the design phase of an experiment,
                                                   a researcher can assess how many mice need to be included in order to detect a true effect with sufficient probability. 
                                                   This assessment is important because an underpowered experiment (too few mice) can miss an effect that truly exists. 
                                                   An overpowered experiment (too many mice) can detect an effect that truly exists but is so small that is of practical relevance.
                                                   In both situations, resources spent on an experiment, such as money, time or mice's lives, are wasted",
                                                   
                                                   "<br/>","<br/>", "<br/>", "<br/>",
                                                   "* More Power increases chances of finding significant result.",
                                                   "<br/>",
                                                   "* More Power increases chances of replicating prior findings",
                                                   "<br/>",
                                                   "* More Power increases confidence about results, either significant or not",
                                                   "<br/>",
                                                   "<br/>", "<br/>", "<br/>",
                                                   
                                                   "So far, we assumed that a true effect does or does not exist. In reality, this is unknown. Let R be the probability that a true effect exists for a particular experiment or, 
                                                   in a large number of experiments (e.g., all experiments in a career), the proportion of experiments with a true effect. The table of decisions is then given by:  ",
                                                   "<br/>","<br/>","<br/>"
                                                   )))),
                                                 
                                                 tags$img(src = "images/Power_table.PNG", width = "800px", height = "300px"),
                                                 
                                                 fluidRow(
                                                   column(8, HTML(paste0(
                                                   "<br/>","<br/>","<br/>",
                                                   "Assume a scientist selects experiments so that a true effect exists for half of his experiments.
                                                   If he chooses the sample sizes so that power is 80%, he is expected to obtain significant tests for 40 of the 50 experiments with a true effect 
                                                   and miss the effect for the remaining 10 experiments. If power is 50%, only 25 of the 50 true effects will, on average, be identified. 
                                                   For each mice experiment, four important measures are considered:"
                                                 )))),
                                                 fluidRow(
                                                   column(8, HTML(paste0("<br/>","<br/>","<br/>",
                                                             h4("For each mice experiment, four important measures are considered:"),
                                                             "<br/>", "<br/>","<br/>",
                                                             "1.$$\\text{True positive rate} = \\frac{Power*R}{Power*R + (1-Power)*R} = Power$$",
                                                             "It is the probability of a significant result if the effect truly exists in nature",
                                                             "<br/>","<br/>","<br/>",
                                                             "2.$$\\text{True negative rate} =  \\frac{(1-\\alpha)*(1-R)}{(1-\\alpha)*(1-R) + \\alpha*(1-R)} = (1-\\alpha)$$",
                                                             "It is the probability of a non-significant result if the effect does not exist in nature. It is the complement of type-I error  \\(\\alpha\\)",
                                                             "<br/>","<br/>","<br/>",
                                                             "3.$$\\text{Positive Predictive Value(PPV)} = \\frac{Power*R}{Power*R + \\alpha*(1-R)}$$",
                                                             "It is the probability that the effect exists in nature given a significant result of the statistical test. As can be seen from the formula and the graph below, that probability increases with increasing power and R.",
                                                             "<br/>","<br/>","<br/>")))),
                                                 
                                                             tags$img(src = "images/PPV_0.05.png", width = "800px", height = "500px"),
                                                             
                                                 fluidRow(
                                                   column(8, HTML(paste0("<br/>","<br/>","<br/>",
                                                             "4.$$\\text{False Positive Report Probability(FPRP)} = 1-PPV = \\frac{\\alpha*(1-R)}{Power*R + \\alpha*(1-R)}$$",
                                                             "It is the probability that there is no effect in nature if the statistical test is significant. As can be seen from the formula and the graph below, this probability decreases with increasing power and R.",
                                                            
                                                             "<br/>", "<br/>", "<br/>"
                                                 )))),
                                                 
                                                 tags$img(src = "images/FPRP_0.05.png", width = "800px", height = "500px"),
                                                 
                                                 
                                                 fluidRow(
                                                   column(8, HTML(paste0("<br/>", "<br/>", "<br/>",
                                                             "Combining type 1 and type 2 errors, the false conclusion rate can be determined. As is illustrated in the graph below, this rate decreases with increasing power and decreasing R.
                                                             Moreover, when the prior probability of the effect existence is maximum, i.e. R=1, then the false conclusion rate depends only on the power level of the test and more precisely, 
                                                              it is actually equal to the Type II error rate \\(\\beta\\) or equivalently to 1-power. On the contrary situation, i.e. when R = 0, the false conclusion rate is equal to the Type I error rate \\(\\alpha\\).
                                                              As the power and R increase, this rate approaches zero. For fixed value of \\(\\alpha\\) and power, higher probability R is associated with more false experimental results and the lower the power the higher the influence of R on the false conclusion rate.",
                                                             "<br/>", "<br/>", "<br/>",
                                                             "For fixed value of \\(\\alpha\\) and power, higher probability R is associated with more false experimental results and the lower the power the higher the influence of R on the false conclusion rate.",  
                                                            "<br/>", "<br/>", "<br/>")))),
                                                 
                                                 
                                                 tags$img(src = "images/FC_0.05.png", width = "800px", height = "500px"),
                                                 
                                                 
                                                 HTML("<br/>", "<br/>", "<br/>","<br/>", "<br/>", "<br/>")
                                                 ),
                                       
                                        
                                        
                                        tabPanel("How to calculate Sample Size and Power ?", value = "tab2",
                                                 HTML("<script>$('#tab2').click(function() {
                                                      tabs = $('.tabbable .nav.nav-tabs li')
                                                      tabs.each(function() {
                                                      $(this).removeClass('active')
                                                      })
                                                      $(tabs[1]).addClass('active')
                                                      
                                                      tabsContents = $('.tabbable .tab-content .tab-pane')
                                                      tabsContents.each(function() {
                                                      $(this).removeClass('active')
                                                      })
                                                      $(tabsContents[1]).addClass('active')
                                                      
                                                      $('#tab2').trigger('change').trigger('shown');
                                                      $(document).ready(function(){$(window).scrollTop(0);});
                                                      })</script>"),
                                                 
                                                 fluidRow(
                                                   column(8, HTML(paste0("<br/>", "<br/>", "<br/>", "<br/>",
                                                             "<i><strong><font color='red'>Statistical power depends on three factors</font></strong></i>",
                                                             "<br/>","<br/>","<br/>",
                                                             "To determine the power of an analysis we need firstly to specify the alternative Hypothesis, Ha, or in other words,
                                                             the effect size that we are interesting in detecting. Further, and for most of analyses, power is proportional to the following:",
                                                             "<br/>", "<br/>",
                                                             
                                                             "*<i><strong><font color='red'>Effect size</font></strong></i> : an estimate of the size of the effect which can be measured as a difference in mean/median values, survival outcomes, proportions 
                                                             or growth rates; the bigger the effect size the higher the power",
                                                             "<br/>","<br/>", "<br/>", 
                                                             "*<i><strong><font color='red'>Sample size</font></strong></i> : the number of mice included in an experiment; the higher the number of mice the higher the power.",
                                                             "<br/>","<br/>", "<br/>", 
                                                             "*<i><strong><font color='red'>Significance level(\\(\\alpha\\))</font></strong></i> : the type-I error of a test; the higher the \\(\\alpha\\) the higher the power, but \\(\\alpha\\) is almost always fixed at 0.05.",
                                                             "<br/>","<br/>", "<br/>",
                                                             
                                                             "The power level can be assessed when the three factors are known or the required sample size needed for an experiment can be calculated when the power level and the two other factors are fixed.
                                                             Sample size can be calculated for any study design and statistical test. The results are only valid for an experiment using the specific design and statistical test.",
                                                             "<br/>", "<br/>", "<br/>", "<br/>", "<br/>")))),
                                                 
                                                 fluidRow(
                                                   column(8, HTML(paste0("<br/>","<br/>","<br/>",
                                                             "<i>The correct sample size can be obtained through the following steps:</i>",
                                                             "<br/>","<br/>","<br/>",
                                                             "1. Formulating the research question, i.e. defining clearly what the hypothesis of interest is.",
                                                             "<br/>","<br/>","<br/>",
                                                             "2. Identifying the statistical test to be performed on the data from the experiment.",
                                                             "<br/>","<br/>","<br/>",
                                                             "3. Determining a reasonable value for the expected effect size based on substantive knowledge, literature, or previous experiments, or selecting the smallest effect size that is considered as clinically important.",
                                                             "<br/>","<br/>","<br/>",
                                                             "4. Selecting the desired \\(\\alpha\\) level",
                                                             "<br/>","<br/>","<br/>",
                                                             "5. Selecting the desired power level and calculating the required sample size.",
                                                             "<br/>","<br/>","<br/>","<br/>","<br/>","<br/>"
                                                 ))))
                                                 
                                                 ),
                                        tabPanel("Other Statistical issues",
                                                 fluidRow(
                                                   column(8, HTML(paste0("<br/>", "<br/>", "<br/>","<br/>", "<br/>", "<br/>",
                                                             "<i><center><strong><font color='red'>Multiple Comparisons</font></strong></i></center>",
                                                             "<br/>", "<br/>", "<br/>","<br/>", "<br/>", "<br/>",
                                                             "When an experiment involves more than 1 comparison, the overall probability of Type I error in the experiment is higher than the selected \\(\\alpha\\) level of one test. This overall probability is also called the familywise error rate or experiment-wise error rate
                                                              and is the probability that at least one comparison leads to a false positive finding. It is calculated with the formula: $$1-(1-\\alpha)^{n}$$
                                                              where \\(\\alpha\\) is the significance level for an individual comparison and n is the total number of comparisons in the experiment. For instance an experiment with 4 groups involves 6 pairwise comparisons. 
                                                              The probability that at least one comparison leads to a false-positive conclusion is equal to  
                                                              $$1-(.95)^{6}=26\\%$$",
                                                             "<br/>", "<br/>", "<br/>",
                                                             
                                                             "Many statistical techniques have been developed in order to deal with this issue, i.e. to control the familywise error rate. The most common approach is the Bonferroni correction:
                                                            the overall desired familywise error rate is divided by the number of comparisons in the experiment to find the individual \\(\\alpha\\) level to be used for each comparison. 
                                                             So, if a researcher wants to conduct 10 statistical tests with the familywise error rate at 0.05, then the significance level for each individual test should be 0.05/10=0.005,
                                                             which means that only those comparisons with P < 0.005 are considered significant",
                                                              "<br/>", "<br/>", "<br/>", 
                                                             "The control of the familywise error rate needs to be taken into account not only in the data analysis phase of an experiment but also when sample size calculations are performed.",
                                                             "<br/>", "<br/>", "<br/>","<br/>", "<br/>", "<br/>")))),
                                                 
                                                 tags$img(src = "images/FWER.PNG", width = "800px", height = "500px"),
                                                 
                                                 fluidRow(
                                                   column(8, HTML(paste0("<br/>", "<br/>", "<br/>","<br/>", "<br/>", "<br/>",
                                                                         "<i><center><strong><font color='red'>One-sided vs two-sided tests</font></strong></i></center>",
                                                                         "<br/>", "<br/>", "<br/>","<br/>", "<br/>", "<br/>",
                                                                         "In statistical significance testing, a one-tailed test and a two-tailed test are alternative ways of computing the statistical significance of a parameter inferred from a data set, in terms of a test statistic. 
                                                                         A two-tailed test is appropriate if the estimated value may be more than or less than the reference value, for example, whether a test taker may score above or below the historical average. 
                                                                         A one-tailed test is appropriate if the estimated value may depart from the reference value in only one direction, for example, whether a machine produces more than one-percent defective products.
                                                                         Alternative names are one-sided and two-sided tests; the terminology 'tail' is used because the extreme portions of distributions, where observations lead to rejection of the null hypothesis,
                                                                         are small and often 'tail off' toward zero as in the normal distribution or 'bell curve'.",
                                                                         "<br/>", "<br/>", "<br/>",
                                                                         "In medical research is rarely appropriate to use one-sided test, e.g., when it is (almost) impossible that the mean in one group is higher than
                                                                          in the other. An example would be a group of mice with food ad libitum and one with a severely restricted diet. A test
                                                                          comparing mean weight gain in the ad libitum group (\\(\\mu_{1}\\)) with that in the restricted group (\\(\\mu_{2}\\)) should perhaps
                                                                          be one-sided: \\(\\mu_{1}\\) == \\(\\mu_{2}\\) versus \\(\\mu_{1}\\) > \\(\\mu_{2}\\)."
                                                   )))),
                                                 
                                                 fluidRow(
                                                   column(8, HTML(paste0("<br/>", "<br/>", "<br/>","<br/>", "<br/>", "<br/>",
                                                   tags$a("Here ", href="https://stats.idre.ucla.edu/other/mult-pkg/faq/general/faq-what-are-the-differences-between-one-tailed-and-two-tailed-tests/", target="_blank"),
                                                   "and ",
                                                   tags$a(href="http://www.statisticssolutions.com/should-you-use-a-one-tailed-test-or-a-two-tailed-test-for-your-data-analysis/", " here", target = "_blank"),
                                                           HTML(" , you can find more about this issue, and when it is appropriate to use one-sided test"),
                                                           "<br/>","<br/>","<br/>","<br/>","<br/>"
                                                                         
                                                                         ))))
                                                                         
                                                                         
                                                 
                                                 
                                                 
                                                 ),
                                        
                                        tabPanel("References",
                                                 fluidRow(
                                                   column(8, HTML(paste0("<br/>","<br/>","<br/>",
                                                             "Below are provided links with some interesting articles, websites etc., where more information about statistcal power and p-value interpretation
                                                             is given.")),
                                                 HTML("<br/>","<br/>"),
                                                 tags$a("The p value and the base rate fallacy", href = "https://www.statisticsdonewrong.com/p-value.html", target = "_blank"),
                                                 HTML("<br/>","<br/>"),
                                                 tags$a("Scientific method: Statistical errors", href = "https://www.nature.com/news/scientific-method-statistical-errors-1.14700", target = "_blank"),
                                                 HTML("<br/>","<br/>"),
                                                 tags$a("The fickle P value generates irreproducible results", href = "https://www.nature.com/articles/nmeth.3288", target = "_blank"),
                                                 HTML("<br/>","<br/>"),
                                                 tags$a("Observed power, and what to do if your editor asks for post-hoc power analyses", href = "http://daniellakens.blogspot.com/2014/12/observed-power-and-what-to-do-if-your.html", target = "_blank"),
                                                 HTML("with some linked papers inside which are also very intersting","<br/>","<br/>"),
                                                 tags$a("Statistical Considerations for Preclinical Studies", href = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4466166/", target = "_blank"),
                                                 HTML("<br/>","<br/>")))
                                                 
                                        ),

                                                 tabPanel("Software for Sample size and power calculation",
                                                 HTML(paste0("<br/>","<br/>","<br/>",
                                                      "<center><strong>G*Power: Statistical Power Analyses for Windows and Mac</strong></center>")),
                                                 
                                                 fluidRow(
                                                   column(8, HTML(paste0("<br/>","<br/>",
                                                             "G*Power calculates sample size for many different tests. G*Power also computes effect sizes and
                                                             displays graphically the results of sample size calculations.",
                                                             "<br/>",
                                                             "G*Power is a freely available in the following link:",
                                                             "<br/>","<br/>","<br/>")))),
                                                             a("G*Power link", href="http://www.gpower.hhu.de/", target = "_blank")

                                                 ))
                                                 ),
                          
                           tabPanel("Mean/Median Analysis",  value = "Page4",
                                    withMathJax(),
                                    tabsetPanel(
                                      
                                      tabPanel("Basic information",
                                               fluidRow(
                                                 column(8, HTML(paste0("<br/>","<br/>", "<br/>",
                                                           "T-test and Wilcoxon-Mann-Whitney test compare two groups, while ANOVA and Kruskal-Wallis test compare more than two groups with respect to mean or median values of the outcome respectively.
                                                           T-test and ANOVA are parametric tests that rely on certain assumptions and these assumptions need to be met to obtain reliable test results. Validation of these assumptions becomes impossible when the sample is small, which is generally the case with animal experiments.
                                                           Then, a researcher should use non-parametric tests instead, namely the Mann-Whitney-Wilcoxon or Kruskal-Wallis test. In an experiment with more than two groups of mice, the researcher wants to do all the possible pairwise comparisons and find out which groups are different from each other. 
                                                           Therefore,  the Wilcoxon-Mann-Whitney test can be used for those pairwise comparisons, in order to find the groups with significantly different median values of the outcome of interest.",
                                                           "<br/>","<br/>","<br/>")))),
                                               
                                               tags$a(href="https://www.analyticsvidhya.com/blog/2017/11/a-guide-to-conduct-analysis-using-non-parametric-tests/", "Here", target = "_blank"),
                                               HTML(" and "),
                                               tags$a(href="http://sphweb.bumc.bu.edu/otlt/mph-modules/bs/bs704_nonparametric/BS704_Nonparametric4.html", "Here", target = "_blank"),
                                               
                                               HTML(" , you can find more details about the Wilcoxon-Mann-Whitney test and non-parametric tests",
                                               "<br/>","<br/>","<br/>","<br/>","<br/>"),
                                               
                                               fluidRow(
                                                 column(8, HTML(paste0("However, one needs to be aware that conducting multiple pairwise tests increases the probability of a false positive result and a correction of the significance level \\(\\alpha\\) should be implemented. 
                                                           The most popular correction is the Bonferroni adjustment which divides the \\(\\alpha\\) by the total number of comparisons performed.
                                                           For instance, in an experiment with three treatment groups (A, B, C), three pairwise comparisons can be performed (A-B, A-C, B-C). 
                                                           For an overall \\(\\alpha\\) of 0.05, i.e. a maximally 5% chance that at least one of the comparisons resulting  in a false positive conclusion, a significance level of \\(\\alpha\\)/(number of comparisons)=0.05/3=0.0167
                                                           for each pairwise test should be used.",
                                                           "<br/>","<br/>","<br/>",
                                                           tags$a(href="http://www.biostathandbook.com/multiplecomparisons.html", "Here", target = "_blank"),
                                                           
                                                           HTML(" , you can find more about this and some alternative solutions to the multiple comparisons issue."),
                                                           "<br/>","<br/>","<br/>","<br/>","<br/>"))))
                                                           
                                               ),
                                               
                                      
                                      tabPanel("Example", value = 'Example_means',
                                               HTML(paste0("<br/>","<br/>","<br/>",
                                                           "<center><i><strong><font color='blue'>For 2 groups</font></strong></i></center>")),
                                               h5("Example:"),
                                               fluidRow(
                                                 column(8, HTML("A scientist wants to test the hypothesis that a novel compound reduces high-density lipoprotein (HDL) cholesterol levels in a transgenic C57Bl/6J strain of mice. Therefore she wants to conduct a new study
                                                    by randomizing mice to control treatment groups, in order to compare the average HDL cholesterol levels from the two groups. From a previous experiment, the following measurements of HDL are observed:",
                                               "<br/>", "<br/>","<br/>"))),
                                               fluidRow(
                                                 column(6, DTOutput("Example_1"))),
                                               
                                               fluidRow(
                                                 column(8, HTML("<br/>", "<br/>","<br/>",
                                                    "These data can be used to calculate the required sample size for the new experiment. The following information from the observed previous data is needed: ",
                                                    "<ol>
                                                      <li>Mean in group A (Control)</li>
                                                      <li>Mean in group B (Treatment)</li>
                                                      <li>Standard Deviation in group A</li>
                                                      <li>Standard Deviation in group B</li>
                                                      </ol>",
                                                    "<br/>", "<br/>","<br/>",
                                                    "Further, the power level and the significance level(\\(\\alpha\\)) of a test, or in other words, the desired Type-I error, need to be specified. Usually, power is set to 80% and \\(\\alpha\\) to 5%.",
                                                    "<br/>","<br/>","<br/>","<br/>","<br/>"))),
                                               
                                               
                                               HTML("<center><i><strong><font color='blue'>For more than 2 groups</font></strong></i></center>"),
                                               h5("Example:"),
                                               fluidRow(
                                                 column(8, HTML("A scientist wants to test the hypothesis that two novel compounds reduce high-density lipoprotein (HDL) cholesterol levels in a transgenic C57Bl/6J strain of mice.
                                                    Therefore she wants to conduct a new study by randomizing mice to control and two treatment groups, in order to compare the average HDL cholesterol levels from the three groups. 
                                                    From a previous experiment, the following measurements of HDL are observed:",
                                                    "<br/>", "<br/>","<br/>"))),
                                               fluidRow(
                                                 column(6, DTOutput("Example_2"))),
                                               fluidRow(
                                                 column(8, HTML("<br/>", "<br/>","<br/>",
                                                    "In case of more than two groups, sample size is based on the two groups with the smallest difference between the mean of the outcome because a larger sample is required to detect a smaller effect. If an experiment is powered for the smallest difference,
                                                    it is also powered to detect a larger difference. After finding these 2 groups, the procedure is exactly the same as in the case of 2 groups.",
                                                    "<br/>", "<br/>","<br/>","<br/>", "<br/>","<br/>"))),
                                               
                                               HTML("<center><i><strong><font color='blue'>Power Calculation Example</font></strong></i></center>"),
                                               
                                               
                                               tags$img(src = "images/PowerExample_WMW.PNG", width = "350px", height = "500px"),
                                               
                                               fluidRow(
                                                 column(8, HTML("<br/>", "<br/>","<br/>",
                                                    "This is an example of how this app can be used in order to perform sample size calculations. 
                                                    The image above is from the next tab where the actual sample size calculation can be done. In this interactive panel the required input can provided,
                                                    and after clicking on <strong><font color='blue'>Go!</font></strong>, the results will appear (it might take a couple of seconds).
                                                    Along with that, a plot will also be provided with a range of possible sample sizes and their associated power values.",
                                                    "<br/>", "<br/>","<br/>", 
                                                    "For illustration, we will use the previous example for 2 groups and >2 groups"))),
                                               #tags$a("Here", onclick = "customHref('Example_means');"),
                                               
                                               
                                               fluidRow(
                                                 column(6, HTML(paste0("<br/>", "<br/>","<br/>",
                                                    "<center><i><strong><font color='blue'>For 2 groups</font></strong></i></center>",
                                                    "<br/>", "<br/>","<br/>",
                                                    "For this particular example, we have:",
                                                    "<ol>
                                                    <li>Mean in group A (Treatment) = 267.39</li>
                                                    <li>Mean in group B (Control) = 283.46</li>
                                                    <li>Standard Deviation in group A = 14.38</li>
                                                    <li>Standard Deviation in group B = 11.83</li>
                                                    </ol>",
                                                    "<br/>", "<br/>","<br/>",
                                                    "Finally, we specify \\(\\alpha\\) at 5% and the desired power to be 80%.")))),
                                               
                                               fluidRow(
                                                 column(6,  HTML(paste0("<br/>", "<br/>","<br/>",
                                                    "<center><i><strong><font color='blue'>For more than 2 groups</font></strong></i></center>",
                                                    "<br/>", "<br/>","<br/>", 
                                                    "For this particular example, we have:",
                                                    "<ol>
                                                    <li>Mean in group A (Treatment A) = 267.39</li>
                                                    <li>Mean in group B (Treatment B) = 256.48</li>
                                                    <li>Mean in group C (Control)   = 283.46</li>
                                                    <li>Standard Deviation in group A = 14.83</li>
                                                    <li>Standard Deviation in group B = 9.75</li>
                                                    <li>Standard Deviation in group c = 11.83</li>
                                                    </ol>",
                                                    "<br/>", "<br/>","<br/>",
                                                    "We identify the two groups with the smallest difference, namely A and B. Since three pairwise tests will be performed, 
                                                    the \\(\\alpha\\) is adjusted by 0.05/3 = 0.0166. Finally, we specify the desired power to be 80%.")))),
                                               HTML("<br/>", "<br/>","<br/>",
                                                    "If we now provide the input to the panel at the left, results will show up, as in the image below.",
                                                    "<br/>", "<br/>","<br/>"),
                                               
                                               tags$img(src = "images/PowerExample_WMW2.PNG", width = "700px", height = "300px")
                                               
                                              
                                        #       HTML("<br/>", "<br/>","<br/>", 
                                         #           "<strong><font color='#4d3a7d'>In order to achieve</font></strong>"),
                                         #      uiOutput('dynamic_valueP',inline = T),
                                          #     HTML("<strong><font color='#4d3a7d'>power to reject the null hypothesis of equal means when the population
                                           #         mean difference is M1-M2 =</font></strong> "),
                                            #   uiOutput('dynamic_valueD',inline = T),
                                             #  HTML("<strong><font color='#4d3a7d'>with a standard deviation for group A=</font></strong>"),
                                          #     uiOutput('dynamic_valueSDA',inline = T),
                                           #    HTML("<strong><font color='#4d3a7d'>and group B=</font></strong>"),
                                            #   uiOutput('dynamic_valueSDB',inline = T),
                                             #  HTML("<strong><font color='#4d3a7d'>and with a significance level (a) of</font></strong>"),
                                              # uiOutput('dynamic_valueA',inline = T),
                                               #HTML("<br/>", "<br/>","<br/>"),
                                            #   h4("The required sample size per group is:",
                                             #     style = "font-family: 'Lobster', cursive;
                                              #    font-weight: 500; line-height: 1.1; 
                                               #   color: #4d3a7d;"), verbatimTextOutput('results3'), 
                                              # br(), br()
                                               
                                               
                                               ),
                                              
                                      
                                      tabPanel( "Power Calculation",
                                                sidebarLayout(
                                                  sidebarPanel(
                                                    numericInput( "meanAAA2Input", "Mean of group A", 267.39 ),
                                                    numericInput( "meanBBB2Input", "Mean of group B", 283.46 ),
                                                    numericInput( "SDAAA2Input", "SD of group A", 14.38 ),
                                                    numericInput( "SDBBB2Input", "SD of group B", 11.83 ),
                                                    selectInput( "type32Input", "Type of test", c("one.sided", "two.sided"), selected = "two.sided"),
                                                    sliderInput( "Power32Input", "Power", min = 0, max = 100, value = 80, step = 1),
                                                    sliderInput( "error32Input", "Type I error", min = 0, max = 10, value = 5, step = 0.1),
                                                    withBusyIndicatorUI(
                                                      actionButton("button2Input", "Go!", class = "btn-primary")
                                                    )
                                                  ),
                                                  mainPanel(
                                                    HTML("<br/>", "<br/>","<br/>", 
                                                         "<strong><font color='#4d3a7d'>In order to achieve</font></strong>"),
                                                    uiOutput('dynamic_valueP2',inline = T),
                                                    HTML("<strong><font color='#4d3a7d'>% power to reject the null hypothesis of equal means when the population
                                                         mean difference is M1-M2 =</font></strong> "),
                                                    uiOutput('dynamic_valueD2',inline = T),
                                                    HTML("<strong><font color='#4d3a7d'>with a standard deviation for group A=</font></strong>"),
                                                    uiOutput('dynamic_valueSDA2',inline = T),
                                                    HTML("<strong><font color='#4d3a7d'>and group B=</font></strong>"),
                                                    uiOutput('dynamic_valueSDB2',inline = T),
                                                    HTML("<strong><font color='#4d3a7d'>and with a significance level (a) of</font></strong>"),
                                                    uiOutput('dynamic_valueA2',inline = T),
                                                    HTML("<strong><font color='#4d3a7d'>%</font></strong>"),
                                                    HTML("<br/>", "<br/>","<br/>"),
                                                    h4("The required sample size per group is:",
                                                       style = "font-family: 'Lobster', cursive;
                                                       font-weight: 500; line-height: 1.1; 
                                                       color: #4d3a7d;"), verbatimTextOutput('results32'), 
                                                    br(), br(),
                                                    plotOutput("coolplot32"))))
                                      )), 
                                        
                                      tabPanel("Survival Analysis", value = "Page5",
                                               useShinyjs(),
                                               withMathJax(),
                                               tabsetPanel(
                                                 tabPanel("Basic information",
                                                          fluidRow(
                                                            column(8,  HTML(paste0("<br/>","<br/>","<br/>",
                                                                      "In order to evaluate whether the time to death differs between two or more groups of mice, the log-rank test is used. 
                                                                      For each mouse, the time is measured from the start of the experiment, e.g., randomization, until the mouse dies or is sacrificed. 
                                                                      For mice who are still alive at the end of the study, the time between start and end of the study is used, but is labeled 'censored'
                                                                      so that their time is not considered a time to death.",
                                                                      "<br/>","<br/>","<br/>"
                                                                      ))))
                                                          
                                                          ),
                                                 
                                                 tabPanel("Example", value = 'Example_surv', 
                                                          fluidRow(
                                                            column(8, HTML(paste0("<br/>","<br/>","<br/>",
                                                          
                                                          "To evaluate whether treatment with paclitaxel improves survival after esophageal adenocarcinoma (EAC), a scientist uses a peritoneal dissemination xenograft mouse model and injects human EAC cell lines intraperitoneally/subcutaneously into SCID mice.
                                                                Two weeks later, mice are randomly assigned to treatment by vehicle or paclitaxel (20mg/kg, 2 times a week for 2 weeks). 
                                                               Mice are followed until death or the end of the study and  the mouse survival times are compared between the 2 groups.",
                                                          "<br/>","<br/>","<br/>")))),
                                                          fluidRow(
                                                            column(6, DTOutput("Surv_Example_1"))),

                                                          fluidRow(
                                                            column(8, HTML(paste0("<br/>","<br/>","<br/>",
                                                              "For the power calculation based on a logrank test information about the median survival in each group is required. 
                                                               Moreover, total experiment time is needed, which is the planned duration of the experiment. Further, the power level and the significance level (\\(\\alpha\\)) of the test,
                                                               or, in other words, the desired Type-I error, need to be specified. Usually, power is set to 80% and \\(\\alpha\\) to 5%.Therefore, we need:",
                                                               "Therefore, we need:",
                                                               "<br/>", "<br/>","<br/>",
                                                               "<ol>
                                                               <li>	Median Survival in group A  (Control)</li>
                                                               <li>	Median Survival in group B (Treatment)</li>
                                                               <li>	Duration of experiment </li>
                                                               <li>	Power level </li>
                                                               <li>	Significance level </li>
                                                               </ol>",
                                                               "<br/>", "<br/>","<br/>","<br/>", "<br/>","<br/>"
                                                               )))),
                                                          
                                                          HTML("<center><i><strong><font color='blue'>Power Calculation Example</font></strong></i></center>"),
                                                          
                                                          tags$img(src = "images/PowerExample_LRT.PNG", width = "350px", height = "500px"),
                                                          
                                                          fluidRow(
                                                            column(8, HTML("<br/>", "<br/>","<br/>",
                                                               "This is an example of how this app can be used in order to perform sample size calculations. 
                                                               The image above is from the next tab where the actual sample size calculation can be done. In this interactive panel the required input can provided,
                                                               and the results will appear.",
                                                               "<br/>", "<br/>","<br/>", 
                                                               "We show the calculation of the required sample size for the previous example"))),
                                                          #tags$a("Here", onclick = "customHref('Example_surv');"),
                                                          
                                                          HTML("<br/>", "<br/>","<br/>",
                                                               "For this particular example, we have:",
                                                               "<ol>
                                                                   <li>Median Survival in group A (Treatment) = 25.5 days</li>
                                                                   <li>Median Survival in group B (Control) = 35 days</li>
                                                                   <li>Duration of experiment = 60 days</li>
                                                                   </ol>",
                                                               "<br/>", "<br/>","<br/>",
                                                               "Finally, we specify \\(\\alpha\\) at 5% and the desired power to be 80%.")
                                                          
                                                          ,
                                                 HTML("<br/>", "<br/>","<br/>",
                                                      "If we now provide the input to the panel at the left, results will show up, as in the image below.",
                                                      "<br/>", "<br/>","<br/>"),
                                                 
                                                 tags$img(src = "images/PowerExample_LRT2.PNG", width = "700px", height = "300px")
                                                 
                                                 ),
                                                 
                                                 
                                                 tabPanel("Power Calculation",
                                                          sidebarLayout(
                                                            sidebarPanel(
                                                              numericInput( "MedA1Input", "Median survival of group A-days", 60 ),
                                                              numericInput( "MedB1Input", "Median survival of group B-days", 30 ),
                                                              numericInput( "FT1Input", "Experiment's duration-days", 100 ),
                                                              sliderInput( "Power1InputS", "Power", min = 0, max = 100, value = 80, step = 1),
                                                              sliderInput( "error1InputS", "Type I error", min = 0, max = 10, value = 5, step = 1)
                                                              
                                                            ),
                                                            
                                                            mainPanel(
                                                              HTML("<br/>", "<br/>","<br/>", 
                                                                   "<strong><font color='#4d3a7d'>In order to achieve</font></strong>"),
                                                              uiOutput('sdynamic_valueP',inline = T),
                                                              HTML("<strong><font color='#4d3a7d'>% power at a </font></strong> "),
                                                              uiOutput('sdynamic_valueAlp',inline = T),
                                                              HTML("<strong><font color='#4d3a7d'>% significance level to detect a hazard ratio of </font></strong> "),
                                                              uiOutput('sdynamic_valueHR',inline = T),
                                                              HTML("<strong><font color='#4d3a7d'> when the control group median survival is</font></strong>"),
                                                              uiOutput('sdynamic_valueA',inline = T),
                                                              HTML("<strong><font color='#4d3a7d'>for a study that lasts for</font></strong>"),
                                                              uiOutput('sdynamic_valueFT',inline = T),
                                                              HTML("<strong><font color='#4d3a7d'>time periods.</font></strong>"),
                                                              HTML("<br/>", "<br/>","<br/>"),
                                                              h4("The required sample size per group is:",
                                                                 style = "font-family: 'Lobster', cursive;
                                                                 font-weight: 500; line-height: 1.1; 
                                                                 color: #4d3a7d;"), verbatimTextOutput('resultsSurv1'),
                                                              
                                                              br(), br(),
                                                              plotOutput("Survcoolplot33")))
                                                          )
                                                 #tabPanel("Log-rank VS Wilcoxon-Mann-Whitney",
                                                #          HTML(paste0("<br/>","<br/>","<br/>",
                                                #                      "In the data that from the example ")),
                                                #         tags$a("here", onclick = "customHref('Example_surv');"),
                                                #         HTML("we observe that all the mice in the control group have experienced the event of interest during the experiment, 
                                                #               while in the treatment group we have 3 mice that are censored. Two mice have died early but from reasons unrelated 
                                                #               to the experiment and one mouse was still alive at the end of the experiment. Because of the presence of censoring,
                                                #               the log-rank test is the only test that we should use. I order to prove that, we will analyze this data with both the
                                                #              log-rank test and a simple Wilcoxon-Mann-Whitney.",
                                                #              "<br/>", "<br/>","<br/>"),
                                                #         HTML("<center><i><strong><font color='blue'>Log-rank test</font></strong></i></center>"),
                                                #         verbatimTextOutput("Surv_Example_2"),
                                                #         
                                                #         
                                                #         HTML("<br/>", "<br/>","<br/>",
                                                #              "<center><i><strong><font color='blue'>Wilcoxon test</font></strong></i></center>"),
                                                #         verbatimTextOutput("Surv_Example_3"),
                                                #         HTML("So, we see that the log-rank test is significant at the 5% significance level while the Wilcoxon test is not, based on their p-values. 
                                                #             Intuitively this can be explained from the fact that we have 2 early deaths in the treatment group which are not related to our experiment 
                                                #             and thus they are considered censored. This can be seen from the log-rank test which can handle the censoring but not from the t-test which just compares 
                                                #             the times from the 2 groups and does not take into account the censoring. Therefore, we see that when there is censoring in the data, 
                                                #             log-rank test (or any other survival analysis test) is the only choice.",
                                                #              "<br/>", "<br/>","<br/>","<br/>", "<br/>","<br/>",
                                                #             "Let's now consider another example.",
                                                #             "<br/>", "<br/>","<br/>","<br/>", "<br/>","<br/>"),
                                                #         fluidRow(
                                                #           column(6, DTOutput("Surv_Example_4"))),
                                                #         HTML("<br/>", "<br/>","<br/>","<br/>",
                                                #               "Here we have 39 mice, 21 in control and 18 in treatment respectively, and all them have experienced the event except one mice in the treatment group
                                                #               which has died from other causes early in the study. We analyze these data again with both log-rank test and Wilcoxon."),
                                                #         HTML("<br/>", "<br/>","<br/>",
                                                #              "<center><i><strong><font color='blue'>Log-rank test</font></strong></i></center>"),
                                                #         verbatimTextOutput("Surv_Example_5"),
                                                          
                                                          
                                                #         HTML("<br/>", "<br/>","<br/>",
                                                #              "<center><i><strong><font color='blue'>Wilcoxon test</font></strong></i></center>"),
                                                #         verbatimTextOutput("Surv_Example_6"),
                                                #         
                                                #          HTML("<br/>", "<br/>","<br/>",
                                                #                           "Here both tests are highly significant in the 5% significance level. Here we have only 1 out of the 39 mice censored and this does not affect the conclusion 
                                                #              that we make with both tests. Of course, this would cause a problem if the sample size here was smaller, because each mouse would put more weight on the tests.",
                                                #             "<br/>", "<br/>","<br/>")
                                                          
                                                #         )
                                                          
                                                 )),
       
       
                                      tabPanel("Proportion Analysis", value = "Page6",
                                               useShinyjs(),
                                               withMathJax(),
                                               tabsetPanel( 
                                                 tabPanel("Basic information",
                                                          fluidRow(
                                                            column(8, HTML(paste0("<br/>","<br/>","<br/>",
                                                                        "Proportion analysis is used when the goal is to compare two groups of mice based on a categorical outcome, i.e. complete responders: yes/no.
                                                                        The simplest statistical test for this purpose is the Z-test for two proportions.The null hypothesis H0 is that the two population proportions are the same;
                                                                        in other words, that their difference is equal to 0. The notation for the null hypothesis is H0: p1 = p2, where p1 is the proportion from the first population, and p2 is the proportion from the second population.",
                                                                        "<br/>","<br/>","<br/>"
                                                                               )))), 
                                                          tags$a(href="https://www.dummies.com/education/math/statistics/how-to-compare-two-population-proportions/", "Here", target = "_blank"),
                                                          
                                                          HTML(" , you can find more details about the Z-test for proportions",
                                                               "<br/>","<br/>","<br/>","<br/>","<br/>")),
                                                 
                                               tabPanel("Example", value = 'Example_prop',
                                                        fluidRow(
                                                          column(8, HTML(paste0("<br/>","<br/>","<br/>", 
                                                                                "A scientist wants to test the hypothesis that a new combination treatment is able to completely regress tumor more efficiently compared with a control.
                                                                                As complete responders are considered those mice
                                                                                that do not have palpable tumors for 14 days. The proportions of complete responders are compared between treatment groups. 
                                                                                A subset of the data from a previous experiment is presented below, where:",
                                                                                "<br/>","<br/>","<br/>",
                                                                                "<ol>
                                                                                  <li><strong>ID</strong> : ID for each mouse</li>
                                                                                  <li><strong>Treatment</strong> : group of treatment</li>
                                                                                  <li><strong>Day #</strong> : Tumor volume in mm3 at the corresponding 
                                                                                            day of measurement after the start of the treatment</li>
                                                                                  <li><strong>CR</strong> : 1 if complete responder, 0 if not</li>
                                                                                  </ol>",
                                                                                
                                                                                "<br/>","<br/>","<br/>",
                                                                                "The <strong>NA's</strong> denote missing values, which in this case means that a mouse is dead and does not reach the end of the experiment.
                                                                                And as can be seen in this particular example, there are more dead mice in the control group that in the treatment one.",
                                                                                "<br/>","<br/>","<br/>")))), 
                                                                                
                                                                                fluidRow(
                                                                                  column(12, DTOutput("ExamplePrp_1"))),
                                                                                
                                                        fluidRow(
                                                          column(8, HTML(paste0("<br/>","<br/>","<br/>","<br/>","<br/>","<br/>", 
                                                                                
                                                                                "This information can be used to calculate the required sample size for the new experiment.
                                                                                Further, the power level and the significance level  \\(\\alpha\\) of a test, or in other words, the desired Type-I error,
                                                                                need to be specified. Usually, power is set to 80% and \\(\\alpha\\) to 5%")))),
                                               
                                               HTML("<br/>","<br/>","<br/>", "<center><i><strong><font color='blue'>Power Calculation Example</font></strong></i></center>",
                                                    "<br/>","<br/>","<br/>"),
                                               
                                               tags$img(src = "images/PowerExample_PROP1.PNG", width = "350px", height = "500px"),
                                               
                                               fluidRow(
                                                 column(8, HTML("<br/>", "<br/>","<br/>",
                                                                "This is an example of how this app can be used in order to perform sample size calculations. 
                                                               The image above is from the next tab where the actual sample size calculation can be done. In this interactive panel the required input can provided,
                                                               and the results will appear.",
                                                                "<br/>", "<br/>","<br/>", 
                                                                "We show the calculation of the required sample size for the previous example"))),

                                               HTML("<br/>", "<br/>","<br/>",
                                                    "For this particular example, we have:",
                                                    "<ol>
                                                                   <li>Proportion in group A (Control) = 10%</li>
                                                                   <li>Proportion in group B (New Treatment) = 30% </li>
                                                                   </ol>",
                                                    "<br/>", "<br/>","<br/>",
                                                    "Finally, we specify \\(\\alpha\\) at 5% and the desired power to be 80%.")
                                               
                                               ,
                                               HTML("<br/>", "<br/>","<br/>",
                                                    "If we now provide the input to the panel at the left, results will show up, as in the image below.",
                                                    "<br/>", "<br/>","<br/>"),
                                               
                                               tags$img(src = "images/PowerExample_PROP2.PNG", width = "700px", height = "300px")
                                               
                                               ),
                                               
                                              
                                               tabPanel("Power Calculation",
                                                        sidebarLayout(
                                                          sidebarPanel(
                                                            numericInput( "Prop1Input", "Proportion in group A", 0.1, min = 0, max = 1 ),
                                                            numericInput( "Prop2Input", "Proportion in group B", 0.3, min = 0, max = 1 ),
                                                            sliderInput( "Power1InputP", "Power", min = 0, max = 100, value = 80, step = 1),
                                                            sliderInput( "error1InputP", "Type I error", min = 0, max = 10, value = 5, step = 1)
                                                            
                                                          ),
                                                          
                                                          mainPanel(HTML("<br/>", "<br/>","<br/>", 
                                                                         "<strong><font color='#4d3a7d'>In order to achieve</font></strong>"),
                                                                    uiOutput('Pdynamic_valuePOW',inline = T),
                                                                    HTML("<strong><font color='#4d3a7d'>% power at a </font></strong> "),
                                                                    uiOutput('Pdynamic_valueALP',inline = T),
                                                                    HTML("<strong><font color='#4d3a7d'>% significance level to detect a difference between the group proportions of </font></strong> "),
                                                                    uiOutput('Pdynamic_valueDP1',inline = T),
                                                                    HTML("<strong><font color='#4d3a7d'> when the proportion in group 1 is</font></strong>"),
                                                                    uiOutput('Pdynamic_valueP1',inline = T),
                                                                    
                                                                    h4("The required sample size per group is:",
                                                                       style = "font-family: 'Lobster', cursive;
                                                                       font-weight: 500; line-height: 1.1; 
                                                                       color: #4d3a7d;"), verbatimTextOutput('resultsProp')
                                                            

                                                        ))))),    
                                               
       
       
       
       
       
       
       
       
       
       
                                               
                                      tabPanel("Growth Curve Analysis", value = "Page7",
                                               useShinyjs(),
                                               withMathJax(),
                                               tabsetPanel( 
                                                 tabPanel("Basic information",
                                                          fluidRow(
                                                            column(8, HTML(paste0("<br/>","<br/>","<br/>",
                                                                    "In typical mice experiments, tumor cells are injected into mice and the volume of the growing tumor is measured every 2-3 days until the mouse dies or is sacrificed.
                                                                    When the tumor reaches a certain volume, mice are randomized into treatment and control groups, and the rate of tumor growth is to be compared between groups. Often, average tumor size is compared at subjectively selected time points using a t-test or ANOVA.",
                                                                    "<br/>","<br/>","<br/>",
                                                                    "An alternative would be to test differences in tumor growth between groups of mice. This can be done with linear regression analysis, but taking into account the correlation of measurements within each mouse, which vital. Therefore, the whole data from an experiment 
                                                                    is being exploited and arbitrary selection of a time point is avoided.", 
                                                                    "<br/>","<br/>","<br/>",
                                                                    "In such experiments, the only variables included in the model are the time, the treatment and their interaction (time:treatment). By construction of those experimental designs, we are not interested in the main effect of the treatment, since at the time of randomization
                                                                    mice are supposed to have similar tumors, and therefore we exclude it form the model. Finally, an interaction term between time of volume measurement and treatment group is included to evaluate whether tumor growth differs between groups, which is the main interest of this analysis.", 
                                                                    "<br/>","<br/>","<br/>",
                                                                    "In order to take into account that we have multiple measurements per mouse, which leads to correlated observations in the dataset, we will not use the standard method of estimation that a linear regression uses, namely <i><strong>Ordinary Least Squares (OLS)</i></strong>, but instead the <i><strong>Generalized Least Squares (GLS)</i></strong> method, which can accommodate the dependence in the data.", 
                                                                    "<br/>","<br/>","<br/>",
                                                                    "Using GLS we can specify the type of correlation pattern we expect in the data. In longitudinal data, and especially when the measurement period is short, as in mice experiments, the most reasonable is the <i><strong>Autoregressive (AR)</i></strong> correlation pattern. Briefly, AR assumes that measurements closer in time are more correlated than those further apart. 
                                                                    For example, measurement at time 1 is more correlated with measurement at time 2 than with that at time 3 etc. In other words, the correlation decays by time.",
                                                                    "<br/>","<br/>","<br/>","<br/>","<br/>","<br/>"
                                                          ))))),
                                                 tabPanel("Example", value = 'Example_growth',
                                                          fluidRow(
                                                            column(8, HTML(paste0("<br/>","<br/>","<br/>",
                                                                      "A scientist wants to test the hypothesis that a new treatment is able to suppress the tumor growth
                                                                       compared with standard treatment. To assess that she conducts an experiment, where tumor cells are injected into mice and the volume of the growing tumor is measured every 2-3 days until 
                                                                      the mouse dies or is sacrificed (when the tumor volume reaches 1500mm3). When tumors reach 200mm3, she randomizes half of the mice to the standard treatment and the other half to the new treatment,
                                                                      in order to compare the rate of tumor growth between groups.",
                                                                      "<br/>","<br/>","<br/>",
                                                                      "Data from a previous experiment is presented below. This kind of presentation is called <i>wide format</i>, and it is the most familiar data presentation, 
                                                                      where each row corresponds to each mouse, and the different measurements are presented as different columns. { ( The following part will be removed probably and mentioned
                                                                      in the tab with the explanation of the analysis) However, when working with longitudinal data, it is more 
                                                                      convenient to work with another data presentation, called <i>long format</i>, where each row corresponds to each measurement for each mouse, 
                                                                      and hence, there are several rows per mouse equal to the total number of measurements for it}. In this data, the columns are:",
                                                                      "<br/>", "<br/>","<br/>",
                                                                      "<ol>
                                                                      <li><strong>ID</strong> : ID for each mouse</li>
                                                                      <li><strong>Treatment</strong>: group of treatment</li>
                                                                      <li><strong>Day X</strong> : Tumor volume in mm3 at the corresponding 
                                                                                            day of measurement after the start of the treatment</li>
                                                                      </ol>",
                                                                      "<br/>", "<br/>","<br/>",
                                                                      "The <strong>NA's</strong> denote missing values, which in this case means that a mouse is dead and does not reach the end of the experiment.
                                                                      And as can be seen in this particular example, there are more dead mice in the control group that in the treatment one.",
                                                                      
                                                                      "<br/>", "<br/>","<br/>")))),
                                                          fluidRow(
                                                            column(12, DTOutput("ExampleGC_1"))),
                                                          
                                                          
                                                          
                                                          fluidRow(
                                                            column(8, HTML(paste0("<br/>","<br/>","<br/>","<br/>","<br/>","<br/>", 
                                                                                  
                                                           "In order to do a sample size calculation for this kind of experiment, we will make use of simple linear regression. More precisely, 
                                                            we will compute the required sample size based on simple linear regression, and then we will multiply that number by a constant,
                                                            called <i>Design Effect</i>, in order to get the required sample size for longitudinal analysis, like the one we described earlier.",
                                                           "For that, we need first of all the minimum effect that we wish to detect, which in this case is the difference in slope of tumor growth
                                                           between the treatment groups. Then, we need an estimate of the standard deviation of the independend variable (X's), which is the time variable
                                                           in this experiment, as well as an estimate of the standard deviation of the response (Y's), which here is the tumor volume, after using the log-transformation on it. ",
                                                            "Further, the power level and the significance level  \\(\\alpha\\) of a test, or in other words, the desired Type-I error,
                                                             need to be specified. Usually, power is set to 80% and \\(\\alpha\\) to 5%")))),
                                                          
                                                          HTML("<br/>","<br/>","<br/>", "<center><i><strong><font color='blue'>Power Calculation Example</font></strong></i></center>",
                                                               "<br/>","<br/>","<br/>"),
                                                          
                                                          tags$img(src = "images/PowerExample_LR1.PNG", width = "350px", height = "500px"),
                                                          
                                                          fluidRow(
                                                            column(8, HTML("<br/>", "<br/>","<br/>",
                                                               "This is an example of how this app can be used in order to perform sample size calculations. 
                                                               The image above is from the next tab where the actual sample size calculation can be done. In this interactive panel the required input can provided,
                                                               and the results will appear.",
                                                             "<br/>", "<br/>","<br/>", 
                                                               "We show the calculation of the required sample size for the previous example"))),
                                                          
                                                          HTML("<br/>", "<br/>","<br/>",
                                                               "For this particular example, we have:",
                                                               "<ol>
                                                                   <li>Effect of interest (difference in slopes) = -0.1 (The minus sign here means that the treatment group has a less steep tumor growth)</li>
                                                                   <li>Standard deviation of X's = 7.5 </li>
                                                                   <li>Standard deviation of Y's = 0.716 </li>
                                                                   <li>Average number of measurements per mouse = 10 </li>
                                                                   </ol>",
                                                               "<br/>", "<br/>","<br/>",
                                                               "Finally, we specify \\(\\alpha\\) at 5% and the desired power to be 80%.")
                                                          
                                                          ,
                                                          HTML("<br/>", "<br/>","<br/>",
                                                               "If we now provide the input to the panel at the left, results will show up, as in the image below.",
                                                               "<br/>", "<br/>","<br/>"),
                                                          
                                                          tags$img(src = "images/PowerExample_LR2.PNG", width = "700px", height = "300px")
                                                          
                                                 ),
                                                 
                                                 tabPanel("Power Calculation",
                                                          sidebarLayout(
                                                            sidebarPanel(
                                                              numericInput( "Effect", "Interaction effect", -0.04 ),
                                                              numericInput( "SDX", "Standard error of X", 5 ),
                                                              numericInput( "SDY", "Standard error of Y", 0.716 ),
                                                              numericInput( "NoMeasurements", "Average number of measurements per mouse", 8 ),
                                                              sliderInput( "PowerInputLR", "Power", min = 0, max = 100, value = 80, step = 1),
                                                              sliderInput( "errorInputLR", "Type I error", min = 0, max = 10, value = 5, step = 1)
                                                              
                                                            ),
                                                            
                                                            mainPanel(
                                                              HTML("<br/>", "<br/>","<br/>", 
                                                                   "<strong><font color='#4d3a7d'>In order to achieve</font></strong>"),
                                                              uiOutput('dynamic_valuePOWLR',inline = T),
                                                              HTML("<strong><font color='#4d3a7d'>% power at a </font></strong> "),
                                                              uiOutput('dynamic_valueALLR',inline = T),
                                                              HTML("<strong><font color='#4d3a7d'>% significance level to detect a difference in slope between the two treatment groups of</font></strong> "),
                                                              uiOutput('dynamic_valueEffLR',inline = T),
                                                              HTML("<strong><font color='#4d3a7d'> when the standard deviation of the X's is</font></strong>"),
                                                              uiOutput('dynamic_valueSDXLR',inline = T),
                                                              HTML("<strong><font color='#4d3a7d'> , the standard deviation of the Y's is </font></strong>"),
                                                              uiOutput('dynamic_valueSDYLR',inline = T),
                                                              HTML("<strong><font color='#4d3a7d'> , and the total number of measurements per mouse is </font></strong>"),
                                                              uiOutput('dynamic_MeasurementsLR',inline = T),
                                                              HTML("<br/>", "<br/>","<br/>"),
                                                              h4("The required sample size per group is:",
                                                                 style = "font-family: 'Lobster', cursive;
                                                                 font-weight: 500; line-height: 1.1; 
                                                                 color: #4d3a7d;"), verbatimTextOutput('resultsLR'),
                                                              
                                                              br(), br()
                                                              #plotOutput("Survcoolplot33")
                                                              ))
                                                          
                                                          
                                                 ))), inverse = T, collapsible = T
                           
                                               )))
  

