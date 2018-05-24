library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(shinyjs)
library(shinythemes)
library(plotly)
library(samplesize)
library(fBasics)
library(DT)
library(survival)
#library(crayon)

#library(shiny.router)

#source("https://raw.githubusercontent.com/daattali/advanced-shiny/master/busy-indicator/helpers.R")

#saveRDS(withBusyIndicatorUI, file = "shinyhelper1")
#saveRDS(withBusyIndicatorServer, file = "shinyhelper2")

source("Helpers.R")


ui <- tagList(
  tags$head(
    tags$script("src"="app.js")
  ), fluidPage(navbarPage(theme=shinytheme("spacelab"), title = div(h4("Power Calculations", 
                                                                        style = "font-family: 'Lobster', cursive;
                                                                        font-weight: 500; line-height: 1.1; 
                                                                        color: #e8ea0f;"), img(src = "images/logo-AVL.jpg", height = "50px", width = "120px",
                                                                                               style = "position: relative; top: -3px; right: -1200px;")) 
                           , fluid = TRUE,  windowTitle = "Power Calculations",
                           tabPanel("Design for mice experiments", value = "StartPage",
                                    
                                               HTML(paste0("<br/>", "<br/>","<br/>",
                                                    "The purpose of this app is to help researchers conducting mice experiments in the Netherlands Cancer Institute
                                                    with respect to statistical aspects of the studies. One can find here some explanations of basic
                                                    statistical tests that can be applied to illustrate data or calculate sample size and power
                                                    of future experiments. The latter is vital and must be taken into consideration when an 
                                                    experiment is designed, because:",
                                                    "<br/>", "<br/>","<br/>",
                                                    "<i><center><strong> More power increases confidence about results, either significant or not </strong></center></i>",
                                                    "<br/>", "<br/>","<br/>",
                                                    "More about sample size and power calculations can be found  ")),
                                               
                                               #tags$div(
                                              #        id = "sdf",
                                               #       tags$a(onclick = "customHref('Power')", "Power")),
                                               #tags$div(
                                                 #id = "Others",
                                                 tags$a("here", onclick = "customHref('Page2');customHref('tab1');")
                                                 #tags$a("ff2", onclick = "customHref('Others');")
                                              
                                      ,
                                      
                          HTML(paste0("<br/>", "<br/>","<br/>",
                                      "<i><strong><font color='blue'> Experimental Designs </font></strong></i>",
                                      "<br/>", "<br/>","<br/>",
                                      "Below there is a list with the most common experimental designs. Please read them and find the one that fits to your 
                                      own experiment. When you find it follow the link that is provided in order to be transferred to the relevant page.")),
                          
                          HTML(paste0("<br/>", "<br/>","<br/>",
                                      "<i><strong><font color='red'> Mean/Medians Comparison </font></strong></i>",
                                      "<br/>", "<br/>","<br/>",
                                      "<i>Example:</i> A scientist wants to test the hypothesis that a novel compound had a beneficial effect on reducing high-density lipoprotein (HDL) cholesterol levels 
                                      in a transgenic C57Bl/6J strain of mice. Therefore she randomizes half of the mice to control group and the other half to treatment group, 
                                      in order to compare average HDL cholesterol levels from the two groups",
                                      "<br/>", "<br/>","<br/>")),
                          
                          tags$a("Link", onclick = "customHref('Page4');"),
                          
                          
                          HTML(paste0("<br/>", "<br/>","<br/>",
                               "<i><strong><font color='red'> Survival Analysis </font></strong></i>",
                               "<br/>", "<br/>","<br/>",
                               "<i>Example:</i> A scientist wish to establish a peritoneal dissemanation xenograft mouse model of Eshophageal adenocarcinoma(EAC)
                                that would support survival outcome analyses. Human EAC cell lines are injected intraperitoneally/subcutaneously into SCID mice.
                                Mice are randomly grouped two weeks after OE19 cells injection. They are treated intraperitoneally with vehicle or 
                                paclitaxel (20mg/kg, 2 times a week for 2 weeks). The mouse survival times are compared among the 2 groups.",
                               "<br/>", "<br/>","<br/>")),
               
                       tags$a("Link", onclick = "customHref('Page5');"),
                    
                         HTML(paste0("<br/>", "<br/>","<br/>",
                            "<i><strong><font color='red'> Proportions Comparison(NOT READY YET) </font></strong></i>",
                            "<br/>", "<br/>","<br/>",
                            "<i>Example:</i> A scientist wants to test the hypothesis that a novel compound had a beneficial effect on reducing high-density lipoprotein (HDL) cholesterol levels 
                            in a transgenic C57Bl/6J strain of mice. Therefore she randomizes half of the mice to control group and the other half to treatment group, 
                            in order to compare average HDL cholesterol levels from the two groups",
                            "<br/>", "<br/>","<br/>")),
               
                      tags$a("Link", onclick = "customHref('Page7');"),
    
                         HTML(paste0("<br/>", "<br/>","<br/>",
                            "<i><strong><font color='red'> Growth curve Analysis(NOT READY YET) </font></strong></i>",
                          "<br/>", "<br/>","<br/>",
                          "<i>Example:</i> A scientist wants to test the hypothesis that a novel compound had a beneficial effect on reducing high-density lipoprotein (HDL) cholesterol levels 
                            in a transgenic C57Bl/6J strain of mice. Therefore she randomizes half of the mice to control group and the other half to treatment group, 
                            in order to compare average HDL cholesterol levels from the two groups",
                          "<br/>", "<br/>","<br/>")),
  
                      tags$a("Link", onclick = "customHref('Page6');")),
  
  
              
       tabPanel("Fundamentals - Power", value = "Page2",
                                    withMathJax(),
                                      tabsetPanel(
                                        tabPanel("Interpretation of power", value = "tab1",
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
                                                 
                                                 HTML(paste0(
                                                   "<br/>",
                                                   "Power is the probability of finding an effect that is actually true. That is, having a significant
                                                   result while the null hypothesis is actually false. It is the complement of Type-II error, \\(\\beta\\) = 1 - Power, which in turn is the
                                                   probability of a false negative result, that is the probability of not rejecting the null hypothesis while it is false",
                                                   "<br/>",
                                                   "Statistical power gives us a measure of confidence that we will be able to detect a significant effect if it truely exists. It is very usefull and 
                                                   of high importance in the process of designing an experiment, where we would like to assess the required sample size in order to have a 
                                                   certain power in the experiment. If we use too small sample size then we will end up with an underpowered experiment, while by using a much bigger than
                                                   necessary, we spend resources, such as money, time, mice etc, in vain. Moroever, it will be tempting to use as many observations as possible
                                                   in order to increase the power of the test, but this is not a good practice. When for example we wish to compare 2 samples based on a characteristic,
                                                   by construction they will never be exactly equal. Therefore, if we increase the power(in this case by increasing the sample size),
                                                   we will definetely find a significant difference between them. But this difference could be so small, that is of no interest and use
                                                   in practice. Hence, resources, i.e. sample units, should be used in the degree that it is necessary to detect an effect that is 
                                                   really meaningful. The power calculations can also be done in reverse. That is, given the resources available
                                                   for the experiment, what is the expected power? Finally, power calculations are important also for the publication phase, where it can provide 
                                                   justification for publishing non-significant results.",
                                                   "<br/>","<br/>", "<br/>", "<br/>",
                                                   "* More Power increases chances of finding significant result.",
                                                   "<br/>",
                                                   "* More Power increases chances of replicating prior findings",
                                                   "<br/>",
                                                   "* More Power increases confidence about results, either significant or not",
                                                   "<br/>"),
                                                   "<br/>", "<br/>", "<br/>",
                                                   "For intuition the following example can be considered. A scientist who performs 100 experiments in their carreer and hypotherically half of them
                                                   are about a true null hypothesis and the other half about a false null hypothesis, and assuming a statistical power of 80% and Type-I error(\\(\\alpha\\)=5%),
                                                   they will be able to capture 40 out of those 50 false null hypothesis. Instead, with a constant power of 50% across their experiments, the will manage to
                                                   identify only 25 out of the 50."),
                                                 
                                                 HTML(paste0(
                                                   "<br/>", "<br/>",
                                                   "The following table shows the 4 possible outcomes of an experiment. The effect is either right or wrong(columns),
                                                   while the result of the experiment will be either significant or non-significant(rows). Therefore, we either have a 
                                                   correct inference about the effect or a false one. The false conclusions are in cells B & C, and corrspond to Type-I and Type-II errors respectively.
                                                   Type-I error, which is denoted by the Greek letter \\(\\alpha\\), is the probability of rejecting the null hypothesis when it is true 
                                                   and the Type-II error is when we do not reject it while we should, and it is denoted by the Greek letter \\(\\beta\\).
                                                   \\(\\alpha\\) is also called the significance level of the experiment/test. Finally, R denotes the prior probability of the effect being true. Or in other
                                                   words, the probability of the null hypothesis being false.",
                                                   "<br/>","<br/>","<br/>")),
                                                 tags$img(src = "images/ResultsTable.PNG", width = "800px", height = "500px"),
                                                 HTML(paste0("<br/>","<br/>","<br/>",
                                                             h4("Below some very important measures are given, which can be calculated from the 2x2 table above:"),
                                                             "<br/>", "<br/>","<br/>",
                                                             "1.$$\\text{Sensitivity(Power)} = \\frac{A}{A+C}*100 = \\frac{Power*R}{Power*R + (1-Power)*R}*100 = Power*100$$",
                                                             "It is the probability of a significant test(A) given that the effect is indeed true(A+C)",
                                                             "<br/>","<br/>","<br/>",
                                                             "2.$$\\text{Specificity}(1-\\alpha) = \\frac{D}{D+B} = \\frac{(1-\\alpha)*(1-R)}{(1-\\alpha)*(1-R) + \\alpha*(1-R)}*100 = (1-\\alpha)*100$$",
                                                             "It is the probability of a non-significant test(D) given that the effect is actually false(D+B). It is the complement of type-I error \\alpha",
                                                             "<br/>","<br/>","<br/>",
                                                             "3.$$\\text{Positive Predictive Value(PPV)} = \\frac{A}{A+B}*100 = \\frac{Power*R}{Power*R + \\alpha*(1-R)}*100$$",
                                                             "It is the probability of the null hypotehsis being False given that it was rejected(A+B)",
                                                             "<br/>","<br/>","<br/>",
                                                             "4.$$\\text{False Positive Report Probability(FPRP)} = 1-PPV = \\frac{B}{A+B}*100 = \\frac{\\alpha*(1-R)}{Power*R + \\alpha*(1-R)}*100$$",
                                                             "It is the probability of the null hypotehsis being True given that it was rejected(A+B). It is the complement of the PPV, 
                                                             with FPPV = 1 -  PPV",
                                                             "<br/>","<br/>","<br/>",
                                                             h4("Below are given some plots showing how these measures are affected by the elements that are used to calculate them"),
                                                             
                                                             "<br/>", "<br/>", "<br/>",
                                                             "First of all, the Positive Predictive Value is plotted against power for a fixed value of \\(\\alpha\\) = 0.05, and 
                                                             for different values of prior probabilities that the null hypothesis is false. We see here that the probability of the null hypothesis being
                                                             false given that it was rejected by the test(PPV) increases with power.",
                                                             "<br/>", "<br/>", "<br/>"
                                                 )),
                                                 tags$img(src = "images/PPV vs power.png", width = "800px", height = "500px"),
                                                 #tags$img(src = "NPV vs power.png", width = "800px", height = "500px"),
                                                 HTML(paste0("<br/>", "<br/>", "<br/>",
                                                             "Next, we plot the False Positive Report Probability against R, for a fixed value of \\(\\alpha\\) and also power,
                                                             for which we use 3 different values, namely 0.5, 0.7, 0.8. Moreover, we use a p-value = 0.05,
                                                             and a range of prior probabilities for the null hypothesis being false(R) between 0.1 & 0.9. The figure shows that as we increase power, 
                                                             the probability of the null hypothesis being true given that it was rejected by the test(FPRP) decreases",
                                                             "<br/>", "<br/>", "<br/>")),
                                                 #tags$img(src = "FPRP vsR_2005.png", width = "800px", height = "500px"),
                                                 tags$img(src = "images/FPRPvsR_0.05.png", width = "800px", height = "500px"),
                                                 #tags$img(src = "FPRPvsR_2.png", width = "800px", height = "500px"),
                                                 #tags$img(src = "FPRP vs R.png", width = "800px", height = "500px"), 
                                                 
                                                 HTML(paste0("<br/>", "<br/>", "<br/>",
                                                             "Further, we plot power against the percentage of false conclusions that are being made for a set of prior probabilities
                                                             for the null hypothesis being false(R), while we fix \\(\\alpha = 0.05\\). Clearly, this percentage goes down as we increase the 
                                                             power of the test.",
                                                             "<br/>", "<br/>", "<br/>" 
                                                 )),
                                                 tags$img(src = "images/FC vs Power.png", width = "800px", height = "500px"),
                                                 HTML(paste0("<br/>", "<br/>", "<br/>",
                                                             "We observe that when the prior probability of the effect being true(R) is maximum, Pr(Ho False)=1, then the % of false conclusions
                                                             depends only on the power of the test and more precisely, it is equal to the type-II error \\(\\beta\\), which is equal to 1-power. 
                                                             On the contrary, when Pr(Ho is False) = 0, the % of false conclusions is equal to the type-I error \\(\\alpha\\). Moreover, as we increase power and R
                                                             this percentage approach zero. Finally, for fixed value of alpha and power, higher prior probability is associated with more false results,
                                                             and this differences are decreasing as power gets higher.",
                                                             "<br/>","<br/>","<br/>",
                                                             "For instance, with a power=0.2 we reject very difficult the null hypothesis and therefore, if the prior probability of the null hypothesis being false is high,
                                                             we increase the probability of falsely not rejecting it, which means that type-II error(fail to detect the effect) is more influential in this case. But if however, the 
                                                             prior probability of the null hypothesis is low, then the probability of false non-rejection is also low and the % of false conclusions is weighted more by the type-I error
                                                             (finding a significant effect where there is none)."))
                                                 ),
                                       
                                        
                                        
                                        tabPanel("How to calculate Sample size and Power ?", value = "tab2",
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
                                                 
                                                 HTML(paste0("<br/>", "<br/>", "<br/>", "<br/>",
                                                             "<i><strong><font color='red'>What do I need ?</font></strong></i>",
                                                             "<br/>",
                                                             "To determine the power of an analysis we need firstly to specify the alternative Hypothesis, Ha, or in other words,
                                                             the effect size that we are interesting in detecting. Further, and for most of analyses, power is proportional to the following:",
                                                             "<br/>", "<br/>",
                                                             
                                                             "* Effect size - how big a change is of interest. The bigger the effect the higher the chances of detecting it.",
                                                             "<br/>",
                                                             "* Sample size - the number of observations used to perform the analysis. Obviosuly, the more observations the higher the power.",
                                                             "<br/>",
                                                             "* Variance - The dispersion of our observations. It is more difficult to find an effect when the population is more variable",
                                                             "<br/>",
                                                             "* Significance level(\\(\\alpha\\)) - or the type-I error of our analysis. Power is increased with higher values of \\(\\alpha\\)",
                                                             "<br/>",
                                                             "These elements are used differently for each particular test that is going to be used. That is, there is a specific equation relating
                                                             all these elements together for each test. For that reason, it is very important to know which test we will
                                                             use for our experiment in order to do the correct power analysis. Moreover, and as mentioned earlier, these equations can be used for different reasons,
                                                             depending on what information is available and what we is the quantity of interest. Such as, finding the power given the other elements, or calculating
                                                             the required sample size etc.",
                                                             "<br/>", "<br/>", "<br/>", "<br/>", "<br/>")),
                                                 
                                                 HTML(paste0("<i><strong><font color='red'>Order of actions</font></strong></i>",
                                                             "<br/>",
                                                             "When a researcher is designing an experiment it is highly advised to follow the next steps.",
                                                             "<br/>","<br/>",
                                                             "1. Formulate the null hypothesis. That is, define clearly what is the hypothesis that is going to be tested in the experiment.",
                                                             "<br/>",
                                                             "2. Identify the statistical test that is going to be performed for analyzing the results of the experiment.",
                                                             "<br/>",
                                                             "3. Be aware of the assumptions that come along with this test. If it is already know, i.e. from previous studies, that the assumptions
                                                             are not going to be met and there is almost certainty that a transformation will be used, then it is necessary to do the power calculations
                                                             with the transformed data. For instance, tumor volumes are usually analysed in the log-scale. Therefore, in the power calculations in the designing
                                                             phase, the log-scale of the data must be used.",
                                                             "<br/>",
                                                             "4. Obtain an estimate of the variation that is expected in the data. This is often found from previous experiments, pilot studies etc. As explained in
                                                             the previous step, the variance must be defined in the appropriate scale.",
                                                             "<br/>",
                                                             "5. Clearly define the effect size of interest. It is very important here to decide what effect would be of interest, and not
                                                             an effect that will be pointless from the biological perspective. For instance, a very small effect, altough statistically significant, 
                                                             has no meaning in the real-life practice, and on the other hand, a big effect will be easier to be detected, but the added information
                                                             might be minor. Therefore, it must be well understood that the decision of the effect size is not statistical but rather biological.",
                                                             "<br/>",
                                                             "In many cases it is difficult to select an effect size. Then, an alternative to a single effect size is to use a range of possible effects.
                                                             By doing that, and maybe plot or tabulate the different effects against power or sample sizes, we can get an idea of what we can achieve."))
                                                 
                                                 
                                                 
                                                 
                                                 ),
                                        tabPanel("Other Statistical issues",
                                                 HTML(paste0("<br/>", "<br/>", "<br/>","<br/>", "<br/>", "<br/>",
                                                             "<i><center><strong><font color='red'>Multiple Comparisons</font></strong></i></center>",
                                                             "<br/>", "<br/>", "<br/>","<br/>", "<br/>", "<br/>",
                                                             "When an experiment involves more than 1 comparisons, then this has to be taken into account. Because at an error rate(\\(\\alpha\\) of 5% for each comparison, 
                                                             this means you have an overall chance of up to $$1-(1-\\alpha)^{n}$$  of making a type-I error in case of n comparisons
                                                             (if all n comparisons were independent). If you wanted to compare 6 groups for instance, you'd have to do 15 pairwise t-tests, 
                                                             which would give you a high chance of finding something significant just by chance (if all tests were independent with a type-I error rate of 5% each).
                                                             The probability of at least one type-I error = $$1-(.95)^{15}=54\\%$$.",
                                                             "<br/>", "<br/>", "<br/>",
                                                             
                                                             "Many techniques have been developed on order to deal with this issue. The classic approach is to control the familywise error rate. 
                                                             Instead of setting the test's level of significance, or \\(\\alpha\\), to 0.05, you use a lower critical value. If the null hypothesis 
                                                             is true for all of the tests, the probability of getting one result that is significant at this new, lower critical value is 0.05.
                                                             In other words, if all the null hypotheses are true, the probability that the family of tests includes one or more false positives due to chance is 0.05. 
                                                             The most common way to control the familywise error rate is with the Bonferroni correction. You find the critical value (\\(\\alpha\\) for an individual test
                                                             by dividing the familywise error rate (usually 0.05) by the number of tests. Thus if you are doing 10 statistical tests, the critical value 
                                                             for an individual test would be 0.05/10=0.005, and you would only consider individual tests with P<0.0005 to be significant.",
                                                             "<br/>", "<br/>", "<br/>", "<br/>", "<br/>", "<br/>")),
                                                 tags$img(src = "images/FWER.PNG", width = "800px", height = "500px"),
                                                 
                                                 HTML(paste0("<br/>", "<br/>", "<br/>","<br/>", "<br/>", "<br/>",
                                                             "<i><center><strong><font color='red'>One-sided vs Two-sided tests</font></strong></i></center>",
                                                             "<br/>", "<br/>", "<br/>","<br/>", "<br/>", "<br/>",
                                                             "<strong><font color=' #3380ff '>What is a two-tailed test?</font></strong></i>", 
                                                             "<br/>", "<br/>", "<br/>",
                                                             "If you are using a significance level of 0.05, a two-tailed test puts half of your \\(\\alpha\\) to testing the statistical significance in one direction 
                                                             and half of your alpha to testing statistical significance in the other direction. This means that .025 is in each tail of the distribution of your test statistic. 
                                                             When using a two-tailed test, regardless of the direction of the relationship you hypothesize, you are testing for the possibility of the relationship in both directions.  
                                                             For example, we may wish to compare the mean of a sample to a given value x using a t-test.  Our null hypothesis is that the mean is equal to x. 
                                                             A two-tailed test will test both if the mean is significantly greater than x and if the mean significantly less than x. The mean is considered significantly different from x if 
                                                             the test statistic is in the top 2.5% or bottom 2.5% of its probability distribution, resulting in a p-value less than 0.05.")),
                                                 HTML(paste0("<br/>", "<br/>", "<br/>",
                                                             "<i><strong><font color=' #3380ff '>What is a one-tailed test?</font></strong></i>",
                                                             "<br/>", "<br/>", "<br/>",
                                                             "If you are using a significance level of 0.05, a one-tailed test puts all of your \\(\\alpha\\) to testing the statistical significance
                                                             in the one direction of interest.  This means that .05 is in one tail of the distribution of your test statistic. When using a one-tailed test, 
                                                             you are testing for the possibility of the relationship in one direction and completely disregarding the possibility of a relationship in the other direction.  
                                                             For example, in comparing the mean of a sample to a given value x using a t-test, our null hypothesis is that the mean is equal to x. 
                                                             A one-tailed test will test either if the mean is significantly greater than x or if the mean is significantly less than x, but not both. 
                                                             Then, depending on the chosen tail, the mean is significantly greater than or less than x if the test statistic is in the top 5% of its probability distribution
                                                             or bottom 5% of its probability distribution, resulting in a p-value less than 0.05. The one-tailed test provides more power to detect an effect in one direction 
                                                             by not testing the effect in the other direction.")),
                                                 
                                                 HTML(paste0("<br/>", "<br/>", "<br/>",
                                                             "<i><strong><font color=' #3380ff '>When is a one-tailed test NOT appropriate?</font></strong></i>",
                                                             "<br/>", "<br/>", "<br/>",
                                                             "Choosing a one-tailed test for the sole purpose of attaining significance is not appropriate.  Choosing a one-tailed test after running a two-tailed test
                                                             that failed to reject the null hypothesis is not appropriate, no matter how 'close' to significant the two-tailed test was.
                                                             Using statistical tests inappropriately can lead to invalid results that are not replicable and highly questionable"))
                                                 
                                                 
                                                 
                                                 ),
                                        
                                        
                                        
                                        tabPanel("Softwares for Sample size and power calculation",
                                                 HTML("<center><strong>G*Power: Statistical Power Analyses for Windows and Mac</strong></center>"),
                                                 
                                                 HTML(paste0("<br/>","<br/>",
                                                             "G*Power is a tool to compute statistical power analyses for many different t tests, F tests, t tests, z tests and some exact tests. 
                                                             G*Power can also be used to compute effect sizes and to display graphically the results of power analyses.",
                                                             "<br/>",
                                                             "G*Power is a freely available software and currently it is condidered one of the best for power calculations.
                                                             More information about it, such as a manual, as well as guidelines about how to download it can be found in the following link:",
                                                             "<br/>",
                                                             a("G*Power link", href="http://www.gpower.hhu.de/", target = "_blank"),
                                                             "<br/>", "<br/>", "<br/>", "<br/>",
                                                             "Some guidelines are provided also here for each test in the associated tab"
                                                 ))))
                                                 ),
                          
                           tabPanel("Wilcoxon ranks-sum test",  value = "Page4",
                                    withMathJax(),
                                    tabsetPanel(
                                      
                                      tabPanel("Basic information",
                                               HTML(paste0("<br/>",
                                                           "A T-test or Wilcoxon-Mann-Whitney test can be used for comparison of two groups, while an ANOVA or Kruskal-Wallis test can be used for comparison of more than two groups.
                                                           based on mean or median values of the outcome.
                                                           T-test and ANOVA are parametric tests that rely on certain assumptions and these assumptions need to be met to get reliable test results. Validation of these assumptions
                                                           becomes impossible when sample is small and a researcher should use non-parametric alternative tests instead, namely Mann-Whitney-Wilcoxon or Kruskal-Wallis test.
                                                           Since animal experiments are usually conducted with small number of mice, the recommendation is to use Wilcoxon-Mann-Whitney test or Kruskal-Wallis test that are described in more details here. 
                                                           In an experiment with more than two groups of mice, a researcher can first use Kruskal-Wallis test to determine whether there are any statistically significant differences between the groups, 
                                                           i.e. whether at least one group of mice is different from others. This test is called the omnibus test and when significant results are detected, a researcher does not know which group or groups 
                                                           are actually different from each other. Therefore, as the next step, a researcher can perform pairwise comparison tests, also called post hoc pairwise tests, to find the group or groups with significantly different 
                                                           average values of the outcome of interest. In this step, Wilcoxon-Mann-Whitney test can simply be used for comparison of two groups.",
                                                           "<br/>","<br/>","<br/>",
                                                           "However, one needs to be aware that conducting multiple pairwise tests increases 
                                                           the probability of a false positive result and a correction of the significance level \\(\\alpha\\) should be implemented. The most popular correction is the Bonferroni adjustment which divides the \\(\\alpha\\) by the total number of comparisons 
                                                           that are performed. For instance, in an experiment with three treatment groups (A, B, C), three pairwise comparisons can be performed (A-B, A-C, B-C). 
                                                           To have the overall \\(\\alpha\\) of 0.05, i.e. 5% chance that at least one of the comparison is false positive, a significance level of \\(\\alpha\\)/(number of comparisons)=0.05/3=0.0167 for each pairwise test should be used.",
                                                           "<br/>","<br/>","<br/>",
                                                           tags$a(href="https://www.nki.nl/", "Here"),
                                                           
                                                           HTML(" (probably an outside the app reference), you can find more about this and some alternative solutions to the multiple comparisons issue."),
                                                           "<br/>","<br/>","<br/>","<br/>","<br/>"
                                               ))),
                                               
                                      
                                      tabPanel("Example", value = 'Example_means',
                                               HTML("<center><i><strong><font color='blue'>For 2 groups</font></strong></i></center>"),
                                               h5("Example:"),
                                               HTML("A scientist wants to test the hypothesis that a novel compound had a beneficial effect on reducing high-density lipoprotein (HDL) cholesterol levels 
                                      in a transgenic C57Bl/6J strain of mice. Therefore she wants to conduct a new study by randomizing mice to control treatment groups, in order to compare the average HDL 
                                      cholesterol levels from the two groups. From a previous experiment, following measurements of HDL are observed:",
                                               "<br/>", "<br/>","<br/>"),
                                               DT::dataTableOutput("Example_1"),
                                               
                                               HTML("<br/>", "<br/>","<br/>",
                                                    "These data can be used to calculate power and required sample size for The new experiment. The following information from the 
                                                    observed previous data is needed:",
                                                    "<ol>
                                                      <li>Mean in group A (Control)</li>
                                                      <li>Mean in group B (Treatment)</li>
                                                      <li>Standard Deviation in group A</li>
                                                      <li>Standard Deviation in group B</li>
                                                      </ol>",
                                                    "<br/>", "<br/>","<br/>",
                                                    "Further, the power level and the significance level(\\(\\alpha\\)) of a test, or in other words the desired Type-I error,
                                                    need to be specified. Usually, power is set to 80% and \\(\\alpha\\) to 5%.",
                                                    "<br/>","<br/>","<br/>","<br/>","<br/>"),
                                               
                                               
                                               HTML("<center><i><strong><font color='blue'>For more than 2 groups</font></strong></i></center>"),
                                               h5("Example:"),
                                               HTML("A scientist wants to test the hypothesis that a novel compound had a beneficial effect on reducing high-density lipoprotein (HDL) cholesterol levels 
                                      in a transgenic C57Bl/6J strain of mice. Therefore she wants to conduct a new study by randomizing mice to control and two treatment groups, in order to compare the average HDL 
                                      cholesterol levels from the three groups. From a previous experiment, following measurements of HDL are observed:",
                                                    "<br/>", "<br/>","<br/>"),
                                               DT::dataTableOutput("Example_2"),
                                               HTML("<br/>", "<br/>","<br/>",
                                                    "In case of more than two groups, for the power calculation we use information about two groups with the
                                                    smallest difference because a larger sample is required to detect a smaller effect. If an experiment is powered for 
                                                    the smallest difference, it is also powered to detect a larger difference. After finding these 2 groups, 
                                                    the procedure is exactly the same with the case of 2 groups.",
                                                    "<br/>", "<br/>","<br/>")
                                               
                                               ),
                                              
                                      tabPanel( "Example - Power Calculation",
                                                sidebarLayout(
                                                  sidebarPanel(
                                                    numericInput( "meanAAAInput", "Mean of group A", 267.39 ),
                                                    numericInput( "meanBBBInput", "Mean of group B", 283.46 ),
                                                    numericInput( "SDAAAInput", "SD of group A", 14.38 ),
                                                    numericInput( "SDBBBInput", "SD of group B", 11.83 ),
                                                    selectInput( "type3Input", "Type of test", c("one.sided", "two.sided"), selected = "two.sided"),
                                                    sliderInput( "Power3Input", "Power", min = 0, max = 1, value = 0.8 ),
                                                    numericInput( "error3Input", "Type I error", 0.05, min = 0, max = 1 ),
                                                    withBusyIndicatorUI(
                                                      actionButton("buttonInput", "Go!", class = "btn-primary")
                                                    )
                                                  ),
                                                  mainPanel(
                                                    HTML("<br/>", "<br/>","<br/>",
                                                         "Here it is an example of how this app can be used in order to perform power calculations. 
                                                         In the left side, there is a panel where you can provide the input that is required for it,
                                                         and after clicking on <strong><font color='blue'>Go!</font></strong>, the results will appear in the right side(it might take a couple of seconds).
                                                         Along with that, a plot will also be provided with a range of possible sample sizes and their associated power values.",
                                                         "<br/>", "<br/>","<br/>", 
                                                         "For illustration, we will use the example that we saw"),
                                                    tags$a("Here", onclick = "customHref('Example_means');"),
                                                         
                                                    HTML("for both cases (2 groups & >2 groups)",
                                                        "<br/>", "<br/>","<br/>",
                                                        "<center><i><strong><font color='blue'>For 2 groups</font></strong></i></center>",
                                                         "For this particular example, we have:",
                                                         "<ol>
                                                         <li>Mean in group A (Treatment) = 267.39</li>
                                                         <li>Mean in group B (Control) = 283.46</li>
                                                         <li>Standard Deviation in group A = 14.38</li>
                                                         <li>Standard Deviation in group B = 11.83</li>
                                                         </ol>",
                                                         "<br/>", "<br/>","<br/>",
                                                         "Finally, we specify \\(\\alpha\\) at 5% and the desired power to be 80%."),
                                                    
                                                    HTML("<br/>", "<br/>","<br/>",
                                                         "<center><i><strong><font color='blue'>For more than 2 groups</font></strong></i></center>",
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
                                                         "As we explained, in this case we should find the two groups with the smaller difference.
                                                         We observe here that these two groups are Treatment A and Treatment B, and therefore these will be used for 
                                                         the power calculation. Since three pairwise tests will be performed, the \\(\\alpha\\) is adjusted by 0.05/3 = 0.0166.
                                                         Finally, we specify the desired power to be 80%."),
                                                    
                                                    HTML("<br/>", "<br/>","<br/>",
                                                         "If we now provide the input to the panel at the left, we will see below the results."),
                                                    
                                                    HTML("<br/>", "<br/>","<br/>", 
                                                         "<strong><font color='#4d3a7d'>In order to achieve</font></strong>"),
                                                       uiOutput('dynamic_valueP',inline = T),
                                                    HTML("<strong><font color='#4d3a7d'>power to reject the null hypothesis of equal means when the population
                                                        mean difference is M1-M2 =</font></strong> "),
                                                       uiOutput('dynamic_valueD',inline = T),
                                                    HTML("<strong><font color='#4d3a7d'>with a standard deviation for group A=</font></strong>"),
                                                       uiOutput('dynamic_valueSDA',inline = T),
                                                    HTML("<strong><font color='#4d3a7d'>and group B=</font></strong>"),
                                                       uiOutput('dynamic_valueSDB',inline = T),
                                                    HTML("<strong><font color='#4d3a7d'>and with a significance level (a) of</font></strong>"),
                                                       uiOutput('dynamic_valueA',inline = T),
                                                    HTML("<br/>", "<br/>","<br/>"),
                                                    h4("The required sample size per group is:",
                                                        style = "font-family: 'Lobster', cursive;
                                                       font-weight: 500; line-height: 1.1; 
                                                       color: #4d3a7d;"), verbatimTextOutput('results3'), 
                                                    br(), br(),
                                                    
                                                    plotOutput("coolplot3")))),
                                      
                                      tabPanel( "Power Calculation",
                                                sidebarLayout(
                                                  sidebarPanel(
                                                    numericInput( "meanAAA2Input", "Mean of group A", 267.39 ),
                                                    numericInput( "meanBBB2Input", "Mean of group B", 283.46 ),
                                                    numericInput( "SDAAA2Input", "SD of group A", 14.38 ),
                                                    numericInput( "SDBBB2Input", "SD of group B", 11.83 ),
                                                    selectInput( "type32Input", "Type of test", c("one.sided", "two.sided"), selected = "two.sided"),
                                                    sliderInput( "Power32Input", "Power", min = 0, max = 1, value = 0.8 ),
                                                    numericInput( "error32Input", "Type I error", 0.05, min = 0, max = 1 ),
                                                    withBusyIndicatorUI(
                                                      actionButton("button2Input", "Go!", class = "btn-primary")
                                                    )
                                                  ),
                                                  mainPanel(
                                                    HTML("<br/>", "<br/>","<br/>", 
                                                         "<strong><font color='#4d3a7d'>In order to achieve</font></strong>"),
                                                    uiOutput('dynamic_valueP2',inline = T),
                                                    HTML("<strong><font color='#4d3a7d'>power to reject the null hypothesis of equal means when the population
                                                         mean difference is M1-M2 =</font></strong> "),
                                                    uiOutput('dynamic_valueD2',inline = T),
                                                    HTML("<strong><font color='#4d3a7d'>with a standard deviation for group A=</font></strong>"),
                                                    uiOutput('dynamic_valueSDA2',inline = T),
                                                    HTML("<strong><font color='#4d3a7d'>and group B=</font></strong>"),
                                                    uiOutput('dynamic_valueSDB2',inline = T),
                                                    HTML("<strong><font color='#4d3a7d'>and with a significance level (a) of</font></strong>"),
                                                    uiOutput('dynamic_valueA2',inline = T),
                                                    HTML("<br/>", "<br/>","<br/>"),
                                                    h4("The required sample size per group is:",
                                                       style = "font-family: 'Lobster', cursive;
                                                       font-weight: 500; line-height: 1.1; 
                                                       color: #4d3a7d;"), verbatimTextOutput('results32'), 
                                                    br(), br(),
                                                    plotOutput("coolplot32")))),
                                      tabPanel("Wilcoxon ranks-sum test description")
                                      )), 
                                        
                                      tabPanel("Log-rank test", value = "Page5",
                                               useShinyjs(),
                                               withMathJax(),
                                               tabsetPanel(
                                                 tabPanel("Basic information", htmlOutput("tutorialSurv")
                                                          #withMathJax(htmlOutput("formulaSurv")),
                                                          #h4("where:"),
                                                          #helpText("\\(\\bar{x}_{1}\\): The mean of sample 1"),
                                                          #helpText("\\(\\bar{x}_{2}\\): The mean of sample 2"),
                                                          #helpText("\\(n_{1}\\),\\(n_{2}\\): The sample sizes of sample 1 and 2 respectively"),
                                                          #helpText("\\(S_{p}\\): The pooled standard deviation from the 2 samples, with:
                                                          #         $$S_{p}=\\sqrt{\\frac{(n_{1}-1)S_{1}^2+(n_{2}-1)S_{2}^2}{n_{1}+n_{2}-2}}$$"),
                                                          #helpText("\\(S_{1}\\): The standard deviation from sample 1"),
                                                          #helpText("\\(S_{2}\\): The standard deviation from sample 2")
                                                          #htmlOutput("tutorial2"),
                                                          #withMathJax(htmlOutput("formula1")),
                                                          #htmlOutput("tutorial22"),
                                                          #withMathJax(htmlOutput("formula11")))
                                                          ),
                                                 
                                                 tabPanel("Example", value = 'Example_surv', 
                                                          
                                                          htmlOutput("ExampleSurv"),
                                                          
                                                          DT::dataTableOutput("Surv_Example_1"),
                                                          
                                                          HTML("For the power calculation based on a logrank test information on  the proportions of  surviving  until a particular time point  is required. 
                                                                Moreover, information on accrual time and total experiment time is needed.  The accrual time is the duration of time of mice enrolment into the study,
                                                                which is equal to zero  when all mice are included in the experiment at the same time.  The total experiment time is the planned duration of the experiment. 
                                                               Further, the <font color = 'red'>power level</font> and the <font color = 'red'>significance level</font> (\\(\\alpha\\)) of the test, or in other words the desired Type-I error, need to be specified.  
                                                                Usually, power is set to 80% and \\(\\alpha\\) to 5%.",
                                                               "Therefore, we need:",
                                                               "<br/>", "<br/>","<br/>",
                                                               "<ol>
                                                               <li>	Median Survival in group A  (Control)</li>
                                                               <li>	Median Survival in group B (Treatment)</li>
                                                               <li>	Accrual time      </li>
                                                               <li>	Follow-up time </li>
                                                               <li>	Power level </li>
                                                               <li>	Significance level </li>
                                                               </ol>",
                                                               "<br/>", "<br/>","<br/>","<br/>", "<br/>","<br/>"
                                                               )
                                                          
                                                          ),
                                                 
                                                 tabPanel("Example - Power Calculation",
                                                          sidebarLayout(
                                                            sidebarPanel(
                                                              numericInput( "MedAInput", "Median survival of group A-days", 25.5 ),
                                                              numericInput( "MedBInput", "Median survival of group B-days", 35 ),
                                                              numericInput( "FTInput", "Experiment's duration-days", 60 ),
                                                              sliderInput( "PowerInputS", "Power", min = 0, max = 1, value = 0.8 ),
                                                              numericInput( "errorInputS", "Type I error", 0.05, min = 0, max = 1 )
                                                              
                                                            ),
                                                            
                                                            mainPanel(
                                                              HTML("<br/>", "<br/>","<br/>",
                                                                   "Here it is an example of how this app can be used in order to perform power calculations. 
                                                                   In the left side, there is a panel where you can provide the input that is required for it,
                                                                   and the results will appear in the right side(it might take a couple of seconds).
                                                                   Along with that, a plot will also be provided with a range of possible sample sizes and their associated power values.",
                                                                   "<br/>", "<br/>","<br/>", 
                                                                   "For illustration, we will use the example that we saw"),
                                                              tags$a("Here", onclick = "customHref('Example_surv');"),
                                                              HTML("<br/>", "<br/>","<br/>",
                                                                   "For this particular example, we have:",
                                                                   "<ol>
                                                                   <li>Median Survival in group A (Treatment) = 25.5</li>
                                                                   <li>Median Survival in group B (Control) = 35</li>
                                                                   <li>Accrual time= 1</li>
                                                                   <li>Follow-up time = 60</li>
                                                                   </ol>",
                                                                   "<br/>", "<br/>","<br/>",
                                                                   "Finally, we specify \\(\\alpha\\) at 5% and the desired power to be 80%."),
                                                              
                                                              h4("The sample size per group is:",  style = "font-family: 'Lobster', cursive;
                                                                 font-weight: 500; line-height: 1.1; 
                                                                 color: #4d3a7d;"), verbatimTextOutput('resultsSurv'), 
                                                              br(), br(),
                                                              plotOutput("coolplotSurv")))
                                                          ),
                                                 
                                                 tabPanel("Power Calculation",
                                                          sidebarLayout(
                                                            sidebarPanel(
                                                              numericInput( "MedA1Input", "Median survival of group A-days", 60 ),
                                                              numericInput( "MedB1Input", "Median survival of group B-days", 30 ),
                                                              numericInput( "FT1Input", "Experiment's duration-days", 100 ),
                                                              sliderInput( "Power1InputS", "Power", min = 0, max = 1, value = 0.8 ),
                                                              numericInput( "error1InputS", "Type I error", 0.05, min = 0, max = 1 )
                                                              
                                                            ),
                                                            
                                                            mainPanel(
                                                              
                                                              h4("The sample size per group is:",  style = "font-family: 'Lobster', cursive;
                                                                 font-weight: 500; line-height: 1.1; 
                                                                 color: #4d3a7d;"), verbatimTextOutput('resultsSurv1'), 
                                                              br(), br(),
                                                              plotOutput("coolplotSurv1")))
                                                          ),
                                                 tabPanel("Log-rank test description",
                                                          HTML("In the data that from the example "),
                                                          tags$a("here", onclick = "customHref('Example_surv');"),
                                                          HTML("we observe that all the mice in the control group have experienced the event of interest during the experiment, 
                                                                while in the treatment group we have 3 mice that are censored. Two mice have died early but from reasons unrelated 
                                                                to the experiment and one mouse was still alive at the end of the experiment. Because of the presence of censoring,
                                                                the log-rank test is the only test that we should use. I order to prove that, we will analyze this data with both the
                                                                log-rank test and a simple Wilcoxon-Mann-Whitney.",
                                                               "<br/>", "<br/>","<br/>"),
                                                          HTML("<center><i><strong><font color='blue'>Log-rank test</font></strong></i></center>"),
                                                          verbatimTextOutput("Surv_Example_2"),
                                                          
                                                          
                                                          HTML("<br/>", "<br/>","<br/>",
                                                               "<center><i><strong><font color='blue'>Wilcoxon test</font></strong></i></center>"),
                                                          verbatimTextOutput("Surv_Example_3"),
                                                          HTML("So, we see that the log-rank test is significant at the 5% significance level while the Wilcoxon test is not, based on their p-values. 
                                                              Intuitively this can be explained from the fact that we have 2 early deaths in the treatment group which are not related to our experiment 
                                                              and thus they are considered censored. This can be seen from the log-rank test which can handle the censoring but not from the t-test which just compares 
                                                              the times from the 2 groups and does not take into account the censoring. Therefore, we see that when there is censoring in the data, 
                                                              log-rank test (or any other survival analysis test) is the only choice.",
                                                               "<br/>", "<br/>","<br/>","<br/>", "<br/>","<br/>",
                                                              "Let's now consider another example.",
                                                              "<br/>", "<br/>","<br/>","<br/>", "<br/>","<br/>"),
                                                          DT::dataTableOutput("Surv_Example_4"),
                                                          HTML("<br/>", "<br/>","<br/>","<br/>",
                                                                "Here we have 39 mice, 21 in control and 18 in treatment respectively, and all them have experienced the event except one mice in the treatment group
                                                                which has died from other causes early in the study. We analyze these data again with both log-rank test and Wilcoxon."),
                                                          HTML("<br/>", "<br/>","<br/>",
                                                               "<center><i><strong><font color='blue'>Log-rank test</font></strong></i></center>"),
                                                          verbatimTextOutput("Surv_Example_5"),
                                                          
                                                          
                                                          HTML("<br/>", "<br/>","<br/>",
                                                               "<center><i><strong><font color='blue'>Wilcoxon test</font></strong></i></center>"),
                                                          verbatimTextOutput("Surv_Example_6"),
                                                          
                                                          HTML("<br/>", "<br/>","<br/>",
                                                              "Here both tests are highly significant in the 5% significance level. Here we have only 1 out of the 39 mice censored and this does not affect the conclusion 
                                                               that we make with both tests. Of course, this would cause a problem if the sample size here was smaller, because each mouse would put more weight on the tests.",
                                                              "<br/>", "<br/>","<br/>")
                                                          
                                                          )
                                                          
                                                 )),
                                                 
                                      tabPanel("Longitudinal Analysis", value = "Page6"), inverse = T,collapsible = T
                           
                                               )))
  


