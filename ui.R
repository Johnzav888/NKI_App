ipak <- function( pkg ){  # Check if the packages are installed, and if not install them. Finally load them!
  new.pkg <- pkg[!( pkg %in% installed.packages()[, "Package"] ) ]
  if ( length( new.pkg ) ) 
    install.packages(new.pkg, dependencies = TRUE )
  for(package_name in pkg)
  {library(package_name,character.only=TRUE, quietly = TRUE)}

}

# usage
#packages <- c("xlsx", "nlme", "shiny", "ggplot2", "dplyr", "shinyjs", "shinycssloaders", "shinythemes", "plotly", 
#               "samplesize","fBasics", "DT", "survival", "powerMediation", "stringi", "stringr", "statmod")
#ipak(packages)


library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(shinyjs)
library(shinythemes)
library(plotly)
library(samplesize)
library(fBasics)
library(survival)
library(crayon)
library(shinycssloaders)
library(stringr)
library(xlsx)
library(nlme)
library(statmod)
library(rlang)

#library(shiny.router)

#source("https://raw.githubusercontent.com/daattali/advanced-shiny/master/busy-indicator/helpers.R")

#saveRDS(withBusyIndicatorUI, file = "shinyhelper1")
#saveRDS(withBusyIndicatorServer, file = "shinyhelper2")

source("Helpers.R")
#options(shiny.launch.browser = .rs.invokeShinyWindowViewer)


ui <- tagList(
  tags$head(
    tags$script("src"="app.js")
  ), fluidPage(navbarPage(theme=shinytheme("spacelab"), title = div(img(src = "images/logo-AVL2.jpg", height = "55px", width = "150px"
                                                                        ))#, h4("NKI_statistics", 
                                                                         #style = "font-family: 'Lobster', cursive;
                                                                        #font-weight: 500; line-height: 1.1; 
                                                                        #color: #e8ea0f;")) 
                           , fluid = FALSE, windowTitle = "NKI_statistics", 
                           tabPanel("Design and statistical analysis of mice experiments", value = "StartPage",
                                    
                                    fluidRow(
                                      column(10, HTML(paste0("<br/>",
                                                    "This website is provided by the NKI Biostatistics Center ",
                                                    tags$a("(here) ", href = "https://www.nki.nl/topmenu/biostatistics-center/", target = "_blank"), 
                                                    "and supports researchers conducting mice experiments at the Netherlands Cancer Institute
                                                    in the statistical aspects of the studies. It provides explanations of basic statistical concepts and tests.
                                                    Moreover, researchers can use this app to calculate the required sample size when an experiment is being designed.
                                                    The latter is vital and must be performed, because :",
                                                    "<br/>", "<br/>",
                                                    "<i><center><strong> More power increases the confidence in the results, whether they are significant or not </strong></center></i>",
                                                    "<br/>", "<br/>",
                                                    "Some background about sample size calculations can be found in the  ")),
                                               
                                               #tags$div(
                                              #        id = "sdf",
                                               #       tags$a(onclick = "customHref('Power')", "Power")),
                                               #tags$div(
                                                 #id = "Others",
                                                 tags$a("Statistical Power", onclick = "customHref('Page2');customHref('tab1');"),
                                                 #tags$a("ff2", onclick = "customHref('Others');")
                                              HTML("section")))
                                      ,
                                      
                                    fluidRow(
                                      column(10, HTML(paste0("<br/>", "<br/>",
                                      "Sample size calculation depends on the type of experiment. In  most of the mouse experiments conducted in the Netherlands Cancer Institute,  
                                      groups of mice are compared with respect to mean/median values, survival outcomes, proportions and tumor growth. 
                                      Examples of such experiments are listed below and more information can be found under the specific tabs.")))),
                          
                                    fluidRow(
                                      column(10, HTML(paste0("<br/>", "<br/>",
                                      "<i><strong><font color='red'> Comparison of means/medians</font></strong></i>",
                                      "<br/>", "<br/>",
                                      "<i>Example:</i> : A scientist wants to test the hypothesis that a novel compound is superior in reducing high-density lipoprotein (HDL)
                                          cholesterol levels in a transgenic C57Bl/6J strain of mice in comparison  to a standard treatment. In the experiment, one group of  mice
                                          receives the standard treatment and the other group receives the novel compound. At the end of the experiment, the HDL cholesterol levels are
                                          determined in all mice and the average HDL cholesterol level between both groups is compared.",
                                          "<br/>", "<br/>")))),
                          
                          tags$a("Go to Means/Median Analysis", onclick = "customHref('Page4');"),
                          
                          
                          fluidRow(
                            column(10, HTML(paste0("<br/>", "<br/>",
                               "<i><strong><font color='red'> Survival Analysis </font></strong></i>",
                               "<br/>", "<br/>",
                               "<i>Example:</i> To evaluate whether the chemotherapeutic agent  paclitaxel improves survival after esophageal adenocarcinoma (EAC), 
                                a scientist uses a peritoneal dissemination xenograft mouse model and injects human EAC cell lines intraperitoneally into severe combined immunodeficiency (SCID) mice. 
                                Two weeks later, the mice are randomly assigned to either  vehicle or paclitaxel (20mg/kg, 2 times a week for 2 weeks) groups. 
                                Mice are followed until death or the end of the study duration to compare the survival distributions  between the two  groups.",
                               "<br/>", "<br/>")))),
               
                       tags$a("Go to Survival Analysis", onclick = "customHref('Page5');"),
                    
                       fluidRow(
                         column(10, HTML(paste0("<br/>", "<br/>",
                            "<i><strong><font color='red'> Proportion Analysis</font></strong></i>",
                            "<br/>", "<br/>","<br/>",
                            "<i>Example:</i> A scientist wants to test the hypothesis that a new combination treatment is associated with a higher proportion of mice having
                              a complete response  compared with  a  standard treatment.  Mice without palpable tumors for 14 days are considered complete responders. 
                              The proportions of complete responders are compared between treatment groups.",
                            "<br/>", "<br/>")))),
               
                      tags$a("Go to Proportion Analysis", onclick = "customHref('Page6');"),
    
                      fluidRow(
                        column(10, HTML(paste0("<br/>", "<br/>",
                            "<i><strong><font color='red'> Growth Curve Analysis</font></strong></i>",
                          "<br/>", "<br/>",
                          "<i>Example:</i> A scientist wants to test the hypothesis that a new treatment is able to slow down tumor growth.
                            An experiment is conducted where tumor cells are injected into mice and volume of the  tumor is measured every 2-3 days.
                            When tumors reach a pre-defined volume of 1500 mm3,  mice are randomized to receive either the standard treatment or the new treatment.
                            Tumor volume is measured until mice die or are sacrificed. The rate of tumor growth is compared between treatment groups.",
                          "<br/>", "<br/>")))),
  
                      tags$a("Go to Growth Curve Analysis", onclick = "customHref('Page7');"),
  
                      HTML(paste0("<br/>", "<br/>","<br/>")),
                      tags$footer(HTML(paste0("Please note that this is the first version of the website.",
                                              "<br/>", "<br/>",
                                              "Should you encounter any bugs, glitches, lack of functionality or other problems on the website,
                          please let us know immediately so we can rectify these accordingly. Your help in this regard is greatly appreciated!
                          You can send your comments to j.zavrakidis@nki.nl",
                                              "<br/>", "<br/>","<br/>","<br/>","<br/>")),
                                  style = "text-align:center;
                              margin-left:150px;
                              bottom:0;
                              width:50%;
                              height:120px;   /* Height of the footer */
                              color: red;
                              background-color:lightblue;
                              background-size: 100% 100%;
                              ")
                      ),
              
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
                                                   column(10, HTML(paste0("<br/>","<br/>",
                                                            "An experiment is conducted to answer a particular research question, for instance, 
                                                            to investigate whether the outcome after a new treatment differs from the outcome after the standard treatment,
                                                            i.e. whether there is an effect of the new treatment. The research question is typically framed into a null hypothesis
                                                            (e.g., there is no treatment effect) and an alternative hypothesis (e.g., there is a treatment effect). The answer to the
                                                            research question is based on a statistical hypothesis test, which either leads to the rejection of the null hypothesis or not. 
                                                            The statistical test is designed to generally reflect the true state of nature, but there is a chance for erroneous decisions. 
                                                            A researcher can make two types of correct decisions and two types of errors, which is shown in the table below.",
                                                            "<br/>","<br/>","<br/>")))),
                                                
                                                 tags$img(src = "images/HypothesisTest_table2.PNG", width = "800px", height = "300px"),
                                                 
                                                 
                                                 fluidRow(
                                                   column(10, HTML(paste0(
                                                   "<br/>","<br/>","<br/>",
                                                   "The effect either exists or not in nature, while the result of the statistical analysis is either significant or non-significant. Therefore, based on the statistical analysis, 
                                                   a researcher either makes a correct inference about the effect or a false one.  The type 1 error (\\(\\alpha\\)) is the probability of finding an 
                                                   effect, i.e. rejecting the null hypothesis of no effect, when it does not exist. It is also called the significance level of a test. The type 2 error (\\(\\beta\\)) is the probability of not finding
                                                   an effect, i.e., not rejecting the null hypothesis of no effect, when the effect exists. The type 1 error is the probability of a false positive finding, while the type 2 error is the probability 
                                                   of a false negative finding. Complements of the two probabilities, 1-\\(\\alpha\\)  and 1-\\(\\beta\\) , are probabilities of correctly not finding an effect
                                                   (true negative finding) and correctly finding an effect (true positive finding), respectively. The latter probability, 1- \\(\\beta\\), is also called 
                                                   the statistical power of a test. The value of \\(\\alpha\\) is usually fixed at 0.05. The value of beta decreases with increasing effect size and sample size.
                                                   ")))),
                                                 
                                                 
                                                 fluidRow(
                                                   column(10, HTML(paste0("<br/>","<br/>",
                                                  "If there is a true effect of a treatment, researchers would like to detect it with high probability.
                                                   A power level of 0.8 or 0.9 is usually considered sufficient. For illustration, if 100 experiments are conducted with an existing true effect and
                                                  each experiment has a power of 0.8 (i.e., 80%), the statistical analyses would be significant for 80 experiments (and result in rejection of the
                                                  hypothesis of no effect), while 20 experiments would yield a non-significant result of the statistical test, i.e., the true effect would be missed 
                                                  (false negative finding). On the other hand, if none of the 100 experiments is based on a true effect, and a significance level of \\(\\alpha\\)=0.05 (i.e., 5%) 
                                                  is used, then the statistical analysis of 5 experiments would be expected to be statistically significant (p<0.05), i.e., reflecting false positive 
                                                  (or chance) findings.",
                                                   "<br/>","<br/>","<br/>",
                                                   "Statistical power is a measure of confidence to detect an effect (i.e., a significant result) if it truly exists. The power depends on the sample size of 
                                                  an experiment and the magnitude of the effect. During the design phase of an experiment, a researcher can assess how many mice need to be included in
                                                  order to detect a true effect with sufficient probability. This assessment is important because an underpowered experiment (too few mice) can miss an
                                                  effect that truly exists. An overpowered experiment (too many mice) can detect an effect that truly exists but is so small that it is not of practical
                                                  relevance. In both situations, resources spent on an experiment, such as money, time or animals' lives are wasted.",
                                                   
                                                   "<br/>","<br/>", 
                                                   "* More power increases the chance of a significant result.",
                                                   "<br/>",
                                                   "* More power increases the chance of replicating prior findings, if true.",
                                                   "<br/>",
                                                   "* More power increases the confidence about the results, either significant or not.",
                                                   "<br/>",
                                                   "<br/>", 
                                                   
                                                   "So far, we assumed that a true effect does or does not exist. In reality, this is unknown. Let R be the probability that a true effect exists for a particular experiment or, 
                                                    in a large number of experiments (e.g., all experiments done in a career), the proportion of experiments with a true effect.
                                                    The table of possible decisions based on statistical tests is then given by:    ",
                                                   "<br/>","<br/>","<br/>"
                                                   )))),
                                                 
                                                 tags$img(src = "images/Power_table.PNG", width = "800px", height = "300px"),
                                                 
                                                 fluidRow(
                                                   column(10, HTML(paste0(
                                                   "<br/>","<br/>",
                                                   "Assume a scientist develops and tests hypotheses so that a true effect exists (i.e., the null hypothesis is wrong) for half of her experiments (R=0.5).
                                                   If she chooses the sample sizes of 100 experiments so that power is 80%, she is expected to obtain significant tests for 40 of the 50 experiments with a true effect
                                                   (i.e., reject 40 of the 50 wrong null hypotheses) and miss the effect for the remaining 10 experiments (i.e., not reject 10 of the 50 wrong null 
                                                   hypotheses). If power is 50%, only 25 of the 50 true effects will, on average, be identified. For each experiment, four important measures are considered:"
                                                 )))),
                                                 
                                                 
                                                 fluidRow(
                                                   column(10, HTML(paste0("<br/>","<br/>","<br/>",
                                                             
                                                             "1.$$\\text{True positive rate} = \\frac{Power*R}{Power*R + (1-Power)*R} = Power$$",
                                                             "The probability of a significant result if the effect truly exists.",
                                                             "<br/>","<br/>","<br/>",
                                                             "2.$$\\text{True negative rate} =  \\frac{(1-\\alpha)*(1-R)}{(1-\\alpha)*(1-R) + \\alpha*(1-R)} = 1-\\alpha$$",
                                                             "The probability of a non-significant result if the effect does not exist. It is the complement of the type 1 error \\(\\alpha\\).",
                                                             "<br/>","<br/>","<br/>",
                                                             "3.$$\\text{Positive predictive value(PPV)} = \\frac{Power*R}{Power*R + \\alpha*(1-R)}$$",
                                                             "The probability that the effect exists given a significant result of the statistical test.
                                                             As can be seen from the formula and the graph below, this probability increases with increasing power and R.",
                                                             "<br/>","<br/>","<br/>")))),
                                                 
                                                             tags$img(src = "images/PPV_0.05.png", width = "800px", height = "500px"),
                                                             
                                                 fluidRow(
                                                   column(10, HTML(paste0("<br/>","<br/>","<br/>",
                                                             "4.$$\\text{False Positive Report Probability(FPRP)} = 1-PPV = \\frac{\\alpha*(1-R)}{Power*R + \\alpha*(1-R)}$$",
                                                             "The probability that there is no effect if the statistical test is significant.
                                                             As can be seen from the formula and the graph below, this probability decreases with increasing power and R.",
                                                            
                                                             "<br/>", "<br/>", "<br/>"
                                                 )))),
                                                 
                                                 tags$img(src = "images/FPRP_0.05.png", width = "800px", height = "500px"),
                                                 
                                                 
                                                 fluidRow(
                                                   column(10, HTML(paste0("<br/>", "<br/>", "<br/>",
                                                             "A false conclusion is either making a type 1 or type 2 error. The false conclusion rate can be determined by combining the type 1 and type 2 errors.
                                                              As is illustrated in the graph below, this rate decreases with increasing power and decreasing R.
                                                              Moreover, when the prior probability of the effect is maximum, i.e., R=1, then the false conclusion rate depends only on the power of the test. 
                                                              More precisely, it is actually equal to the type 2 error \\(\\beta\\) or equivalently to 1-power. When R = 0, the false conclusion rate is equal to the 
                                                              type 1 error \\(\\alpha\\) .
                                                              For fixed values of \\(\\alpha\\) and power, a higher probability R is associated with more false conclusions. The lower the power, 
                                                              the higher the influence of R on the false conclusion rate.",
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
                                                   column(10, HTML(paste0("<br/>", "<br/>", 
                                                             "<i><strong><font color='red'>Statistical power depends on three factors</font></strong></i>",
                                                             "<br/>","<br/>","<br/>",
                                                             "To determine the power of an analysis we need firstly to specify the alternative hypothesis, \\(H_a\\), or in other words,
                                                             the effect size that we are interested in detecting. Further, and for most of analyses, power is proportional to the following:",
                                                             "<br/>", "<br/>",
                                                             
                                                             "*<i><strong><font color='red'>Effect size</font></strong></i> : the size of the effect, which can be measured as a difference in mean/median values, survival outcomes, 
                                                                          proportions or growth rates; the bigger the effect size the higher the power",
                                                             "<br/>","<br/>", "<br/>", 
                                                             "*<i><strong><font color='red'>Sample size</font></strong></i> : the number of mice included in an experiment; the higher the number of mice the higher the power.",
                                                             "<br/>","<br/>", "<br/>", 
                                                             "*<i><strong><font color='red'>Significance level(\\(\\alpha\\))</font></strong></i> : the  type 1 error of a test; the higher the \\(\\alpha\\) the higher the power, but \\(\\alpha\\) is almost always fixed at 0.05.",
                                                             "<br/>","<br/>", "<br/>",
                                                             
                                                             "Power can be calculated based on the three factors. More often, the required sample size is calculated as a function of power, 
                                                             \\(\\alpha\\) and an estimate of the effect size. Sample size can be calculated for any study design and statistical test.",
                                                             "<br/>", "<br/>")))),
                                                 
                                                 fluidRow(
                                                   column(10, HTML(paste0("<br/>","<br/>",
                                                             "<i>The correct sample size can be obtained through the following steps:</i>",
                                                             "<br/>","<br/>","<br/>",
                                                             "1. Formulate the research question, i.e., define clearly what the null hypothesis and the alternative
                                                              hypothesis of interest are",
                                                             "<br/>","<br/>",
                                                             "2. Identify the statistical test to be performed on the data from the experiment",
                                                             "<br/>","<br/>",
                                                             "3. Determine  a reasonable value for the expected effect size based on substantive knowledge, literature, previous experiments, or select the smallest effect size that is considered as clinically important",
                                                             "<br/>","<br/>",
                                                             "4. Select the desired \\(\\alpha\\) level (almost always 0.05)",
                                                             "<br/>","<br/>",
                                                             "5. Select the desired power level (mostly 0.8 or 0.9) and calculate the required sample size ",
                                                             "<br/>","<br/>","<br/>",
                                                             "Other tabs and links to external sources on this website allow you to calculate the sample size needed in
                                                             different situations.",
                                                             "<br/>","<br/>","<br/>","<br/>"
                                                 ))))
                                                 
                                                 ),
                                        tabPanel("Other Statistical issues", value = "tab3",
                                                 fluidRow(
                                                   column(10, HTML(paste0("<br/>", "<br/>", "<br/>",
                                                             "<i><strong><font color='red'>Multiple Comparisons</font></strong></i>",
                                                             "<br/>", "<br/>", "<br/>",
                                                             "When an experiment involves more than one comparison, i.e., more than one null hypothesis and
                                                              therefore more than one statistical test, the overall conclusion is that a subset of null hypotheses is rejected. 
                                                              For each test, the probability of a type 1 error is \\(\\alpha\\), but for all tests combined, the probability of a type 1 error is higher. 
                                                              This overall probability is also called the family-wise error rate or experiment-wise error rate. It is the probability that at 
                                                              least one comparison leads to a false positive finding and is calculated as:  
                                                              $$1-(1 - a)^{m}$$",
                                                             "<br/>",
                                                             
                                                             "where \\(\\alpha\\) is the significance level for an individual comparison and m is the total number of comparisons in the experiment.
                                                             For instance, an experiment with 4 groups involves 6 pairwise comparisons. The probability that at least one comparison leads 
                                                             to a false-positive conclusion is equal to ,
                                                             
                                                             $$1-(1 - 0.05)^{6} = .265$$",

                                                              "<br/>", 
                                                             
                                                             "Many statistical techniques have been developed in order to deal with this issue, i.e., to control the family-wise error rate.
                                                             The most common approach is the Bonferroni correction: the overall desired family-wise error rate (e.g., 0.05) is divided by the number
                                                             of comparisons m in the experiment to find the individual \\(\\alpha\\) level to be used for each comparison. So, if a researcher wants to conduct m=10
                                                             statistical tests with a family-wise error rate of 0.05, the significance level for each individual test should be 0.05/10=0.005. 
                                                             This means that only those comparisons with P < 0.005 are considered significant and the probability that even one of the rejected hypotheses is a
                                                             false-positive is less than 0.05.",
                                                             
                                                             "The control of the family-wise error needs to be taken into account not only in the data analysis phase of an experiment but also when sample size calculations are performed.",
                                                             "<br/>", "<br/>", "<br/>")),
                                                 
                                                 tags$img(src = "images/FWER2.png", width = "800px", height = "500px"),
                                                 
                                                 HTML(paste0("<br/>", "<br/>", "<br/>",
                                                      "More information about the Bonferroni method can be found ")),
                                                 
                                                 tags$a("here ", href = "https://www.bmj.com/content/310/6973/170.full", target = "_blank"))),
                                                 
                                                 
                                                 fluidRow(
                                                   column(10, HTML(paste0("<br/>", "<br/>", "<br/>","<br/>",
                                                                         "<i><strong><font color='red'>One-sided vs two-sided tests</font></strong></i>",
                                                                         "<br/>", "<br/>", 
                                                                         "Consider the example of a group of mice with food ad libitum and another group of similar mice with a severely restricted diet. 
                                                                            A test comparing mean weight gain in the ad libitum group(\\(\\mu_{1}\\)) with that in the restricted group(\\(\\mu_{2}\\)) 
                                                                        evaluates the null hypothesis that \\(\\mu_{1}\\)=\\(\\mu_{2}\\).
                                                                         The alternative hypothesis could be \\(\\mu_{1}\\) \\(\\neq\\) \\(\\mu_{2}\\) (two-sided) or \\(\\mu_{1}\\)>\\(\\mu_{2}\\) (one-sided) or \\(\\mu_{2}\\)>\\(\\mu_{1}\\) (one-sided).
                                                                          A two-tailed test is appropriate if a difference between groups in both directions is possible and of interest. For instance,
                                                                        the comparison of two cancer treatments can show an effect in both directions, i.e., treatment A is better than treatment B or treatment B is better than
                                                                        treatment A. A one-tailed test 
                                                                         is appropriate if a difference between groups is only possible in one direction and is practically impossible in the other direction. 
                                                                        In the above weight gain example, a one-sided alternative of \\(\\mu_{1}\\)>\\(\\mu_{2}\\) appears appropriate. In clinical research, one-sided tests are rarely appropriate.",
                                                                         
                                                                         "<br/>", "<br/>", "<br/>"
                                                                         
                                                                         
                                                   )),
                                                 
                                                HTML("More information about one and two sided tests can be found "),
                                              tags$a("here", href = "https://www.bmj.com/content/bmj/309/6949/248.full.pdf", target = "_blank"))),
                                                 
                                              HTML("<br/>","<br/>", "<br/>")
                                                 
                                                 ),
                                        
                                        tabPanel("References",
                                                 fluidRow(
                                                   column(10, HTML(paste0("<br/>","<br/>","<br/>",
                                                             "<i><strong><font color='red'>REFERENCES</font></strong></i>")),
                                                 HTML("<br/>","<br/>"),
                                                 tags$a("The p value and the base rate fallacy", href = "https://www.statisticsdonewrong.com/p-value.html", target = "_blank"),
                                                 HTML("<br/>","<br/>"),
                                                 tags$a("Scientific method: Statistical errors", href = "https://www.nature.com/news/scientific-method-statistical-errors-1.14700", target = "_blank"),
                                                 HTML("<br/>","<br/>"),
                                                 tags$a("The fickle P value generates irreproducible results", href = "https://www.nature.com/articles/nmeth.3288", target = "_blank"),
                                                 HTML("<br/>","<br/>"),
                                                 tags$a("Observed power, and what to do if your editor asks for post-hoc power analyses", href = "http://daniellakens.blogspot.com/2014/12/observed-power-and-what-to-do-if-your.html", target = "_blank"),
                                                 HTML("<br/>","<br/>"),
                                                 tags$a("Statistical considerations for preclinical studies", href = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4466166/", target = "_blank"),
                                                 HTML("<br/>","<br/>"),
                                                 tags$a("A biologist's guide to statistical thinking and analysis", href = "http://www.wormbook.org/chapters/www_statisticalanalysis/statisticalanalysis.html#sec1", target = "_blank"),
                                                 HTML("<br/>","<br/>")
                                                 
                                                 
                                        ))),

                                       tabPanel("Software for Sample size and power calculation",
                                                 HTML(paste0("<br/>","<br/>",
                                                      "There are 2 free softwares that are widely used for sample size calculations.",
                                                      "<br/>","<br/>","<br/>")),
                                                 
                                                             a("G*Power", href="http://www.gpower.hhu.de/", target = "_blank"),
                                                            HTML("<br/>","<br/>","<br/>"),
                                                             a("PS: Power and Sample Size Calculation", href="http://biostat.mc.vanderbilt.edu/wiki/Main/PowerSampleSize", target = "_blank")

                                                 ))
                                                 )
                                        ,
                          
                           tabPanel("Mean/Median Analysis",  value = "Page4",
                                    withMathJax(),
                                    tabsetPanel(
                                      
                                      tabPanel("Basic information",
                                               fluidRow(
                                                 column(10, HTML(paste0("<br/>","<br/>", "<br/>",
                                                           "T-test and Mann-Whitney-Wilcoxon tests compare the mean or median of an outcome variable between two groups while ANOVA and Kruskal-Wallis test compare 
                                                           more than two groups. T-test and ANOVA are parametric tests that rely on certain distributional assumptions to obtain reliable test results. 
                                                           Validation of these assumptions becomes impossible when group sizes are  small, which is the case with most animal experiments. 
                                                           Then, non-parametric tests should be used instead, namely Mann-Whitney-Wilcoxon or Kruskal-Wallis tests.",
                                                           "<br/>","<br/>",
                                                           "In an experiment with more than two groups
                                                           of mice, the Kruskal-Wallis test indicates if at least one group differs from others (heterogeneity). To find the groups which differ, 
                                                           pairwise comparisons can be performed using Mann-Whitney-Wilcoxon tests.
                                                           However, conducting multiple pairwise tests increases the overall probability of a false positive result (type 1 error), see ",
                                                           
                                                           tags$a("Multiple Comparisons.", onclick = "customHref('Page2');customHref('tab3');"),
                                                           
                                                           "To control this type 1 error at sufficient statistical power, a larger sample size is needed.",
                                                           "<br/>","<br/>")))),
                                               
                                               fluidRow(
                                                 column(10, HTML("More information about the Mann-Whitney-Wilcoxon test can be found "),
                                                    
                                               tags$a(href="http://sphweb.bumc.bu.edu/otlt/mph-modules/bs/bs704_nonparametric/BS704_Nonparametric4.html", "here", target = "_blank")))
                                                          
                                               ),
                                      
                                      tabPanel("Example", value = 'Example_means',
                                               HTML(paste0("<br/>","<br/>",
                                                           "<i><strong><font color='blue'>For 2 groups</font></strong></i>")),
                                               
                                               fluidRow(
                                                 column(10, HTML("<br/>",
                                                                "A scientist wants to test the hypothesis that a novel compound reduces high-density lipoprotein (HDL) cholesterol levels in a transgenic C57Bl/6J strain of mice.
                                                                Therefore, a new study is planned where mice will be  randomized to a control and a treatment group, in order to compare the average HDL cholesterol levels 
                                                                between both groups. In an experiment, the following measurements of HDL were observed:",
                                               "<br/>", "<br/>","<br/>"))),
                                               
                                               fluidRow(
                                                 column(6, DTOutput("Example_1"))),
                                               
                                               fluidRow(
                                                 column(10, HTML("<br/>", "<br/>","<br/>",
                                                    "Analysis of such  data can be carried out using the GraphPad software and following the steps described "),
                                                    
                                                    tags$a(href="https://www.graphpad.com/guides/prism/7/statistics/index.htm?stat_how_to_do_an_unpaired_t_test_w_3.htm",
                                                           "here", target = "_blank"),
                                                    
                                                    
                                                    HTML("<br/>", "<br/>","<br/>",
                                               
                                               
                                                    
                                                    "The following information from these data can be used to calculate the required sample size for a new experiment: ",
                                                    "<ol>
                                                      <li>Mean HDL in control group </li>
                                                      <li>Mean HDL in treatment group </li>
                                                      <li>Standard deviation HDL in control group </li>
                                                      <li>Standard deviation HDL in treatment group </li>
                                                      </ol>",
                                                    "<br/>", "<br/>","<br/>"
                                                    ))),
                                               
                                               
                                               HTML("<i><strong><font color='blue'>For more than 2 groups</font></strong></i>"),
                                               
                                               fluidRow(
                                                 column(10, HTML("<br/>", "A scientist wants to test the hypothesis that two novel compounds reduce high-density lipoprotein (HDL) cholesterol levels in a transgenic C57Bl/6J 
                                                    strain of mice. Therefore a new study is planned where mice will be randomized to the two treatment groups and an untreated group, in order to compare the average HDL cholesterol 
                                                    levels between the three groups. During an experiment, the following measurements of HDL are observed:",
                                                    "<br/>", "<br/>","<br/>"))),
                                               
                                               fluidRow(
                                                 column(6, DTOutput("Example_2"))),
                                               fluidRow(
                                                 column(10, HTML("<br/>", "<br/>",
                                                    "For an experiment with more than two groups,  the required sample size can be calculated using information about  the two groups 
                                                    with the smallest difference between the average  outcome, i.e., HDL. If an experiment is powered for the smallest difference, 
                                                    it is also powered to detect a larger difference, but multiple comparisons should be taken into account (see "),
                                                    
                                                    tags$a("Multiple Comparisons).", onclick = "customHref('Page2');customHref('tab3');"),
                                                    
                                                    HTML("<br/>", "<br/>",
                                                         "Usually, a control group is also included in the experiment. If the controls are only technical controls and not of direct
                                                         interest, it is not necessary to include them as a group in the
                                                         analysis and in the adjustment of the type 1 error.",
                                                          "<br/>", "<br/>","<br/>")))
                                               
                                                                                                             
                                               ,
                                               
                                               HTML("<i><strong><font color='blue'>Sample size calculation Example</font></strong></i>"),
                                               
                                               
                                               #tags$img(src = "images/PowerExample_WMW.PNG", width = "400px", height = "500px"),
                                               
                                               fluidRow(
                                                 column(10, HTML("<br/>", "<br/>",
                                                    "Calculations of the required sample size can be performed under the Sample Size Calculation tab.  
                                                    For illustration, we will use  the examples described above."
                                                    ))),
                                      
                                               
                                               fluidRow(
                                                 column(6, HTML(paste0("<br/>", "<br/>",
                                                    "<i><strong><font color='blue'>For 2 groups</font></strong></i>",
                                                    "<br/>", "<br/>",
                                                    "For this particular example, we have:",
                                                    "<ol>
                                                    <li>Mean HDL in treatment group = 267.39</li>
                                                    <li>Mean HDL in control group = 283.46</li>
                                                    <li>Standard Deviation HDL in treatment group = 14.38</li>
                                                    <li>Standard Deviation HDL in control group = 11.83</li>
                                                    </ol>",
                                                    "<br/>"
                                                    )))),
                                               
                                               tags$img(src = "images/PowerExample_WMW6.PNG", width = "700px", height = "550px"),
                                               
                                               
                                               HTML("<br/>", "<br/>",
                                                    "For such an experiment the required sample size is 14 per group.",
                                                    "<br/>", "<br/>"),
                                               
                                               fluidRow(
                                                 column(6,  HTML(paste0("<br/>",
                                                    "<i><strong><font color='blue'>For more than 2 groups</font></strong></i>",
                                                    "<br/>", "<br/>","<br/>", 
                                                    "For this particular example, we have:",
                                                    "<ol>
                                                    <li>Mean HDL in treatment group A = 267.39</li>
                                                    <li>Mean HDL in treatment group B = 256.48</li>
                                                    <li>Mean HDL in treatment group C = 283.46</li>
                                                    <li>Standard Deviation HDL in treatment group A = 14.83</li>
                                                    <li>Standard Deviation HDL in treatment group B = 9.75</li>
                                                    <li>Standard Deviation HDL in treatment group C = 11.83 </li>
                                                    </ol>",
                                                    "<br/>", "<br/>",
                                                    "The smallest difference in outcome is between groups A and B. Since three pairwise tests will be performed, an \\(\\alpha\\)
                                                    value of  0.05/3 = 0.0166 is used.",
                                                    "<br/>", "<br/>","<br/>")))),
                                              
                                               tags$img(src = "images/PowerExample_WMW5.PNG", width = "700px", height = "550px"),
                                               
                                              
                                              HTML("<br/>", "<br/>",
                                                   "For such an experiment the required sample size is 31 per group.",
                                                   "<br/>", "<br/>", "<br/>", "<br/>")
                                               
                                               ),
                                              
                                      
                                      tabPanel( "Sample Size Calculation",
                                                sidebarLayout(
                                                  sidebarPanel(
                                                    numericInput( "meanAAA2Input", "Mean of group A", 267.39 ),
                                                    numericInput( "meanBBB2Input", "Mean of group B", 256.48 ),
                                                    numericInput( "SDAAA2Input", "SD of group A", 14.38 ),
                                                    numericInput( "SDBBB2Input", "SD of group B", 9.75 ),
                                                    selectInput( "type32Input", "Type of test", c("one.sided", "two.sided"), selected = "two.sided"),
                                                    sliderInput( "Power32Input", "Power", min = 0, max = 100, value = 80, step = 1),
                                                    sliderInput( "error32Input", "Type I error", min = 0, max = 10, value = 5, step = 0.01),
                                                    #withBusyIndicatorUI(
                                                      actionButton("button2Input", "Go!", class = "btn-primary")
                                                    #)
                                                  ),
                                                  mainPanel(HTML(paste0("<br/>","<br/>", "Information on how to use this online calculator is provided in the ",
                                                                 tags$a("Example", onclick = "customHref('Page4');customHref('Example_means');"),
                                                                        ". This is a simulation based calculation and therefore it might take few seconds depending on the input.")),
                                                    HTML("<br/>", "<br/>","<br/>", 
                                                         "<strong><font color='#4d3a7d'>Means for group A and B are assumed to be </font></strong>"),
                                                    uiOutput('dynamic_valueM1',inline = T),
                                                    HTML("<strong><font color='#4d3a7d'> and</font></strong>"),
                                                    uiOutput('dynamic_valueM2',inline = T),
                                                    HTML("<strong><font color='#4d3a7d'> respectively, while standard deviations are </font></strong>"), 
                                                    uiOutput('dynamic_valueSDA2',inline = T),
                                                    HTML("<strong><font color='#4d3a7d'> for group A and </font></strong>"), 
                                                    uiOutput('dynamic_valueSDB2',inline = T),
                                                    HTML("<strong><font color='#4d3a7d'> for group B. To detect the population mean difference of M1-M2 = </font></strong>"), 
                                                    uiOutput('dynamic_valueD2',inline = T),
                                                    HTML("<strong><font color='#4d3a7d'> with an \\(\\alpha\\) of </font></strong>"), 
                                                    uiOutput('dynamic_valueA2',inline = T),
                                                    HTML("<strong><font color='#4d3a7d'>% and </font></strong>"), 
                                                    uiOutput('dynamic_valueP2',inline = T),
                                                    HTML("<strong><font color='#4d3a7d'>% power based on a Mann-Whitney-Wilcoxon test, the required sample size per group is: </font></strong>"), 
                                                    HTML("<br/>", "<br/>"),
                                                    withSpinner(verbatimTextOutput('results32')), 
                                                    br(), br()
                                                    #plotOutput("coolplot32")
                                                    )))
                                      )), 
                                        
                                      tabPanel("Survival Analysis", value = "Page5",
                                               useShinyjs(),
                                               withMathJax(),
                                               tabsetPanel(
                                                 tabPanel("Basic information",
                                                          fluidRow(
                                                            column(10,  HTML(paste0("<br/>","<br/>",
                                                                  "The log-rank test is used to test the null hypothesis that the time to an event (e.g., death or a tumor exceeding a pre-defined volume)
                                                                  between groups of mice is equal. For each mouse, the survival time is measured from the start of the experiment, for example from the time of 
                                                                  randomization, until the mouse experiences the outcome of interest  or is sacrificed or the experiment ends. For mice that do not experience 
                                                                  the outcome during the study duration, the time to the outcome event is unknown and therefore their survival time is censored.
                                                                  The log-rank test compares differences in survival time based on the hazard ratio as a measure of effect size.
                                                                  The hazard ratio equals the ratio of the median survival times in both groups for exponential survival distributions.",
                                                                  "<br/>","<br/>","<br/>",
                                                                      
                                                                      tags$a(href="https://www.ncbi.nlm.nih.gov/pmc/articles/PMC403858/", "Here", target = "_blank"),
                                                                      
                                                                      HTML(" , you can find more information about the log-rank test.",
                                                                      "<br/>","<br/>","<br/>","<br/>","<br/>")
                                                                      ))))
                                                          
                                                          ),
                                                 
                                                 tabPanel("Example", value = 'Example_surv', 
                                                          fluidRow(
                                                            column(10, HTML(paste0("<br/>","<br/>",
                                                          "To evaluate whether the chemotherapeutic agent  paclitaxel improves survival after esophageal adenocarcinoma (EAC), 
                                                          a scientist uses a peritoneal dissemination xenograft mouse model and injects human EAC cell lines intraperitoneally
                                                          into severe combined immunodeficiency (SCID) mice. Two weeks later, the mice are randomly assigned to either 
                                                          vehicle or paclitaxel (20mg/kg, 2 times a week for 2 weeks) groups. Mice are followed until death or the end of the study duration
                                                          to compare the survival distributions  between the two  groups.",
                                                          "<br/>","<br/>","<br/>")))),
                                                          fluidRow(
                                                            column(6, DTOutput("Surv_Example_1"))),
                                                          
                                                          fluidRow(
                                                            column(6, HTML("<br/>","<br/>",
                                                                           "Analysis of such data can be carried out using the GraphPad software and following the steps described "),
                                                                                  
                                                                                  tags$a(href="https://www.graphpad.com/guides/prism/7/statistics/index.htm?stat_key_concepts__survival_curves.htm",
                                                                                         "here", target = "_blank")
                                                                                  )),

                                                          fluidRow(
                                                            column(10, HTML(paste0("<br/>","<br/>",
                                                              "To calculate the sample size for a new experiment the following information is needed:",
                                                               "<br/>", "<br/>",
                                                               "<ol>
                                                               <li>	Median survival time in the control group </li>
                                                               <li>	Median survival time in the treatment group</li>
                                                               <li>	Duration of the experiment </li>
                                                               <li>	Power level </li>
                                                               <li>	Significance level </li>
                                                               </ol>",
                                                               "<br/>", "<br/>","<br/>"
                                                               ))))
                                                          
                                                          ,
                                                          
                                                          
                                                          HTML("<i><strong><font color='blue'>Sample size calculation example</font></strong></i>"),
                                                          
                                                          
                                                          fluidRow(
                                                            column(10, HTML("<br/>", "<br/>",
                                                               "Calculations of the required sample size can be performed using this website under the sample size calculation tab.
                                                               For illustration, we will use the example described above.",
                                                               "<br/>"))),
                                                          
                                                          HTML("<br/>", "<br/>",
                                                               "For this particular example, we have:",
                                                               "<ol>
                                                                   <li>Median Survival in the control group = 18.5 days</li>
                                                                   <li>Median Survival in the treatment group = 40.5 days</li>
                                                                   <li>Duration of the experiment = 60 days</li>
                                                                   </ol>",
                                                               "<br/>", "<br/>","<br/>")
                                                          
                                                          ,
                                                 
                                                 tags$img(src = "images/PowerExample_LRT3.PNG", width = "600px", height = "500px"), 
                                                 
                                                 
                                                 
                                                 
                                                 tags$img(src = "images/PowerExample_LRT6.png", width = "800px", height = "300px")
                                                 
                                                 ),
                                                 
                                                 
                                                 tabPanel("Sample Size Calculation",
                                                          sidebarLayout(
                                                            sidebarPanel(
                                                              numericInput( "MedA1Input", "Median survival of group A", 18.5 ),
                                                              numericInput( "MedB1Input", "Median survival of group B", 40.5 ),
                                                              numericInput( "FT1Input", "Experiment's duration", 60 ),
                                                              sliderInput( "Power1InputS", "Power", min = 0, max = 100, value = 80, step = 1),
                                                              sliderInput( "error1InputS", "Type I error", min = 0, max = 10, value = 5, step = 0.01)
                                                              
                                                            ),
                                                            
                                                            mainPanel(HTML(paste0("<br/>","<br/>", "Information on how to use this online calculator is provided in the ",
                                                                           tags$a("Example", onclick = "customHref('Page5');customHref('Example_surv');"))),
                                                              HTML("<br/>", "<br/>","<br/>", 
                                                                   "<strong><font color='#4d3a7d'>The median survival time of the control group is expected to be </font></strong>"),
                                                              uiOutput('sdynamic_valueA',inline = T),
                                                              HTML("<strong><font color='#4d3a7d'> days and the total duration of the experiment is assumed to be </font></strong> "),
                                                              uiOutput('sdynamic_valueFT',inline = T),
                                                              HTML("<strong><font color='#4d3a7d'> days. To detect the ratio between the median survival time of the treatment group to control group of </font></strong> "),
                                                              uiOutput('sdynamic_valueHR',inline = T),
                                                              HTML("<strong><font color='#4d3a7d'> with an \\(\\alpha\\) of </font></strong>"),
                                                              uiOutput('sdynamic_valueAlp',inline = T),
                                                              HTML("<strong><font color='#4d3a7d'>% and </font></strong>"),
                                                              uiOutput('sdynamic_valueP',inline = T),
                                                              HTML("<strong><font color='#4d3a7d'>% power based on a logrank test, the required sample size per group is:  </font></strong>"),
                                                              HTML("<br/>", "<br/>","<br/>"),
                                                              verbatimTextOutput('resultsSurv1')
                                                                                               
                                                              )))
                                                          
                                                 )),
       
       
                                      tabPanel("Proportion Analysis", value = "Page6",
                                               useShinyjs(),
                                               withMathJax(),
                                               tabsetPanel( 
                                                 tabPanel("Basic information",
                                                          fluidRow(
                                                            column(10, HTML(paste0("<br/>","<br/>","<br/>",
                                                                        "When the outcome is binary, the comparison of groups is a comparison of proportions. 
                                                                        For example, after treatment, mice can have
                                                                        a complete response (coded as 1) or not complete response (coded as 0). This situation can be analyzed with a Z-test comparing the 
                                                                        proportions of responding mice between two different treatments. But because of the small sample sizes that are usually used
                                                                        in mice experiments, it is more appropriate to use <strong>Fisher's exact test</strong>",
                                                                        "<br/>","<br/>","<br/>"
                                                                               )))), 
                                                          
                                                          
                                                          HTML("More information about the Z-test for proportions can be found "),
                                                               
                                                          
                                                          tags$a(href = "https://www.statisticshowto.datasciencecentral.com/z-test/", "here", target = "_blank"),
                                                          
                                                          
                                                          HTML("and about the Fisher's exact test "),
                                                          
                                                          
                                                          tags$a(href = "http://www.biostathandbook.com/fishers.html", "here", target = "_blank")
                                                          ),
                                                 
                                               tabPanel("Example", value = 'Example_prop',
                                                        fluidRow(
                                                          column(10, HTML(paste0("<br/>","<br/>",
                                                                                "A scientist wants to test the hypothesis that a new combination treatment leads to a higher proportion of tumor regression compared
                                                                                with a standard treatment . Mice that do not have palpable tumors for 14 days are considered responders. The proportions of responders 
                                                                                are compared between treatment groups. The data from an experiment are presented below.",
                                                                                
                                                                                "<br/>","<br/>","<br/>")))), 
                                                                                
                                                        fluidRow(
                                                          column(12, div(DTOutput("ExamplePrp_11"), style = "width: 50%"))),
                                                        
                                                        
                                                        
                                                        fluidRow(
                                                          column(6, HTML(paste0("<br/>","<br/>",
                                                                                "Analysis of such data can be carried out using the GraphPad software and following the steps described ")),
                                                                 
                                                                 
                                                                 tags$a(href="https://www.graphpad.com/guides/prism/7/statistics/index.htm?contingency_tables.htm",
                                                                        "here.", target = "_blank"))),
                                                                                
                                                        fluidRow(
                                                          column(10, HTML(paste0("<br/>","<br/>", 
                                                                                "To calculate the required sample size for a new similar experiment, 
                                                                                the proportions of responding mice in control and treatment group need to be estimated from that data.")))),
                                               
                                                        
                                               HTML("<br/>","<br/>","<br/>", "<i><strong><font color='blue'>Sample Size calculation example</font></strong></i>"
                                                    ),
                                               
                                               
                                               fluidRow(
                                                 column(10, HTML("<br/>", "<br/>","<br/>",
                                                                "Calculations of the required sample size can be performed under the Sample Size Calculation tab.
                                                                For illustration, we will use the example described above.",
                                                                "<br/>", "<br/>","<br/>"))), 
                                                                

                                               HTML("<ol>
                                                    <li>Proportion responders after standard treatment = 10%</li>
                                                    <li>Proportion responders after new combination treatment = 30% </li>
                                                    </ol>", 
                                                    "<br/>", "<br/>","<br/>")
                                                    
                                               ,
                                               
                                               tags$img(src = "images/PowerExample_PROP1.PNG", width = "350px", height = "500px"),
                                               HTML("<br/>", "<br/>"),
                                               tags$img(src = "images/PowerExample_PROP6.PNG", width = "900px", height = "300px")

                                               ),
                                               
                                              
                                               tabPanel("Sample Size Calculation",
                                                        sidebarLayout(
                                                          sidebarPanel(
                                                            numericInput( "Prop1Input", "Proportion in group A", 0.1, min = 0.05, max = 0.95, step = 0.05 ),
                                                            numericInput( "Prop2Input", "Proportion in group B", 0.3, min = 0.05, max = 0.95, step = 0.05 ),
                                                            sliderInput( "Power1InputP", "Power", min = 0, max = 100, value = 80, step = 1),
                                                            sliderInput( "error1InputP", "Type 1 error", min = 0, max = 10, value = 5, step = 0.1),
                                                            actionButton("buttonPrInput", "Go!", class = "btn-primary")
                                                          ),
                                                          
                                                          mainPanel(HTML(paste0("<br/>","<br/>", "Information on how to use this online calculator is provided in the ",
                                                                         tags$a("Example", onclick = "customHref('Page6');customHref('Example_prop');"),
                                                                         ". This is a simulation based calculation and therefore it might take a few seconds depending on the input.")),
                                                            
                                                            HTML("<br/>", "<br/>","<br/>", 
                                                                         "<strong><font color='#4d3a7d'>The proportions in two groups are expected to be </font></strong>"),
                                                                    uiOutput('Pdynamic_valueP1',inline = T),
                                                                    HTML("<strong><font color='#4d3a7d'> and </font></strong> "),
                                                                    uiOutput('Pdynamic_valueP2',inline = T),
                                                                    HTML("<strong><font color='#4d3a7d'> . To detect the difference between these proportions of </font></strong> "),
                                                                    uiOutput('Pdynamic_valueDP1',inline = T),
                                                                    HTML("<strong><font color='#4d3a7d'> with an \\(\\alpha\\) of </font></strong>"),
                                                                    uiOutput('Pdynamic_valueALP',inline = T),
                                                                    HTML("<strong><font color='#4d3a7d'>% and </font></strong>"),
                                                                    uiOutput('Pdynamic_valuePOW',inline = T),
                                                                    HTML("<strong><font color='#4d3a7d'>% power based on </font></strong>"),
                                                            uiOutput('Pdynamic_valueTEST', inline =T),   
                                                                    HTML("<strong><font color='#4d3a7d'>, the required sample size per group is: </font></strong>"),
                                                                    HTML("<br/>", "<br/>"),
                                                                    withSpinner(verbatimTextOutput('resultsProp'))
                                                            

                                                        ))))),    
                                               
            
                                      tabPanel("Growth Curve Analysis", value = "Page7",
                                               useShinyjs(),
                                               withMathJax(),
                                               tabsetPanel( 
                                                 tabPanel("Basic information",
                                                          fluidRow(
                                                            column(10, HTML(paste0("<br/>","<br/>","<br/>",
                                                                    "In this type of experiments, tumor cells are injected into mice and the volume of the tumor is measured every 2-3 days.
                                                                    When the tumor reaches a certain volume, e.g. \\(200mm^3\\), mice are randomized into treatment and control groups.
                                                                    Tumor volume is regularly measured until mice die or are sacrificed. The objective is to compare the  tumor growth
                                                                    between groups. Often, average tumor size is compared at arbitrary time points using a T-test or ANOVA.This approach is inappropriate. A proper
                                                                    test if tumor growth rates differ between groups should be based on a linear regression which uses all measured tumor volumes for each
                                                                    mouse and accounts for the correlation between observations from the same mouse. If the tumor volume measure does not follow a normal distribution,
                                                                    its transformed values should be used as the outcome in a linear regression. The most common transformation applied in such studies is the logarithmic transformation.", 
                                                                    "<br/>","<br/>","<br/>"))))),
                                                          
                                                 tabPanel("Example", value = 'Example_growth',
                                                          fluidRow(
                                                            column(10, HTML(paste0("<br/>","<br/>",
                                                                      "A scientist wants to test the hypothesis that a new treatment is able to suppress the tumor growth.  
                                                                      An experiment is conducted where tumor cells are injected into mice and volume of the  tumor is measured every 2-3 days.
                                                                      When tumors reach a pre-defined volume of \\(1500mm^3\\),  mice are randomized to receive either the standard treatment or the
                                                                      new treatment. Tumor volume is measured until mice die or are sacrificed in order to compare the rate of tumor growth between 
                                                                      treatment groups.",
                                                                      "<br/>","<br/>",
                                                                      "The data from an experiment are presented below. The NA denotes an unmeasured volume for mice 
                                                                      that died or were sacrificed before the end of the study duration.",
                                                                      
                                                                      "<br/>", "<br/>","<br/>")))),
                                                          
                                                          fluidRow(
                                                            column(10, HTML(paste0(DTOutput("ExampleGC_1", width = "80%"),
                                                                               "<br/>", "<br/>","<br/>"))))
                                                          ,
                                                          
                                                          fluidRow(
                                                            column(10, HTML(paste0(
                                                                      "Such data cannot be analyzed with Graphpad software, but SPSS software can be used instead.
                                                                      Installation of SPSS software can be requested from the IT department free of charge for NKI employees.",
                                                                      "<br/>", "<br/>",
                                                                      "Data in a long format should be loaded in SPSS. In such data format, each row corresponds
                                                                       to one measurement per mouse and there are as many rows per mouse as there are volume measurements available.",
                                                                      "<br/>", "<br/>","<br/>")))),
                                                                      
                                                            
                                                           fluidRow(
                                                              column(10, DTOutput("ExampleGC_2", width = "80%")))
                                                          ,
                                                          
                                                          
                                                          fluidRow(
                                                              column(10, HTML(paste0("<br/>","<br/>","<br/>",
                                                                    "1. Loading data into SPSS: File -> Open -> Data",
                                                                    "<br/>", "<br/>","<br/>",
                                                                    "2. Creating a binary indicator variable ‘TreatmentGR’ with values 0 for control group and 1 for treatment group:",
                                                                    "<br/>", "<br/>","<br/>",
                                                                    "Transform -> Compute Variable -> Target Variable: TreatmentGR ; Numeric Expression : 0
                                                                    -> If -> Include if case satisfies condition : Treatment = 'Control';", 
                                                                    "<br/>", "<br/>","<br/>",
                                                                    "Transform -> Compute Variable -> Target Variable: TreatmentGR ; Numeric Expression : 1
                                                                    -> If -> Include if case satisfies condition : Treatment = 'anti-PD1'",
                                                                    "<br/>", "<br/>","<br/>",
                                                                    "Screenshots below show how to do it for the control group.",
                                                                    
                                                                    "<br/>", "<br/>","<br/>",
                                                                    
                                                                    tags$img(src = "images/GLS_Example2.png", width = "800px", height = "400px"),
                                                                    
                                                                    "<br/>", "<br/>","<br/>",
                                                                    
                                                                    tags$img(src = "images/GLS_Example20.PNG", width = "800px", height = "400px"),
                                                                    
                                                                    "<br/>", "<br/>","<br/>",
                                                                    
                                                                    tags$img(src = "images/GLS_Example21.PNG", width = "800px", height = "400px"),
                                                                    
                                                                    "<br/>", "<br/>","<br/>",
                                                                    
                                                                    tags$img(src = "images/GLS_Example200.PNG", width = "800px", height = "400px"),
                                                                    
                                                                    "<br/>","<br/>","<br/>",
                                                                    
                                                                    
                                                                    "3. Creating a variable ‘Days_TreatmentGr’ for the interaction between time (variable 'days') and treatment (variable 'TreatmentGR'):",
                                                                    "<br/>", "<br/>","<br/>",
                                                                     "Transform  ->  Compute Variable -> Target Variable: Days_TreatmentGr; Numeric Expression: Days*TreatmentGr",
                                                                    
                                                                    "<br/>","<br/>","<br/>",
                                                                    
                                                                    tags$img(src = "images/GLS_Example24.png", width = "800px", height = "400px"),
                                                                    
                                                                    "<br/>","<br/>","<br/>","<br/>",
                                                                    
                                                                    tags$img(src = "images/GLS_Example23.PNG", width = "800px", height = "400px"),
                                                                    
                                                                    "<br/>","<br/>","<br/>",
                                                                    
                                                                    "4. Performing a linear regression that accounts for the correlation between observations from the same mouse:",
                                                                    "<br/>","<br/>","<br/>",
                                                                    
                                                                    "Analyze -> Mixed Models -> Linear... -> Subjects: ID; Repeated: days; Repeated Covariance Type: AR(1)
                                                                     ->  Continue -> Dependent Variable : log_volume; Covariate(s): days, Days_TreatmentGr
                                                                     -> Fixed -> Model: days, Days_TreatmentGr using Main Effects option  -> Continue -> Statistics -> 
                                                                    Model Statistics: Parameter estimates for fixed effects -> Continue. ",
                                                                    "<br/>","<br/>","<br/>",
                                                                    
                                                                    tags$img(src = "images/GLS_Example25.png", width = "800px", height = "400px"),
                                                                    
                                                                  
                                                                    "<br/>","<br/>","<br/>","<br/>","<br/>","<br/>",
                                                                    
                                                                    tags$img(src = "images/GLS_Example26.PNG", width = "850px", height = "450px"),
                                                                  
                                                                    
                                                                    "<br/>","<br/>","<br/>","<br/>","<br/>","<br/>",
                                                                    
                                                                    tags$img(src = "images/GLS_Example27.PNG", width = "800px", height = "400px"),
                                                                    
                                                                    "<br/>","<br/>","<br/>","<br/>","<br/>","<br/>",
                                                                    
                                                                    tags$img(src = "images/GLS_Example28.PNG", width = "800px", height = "400px"),
                                                                    
                                                                    "<br/>","<br/>","<br/>","<br/>","<br/>","<br/>",
                                                                    
                                                                    tags$img(src = "images/GLS_Example29.PNG", width = "800px", height = "400px"),
                                                                    
                                                                    "<br/>","<br/>","<br/>",
                                                                    
                                                                    "5.	Interpreting results of the analysis:",
                                                                    
                                                                    "<br/>","<br/>","<br/>",
                                                                    
                                                                    tags$img(src = "images/GLS_Example30.PNG", width = "900px", height = "500px"),
                                                                   
                                                                    "<br/>","<br/>","<br/>",
                                                                    
                                                                    "Slope of the tumor growth in the control group = 0.226 (estimate for variable ‘days’): in the control group tumor volume on the log scale increases each day by 0.226 
                                                                    Difference in the slope of the tumor growth between the control and treatment groups = -0.101 (estimate for variable ‘Days_TreatmentGR’): in the treatment group tumor
                                                                    volume on the log scale increases each day by 0.124 (0.226-0.101 = 0.124)
                                                                    Correlation between the closest two measurements within mice = 0.698 (estimate for ‘AR(1) rho’) 
                                                                    Residual variance = 0.692 (estimate for ‘AR1 diagonal’)",
                                                                    
                                                                    "<br/>","<br/>",
                                                                    
                                                                    "6.	Performing a linear regression that does not account for the correlation between observations from the same mouse:",
                                                                    "<br/>","<br/>","<br/>",
                                                                    
                                                                    "Analyze -> Mixed Models -> Linear... -> Subjects: ID; Repeated: days; Repeated Covariance Type: Scale Identity
                                                                     ->  Continue -> Dependent Variable : log_volume; Covariate(s): days, Days_TreatmentGr
                                                                     -> Fixed -> Model: days, Days_TreatmentGr using Main Effects option  -> Continue -> Statistics -> 
                                                                    Model Statistics: Parameter estimates for fixed effects -> Continue. ",
                                                                    "<br/>","<br/>","<br/>"
                                                                    
                                                          ))))
                                                          ,
                                                          
                                                          fluidRow(
                                                            column(10, HTML(paste0("More details on how to conduct <i>mixed-models analysis</i> in SPSS can be found ",
                                                          tags$a(href="https://stats.idre.ucla.edu/spss/seminars/spss-mixed-command/", "here", target = "_blank")
                                                          
                                                          )))),
                                                          
                                                          
                                                          HTML("<br/>","<br/>","<br/>", "<i><strong><font color='blue'>Sample size calculation example</font></strong></i>",
                                                               "<br/>"),
                                                          
                                                          
                                                          fluidRow(
                                                            column(10, HTML(paste0("<br/>",
                                                                                  
                                                           "The sample size required for this kind of experiment is computed from the sample size required for a simple linear regression  multiplied 
                                                           by a factor called <strong>design effect</strong>. This factor corrects for the correlation between measurements from the same mouse. The design effect
                                                           is defined as the ratio between the variance of the interaction term from a linear model that accounts for correlated measurements and
                                                           the variance of the interaction term from a linear model that does not account for correlated measurements.",
                                                           
                                                           "<br/>","<br/>",
                                                           
                                                           
                                                            "Calculations of the required sample size can be performed under the Sample Size Calculation tab.
                                                             For illustration, we will use the example described above. As was already mentioned, this analysis should be 
                                                             done using the logarithmic transformation of the tumor volume as the response.
                                                             For that reason the sample size calculation should be done using values on this scale and not the raw ones. Further, 
                                                             the number of measurements that will be used for this calculation should be realistic. It should reflect the number
                                                             of measurements that is expected to be taken for each mouse. If many mice do not reach that number because of early death or
                                                             sacrifice, then the sample size calculation would not be correct anymore.")))),
                                                     
                                                          
                                                          HTML("<br/>", "<br/>",
                                                               "<ol>
                                                                   <li>Difference in tumor growth rates   = -0.101 </li>
                                                                   <li>Variance of the residuals from the model that does not account for correlation = 0.636 </li>
                                                                   <li>Standard error of the interaction effect from the model that does not account for correlation   = 0.0148 </li>
                                                                   <li>Standard error of the interaction effect from the model that accounts for correlation  = 0.0249 </li>
                                                                   <li>Average number of measurements per mouse = 8 </li>
                                                                   <li>Time difference between two measurements = 2 </li>
                                                                   </ol>",
                                                               "<br/>"),
                                                          
                                                          
                                                          HTML(paste0("The number of measurements used for the calculation is the expected number of observations per mouse.",
                                                                      "<br/>", "<br/>",
                                                                      "Required number of mice for growth curve data is obtained by estimating required number of observations for a model that does 
                                                                       not account for correlated measurements. Then the estimate is multiplied by a design factor (= variance of the interaction effect
                                                                       from the model that accounts for correlation / variance of the interaction effect from the model that does not account for correlation) 
                                                                       and divided by the expected number of observations per mouse.",
                                                                      "<br/>", "<br/>"))
                                                                      
                                                               
                                                          ,
                                                          tags$img(src = "images/PowerExample_GCA3.PNG", width = "500px", height = "900px"),
                                                          
                                                          tags$img(src = "images/PowerExample_GCA5.png", width = "800px", height = "300px"),
                                                          HTML("<br/>", "<br/>","<br/>","<br/>", "<br/>","<br/>")
                                                          
                                                 ),
                                                 
                                                 tabPanel("Sample Size Calculation",
                                                          sidebarLayout(
                                                            sidebarPanel(
                                                              numericInput( "Effect", "Difference in tumor growth rates", -0.101, step = 0.001 ),
                                                              numericInput( "VARres", "Variance of the residuals", min = 0, 0.636, step = 0.001 ),
                                                              numericInput( "SE_LR", "Standard error of the interaction effect - Simple model", min = 0, 0.0148, step = 0.001 ),
                                                              numericInput( "SE_AR", "Standard error of the interaction effect - AR(1) model", min = 0, 0.0249, step = 0.001 ),
                                                              numericInput( "NoMeasurements", "Average number of measurements per mouse", min = 1, 8, step = 1 ),
                                                              numericInput( "Space", "Time difference between measurements", 2, min = 1, step = 1),
                                                              sliderInput( "PowerInputLR", "Power", min = 0, max = 100, value = 80, step = 1),
                                                              sliderInput( "errorInputLR", "Type I error", min = 0, max = 10, value = 5, step = 0.1)
                                                              
                                                            ),
                                                            
                                                            mainPanel(HTML(paste0("<br/>","<br/>", "Information on how to use this online calculator is provided in the ",
                                                                         tags$a("Example", onclick = "customHref('Page7');customHref('Example_growth');"))),
                                                                         
                                                              HTML("<br/>", "<br/>","<br/>", 
                                                                   "<strong><font color='#4d3a7d'>The average number of measurements per mouse is </font></strong>"),
                                                              uiOutput('dynamic_MeasurementsLR',inline = T),
                                                              HTML("<strong><font color='#4d3a7d'> and measurements are obtained approximately every </font></strong> "),
                                                              uiOutput('dynamic_DistaLR',inline = T),
                                                              HTML("<strong><font color='#4d3a7d'> time units. Standard errors of the interaction effect obtained with a linear regression that does and does not account for the correlated measurements are </font></strong> "),
                                                              uiOutput('dynamic_valueSDX2LR',inline = T),
                                                              HTML("<strong><font color='#4d3a7d'>  and  </font></strong>"),
                                                              uiOutput('dynamic_valueSDX1LR',inline = T),
                                                              HTML("<strong><font color='#4d3a7d'> , respectively. The ratio is the design effect, i.e., </font></strong>"),
                                                              uiOutput('dynamic_valueDE',inline = T),
                                                              HTML("<strong><font color='#4d3a7d'> . To detect a difference in growth rates of </font></strong>"),
                                                              uiOutput('dynamic_valueEffLR',inline = T),
                                                              HTML("<strong><font color='#4d3a7d'> with an \\(\\alpha\\) of </font></strong>"),
                                                              uiOutput('dynamic_valueALLR',inline = T),                                               
                                                              HTML("<strong><font color='#4d3a7d'>% and </font></strong>"),
                                                              uiOutput('dynamic_valuePOWLR',inline = T),                                                             
                                                              HTML("<strong><font color='#4d3a7d'>% power, the required sample size per group is: </font></strong>",
                                                                   "<br/>", "<br/>","<br/>"),

                                                              verbatimTextOutput('resultsLR'),
                                                              
                                                              br(), br()
                                                              #plotOutput("Survcoolplot33")
                                                              ))
                                                          
                                                          
                                                 )   # TabPanel
                                                 
                                                 
                                              #   tabPanel("Analysis", value = "AnalysisGCA",
                                                          
                                                  #       radioButtons(
                                                 #           "fileType_Input",
                                                #            label = h4("Choose File type"),
                                               #             choices = list(".csv/txt" = 1, ".xlsx" = 2),
                                              #              selected = 1,
                                             #               inline = TRUE
                                            #              ),
                                                          
                                                        #  fileInput(
                                                       #     'file1',
                                                      #      h4('Upload Itemns List'),
                                                     #       accept = c(
                                                    #          'text/csv',
                                                         #     'text/comma-seperated-values,text/plain',
                                                        #      '.csv',
                                                       #       '.xlsx')
                                                      #    ),
                                                            
                                                     #     DT::dataTableOutput("sample_table"),
                                                          
                                                    #      HTML("<br/>", "<br/>", "<br/>",
                                                   #            "Analysis-Results",
                                                  #             "<br/>", "<br/>", "<br/>",
                                                 #              "Output of the model with Independence structure",
                                                #               "<br/>", "<br/>"),
                                                          
                                                          # These results are with an arbitrary reference level....
                                                          
                                                        #  verbatimTextOutput('resultsGC2'),
                                                       #   
                                                      #    HTML("<br/>", "<br/>", 
                                                     #          "Output of the model with AR(1) structure",
                                                    #           "<br/>", "<br/>"),
                                                   #       
                                                  #        verbatimTextOutput('resultsGC_AR2'),
                                                 #
                                                      #    HTML("<br/>", "<br/>", 
                                                     #          "The correlation between measurements that are 1 day apart is: "),
                                                    #           
                                                   #       textOutput('resultsGC_AR2_COR', inline = T),
                                                  #        
                                                 #         
                                                #          HTML("<br/>", "<br/>",
                                               #                "Below are presented the necessary values for doing a sample size calculation.",
                                              #                 "<br/>", "<br/>", "<br/>", "<br/>")
                                                          
                                                          
                                                    #  ) # TabPanel
                                                 
                                                 
                                                 )), inverse = T, collapsible = T
                           
                                               ))
  


)
  

