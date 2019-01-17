
server <- function(input, output){
  
  #output$results0 <- eventReactive(input$Gobutton,{
   # withBusyIndicatorServer("Gobutton", {
    #  Sys.sleep(1)
     # if(input$type2Input == '1'){
      #  if(input$type3Input == "Categorical"){
       #   if(input$type4Input == "1"){
        #    testis <- "One sample t-test"
         # }else if(input$type4Input == "2"){
          #  if(input$independentInput == "No"){
           #   testis <- "Two sample t-test"
    #        }else{
     #         testis <- "Paired t-test"
      #      }
       #   }else{
        #    testis <- "One-way ANOVA"
         # }}else{
          #  testis <- "Regression"
            
  #        }}else if(input$type5Input == "Continuous"){
   #         if(input$type3Input == "Continuous"){
    #          testis <- "Multiple Regresion"
              
     #       }else{
      #        testis <- "ANCOVA"  
       #     }
        #  }else if(input$type5Input == "Categorical"){
      #      if(input$type3Input == "Continuous"){
       #       testis <- "ANCOVA"  
        #    }else{
         #     testis <- "Two-way ANOVA"
              
          #  }}
    #})})
  
  
  
#  var1 <- reactive({
 #   ifelse(input$SDAInput == input$SDBInput, "equal", "unequal")
#  })
  
 # sample_size <- reactive({
  #  ceiling( n.ttest(mean.diff = abs(input$meanAInput - input$meanBInput), power = input$PowerInput,
   #                  alpha = input$errorInput, k=1, sd1 = input$SDAInput, sd2 = input$SDBInput, variance = var1())$`Sample size group 1`)
  #})
  #output$results <- renderPrint({sample_size()})
  
  #output$coolplot <- renderPlot({
   # qplot(1:(2*sample_size()), 
    #      power.t.test(delta = abs(input$meanAInput - input$meanBInput), sd = input$SDAInput, n = 1:(2*sample_size())
     #                  ,alternative = input$typeInput, sig.level = input$errorInput, type = "two.sample")$power,
      #    ylab = "Power", xlab = "Sample size per group") + 
      #geom_line(yintercept = 0.8, col = "red") +
  #    scale_y_continuous(breaks = seq(0, 1, 0.1)) +
   #   scale_x_continuous(breaks = seq(1, (2*sample_size()), 1)) +
    #  geom_hline(yintercept = input$PowerInput, col = "red") + 
     # geom_vline(xintercept = sample_size(), col = "red") +
      #ggtitle("Sample size and Power")
    
#  })
  
 # output$tutorial <- renderUI({ 
    
  #  HTML(paste0("<br/>",
   #             "We use the unpaired two-sample t-test when we want to compare the means from two independent samples.
    #            For instance, when we want to compare 2 experimental groups based on a quantitative measurement, i.e. weight.",
     #           "<br/>","<br/>", "<br/>",
    #            "<strong> Example:</strong>","<br/>",
     #           "A scientist wanted to test the hypothesis that a novel compound had a beneficial effect on reducing high-density lipoprotein (HDL)
      #          cholesterol levels in a transgenic C57Bl/6J strain of mice. Therefore she randomized half of the mice in the control 
       #         and the other half in the treatment group respectively, in order to compare the HDL cholesterol level means from the 2 groups.",
        #        "<br/>","<br/>","<br/>","<br/>","<br/>",
         #       'In order to make this comparison, we will use the t-statistic. It is calculated as below:'))
    
    
#  })
  
#  output$formula <- renderUI({
 #   withMathJax("$$t=\\frac{\\bar{x_1}-\\bar{x_2}}{S_{p}\\sqrt{\\frac{1}{n_{1}}+\\frac{1}{n_{2}}}}$$")
    
  #})
  
#  output$tutorial2 <- renderUI({ 
 #   HTML(paste0("In the above formula we assumed that the 2 samples have approximately the same variance and for that reason we pooled
  #              them in order to derive S, which we use later in the t-test. The t-statistic follows a Student's t-distribution with
   #             degrees of freedom( df ). Hence, after we have calculated the t-statistic we have to compare it with the critical value of its distribution,
    #            and if it is greater  than the critical value we reject the null hypothesis that the sample means are equal. In order to do that, 
     #           first we have to specify the significance-level of our test with most usual values being . 
      #          A very important information is also the form of our test, if it is one-tailed or two-tailed. 
       #         Then, we can look up on the t-tables and find the critical value based on these information.", "<br/>",
        #        "<strong> Assumptions:</strong>","<br/>",
                
#                "Two-sample t-test comes along with some important conditions that must be met, and they are listed below.","<br/>",
                
 #               "1)	The response variable, the one that we would like to test, must be continuous, such as blood pressure, height, weight etc.", "<br/>", 
  #              "2)	The observations must be independent","<br/>" ,
  #              "3)	Your independent variable should consist of two categorical, independent groups. For instance, when comparing two treatments, where the treatment variable is the independent variable with two levels, i.e. control and treatment A.","<br/>",
  #             "4)	There should be no significant outliers in the data. Outliers are values that are unusual comparing to the rest of the data. For instance, if the response is the IQ score and the mean in our data is 105, if one subject has an IQ score of 160, this is considered an outlier and must seriously affect the results of the t-test.","<br/>",
  #             "5)	The response variable should be approximately normally distributed for each group. Minor deviations from the normality are expected and will not affect the results severely. However, if this violation is bigger, then action must be taken before continuing with the t-test analysis.","<br/>",
  #             "6)	There needs to be homogeneity of variances. That is, the variances in the two groups must be approximately equal.", "<br/>",
  #             
  #             
  #             "It is very important to understand that these
  #             assumptions are very crucial and must be checked prior to the analysis, because if they are not met, 
  #             the results might not be valid. One possible to way to correct for the departure from the normality assumption, 
  #             is to apply a transformation in our response variable. The most often used transformations are the log, the square root, 
  #             or square of the response. Usually, the transformed data will meet the normality assumption, but it is also very possible
  #             that it will not. In that case, we have to switch to a non-parametric equivalent of t-test, with the most widely used being
  #             the Wilcoxon-Mann-Whitney test( see section Wilcoxon-Mann-Whitney test ). Another very important reason to opt for the non-parametric 
  #             test is the size of the sample and  as a rule of thumb we use a cut off value of n=20. That is because when the sample size is small,
  #             it is not possible to validate these assumptions. Finally, maybe the most crucial condition for a 
  #             two-sample t-test is the homogeneity of variances. If the variances are not approximately equal in the two groups, then the 
  #             non-parametric equivalent is the Welch's t-test. This test is also used when the sample sizes in the two groups are not equal. It is
  #             exactly the same as the t-test formula above, but now the denomiantor is different since we have to account for both variances. 
  #             Unlike the classic Student’s t-test, Welch t-test formula involves the variance of each of the two groups (\\S_1 and \\S_2) being compared.
  #             In other words, it does not use the pooled varianceS. In that case the formula becomes:")
  # )
  #})
  
  #output$formula1 <- renderUI({
  # withMathJax("$$t=\\frac{\\bar{x_1}-\\bar{x_2}}{\\sqrt{\\frac{S^2_1}{n_{1}}+\\frac{S^2_2}{n_{2}}}}$$")
    
  #  })
  
  #output$tutorial22 <- renderUI({ 
    
  #  HTML(paste0("<br/>", "<br/>", "<br/>", "<br/>",   
  #             "The degrees of freedom of Welch T-test are estimated as follows:"))
  #})
  
  
  #output$formula11 <- renderUI({
  # withMathJax("$$df=\\frac{(\\frac{S^2_1}{n_1} + \\frac{S^2_2}{n_2})^2}{\\frac{(\\frac{S^2_1}{n_1})^2}{n_1-1} + \\frac{(\\frac{S^2_2}{n_2})^2}{n_2-1}}$$")
    
  #})
  
  #output$tutorial223 <- renderUI({ 
    
  #  HTML(paste0("<br/>", "<br/>", "<br/>", "<br/>",   
  #             "Where \\(n_1\\) and \\(n_2\\) refer to the sample sizes of the 2 groups respectively. Usually, the results of the classical t-test and the Welch t-test are very similar unless both the group sizes and the standard deviations
  #             are very different."))
    
  #})
  
  #output$tutorial3 <- renderUI({ 
    
  # HTML(paste0("<br/>",
  #             "We use the paired two-sample t-test when we want to compare the means from two independent samples.
  #             For instance, when we want to compare 2 experimental groups based on a quantitative measurement, i.e. weight.",
  #             "<br/>","<br/>", "<br/>",
  #             "<strong> Example:</strong>","<br/>",
  #             "A scientist wanted to test the hypothesis that a novel compound had a beneficial effect on reducing high-density lipoprotein (HDL)
  #             cholesterol levels in a transgenic C57Bl/6J strain of mice. Therefore she randomized half of the mice in the control 
  #             and the other half in the treatment group respectively, in order to compare the HDL cholesterol level means from the 2 groups.",
  #             "<br/>","<br/>","<br/>","<br/>","<br/>",
  #             'In order to make this comparison, we will use the t-statistic. It is calculated as below:'))
    
    
  #})
  
  

  
  output$formulaSurv <- renderUI({
    withMathJax("$$t=\\frac{\\bar{x_1}-\\bar{x_2}}{S_{p}\\sqrt{\\frac{1}{n_{1}}+\\frac{1}{n_{2}}}}$$")
    
  })
  
  
  #sample_size2 <- reactive({
  #  ceiling( power.t.test(delta = abs(input$meanAAInput - input$meanBBInput)/input$SDAAInput, power = input$Power2Input,
   #                       type = "paired", sig.level = input$error2Input,alternative = input$type2)$n)
    
  #})
  #output$results2 <- renderPrint({sample_size2()})
  
  #output$coolplot2 <- renderPlot({
  # qplot(1:(2*sample_size2()), 
  #       power.t.test(delta = abs(input$meanAAInput - input$meanBBInput)/input$SDAAInput, n = 1:(2*sample_size2())
  #                    ,alternative = input$type2, sig.level = input$error2Input, type = "paired")$power,
  #       ylab = "Power", xlab = "Total Sample size(No of pairs)") + 
  #   scale_y_continuous(breaks = seq(0, 1, 0.1)) +
  #   scale_x_continuous(breaks = seq(1, (2*sample_size2()), 1)) +
  #   geom_hline(yintercept = input$Power2Input, col = "red") + 
  #   geom_vline(xintercept = sample_size2(), col = "red") +
  #   ggtitle("Sample size and Power")
    
  #})
  
  output$Example_1 <- renderDT(matrix(c(1:10, 288.67, 286.39, 295.82, 282.82, 264.21, 265.69, 266.26, 291.32, 260.67, 253.03,
                                                                  rep("Control", 5), rep("Treatment", 5)), ncol = 3),
                                                         colnames = c("ID", "HDL level", "Group"),
                                                         selection = 'none',
                                                         options = list(lengthMenu = c(5, 5, 5), pageLength = 10,
                                                                        dom = 't',lengthChange = FALSE))
  
  
  output$Example_2 <- renderDT(matrix(c(1:15, 265.69, 266.26, 291.32, 260.67, 253.03,
                                                                  257.50, 240.63, 263.98, 264.81, 255.51, 288.67, 286.39, 295.82, 282.82,264.21,
                                                                  rep("Treatment A", 5), rep("Treatment B", 5), rep("Control", 5)), ncol = 3),
                                                         colnames = c("ID", "HDL level", "Group"),
                                                         selection = 'none',
                                                         options = list(lengthMenu = c(5, 5, 5), pageLength = 15,
                                                                        dom = 't', lengthChange = FALSE))
  
  
  powers <- eventReactive(input$buttonInput,{ 
    withBusyIndicatorServer("buttonInput", {
      
      sapply(seq(50), function(x){
        tt <- replicate(500, wilcox.test(rnorm(x, input$meanAAAInput, input$SDAAAInput), rnorm(x, input$meanBBBInput, input$SDBBBInput))$p.value)
        sum(tt < 0.05) / 500})
    })
  })
  
  sample_size3 <- eventReactive(
    input$buttonInput,{
      withBusyIndicatorServer("buttonInput", {
        
        which(powers() == min(powers()[powers() >= input$Power3Input]))
      })
    })
  
  output$results3 <- renderPrint({
    if(input$buttonInput){
      isolate({sample_size3()})
    }
  })
  
  pocoolplot3 <- eventReactive(input$buttonInput,{
    withBusyIndicatorServer("buttonInput", {
      
      qplot (seq(50), powers(),
             scale_y_continuous(breaks = seq(0, 1, 0.1)) +
               scale_x_continuous(breaks = seq(1, 50), 1)) +
        geom_hline(yintercept = input$Power3Input, col = "red") + 
        geom_vline(xintercept = sample_size3(), col = "red") +
        ggtitle("Sample size and Power") +
        labs(x = "Sample size", y = "Power")  
    })
  })
  
  output$coolplot3 <- renderPlot({
    pocoolplot3()    
    
  })
  
  
  powers2 <- eventReactive(input$button2Input,{ 
    withBusyIndicatorServer("button2Input", {
      
      sapply(seq(100), function(x){
        tt <- replicate(500, wilcox.test(rnorm(x, input$meanAAA2Input, input$SDAAA2Input), rnorm(x, input$meanBBB2Input, input$SDBBB2Input))$p.value)
        sum(tt < (input$error32Input / 100)) / 500})
    })
  })
  
  sample_size32 <- eventReactive(
    input$button2Input,{
      withBusyIndicatorServer("buttonInput", {
        
        which(powers2() == min(powers2()[powers2() >= (input$Power32Input)/100]))
      })
    })
  
  output$results32 <- renderPrint({
    if(input$button2Input){
      isolate({sample_size32()})
    }
  })
  
  pocoolplot32 <- eventReactive(input$button2Input,{
    withBusyIndicatorServer("buttonInput", {
      
      qplot(seq(100), powers2()) +
        coord_cartesian(ylim=c(0, 1))+     
        geom_hline(yintercept = input$Power32Input/100, col = "red") + 
        geom_vline(xintercept = sample_size32(), col = "red") +
        ggtitle("Sample size and Power") +
        labs(x = "Sample size per group", y = "Power")  
    })
  })
  
  output$coolplot32 <- renderPlot({
    pocoolplot32()    
    
  })
  
  
  output$dynamic_valueD<- renderPrint({
    cat(input$meanAAAInput - input$meanBBBInput)})

  
  output$dynamic_valueSDA <- renderPrint({
    cat(input$SDAAAInput)})
  
  output$dynamic_valueSDB <- renderPrint({
    cat(input$SDBBBInput)})
  
  output$dynamic_valueP <- renderPrint({
    cat(input$Power3Input)})
  
  output$dynamic_valueA <- renderPrint({
    cat(input$error3Input)})
  
  
  
  output$dynamic_valueD2 <- renderPrint({
    cat(input$meanAAA2Input - input$meanBBB2Input)})
  
  
  output$dynamic_valueM1 <- renderPrint({
    cat(input$meanAAA2Input)})
  
  
  
  output$dynamic_valueM2 <- renderPrint({
    cat(input$meanBBB2Input)})
  
  output$dynamic_valueSDA2 <- renderPrint({
    cat(input$SDAAA2Input)})
  
  output$dynamic_valueSDB2 <- renderPrint({
    cat(input$SDBBB2Input)})
  
  output$dynamic_valueP2 <- renderPrint({
    cat(input$Power32Input)})
  
  output$dynamic_valueA2 <- renderPrint({
    cat(input$error32Input)})
  
  
  
  
#sample_sizeSurv <- reactive({
  
    
#(qnorm(input$errorInputS/2) + qnorm(1-input$PowerInputS))^2 * (log(input$MedBInput/input$MedAInput)^(-2))*
#    (2*(1 - ((1 - (exp(-log(2)/((input$MedAInput + input$MedBInput) / 2)))) / (log(2)/((input$MedAInput + input$MedBInput) / 2))) *
#          exp(-log(2)*input$FTInput/((input$MedAInput + input$MedBInput) / 2)))^(-1))  # Sample size
                                                                                                             
#}) 



#output$resultsSurv <- renderPrint({sample_sizeSurv()})

sample_sizeSurv2 <- reactive({
  
  
  (qnorm(((input$error1InputS)/100)/2) + qnorm(1-(input$Power1InputS)/100))^2 * (log(input$MedB1Input/input$MedA1Input)^(-2))*
    (2*(1 - ((1 - (exp(-log(2)/((input$MedA1Input + input$MedB1Input) / 2)))) / (log(2)/((input$MedA1Input + input$MedB1Input) / 2))) *
          exp(-log(2)*input$FT1Input/((input$MedA1Input + input$MedB1Input) / 2)))^(-1))  # Sample size
  
}) 


output$resultsSurv1 <- renderPrint({ceiling(sample_sizeSurv2())})



sample_sizeSurvplot <- reactive({
  
  j = 1
  
  pow <- seq(0.1, 0.9, by = 0.01)
  n1 <- numeric(length(pow))
  
  for(i in 1:pow){
  
  n1[j] <- (qnorm(((input$error1InputS)/100)/2) + qnorm(1-i/100))^2 * (log(input$MedB1Input/input$MedA1Input)^(-2))*
    (2*(1 - ((1 - (exp(-log(2)/((input$MedA1Input + input$MedB1Input) / 2)))) / (log(2)/((input$MedA1Input + input$MedB1Input) / 2))) *
          exp(-log(2)*input$FT1Input/((input$MedA1Input + input$MedB1Input) / 2)))^(-1))  # Sample size
  
  j = j + 1
  
  }
  
  n1
}) 



Survcoolplot32 <- reactive({
  
  qplot(sample_sizeSurvplot(), seq(0.1, 0.9, by = 0.01)) +
    coord_cartesian(ylim = c(0, 1)) +
    xlim(c(1, 300)) +
    geom_hline(yintercept = input$Power1InputS/100, col = "red") + 
    geom_vline(xintercept = sample_sizeSurv2(), col = "red") +
    ggtitle("Sample size and Power") +
    labs(x = "Sample size per group", y = "Power")  
  
  }) 


output$Survcoolplot33 <- renderPlot({
  Survcoolplot32()    
  
})

output$tutorialSurv <- renderUI({ 
  
  HTML(paste0("<br/>",  
              "Survival analysis can be used for comparison of groups based on information of an event where main interests of the study is 'whether' and/or 'when' the particular event occurs. 
              In mice experiments, the event of interest is usually death. For each mouse in an experiment, the time until death is measured from the day when an experiment starts,
              for example when a mouse is randomized to a treatment group, until a mouse dies or is scarified due to reaching a human endpoint. When a mouse is still alive at the end of an experiment,
              the observation for that mouse is censored. Censoring in survival analysis means that the information about the survival time is incomplete, like  for a mouse that is still alive at the 
              end of an experiment, the only available information on survival time is that time of death has not been observed during the duration of the experiment, so death happened later on but an investigator
              does not known the exact time of death.",
              "<br/>","<br/>","<br/>","<br/>","<br/>",
              "Survival times of two or more treatment groups can be compared with the log-rank test that is also called Mantel-Cox test.
              It is a non-parametric test  that  does not make any distributional assumptions about observations.   It looks at the order in which events happened and calculates number of observed and expected events in each group at each observed event time.
              Thus, the test compares survival across the whole spectrum of time, not just at one or two time points. If there are no censored observations in  an experiment, a researcher can use a Wilcoxon-Mann-Whitney test instead to compare average survival times between groups.",
              "<br/>","<br/>","<br/>","<br/>","<br/>"))
  
  
})


output$ExampleSurv <- renderUI({
  HTML("A scientist aims to establish a peritoneal dissemination xenograft mouse model of Esophageal adenocarcinoma (EAC).
                                                              Human EAC cell lines OE19  are injected intraperitoneally/subcutaneously into SCID mice and two weeks after the injection,
     mice  are randomly allocated to  vehicle or  paclitaxel treatment (20 mg/kg, 2 times a week for 2 weeks). The main interest of that experiment is to compare time of death between the two groups of mice. 
     Data from a previous similar experiment is available and can be used as a basis of  a power calculation for  the following experiment.  
     Data on 20 mice is accessible with information on number of days between start of the experiment and time of death or censoring,
     event status  (status = 1 when a mouse died, status = 0, when  a mouse did not die) and treatment groups (vehicle or treatment group)." 
  )
})


output$Surv_Example_1 <- renderDT(matrix(c(1:20, 10,11,17,17,18,21,19,32,32,35,19,20,28,30,34,41,40,41,46,58,
                                                                     rep(1,10), 0,0, rep(1,7), 0,
                                                                     rep("Vehicle", 10), rep("Treatment", 10)), ncol = 4),
                                                       colnames = c("ID","Survival time (days)", "Outcome", "Group"),
                                                       selection = 'none',
                                                       options = list(lengthMenu = c(5, 5, 5), pageLength = 20,
                                                                      dom = 't'))





output$Surv_Example_2 <- renderPrint({
  
  mydataSurv <- data.frame(Days = c(10,11,17,17,18,21,19,32,32,35,19,20,28,30,34,41,40,41,46,58),
    Status = c(rep(1,10), 0,0, rep(1,7), 0),
    Group = c(rep("Vehicle", 10), rep("Treatment", 10)))
  survdiff(Surv(Days, Status) ~ Group, data = mydataSurv)
  
})

output$Surv_Example_3 <- renderPrint({
  
  mydataSurv <- data.frame(Days = c(15,16,23,23,23,28,30,32,32,35,14,14,28,28,32,35,35,38,46,58),
                           Status = c(rep(1,10), 0,0, rep(1,7), 0),
                           Group = c(rep("Vehicle", 10), rep("Treatment", 10)))
  wilcox.test(Days~Group, data = mydataSurv)
  
})


output$Surv_Example_4 <- renderDT(matrix(c(1:39, 29, 37, 37, 29, 29, 39, 50, 37, 41, 36, 37, 37, 29, 44, 32, 34, 26, 34, 21, 30, 21, 58, 34, 49, 48, 
                                                                     69, 11, 67, 44, 51, 60, 71, 57, 66, 58, 63, 67, 43, 57, 
                                                                     rep(1,26), 0, rep(1,12),
                                                                     rep("Control", 21), rep("Treatment", 18)), ncol = 4),
                                                            colnames = c("ID","Days", "Status", "Group"),
                                                            selection = 'none',
                                                            options = list(lengthMenu = c(5, 5, 5), pageLength = 40,
                                                                           dom = 't'))



output$Surv_Example_5 <- renderPrint({
  mydataSurv2 <- data.frame(Days =c(29, 37, 37, 29, 29, 39, 50, 37, 41, 36, 37, 37, 29, 44, 32, 34, 26, 34, 21, 30, 21, 58, 34, 49, 48, 
           69, 11, 67, 44, 51, 60, 71, 57, 66, 58, 63, 67, 43, 57), 
           Status = c(rep(1,26), 0, rep(1,12)),
           Group = c(rep("Control", 21), rep("Treatment", 18)))
  survdiff(Surv(Days, Status) ~ Group, data = mydataSurv2)
  
})

output$Surv_Example_6 <- renderPrint({
  mydataSurv2 <- data.frame(Days =c(29, 37, 37, 29, 29, 39, 50, 37, 41, 36, 37, 37, 29, 44, 32, 34, 26, 34, 21, 30, 21, 58, 34, 49, 48, 
                                    69, 11, 67, 44, 51, 60, 71, 57, 66, 58, 63, 67, 43, 57), 
                            Status = c(rep(1,26), 0, rep(1,12)),
                            Group = c(rep("Control", 21), rep("Treatment", 18)))
  wilcox.test(Days ~ Group, data = mydataSurv2)
  
})



output$sdynamic_valueA1 <- renderPrint({
  cat(input$MedAInput)})


output$sdynamic_valueHR1 <- renderPrint({
  cat(input$MedBInput/input$MedAInput)})

output$sdynamic_valueFT1<- renderPrint({
  cat(input$FTInput)})

output$sdynamic_valueP1 <- renderPrint({
  cat(input$PowerInputS)})

output$sdynamic_valueAlp1 <- renderPrint({
  cat(input$errorInputS)})


output$sdynamic_valueA <- renderPrint({
  cat(input$MedA1Input)})


output$sdynamic_valueHR <- renderPrint({
  cat(round(input$MedB1Input/input$MedA1Input, digits = 2))})

output$sdynamic_valueFT <- renderPrint({
  cat(input$FT1Input)})

output$sdynamic_valueP <- renderPrint({
  cat(input$Power1InputS)})

output$sdynamic_valueAlp <- renderPrint({
  cat(input$error1InputS)})



# Proportions 

powr <- eventReactive(input$buttonPrInput,{ 
  withBusyIndicatorServer("buttonPrInput", {

  prop1 = input$Prop1Input
  prop2 = input$Prop2Input
    
  if((prop1 > 0.4 & prop1 < 0.6) | (prop2 > 0.4 & prop2 < 0.6)){
    
    if(abs(prop1 - prop2) < 0.3 | isTRUE(all.equal(abs(prop1-prop2), 0.3))){
      
      result <- power.prop.test(p1 = prop1, p2 = prop2, power = input$Power1InputP/100,
                                    sig.level = input$error1InputP/100)$n
      #ss2 <- c(prop1, prop2,input$error1InputP, input$Power1InputP)
      
      
    }else{
      
      if(abs(prop1 - prop2) < 0.45 | isTRUE(all.equal(abs(prop1-prop2), 0.45))){
        
        
        nn <- seq(22, 100)
        #ss1 <- c(prop1, prop2,input$error1InputP, input$Power1InputP)
        
      }else{
        
        
        nn <- seq(8, 45)
        #ss1 <- c(prop1, prop2,input$error1InputP, input$Power1InputP)
        
      }
      
      res <- mapply(FUN = power.fisher.test,
                    MoreArgs = list(p1 = prop1, p2 = prop2,
                                    alpha = input$error1InputP/100, nsim = 1000), nn, nn)
      
      
      result <- nn[which(res == min(res[res >= (input$Power1InputP)/100]))]
      #ss2 <- ss1
      
    }
    
  }else{
    
    if(abs(prop1 - prop2) < 0.25 | isTRUE(all.equal(abs(prop1-prop2), 0.25))){
      
      result <- power.prop.test(p1 = prop1, p2 = prop2, power = input$Power1InputP/100,
                                    sig.level = input$error1InputP/100)$n
      #ss2 <- c(prop1, prop2,input$error1InputP, input$Power1InputP)
      
      
    }else{
      
      nn <- seq(5, 60)
      #ss2 <- c(prop1, prop2,input$error1InputP, input$Power1InputP)
      
      res <- mapply(FUN = power.fisher.test,
                    MoreArgs = list(p1 = prop1, p2 = prop2,
                                    alpha = input$error1InputP/100, nsim = 1000), nn, nn)
      
      result <- nn[which(res == min(res[res >= (input$Power1InputP)/100]))]
      
      
    }
  }
  
  list(SS=result)
  
    
  
#  if(abs(prop1 - prop2) < 0.2 | isTRUE(all.equal(abs(prop1-prop2), 0.2))){
#    SS <- ceiling(power.prop.test(p1 = prop1, p2 = prop2, power = input$Power1InputP/100,
#                                   sig.level = input$error1InputP/100)$n)
#    ss2 <- 10

  #}else if(abs(prop1 - prop2) < 0.3 | isTRUE(all.equal(abs(prop1-prop2), 0.3))){
  
    
   # if((prop1 > 0.4 & prop1 < 0.6) | (prop2 > 0.4 & prop2 < 0.6)){
      
    #  nn <- seq(40, 130)
     # ss1 <- 11
    
#      }else if((prop1 > 0.4 & prop1 < 0.6) | (prop2 > 0.4 & prop2 < 0.6)){
      
 #     nn <- seq(40, 130)
  #    ss1 <- 11
       
   # }else{
      
    #  nn <- seq(30, 80)
     # ss1 <- 12
    #}
    
#    res <- mapply(FUN = power.fisher.test,
 #                 MoreArgs = list(p1 = prop1, p2 = prop2,
  #                                alpha = input$error1InputP/100, nsim = 1000), nn, nn)
   # SS <- nn[which(res == min(res[res >= (input$Power1InputP)/100]))]
  #  ss2 <- ss1
  #}else{
   # nn <- seq(5, 65)
  #  res <- mapply(FUN = power.fisher.test,
   #               MoreArgs = list(p1 = prop1, p2 = prop2,
    #                              alpha = input$error1InputP/100, nsim = 1000), nn, nn)
    #SS <- nn[which(res == min(res[res >= (input$Power1InputP)/100]))]
    #ss2 <- 14
    #}  

  
  #list(SS=SS, ss = ss2)
  
    })
})


#nn <- seq(4, 150)
#powe <- eventReactive(input$buttonPrInput,{ 
#  withBusyIndicatorServer("buttonPrInput", {
#  mapply(FUN = power.fisher.test,
#               MoreArgs = list(p1 = input$Prop1Input, p2 = input$Prop2Input,
#                                alpha = input$error1InputP/100, nsim = 800), nn, nn)
#  })
#})

#sample_sizPr <- eventReactive(
#  input$buttonPrInput,{
#    withBusyIndicatorServer("buttonPrInput", {
      
#      powr()$nn[which(powr()$res == min(powr()$res[powr()$res >= (input$Power1InputP)/100]))]
      
#      })
#  })

output$resultsProp <- renderPrint({
  if(input$buttonPrInput){
    isolate({ceiling(powr()$SS)})
  }
})

output$resultsProp2 <- renderPrint({
  if(input$buttonPrInput){
    isolate({powr()$ss})
  }
})


kksamsiz <- reactive({
  
  ceiling(power.prop.test(p1 = input$Prop1Input, p2 = input$Prop2Input, power = input$Power1InputP/100, sig.level = input$error1InputP/100)$n)
  
  
})
  
output$kkresultsProp <- renderPrint({samsiz()})




output$Pdynamic_valueP1 <- renderPrint({
  cat(input$Prop1Input)})


output$Pdynamic_valueP2 <- renderPrint({
  cat(input$Prop2Input)})


output$Pdynamic_valueDP1 <- renderPrint({
  cat(input$Prop1Input - input$Prop2Input)})

output$Pdynamic_valueALP<- renderPrint({
  cat(input$error1InputP)})

output$Pdynamic_valuePOW <- renderPrint({
  cat(input$Power1InputP)})


output$Pdynamic_valueTEST <- reactive({
  if((abs(input$Prop1Input - input$Prop2Input) < 0.2 | isTRUE(all.equal(abs(input$Prop1Input - input$Prop2Input), 0.2)))){
    HTML("<strong><font color='#4d3a7d'> Z-test for proportions </font></strong>")
    }else{
    HTML("<strong><font color='#4d3a7d'> Fisher's exact test </font></strong>")
  }
})



MydataP <- readRDS("WWW/data/Example_Prop")
output$ExamplePrp_1 <- renderDT(MydataP,
                               selection = 'none',  rownames = FALSE,
                               options = list(lengthMenu = c(5, 5, 5), pageLength = 10,  
                                              lengthChange = FALSE, dom = 't'), server = FALSE)

output$ExamplePrp_11 <- renderDT(matrix(c(1:20, 
                                rep("Control", 10), rep("Treatment", 10),
                                0, 1, rep(0, 8), 1, 0, 0, 1, rep(0, 4), 1, 0), ncol = 3),
                            colnames = c("ID", "Group", "Response"),
                            selection = 'none',
                            options = list(lengthMenu = c(2, 2, 2), pageLength = 20,
                                           dom = 't'))



# Growth Curve 

options(htmlwidgets.TOJSON_ARGS = list(na = 'string'))

Mydata_1 <- readRDS("WWW/data/RubenWide2")

Mydata <- Mydata_1 %>% mutate_if(is.numeric, ceiling)

output$ExampleGC_1 <- renderDT(Mydata,
                               selection = 'none',  rownames = FALSE,
                               options = list(lengthMenu = c(5, 5, 5), pageLength = 10,  
                                              lengthChange = FALSE, dom = 't'), server = FALSE)


Mydata2 <- readRDS("WWW/data/RubenLong2")

Mydata2 <- Mydata2 %>% mutate_if(str_detect(colnames(.), fixed("Volume")), ceiling)  %>%
                       mutate_if(is.numeric, round, 2)

output$ExampleGC_2 <- renderDT(Mydata2,
                               selection = 'none',  rownames = FALSE,
                               options = list(lengthMenu = c(5, 5, 5), pageLength = 10,  
                                              lengthChange = FALSE, dom = 't'), server = FALSE)

# Power Calculation


SDX <- reactive({sd(seq(0, 2*input$NoMeasurements - input$Space, by = input$Space))})


sample_sizeLR <- reactive({
  
  Sigma <- (input$VARres) * (1/(SDX()^2) + 1/(SDX()^2))
  SS <- ((qnorm((input$errorInputLR/100)/2, lower.tail = F) + qnorm((input$PowerInputLR/100)))^2 * Sigma)/((input$Effect)^2)
  
})

#SDX <- sd(seq(0, NoMeasurements, by = Space))
#Sigma <- (VARres) * (1/(SDX^2) + 1/(SDX^2))
#SS <- ((qnorm((alp)/2, lower.tail = F) + qnorm((pow)))^2 * Sigma)/((Effect)^2)

#DE <- (SE_AR^2)/(SE_LR^2)

#resultsLR <- ceiling(DE*SS/(NoMeasurements))


#Power_LR <- reactive({ # Getting Rho
  
  
#  power.SLR(n = 700, lambda.a = input$Effect, sigma.x = SDX, sigma.y = SDX, alpha = input$errorInputLR)
  
#})


#sample_sizeLR1 <- reactive({ # feeding Rho here to get the sample size (total observations)
  
#  ss.SLR(power = input$PowerInputLR/100, lambda.a = input$Effect, sigma.x = input$SDX, sigma.y = input$SDY, 
#         alpha = input$errorInputLR/100, verbose = FALSE)$n
  
#})



# This is the total number of observations, which now we have to divide by the number of mneasurements to get the total number of mice.
# Then, we need to multiply this amount by the Design Effect to account for the multiple measurements per mouse.

# The design effect should be fixed ??? Let's fix it at 2 for now...


DE <- reactive({
  (input$SE_AR^2)/(input$SE_LR^2)
})


output$resultsLR <- renderPrint({ceiling(DE()*sample_sizeLR()/(input$NoMeasurements))})


#output$resultsProp <- renderPrint({samsiz()})




output$dynamic_valueEffLR <- renderPrint({
  cat(input$Effect)})


output$dynamic_valueSDX1LR <- renderPrint({
  cat(input$SE_LR)})

output$dynamic_valueSDX2LR <- renderPrint({
  cat(input$SE_AR)})

output$dynamic_valueDE <- renderPrint({
  cat(round(DE(), digits = 2))})

output$dynamic_valueVARresLR <- renderPrint({
  cat(input$VARres)})


output$dynamic_MeasurementsLR <- renderPrint({
  cat(input$NoMeasurements)})


output$dynamic_DistaLR <- renderPrint({
  cat(input$Space)})

output$dynamic_valueALLR<- renderPrint({
  cat(input$errorInputLR)})


output$dynamic_valuePOWLR <- renderPrint({
  cat(input$PowerInputLR)})





########################################## Not relevant for now......  #######################

# Analysis

df_products_upload <- reactive({
  
  inFile <- input$file1
  
  if(is.null(inFile)){
    return(NULL)}
  
  if(input$fileType_Input == '2'){
    read.xlsx(inFile$datapath,
              header= TRUE,
              sheetIndex = 1,
              stringsAsFactors = FALSE)
  }else{
    read.csv(inFile$datapath,
             header= TRUE,
             stringsAsFactors = FALSE)
  }
  
})

df_products_upload2 <- reactive({
  
Datas <- df_products_upload() %>% mutate_if(str_detect(colnames(.), fixed("Volume")), ceiling)  %>%
  mutate_if(is.numeric, round, 2)

})


output$sample_table <- DT::renderDataTable({
  
  df <- df_products_upload2()
  DT::datatable(df)
  
})


# With Ind structure

resultsGC_Ind <- reactive({
  
  gls(log_volume ~ dag + dag:treatment
      , method = "REML"
      , correlation = NULL
      , data = df_products_upload2())
  
})

  
output$resultsGC2 <- renderPrint({coef(summary(resultsGC_Ind()))})


# With AR(1)
resultsGC_AR <- reactive({
  
  gls(log_volume ~ dag + dag:treatment
      , method = "REML"
      , correlation = corCAR1(form = ~dag|mouse)
      , data = df_products_upload2())
  
})


output$resultsGC_AR2 <- renderPrint({coef(summary(resultsGC_AR()))})

output$resultsGC_AR2_COR <- renderPrint({
                              as.numeric(coef(resultsGC_AR()$model$corStr, unconstrained = FALSE))
  })

} ### END