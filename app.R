#Load Libraries
library(shiny)
library(shinydashboard)
library(iopsych)
library(scales)
library(shinyjs)
library(mvtnorm)
library(ggplot2)
library(dplyr)
library(stringr)
library(grid)
library(gridExtra)
library(gridtext)

#Define Custom Functions
Expectancyfunc <- function(Validity, PredLowerCut, PredUpperCut, CritLowerCut, CritUpperCut){
  n <- 1000
  mean <- c(0,0)
  lower <- c(PredLowerCut, CritLowerCut)
  upper <- c(PredUpperCut, CritUpperCut)
  corr <- diag(2)
  corr[lower.tri(corr)] <- Validity
  corr[upper.tri(corr)] <- Validity
  jtprob <- pmvnorm(lower, upper, mean, corr, algorithm = Miwa(steps=128))
  xprob <- pnorm(PredUpperCut, mean = 0, sd = 1) - pnorm(PredLowerCut, mean = 0, sd = 1)
  expectancy <- jtprob/xprob
  return(expectancy[1])
}

#References
reference_content <- HTML(
  ' <div style="padding-left: 30px; text-indent: -30px;">
    <p>Alexander III, L., Mulfinger, E., & Oswald, F. L. (2019). <i>Investing in People Online</i> (Version 2.0) [Software], Rice University, Houston, Texas. Available from <a  href="https://orgtools.shinyapps.io/IIP3/">https://orgtools.shinyapps.io/IIP3/</a><p>
    <p>Avolio, B. J., Avey, J. B., & Quisenberry, D. (2010). Estimating return on leadership development investment. <i>The Leadership Quarterly, 21</i>(4), 633–644. <a href="https://doi.org/10.1016/j.leaqua.2010.06.006">https://doi.org/10.1016/j.leaqua.2010.06.006</a><p>
    <p>Carson, K. P., Becker, J. S., & Henderson, J. A. (1998). Is utility really futile? A failure to replicate and an extension. <i>Journal of Applied Psychology, 83</i>(1), 84–96. <a href="https://doi.org/10.1037/0021-9010.83.1.84">https://doi.org/10.1037/0021-9010.83.1.84</a><p>
    <p>Cascio, W. F., Boudreau, J. W., & Fink, A. A. (2019). <i>Investing in people: financial impact of human resource initiatives</i> (3rd ed.). Society for Human Resource Management.<p>
    <p>Cucina, J., Berger, J., & Busciglio, H. (2017). Communicating Criterion-Related Validity Using Expectancy Charts: A New Approach. <i>Personnel Assessment and Decisions, 3</i>(1). <a href="https://doi.org/10.25035/pad.2017.001">https://doi.org/10.25035/pad.2017.001</a><p>
    <p>Latham, G. P., & Whyte, G. (1994). The Futility Of Utility Analysis. <i>Personnel Psychology, 47</i>(1), 31–46. <a href="https://doi.org/10.1111/j.1744-6570.1994.tb02408.x">https://doi.org/10.1111/j.1744-6570.1994.tb02408.x</a><p>
    <p>Magnusson, K. (2023, June 9). A Causal Inference Perspective on Therapist Effects. <a href="https://doi.org/10.31234/osf.io/f7mvz">https://doi.org/10.31234/osf.io/f7mvz</a><p>
    <p>Schmidt, F. L. (2013). The economic value of goal setting to employers. In E. A. Locke & G. P. Latham (Eds.), <i>New developments in goal setting and task performance</i> (pp. 16–20). Routledge/Taylor & Francis Group.<p>
    <p>Sturman, M. C. (2000). Implications of utility analysis adjustments for estimates of human resource intervention value. <i>Journal of Management, 26</i>(2), 281–299. <a href="https://doi.org/10.1177/014920630002600206">https://doi.org/10.1177/014920630002600206</a></p>
    </div>'
)

# Opening UI
opening_ui <- fluidPage(
  useShinyjs(),
  titlePanel("Welcome to the Utility Analysis+ App"),
  mainPanel(
    h4("Introduction"),
    p("Thank you for using the Utility Analysis App. You can navigate within the app using the above tabs. The Staffing Utility
         and Training Utility tabs have multiple tools available for calculating returns on staffing and training procedures as well as producing reports and visuals.
        These reports are intended to aid in presenting results of utility to managers. It has been shown that the way that utility
        is presented to managers has a significant impact on managers accepting the results (Sturman, 2000), and this app aims to
        help in that presentation."),
    p("The Glossary tab includes helpful definitions to better understand the purpose of this app. The References tab includes all references
        used in the creation of this app.")
  )
)

#Reference UI
reference_ui <- fluidPage(
  titlePanel("References"),
  mainPanel(
    reference_content
  )
)

#Glossary UI
glossary_ui <- fluidPage(
  tags$style(HTML(".size {font-size: 16px}")),
  titlePanel("Glossary"),
  mainPanel(
    p(HTML("<strong>Break-Even SDy</strong> - Minimum possible value of SDy to recoup losses from conducting a staffing/training program."), class = "size"),
    p(HTML("<strong>Cohen's D</strong> - A type of effect size. This value represents the standardized difference in production between a trained and untrained group."), class = "size"),
    p(HTML("<strong>Discount Rate</strong> - Rate of accrual of monetary investments from production revenues."), class = "size"),
    p(HTML("<strong>Effect Size</strong> - Standard Deviation of the performance difference between a treated group and an untreated group."), class = "size"),
    p(HTML("<strong>Expectancy</strong> - A measure of the ability of a test to predict outcomes. For example, a test with high validity has a high expectancy of predicting high performance based on a high test score."), class = "size"),
    p(HTML("<strong>SDp</strong> - Standard deviation of work output among employees as a percentage of mean output."), class = "size"),
    p(HTML("<strong>SDy</strong> - Monetary value of job performance. This value indicates the difference between an average worker (50th percentile) and a worker with high production (85th percentile). Methods for measuring SDy include the 40% rule, Global Estimation, and CREPID among others."), class = "size"),
    p(HTML("<strong>Selection ratio</strong> - Ratio of the number of employees to be hired from an applicant pool."), class = "size"),
    p(HTML("<strong>Tax Rate</strong> - Company tax rate."), class = "size"),
    p(HTML("<strong>Utility</strong> - A measure of the return on investment in workers. Utility compares the quality and quantity of a treated group of employees to the cost of the treatment. This returns an estimation of the economic value that these employees add to the company."), class = "size"),
    p(HTML("<strong>Validity</strong> - Correlation between selection test performance and actual job performance. A validity of 0 indicates no relationship, while a validity of 1.00 indicates a perfect relationship. Validity will almost always be below .50 (Carson et. al. (1998))."), class = "size"),
    p(HTML("<strong>Variable Costs</strong> - Costs that rise with greater production. For example, material costs, incentives, commission, variable overhead, etc."), class = "size")
  )
)

#Staffing UI
main_ui <- dashboardPage(
  dashboardHeader(title = "Staffing Utility"),
  
  dashboardSidebar(
    sidebarMenu(
      id = "sidebar",
      menuItem("Staffing Utility", tabName = "staffing_utility", icon = icon("file-alt")),
      menuItem("Expectancy", tabName = "expectancy", icon = icon("dashboard")),
      menuItem("Utility Output", tabName = "utility_adjustments", icon = icon("sliders-h"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "staffing_utility",
              fluidPage(
                titlePanel("Staffing Utility"),
                mainPanel(
                  p("This is the staffing utility section of the UA+ app. Staffing utility can be described as a return on investment on improved staffing procedures. 
                    It measures this return on investment by comparing the quantity and quality of workers chosen by the staffing procedure to the cost of the procedure.
                    On the left panel, there is a tab for expectency. Expectancy shows the relationship between predictor scores (in this case validity), and criterion
                    scores such as job performance (Cucina et al., 2017). The Expectancy tab produces an expectancy chart based on the work of Cucina et. al. This chart compares
                    the expected job performance of hires based on their score from staffing procedures. The default values provided are from Latham & Whyte (Latham & Whyte, 1994)."),
                  br(),
                  p("The Utility Output tab is where values for utility are displayed. Along with these values is a plain text description modeled after Carson et. al's descriptions
                    of utility that were found to be effective when presented to managers (Carson et al., 1998). This tab also includes utility adjustments for economic factors
                    and workflows. Economic factor defaults provided are from Sturman (Sturman, 2000) while workflow defaults are from Cascio et al.(Cascio et al., 2019).")
                  
                )
              )
      ),
      tabItem(tabName = "expectancy",
              fluidPage(
                useShinyjs(),
                titlePanel("Expectancy of High Job Performance"),
                sidebarLayout(
                  sidebarPanel(
                    numericInput("n", "Number of hires:", 470, min = 1, step = 1),
                    numericInput("sdy", "SD of performance in monetary units:", 16290, min = 0.01, step = 1),
                    numericInput("rxy1", "Validity of old procedure:", 0.1, min = -1, max = 1, step = 0.01),
                    numericInput("rxy2", "Validity of new procedure:", 0.5, min = -1, max = 1, step = 0.01),
                    numericInput("sr", "Selection ratio:", 0.33, min = 0, max = 1, step = 0.01),
                    numericInput("cost1", "Cost per applicant of old procedure:", 200, min = 0.01, step = 1),
                    numericInput("cost2", "Cost per applicant of new procedure:", 304.33, min = 0.01, step = 1),
                    numericInput("period", "Anticipated tenure of selected employees:", 18, min = 0.01, step = 1),
                    actionButton("go", "Compute Expectancy")
                  ),
                  
                  mainPanel(
                    #Expectancy Chart
                    plotOutput("utility_graph"),
                    textOutput("expectancy_text"),
                    downloadButton("download_plot", "Export PDF")
                  )
                )
              )
      ),
      tabItem(tabName = "utility_adjustments",
              fluidPage(
                useShinyjs(),
                titlePanel("Predicting Returns on Improved Staffing Procedures"),
                sidebarLayout(
                  sidebarPanel(
                    HTML('<h4>Economic Factors</h4>'),
                    numericInput("vcost", "Variable Costs (%):", 35, min = 0, max = 100, step = 1),
                    numericInput("tax", "Tax Rate (%):", 63, min = 0, max = 100, step = 1),
                    numericInput("drate", "Discount Rate (%):", 11, min = 0, max = 100, step = 1),
                    HTML('<h4>Employee Flows</h4>'),
                    numericInput("pyears" , "Program Length" , 15, min = 1, step = 1),
                    numericInput("nadd" , "Employees Added per Year:" , 470, min = 0, step = 1),
                    numericInput("nsub" , "Employees Lost per Year(After Tenure):", 470, min = 0, step = 1),
                    actionButton("go2", "Compute Adjustments")
                  ),
                  mainPanel(
                    uiOutput("h1"),
                    textOutput("total_utility"),
                    textOutput("break_even_SDy"),
                    uiOutput("h2"),
                    textOutput("adjusted_utility"),
                    textOutput("adjusted_utility_perHire"),
                    textOutput("adjusted_utility_perHire_perYear"),
                    br(),
                    textOutput("plainText1"),
                    br(),
                    textOutput("plainText2"),
                    br(),
                    textOutput("plainText3"),
                    downloadButton("download_pdf", "Export Report")
                  )
                )
              )
      )
    )
  )
)

#Training UI
training_ui <- dashboardPage(
  dashboardHeader(title = "Training Utility"),
  
  dashboardSidebar(
    sidebarMenu(
      id = "sidebarTrain",
      menuItem("Training Utility", tabName = "training_utility", icon = icon("file-lines")),
      menuItem("Effect Size", tabName = "effect_size", icon = icon("chart-simple")),
      menuItem("Utility Outputs", tabName = "utility_outputs", icon = icon("sliders-h"))
    )
  ),
  
  dashboardBody(
    
    tabItems(
      tabItem(tabName = "training_utility",
              fluidPage(
                titlePanel("Training Utility"),
                p("This is the training utility section of the UA+ app. Training utility can be described as a return on investment in the development of workers (Avolio et al., 2010). 
                  This return on develpoment investment (RODI) is calculated using the effect size of the training process. In the panel on the left, there is a tab for effect size that gives
                  a descriptive analysis of the effect size that you choose to input. This consists of a modified graph from Alexander et al's Investing in People Online app (Alexander et al., 2019),
                  as well as a modified version of Magnusson's description of effect size (Magnusson, 2023). This tool is useful for displaying how effect size relates to the productivity of trained
                  versus untrained workers."),
                br(),
                p("The Utility Outputs tab displays the results of utility analysis as well as a plain text description. This utility analysis takes into account the quality and quantity of
                the training program and workers and compares this value to the cost of the program. It additionally takes into account economic factors such as variable costs, taxes, and investments.
                The results from this analysis can be displayed in two forms, the RODI as discussed earlier, or as the dollar value of goal-setting. Displaying the results of utility analysis in the form
                of RODI provides managers with an easily understandable way of seeing the production gains from training programs (Avolio et al., 2010). On the other hand, the goal-setting approach has been 
                shown to be a useful tool for increasing job performance that managers react positively to (Schmidt, 2013). Sets of inputs from both studies have been provided.")
              )
      ),
      tabItem(tabName = "effect_size",
              
              fluidPage(
                useShinyjs(),
                titlePanel("Effect Size Description"),
                sidebarLayout(
                  sidebarPanel(
                    id = "sidebar",
                    HTML('<h3>Effect Size Inputs</h3>'),
                    numericInput("dTrain2_1", "Training Effectiveness (Cohen's d)", 0),
                    actionButton("goAvolio_1", "RODI Input"),
                    actionButton("goSchmidt_1", "Goal Setting Input"),
                    radioButtons("outputOpt1", "Output Type:",
                                 choices = c("Training Program", "Goal-Setting"),
                                 selected = "Training Program",
                    ),
                    br(),
                    actionButton("goEffect", "Display Effect Size")
                    
                  ),
                  mainPanel(
                    
                    plotOutput("training_graph_1"),
                    br(),
                    textOutput("effectText"),
                    downloadButton("effect_download", "Download PDF")
                  )
                )
              )
      ),
      tabItem(tabName = "utility_outputs",
              fluidPage(
                useShinyjs(),
                titlePanel("Predicting Returns on Training and Developing Employees"),
                sidebarLayout(
                  sidebarPanel(
                    id = "sidebar",
                    tabsetPanel(
                      id = "pageTabs",
                      tabPanel("Page 1",
                               HTML('<h3>Utility Inputs (Page 1)</h3>'),
                               numericInput("nTrain", "Number of Employees Trained", 0, min = 0, step = 1),
                               numericInput("tTrain", "Training Effect Duration", 0, min = 0),
                               numericInput("dTrain2", "Effect Size of Procedure", 0),
                               numericInput("sdyTrain", "SD of Performance in Monetary Units", 0),
                               numericInput("sdP", "SD of Work Output as % of Mean Output", 20, min = 0, max = 100),
                               numericInput("costTrain2", "Cost per Employee of Procedure", 0, min = 0),
                               actionButton("goAvolio", "RODI Inputs"),
                               actionButton("goSchmidt", "Goal Setting Inputs"),
                               br(),
                               actionButton("goUn", "Compute Utility(No Adjustments)")
                      ),
                      tabPanel("Page 2",
                               HTML('<h3>Utility Inputs (Page 2)</h3>'),
                               HTML('<h4>Economic Factors</h4>'),
                               numericInput("vrateTrain", "Variable Costs (%)", 35, min = 0, max = 100),
                               numericInput("taxTrain", "Tax Rate (%)", 63, min = 0, max = 100),
                               numericInput("discTrain", "Discount Rate (%)", 11, min = 0, max = 100),
                               HTML('<h4>Employee Flows</h4>'),
                               numericInput("lengthTrain", "Program Length", 15, min = 0),
                               numericInput("addTrain", "Employees Trained per Year", 618, min = 0),
                               numericInput("subTrain", "Loss of Trained Employees per Year (After Effect Duration)", 618, min = 0),
                               radioButtons("outputOpt", "Output Type:",
                                            choices = c("Training Program", "Goal-Setting"),
                                            selected = "Training Program"
                               ),
                               actionButton("go4", "Compute Utility")
                      )
                    )
                  ),
                  mainPanel(
                    uiOutput("h3"),
                    textOutput("training_utilityUn"),
                    textOutput("break_even_train"),
                    uiOutput("h4"),
                    textOutput("training_utility"),
                    textOutput("training_utility_per_employee"),
                    textOutput("training_utility_per_employee_per_year"),
                    br(),
                    textOutput("trainingText1"),
                    br(),
                    textOutput("trainingText2"),
                    br(),
                    textOutput("trainingText3"),
                    downloadButton("training_download", "Export Report")
                  )
                )
              )
      )
    )
  )
)

#Server
main_server <- function(input, output, session) {
  utilityUn <- reactiveVal()
  utilityUnPer <- reactiveVal()
  utilityUnPerYear <- reactiveVal()
  utilityAdj <- reactiveVal()
  utilityAdjPer <- reactiveVal()
  utilityAdjPerYear <- reactiveVal()
  staffSDy <- reactiveVal()
  costAdj <- reactiveVal()
  elo <- reactiveVal()
  elmo <- reactiveVal()
  eumo <- reactiveVal()
  eto <- reactiveVal()
  eln <- reactiveVal()
  elmn <- reactiveVal()
  eumn <- reactiveVal()
  etn <- reactiveVal()
  bh <- reactiveVal()
  gh <- reactiveVal()
  expPro <- reactiveVal()
  expPro2 <- reactiveVal()
  expDiff <- reactiveVal()
  expDiff2 <- reactiveVal()
  selectedPage <- reactiveVal(1)
  
  #Toggles
  observe({ 
    #Staffing Toggle 1
    toggleState(
      id = "go",
      condition = 
        input$n > 0 &
        input$sdy >= 0 &
        -1 <= input$rxy1  &
        1 >= input$rxy1 &
        -1 <= input$rxy2  &
        1 >= input$rxy2 &
        input$sr > 0 &
        input$cost1 >= 0 &
        input$cost2 >= 0 &
        input$period > 0
    )
    #Staffing Toggle 2
    toggleState(
      id = "go2",
      condition =
        input$vcost >= 0 &
        input$vcost <= 100 &
        input$tax >= 0 &
        input$tax <= 100 &
        input$drate >= 0 &
        input$drate <= 100 &
        input$pyears >0 &
        input$nadd >= 0 &
        input$nsub >= 0
    )
    #Training Toggle 1
    toggleState(
      id = "goEffect",
      condition =
        input$nTrain_1 > 0 &
        input$tTrain_1 > 0 &
        input$sdyTrain_1 > 0 &
        input$costTrain2_1 >= 0
    )
    if (!isTruthy(input$go)) {
      disable("download_plot")
    }
    if (!isTruthy(input$go2)) {
      disable("download_pdf")
    }
    if (!isTruthy(input$goEffect)) {
      disable("effect_download")
    }
    if (!isTruthy(input$go4)) {
      disable("training_download")
    }
    if (!isTruthy(input$goSchmidt)) {
      disable("sdP")
    }
  })
  #Staffing Utility  
  observeEvent(input$go, {
    
    enable("download_plot")
    
    n <- input$n
    period <- input$period
    
    # Utility Calculation
    utility_value <- utilityBcg(
      n = (input$n),
      sdy = input$sdy,
      rxy = (input$rxy2 - input$rxy1),
      uxs = ux(input$sr),
      sr = input$sr,
      pux = NULL,
      cost = ((input$cost2 - input$cost1) * (input$n / input$sr)),
      period = input$period
    )
    
    # Compute Break Even SDy
    breakEvenSDy <- (
      ((input$cost2)*(input$n/input$sr))/(input$n*(input$rxy2)*input$period*ux(input$sr))
    )
    
    # Compute old top expectancy
    expectancyTopOld <- 100*round(Expectancyfunc(input$rxy1, ux(input$sr)+.67 , Inf, input$rxy1*(ux(input$sr))+.67, Inf),2)
    
    # Compute old upper middle expectancy
    expectancyUMOld <- 100*round(Expectancyfunc(input$rxy1, ux(input$sr)+0 , ux(input$sr)+.67, input$rxy1*(ux(input$sr))+.67, Inf),2)
    
    #Compute old lower middle expectancy
    expectancyLMOld <- 100*round(Expectancyfunc(input$rxy1, ux(input$sr)-.67 , ux(input$sr)+0, input$rxy1*(ux(input$sr))+.67, Inf),2)
    
    #compute old lower expectancy
    expectancyLowOld <- 100*round(Expectancyfunc(input$rxy1, -Inf , ux(input$sr)-.67, input$rxy1*(ux(input$sr))+.67, Inf),2)
    
    # Compute new top expectancy
    expectancyTopNew <- 100*round(Expectancyfunc(input$rxy2, ux(input$sr)+.67 , Inf, input$rxy2*(ux(input$sr))+.67, Inf),2)
    
    # Compute new upper middle expectancy
    expectancyUMNew <- 100*round(Expectancyfunc(input$rxy2, ux(input$sr)+0 , ux(input$sr)+.67, input$rxy2*(ux(input$sr))+.67, Inf),2)
    
    #Compute new lower middle expectancy
    expectancyLMNew <- 100*round(Expectancyfunc(input$rxy2, ux(input$sr)-.67 , ux(input$sr)+0, input$rxy2*(ux(input$sr))+.67, Inf),2)
    
    #compute new lower expectancy
    expectancyLowNew <- 100*round(Expectancyfunc(input$rxy2, -Inf , ux(input$sr)-.67, input$rxy2*(ux(input$sr))+.67, Inf),2)
    
    badHire <- round(((expectancyLowOld-expectancyLowNew)/expectancyLowOld)*100, 0)
    goodHire <- round(((expectancyTopNew-expectancyTopOld)/expectancyTopOld)*100, 0)
    bh(badHire)
    gh(goodHire)
    
    elo(expectancyLowOld)
    elmo(expectancyLMOld)
    eumo(expectancyUMOld)
    eto(expectancyTopOld)
    eln(expectancyLowNew)
    elmn(expectancyLMNew)
    eumn(expectancyUMNew)
    etn(expectancyTopNew)
    
    # Calculate the per-hire utility
    per_hire_utility <- utility_value / n
    
    # Calculate the per-hire per-year utility
    per_year_utility <- per_hire_utility / period
    
    # Format
    formatted_total_utility <- label_dollar(scale = .001, prefix= "~$", suffix = "K")(signif(utility_value, 2))
    utilityUn(formatted_total_utility)
    formatted_per_hire_utility <- label_dollar(scale = .001, prefix= "~$", suffix = "K")(signif(per_hire_utility, 2))
    utilityUnPer(formatted_per_hire_utility)
    formatted_per_year_utility <- label_dollar(scale = .001, prefix= "~$", suffix = "K")(signif(per_year_utility, 2))
    utilityUnPerYear(formatted_per_year_utility)
    formatted_breakEvenSDy <- label_dollar(scale = .001, prefix= "~$", suffix = "K")(signif(breakEvenSDy, 2))
    staffSDy(formatted_breakEvenSDy)
    
    exp_diff <- if (expectancyTopNew > expectancyTopOld) {
      expectancyTopNew - expectancyTopOld
    }
    else {
      expectancyTopOld - expectancyTopNew
    }
    expDiff(exp_diff)
    exp_diff2 <- if (expectancyLowNew > expectancyLowOld) {
      expectancyTopNew - expectancyTopOl
    }
    else {
      expectancyLowOld - expectancyLowNew
    }
    expDiff2(exp_diff2)
    exp_procedure <- if (expectancyTopNew > expectancyTopOld) {
      "new procedure"
    }
    else {
      "old procedure"
    }
    expPro(exp_procedure)
    exp_procedure2 <- if (expectancyLowNew > expectancyLowOld) {
      "increases"
    }
    else {
      "decreases"
    }
    expPro2 <- exp_procedure2
    #expectancy chart
    
    plot_data <- reactive({
      bar_data <- data.frame(
        Quartile = rep(c("Bottom 25%", "Lower Middle 25%", "Upper Middle 25%", "Top 25%"), each = 2),
        Procedure = factor(c("Old", "New"), levels = c("Old", "New")),  # Specify the order of levels
        Probability = c(
          expectancyLowOld, expectancyLowNew,
          expectancyLMOld, expectancyLMNew,
          expectancyUMOld, expectancyUMNew,
          expectancyTopOld, expectancyTopNew
        )
      )
      bar_data$Quartile <- str_wrap(bar_data$Quartile, width = 10)
      max_y_value <- 1.2 * max(max(bar_data$Probability))
      title_text <- paste0("Using the ", exp_procedure, " improves the probability of acquiring a high performer by ", goodHire, "%.")
      wrapped_title <- str_wrap(title_text, width = 90)
      p <- ggplot(bar_data, aes(x = reorder(Quartile, Probability), y = Probability, fill = Procedure)) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(
          x = "", 
          y = "Probability of High Job Performance",
          fill = "Procedure"
        ) +
        ylim(0, max_y_value) +  # Set the y-axis limits
        scale_fill_manual(values = c("Old" = "#FF9999", "New" = "#9999FF")) +
        theme_minimal() +  
        theme(
          plot.title = element_text(hjust = 0, margin = margin(b = 10), size = 16, lineheight = 1.2),
          axis.text = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          axis.text.x = element_text(angle = 0, hjust = .5)
        ) +
        ggtitle(paste0("Figure 1\n", wrapped_title))
      
      return(p + geom_text(aes(label = paste0(Probability, "%")), vjust = -0.5, size = 4, position = position_dodge(width = 0.9)))
    })
    output$utility_graph <- renderPlot({
      plot_data()
    })
    
    output$expectancy_text <- renderText({
      paste0("The above chart compares the expectancy of the new staffing procedure to the old staffing procedure. Expectancy is a 
             measure of the ability of a test to predict outcomes. Using the new procedure improves the chance that we acquire a high 
             performer by ", goodHire, "% [(",etn(), " - ",eto(), ")/", eto() ,"] and avoid a bad hire by ", badHire, "% [(",elo(), " - ",eln(), ")/", elo() ,"].")
    })
    
    output$download_plot <- downloadHandler(
      filename = function() {
        "expectancy.pdf"  
      },
      
      content = function(file) {
        
        capWrap <- strwrap(paste0("The above chart compares the expectancy of the new staffing procedure to the old staffing procedure. Expectancy is a 
             measure of the ability of a test to predict outcomes. Using the new procedure improves the chance that we acquire a high 
             performer by ", goodHire, "% [(",etn(), " - ",eto(), ")/", eto() ,"] and avoid a bad hire by ", badHire, "% [(",elo(), " - ",eln(), ")/", elo() ,"]."), width = 90, simplify = TRUE)
        capWrap <- paste(capWrap, collapse = "\n")
        plot <- plot_data() +
          labs(
            caption = capWrap
          ) +
          theme(
            plot.caption = element_text(
              hjust = 0,
              margin = margin(t = 10, unit = "pt"),
              size = 14
            ),
            plot.margin = margin(1,1,1,1, "in")
          )
        
        
        ggsave(
          file,
          plot,
          device = "pdf",
          width = 10.625,  
          height = 13.75,  
          units = "in",  
          dpi = 300  
        )
      }
    )
    
  })
  
  observeEvent(input$go2,{
    enable("download_pdf")
    varCosts <- -input$vcost/100
    tax <- input$tax
    disc <- input$drate
    costOrd <- input$cost1
    costAc <- input$cost2
    validOrd <- input$rxy1
    validAc <- input$rxy2
    SDjp <- input$sdy
    sr1  <- input$sr
    ordsr1 <- dnorm(qnorm(1-sr1),0,1)
    tenure1 <- input$period
    last <- input$pyears
    add <- input$nadd
    subt <- input$nsub
    
    nk <- 0
    paytot <- 0
    paysel <- 0
    payselfir <- 0
    
    discProp <- disc / 100
    
    valid <- validAc - validOrd
    
    ord <- ordsr1 / sr1
    ck <- add * ((costAc - costOrd) / sr1)
    taxProp <- tax / 100
    discRat <- 1 / (1 + discProp)
    
    numyr <- tenure1 + last
    
    totDelta <- 0
    totDelta1 <- 0
    
    for (i in 1:numyr) {
      if (i > tenure1) {nk <- nk - subt}
      if (i <= last) {nk <- nk + add}
      if (i > last) {ck <- 0}
      if (nk >= 0) {
        delta1 <- nk * ((discRat^i) * valid * ord * SDjp * (1 + varCosts) * (1 - taxProp))
        delta3 <- nk * ((discRat^i) * valid * ord * SDjp * (-varCosts) * (taxProp))
      }
      delta2 <- ck * (1 - taxProp) * (discRat^(i - 1))
      totDelta1 <- totDelta1 + delta2 + delta3
      delta <- delta1 - delta2
      totDelta <- totDelta + delta
    }
    
    adjusted_utility <- totDelta
    adjusted_utility_perHire <- totDelta / (last*add)
    adjusted_utility_perHire_perYear <- (totDelta / (last * add * tenure1))
    
    # Format
    formatted_adjusted_utility <- label_dollar(scale = .001, prefix= "~$", suffix = "K")(signif(adjusted_utility, 2))
    utilityAdj(formatted_adjusted_utility)
    formatted_adjusted_per_hire_utility <- label_dollar(scale = .001, prefix= "~$", suffix = "K")(signif(adjusted_utility_perHire, 2))
    utilityAdjPer(formatted_adjusted_per_hire_utility)
    formatted_adjusted_per_year_utility <- label_dollar(scale = .001, prefix= "~$", suffix = "K")(signif(adjusted_utility_perHire_perYear, 2))
    utilityAdjPerYear(formatted_adjusted_per_year_utility)
    
    costAdj(label_dollar(scale = .001, prefix= "~$", suffix = "K")(signif(totDelta1/last, 2)))
    
    # Display
    
    output$h1 <- renderUI({HTML('<span style="font-size: 14px; font-weight: bold;">Unadjusted Values</span>')
    })
    
    output$h2 <- renderUI({HTML('<span style="font-size: 14px; font-weight: bold;">Adjusted Values</span>')
    })
    
    output$total_utility <- renderText({
      paste("The opportunity costs of failing to use the program are:", 
            utilityUn())
    })
    output$break_even_SDy <- renderText({
      paste("The break-even value for Standard Deviation of the new procedure is:", 
            staffSDy())
    })
    output$adjusted_utility <- renderText({
      paste("Failing to use this program will have total opportunity costs to the company of:", 
            formatted_adjusted_utility)
    })
    output$adjusted_utility_perHire <- renderText({
      paste("Failing to use this program will have opportunity costs to the company of:", 
            formatted_adjusted_per_hire_utility,
            "per hire.")
    })
    output$adjusted_utility_perHire_perYear <- renderText({
      paste("Failing to use this program will have opportunity costs to the company of:", 
            formatted_adjusted_per_year_utility,
            "per hire per year they stay.")
    })
    
    lowHigh <- if(input$rxy1 > input$rxy2) {
      "old"
    }
    else{
      "new"
    }
    ls <- if(input$rxy1 > input$rxy2) {
      "smaller"
    }
    else{
      "larger"
    }
    staff1 <- paste0("In order to assess the effectiveness of new staffing procedures, we have conducted an analysis comparing the quality and quantity of new hires to the cost of the selection processes. As part of this analysis, we have found the validity for each of the staffing procedures. Our research indicates that the new procedure will have an operational validity of ", input$rxy2, ", which is ", ls, " than the operational validity of ", input$rxy1, " defining the old procedure. This means that the ", lowHigh, " procedure more accurately avoids bad hires.")
    staff2 <- paste0("Our cost analysis found that costs associated with administering the new procedure are $", input$cost2, " per applicant, in comparison to the cost of the old procedure which is $", input$cost1, " per applicant. To make this cost estimate more accurate, we've applied three financial adjustments to our SDy figure of $", input$sdy, ". The first is a variable cost adjustment and refers to costs that increase with hiring higher quality talent, such as compensation enhancements. These are expected to offset opportunity costs associated with this program by ", input$vcost, "%. Second, as higher performers should increase profits, returns should be taxed at the company's effective tax rate, which is ", input$tax, "%. Lastly, the cash value of increased performance over time must be discounted to the present to approximate the net present value of the program. The discount rate applied to this program is ", input$drate, "%. Since we will hire ", input$nadd, " new employees from a large selection pool each year, and the new procedure is expected to be used for at least ", input$pyears," years, implementing the new program is expected to cost our company ", costAdj(), " per year.")
    staff3 <- paste0("We found that opportunity cost is also affected by employee flows. When employees selected through the program enter and exit our company, the costs and benefits associated with the program change over time. Our research shows that employees selected using this program are expected to stay with the company an average of ", input$period, " years. This means that when we do not hire using this procedure, the opportunity cost associated with each employee is ", utilityAdjPer(), " or ", utilityAdjPerYear(), " per year they stay. Considering this along with all previous factors, our analysis has found that failing to implement this program will result in a total opportunity cost of ", utilityAdj(), ".")
    
    s1wrap <- strwrap(staff1, width = 97, indent = 8, simplify = TRUE)
    s1wrap <- paste(s1wrap, collapse = "\n")
    s2wrap <- strwrap(staff2, width = 97, indent = 8, simplify = TRUE)
    s2wrap <- paste(s2wrap, collapse = "\n")
    s3wrap <- strwrap(staff3, width = 97, indent = 8, simplify = TRUE)
    s3wrap <- paste(s3wrap, collapse = "\n")
    
    output$plainText1 <- renderText({
      staff1
    })
    output$plainText2 <- renderText({
      staff2
    })
    output$plainText3 <- renderText({
      staff3
    })
    
    elo1 <- elo()
    eln1 <- eln()
    elmo1 <- elmo()
    elmn1 <- elmn()
    eumo1 <- eumo()
    eumn1 <- eumn()
    eto1 <- eto()
    etn1 <- etn()
    
    plot_data <- reactive({
      bar_data <- data.frame(
        Quartile = rep(c("Bottom 25%", "Lower Middle 25%", "Upper Middle 25%", "Top 25%"), each = 2),
        Procedure = factor(c("Old", "New"), levels = c("Old", "New")),
        Probability = c(
          elo1, eln1,
          elmo1, elmn1,
          eumo1, eumn1,
          eto1, etn1
        )
      )
      bar_data$Quartile <- str_wrap(bar_data$Quartile, width = 10)
      max_y_value <- 1.2 * max(max(bar_data$Probability))
      title_text <- paste0("Using the ", expPro(), " improves the probability of acquiring a high performer by ", gh(), " percent.")
      wrapped_title <- strwrap(title_text, width = 100, simplify = TRUE)
      wrapped_title <- paste(wrapped_title, collapse = "\n")
      p <- ggplot(bar_data, aes(x = reorder(Quartile, Probability), y = Probability, fill = Procedure)) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(
          x = "", 
          y = "Probability of High Job Performance",
          fill = "Procedure"
        ) +
        ylim(0, max_y_value) +  # Set the y-axis limits
        scale_fill_manual(values = c("Old" = "#FF9999", "New" = "#9999FF")) +
        theme_minimal() +  # Customize the plot theme (optional)
        theme(
          plot.title = element_text(hjust = 0, margin = margin(b = 10), size = 12, lineheight = 1.2),
          axis.text = element_text(size = 12),
          axis.title.y = element_text(size = 12),
          axis.text.x = element_text(angle = 0, hjust = .5)
        ) +
        ggtitle(paste0("Figure 1\n", wrapped_title))
      
      return(p + geom_text(aes(label = paste0(Probability, "%")), vjust = -0.5, size = 4, position = position_dodge(width = 0.9)))
    })
    
    output$download_pdf <- downloadHandler(
      filename = function() {
        "staffing_report.pdf"
      },
      
      content = function(file) {
        
        title <- textGrob(
          "Staffing Report",
          x = 0.5, y = unit(0.5, "in"),
          gp = gpar(fontsize = 20),
        )
        
        par1 <- textGrob(
          s1wrap,
          x = unit(1, "in"), y = 0.62,
          gp = gpar(fontsize = 14),
          hjust = 0
        )
        
        par2 <- textGrob(
          s2wrap,
          x = unit(1, "in"), y = -0.02,
          gp = gpar(fontsize = 14),
          hjust = 0
        )
        
        par3 <- textGrob(
          s3wrap,
          x = unit(1, "in"), y = -0.76,
          gp = gpar(fontsize = 14),
          hjust = 0
        )
        
        capWrap <- strwrap(paste0("The above chart compares the expectancy of the new staffing procedure to the old staffing procedure. Expectancy is a 
             measure of the ability of a test to predict outcomes. Using the new procedure improves the chance that we acquire a high 
             performer by ", gh(), "% [(",etn(), " - ",eto(), ")/", eto() ,"] and avoid a bad hire by ", bh(), "% [(",elo(), " - ",eln(), ")/", elo() ,"]."), width = 100, simplify = TRUE)
        capWrap <- paste(capWrap, collapse = "\n")
        # Create the ggplot2 plot with a caption
        plot <- plot_data() +
          labs(
            caption = capWrap
          ) +
          theme(
            plot.caption = element_text(
              hjust = 0,
              margin = margin(t = 10, unit = "pt"),
              size = 12
            ),
            plot.margin = margin(2.25,1,1,1, "in")
          )
        
        plot_text <- arrangeGrob(title, par1, par2, par3, plot, nrow= 5, heights = unit(c(1, 1, 1, 1, 5), "null"))
        
        ggsave(
          file,
          plot_text,
          device = "pdf",
          width = 10.625,  
          height = 13.75,  
          units = "in",  
          dpi = 300  
        )
      }
    )
    
  })
  
  #Training Utility
  
  observeEvent(input$selectPage1, {
    updateTabsetPanel(session, "pageTabs", selected = "Page 1")
  })
  
  observeEvent(input$selectPage2, {
    updateTabsetPanel(session, "pageTabs", selected = "Page 2")
  })
  
  observeEvent(input$goAvolio_1, {
    updateNumericInput(session, "dTrain2_1", value = .52)
  })
  
  observeEvent(input$goSchmidt_1, {
    updateNumericInput(session, "dTrain2_1", value = .46)
  })
  
  observeEvent(input$goAvolio, {
    disable("sdP")
    updateNumericInput(session, "nTrain", value = 30, min = 0, step = 1)
    updateNumericInput(session, "tTrain", value = .167, min = 0)
    updateNumericInput(session, "dTrain2", value = .52)
    updateNumericInput(session, "sdyTrain", value = 40000, min = 0)
    updateNumericInput(session, "costTrain2", value = 2155.77, min = 0)
    updateNumericInput(session, "addTrain", value = 30, min = 0)
    updateNumericInput(session, "subTrain", value = 30, min = 0)
  })
  
  observeEvent(input$goSchmidt, {
    enable("sdP")
    updateNumericInput(session, "nTrain", value = 35, min = 0, step = 1)
    updateNumericInput(session, "tTrain", value = 5, min = 0)
    updateNumericInput(session, "dTrain2", value = .46)
    updateNumericInput(session, "sdyTrain", value = 20000, min = 0)
    updateNumericInput(session, "costTrain2", value = 200, min = 0)
    updateNumericInput(session, "addTrain", value = 35, min = 0)
    updateNumericInput(session, "subTrain", value = 35, min = 0)
  })
  #Effect Size Tab  
  observeEvent(input$goEffect,{
    option <- input$outputOpt1
    enable("effect_download")
    
    u3 <- round(pnorm(input$dTrain2_1), 2)*100
    sup <- round(pnorm(input$dTrain2_1/sqrt(2)), 2)*100
    
    dt <- input$dTrain2_1
    z1 <- 0
    z2 <- dt
    
    x <- seq(-4, 4, length = 500)
    y1 <- dnorm(x, mean = z1, sd = 1)
    y2 <- dnorm(x, mean = z2, sd = 1)
    
    # data frame
    df <- data.frame(x = x, y1 = y1, y2 = y2)
    
    gg <- ggplot(df, aes(x = x)) +
      geom_ribbon(aes(ymin = 0, ymax = y1), fill = "red", alpha = 0.3) +
      geom_ribbon(aes(ymin = 0, ymax = y2), fill = "blue", alpha = 0.3) +
      geom_line(aes(y = y1), color = "red") +
      geom_line(aes(y = y2), color = "blue") +
      geom_vline(xintercept = z1, color = "red") +
      geom_vline(xintercept = z2, color = "blue") +
      geom_segment(
        aes(xend = z2, y = 0.2, x = z1, yend = 0.2),
        arrow = arrow(type = "closed", length = unit(0.05, "inches")),
        lineend = "round",
        color = "black"
      ) +
      annotate("text", x = z2/2, y = 0.17, label = paste(dt), color = "black", size = 4) +
      annotate("text", x = z1 - 0.3, y = 0.45, label = "Untrained", color = "red", size = 4) +
      annotate("text", x = z2 + 0.3, y = 0.45, label = "Trained", color = "blue", size = 4) +
      scale_x_continuous(limits = c(-4, 4), breaks = seq(-4, 4, by = 1)) +
      ylim(0, 0.5) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0, margin = margin(b = 10), size = 16, lineheight = 1.2),
        axis.text = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        legend.position = "topright",
        legend.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
      )
    
    
    if (option == "Training Program"){
      output$training_graph_1 <- renderPlot(gg +
                                              labs(
                                                y = "",
                                                x = "Standardized Job Performance"
                                              ) +
                                              ggtitle(paste0("Figure 1\nWith an effect size of ", dt, ", failing to enact this program will result in ", u3, "% less job performance than if enacted.")))
      output$effectText <- renderText({
        paste0("The above graph shows the comparison of the effect size of a trained group to an untrained group. 
             With an effect size of ", input$dTrain2_1, ", ", u3, "% of the untrained group will be below the
             mean production value of the trained group, and there is a ", sup, "% chance that a person picked at
             random from the untrained group will have a lower production value than a person picked at random
             from the trained group.")
      })
      
      output$effect_download <- downloadHandler(
        filename = function() {
          "expectancy.pdf"  
        },
        
        content = function(file) {
          
          capWrap <- strwrap(paste0("The above graph shows the comparison of the effect size of a trained group to an untrained group. 
             With an effect size of ", input$dTrain2_1, ", ", u3, "% of the untrained group will be below the
             mean production value of the untrained group, and there is a ", sup, "% chance that a person picked at
             random from the untrained group will have a lower production value than a person picked at random
             from the trained group."), width = 100, simplify = TRUE)
          capWrap <- paste(capWrap, collapse = "\n")
          titleWrap <- strwrap(paste0("With an effect size of ", dt, ", failing to enact this program will result in ", u3, "% less job performance than if enacted."), width = 100, simplify = TRUE)
          titleWrap <- paste(titleWrap, collapse = "\n")
          plot <- gg +
            labs(
              caption = capWrap
            ) +
            theme(
              plot.caption = element_text(
                hjust = 0,
                margin = margin(t = 10, unit = "pt"),
                size = 14
              ),
              plot.margin = margin(1,0.5,0.5,0.5, "in")
            )+
            labs(
              y = "",
              x = "Standardized Job Performance"
            ) +
            ggtitle(paste0("Figure 1\n", titleWrap)) 
          
          ggsave(
            file,
            plot,
            device = "pdf",
            width = 10.625,  
            height = 6.875,  
            units = "in",  
            dpi = 300  
          )
        }
      )
      
    }
    
    else {
      output$training_graph_1 <- renderPlot(gg +
                                              labs(
                                                y = "",
                                                x = "Standardized Production"
                                              ) +
                                              ggtitle(paste0("Figure 1\nWith an effect size of ", dt, ", failing to enact this program will result in ", u3, "% less production value than if enacted.")))
      output$effectText <- renderText({
        paste0("The above graph shows the comparison of the effect size of a trained group to an untrained group. 
             With an effect size of ", input$dTrain2_1, ", ", u3, "% of the untrained group will be below the
             mean job performance value of the trained group, and there is a ", sup, "% chance that a person picked at
             random from the untrained group will have a lower job performance value than a person picked at random
             from the trained group.")
      })
      
      output$effect_download <- downloadHandler(
        filename = function() {
          "expectancy.pdf"  
        },
        
        content = function(file) {
          
          capWrap <- strwrap(paste0("The above graph shows the comparison of the effect size of a trained group to an untrained group. 
             With an effect size of ", input$dTrain2_1, ", ", u3, "% of the untrained group will be below the
             mean job performance value of the trained group, and there is a ", sup, "% chance that a person picked at
             random from the untrained group will have a lower job performance value than a person picked at random
             from the trained group."), width = 100, simplify = TRUE)
          capWrap <- paste(capWrap, collapse = "\n")
          titleWrap <- strwrap(paste0("With an effect size of ", dt, ", failing to enact this program will result in ", u3, "% less production value than if enacted."), width = 100, simplify = TRUE)
          titleWrap <- paste(titleWrap, collapse = "\n")
          plot <- gg +
            labs(
              caption = capWrap
            ) +
            theme(
              plot.caption = element_text(
                hjust = 0,
                margin = margin(t = 10, unit = "pt"),
                size = 14
              ),
              plot.margin = margin(1,0.5,0.5,0.5, "in")
            )+
            labs(
              y = "",
              x = "Standardized Production"
            ) +
            ggtitle(paste0("Figure 1\n", titleWrap))
          
          ggsave(
            file,
            plot,
            device = "pdf",
            width = 10.625,  
            height = 6.875,  
            units = "in",  
            dpi = 300  
          )
        }
      )
      
    }
    
  })
  
  #Training Utility Output Tab
  
  observeEvent(input$goUn,{
    utilityTrainUn <- input$nTrain*input$dTrain2*input$sdyTrain*input$tTrain-input$nTrain*input$costTrain2
    breakEven <- (input$nTrain*input$costTrain2)/(input$nTrain*input$tTrain*input$dTrain2)
    formatted_breakEvenTrain <- label_dollar(scale = .001, prefix= "~$", suffix = "K")(signif(breakEven, 2))
    
    formatted_utilityTrainUn <-label_dollar(scale = .001, prefix= "~$", suffix = "K")(signif(utilityTrainUn, 2))
    
    output$h3 <- renderUI({HTML('<span style="font-size: 14px; font-weight: bold;">Unadjusted Values</span>')
      
    })
    output$training_utilityUn <- renderText({
      paste("The opportunity costs of failing to use the program are:", formatted_utilityTrainUn)
    })
    output$break_even_train <- renderText({
      paste("The break even value of SDy is:", 
            formatted_breakEvenTrain)
    })
  })
  
  observeEvent(input$go4,{
    enable("training_download")
    
    u3 <- round(pnorm(input$dTrain2), 2)*100
    sup <- round(pnorm(input$dTrain2/sqrt(2)), 2)*100
    
    dt <- input$dTrain2
    z1 <- 0
    z2 <- dt
    
    x <- seq(-4, 4, length = 500)
    y1 <- dnorm(x, mean = z1, sd = 1)
    y2 <- dnorm(x, mean = z2, sd = 1)
    
    df <- data.frame(x = x, y1 = y1, y2 = y2)
    
    gg <- ggplot(df, aes(x = x)) +
      geom_ribbon(aes(ymin = 0, ymax = y1), fill = "red", alpha = 0.3) +
      geom_ribbon(aes(ymin = 0, ymax = y2), fill = "blue", alpha = 0.3) +
      geom_line(aes(y = y1), color = "red") +
      geom_line(aes(y = y2), color = "blue") +
      geom_vline(xintercept = z1, color = "red") +
      geom_vline(xintercept = z2, color = "blue") +
      geom_segment(
        aes(xend = z2, y = 0.2, x = z1, yend = 0.2),
        arrow = arrow(type = "closed", length = unit(0.05, "inches")),
        lineend = "round",
        color = "black"
      ) +
      annotate("text", x = z2/2, y = 0.17, label = paste(dt), color = "black", size = 4) +
      annotate("text", x = z1 - 0.3, y = 0.45, label = "Untrained", color = "red", size = 4) +
      annotate("text", x = z2 + 0.3, y = 0.45, label = "Trained", color = "blue", size = 4)  +
      scale_x_continuous(limits = c(-4, 4), breaks = seq(-4, 4, by = 1)) +
      ylim(0, 0.5) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0, margin = margin(b = 10), size = 16, lineheight = 1.2),
        axis.text = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        legend.position = "topright",
        legend.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
      ) 
    
    option <- input$outputOpt
    varCosts <- -input$vrateTrain/100
    tax <- input$taxTrain
    disc <- input$discTrain
    costAc <- input$costTrain2
    validAc <- input$dTrain2
    SDjp <- input$sdyTrain
    tenure1 <- input$tTrain
    last <- input$lengthTrain
    add <- input$addTrain
    subt <- input$subTrain
    
    intermediate_totDelta <- numeric(0)
    
    nk <- 0
    paytot <- 0
    paysel <- 0
    payselfir <- 0
    
    discProp <- disc / 100
    
    valid <- validAc
    
    ck <- add * (costAc)
    taxProp <- tax / 100
    discRat <- 1 / (1 + discProp)
    
    numyr <- tenure1 + last
    
    totDelta <- 0
    totDelta1 <- 0
    plot_data <- data.frame(step = c(-2, 1), intermediate_totDelta = c(0, 0))
    
    for (i in 1:numyr) {
      if (i > ceiling(tenure1)) {nk <- 0}
      if (i <= last) {nk <- nk + add}
      if (i > last) {ck <- 0}
      if (nk > 0) {
        delta1 <- nk * ((discRat^i) * (valid*add*tenure1)/nk * SDjp * (1 + varCosts) * (1 - taxProp))
        delta3 <- nk * ((discRat^i) * (valid*add*tenure1)/nk * SDjp * (-varCosts) * (taxProp))
      }
      if (nk == 0) {
        delta1 <- 0
      }
      delta2 <- ck * (1 - taxProp) * (discRat^(i - 1))
      totDelta1 <- totDelta1 + delta2 + delta3
      delta <- delta1 - delta2
      totDelta <- totDelta + delta
      
      
      intermediate_totDelta <- c(intermediate_totDelta, totDelta)
      
      # Append to plot_data
      plot_data <- rbind(plot_data, data.frame(step = i, intermediate_totDelta = totDelta))
    }
    
    trainCost <- totDelta1
    formatted_trainCost <- label_dollar(scale = .001, prefix= "~$", suffix = "K")(signif(trainCost, 2))
    
    adjusted_utility <- totDelta
    adjusted_utility_perHire <- totDelta / (last*add)
    adjusted_utility_perHire_perYear <- (totDelta / (last * add * tenure1))
    
    # Format
    formatted_adjusted_utility <- label_dollar(scale = .001, prefix= "~$", suffix = "K")(signif(adjusted_utility, 2))
    formatted_adjusted_per_hire_utility <- label_dollar(scale = .001, prefix= "~$", suffix = "K")(signif(adjusted_utility_perHire, 2))
    formatted_adjusted_per_year_utility <- label_dollar(scale = .001, prefix= "~$", suffix = "K")(signif(adjusted_utility_perHire_perYear, 2))
    
    output$h4 <- renderUI({HTML('<span style="font-size: 14px; font-weight: bold;">Adjusted Values</span>')
      
    })
    
    if (input$tTrain < 1) {
      ym <- paste(round(input$tTrain*12,0), "months")
    }
    else {
      ym <- paste(input$tTrain, "years")
    }
    
    if (option == "Training Program"){
      output$training_utility <- renderText({
        paste("Failing to use this training program will have total RODI losses to the company of:", 
              formatted_adjusted_utility,
              "in job performance.")
      })
      output$training_utility_per_employee <- renderText({
        paste("Failing to use this training program will have RODI losses to the company of:", 
              formatted_adjusted_per_hire_utility,
              "in job performance per employee.")
      })
      output$training_utility_per_employee_per_year <- renderText({
        paste("Failing to use this training program will have RODI losses to the company of:", 
              formatted_adjusted_per_year_utility,
              "in job performance per employee per year.")
      })
      
      training_text1 <- paste0("We have conducted an analysis in order to assess the return on development investment(RODI) for an employee training program.
               The program consisted of ", input$nTrain, " employees attending leadership sessions over a short number of days. Research showed that ", u3, "% of employees who did not participate in the program were assessed to have lower job performance than those who did participate."
                               )
      
      training_text2 <- paste0("Our cost analysis found that costs associated with the training program are $", input$costTrain2, " per employee. To make this cost estimate more accurate, we've applied three financial adjustments to our SDy figure of $", input$sdyTrain, ". The first is a variable cost adjustment and refers to costs that increase with higher job performance, such as compensation enhancements. These are expected to offset RODI associated with training by ", input$vrateTrain, "%. Second, as higher performers should increase profits, returns should be taxed at the company's effective tax rate, which is ", input$taxTrain, "%. Lastly, the cash value of increased performance over time must be discounted to the present to approximate the net present value of the program. The discount rate applied to this program is ", input$discTrain, "%. Since ", input$addTrain, " employees will be trained each year, and the training program is expected to be used for at least ", input$lengthTrain," years, implementing the training program is expected to cost our company ", formatted_trainCost, " per year."
                               )
      training_text3 <- paste0("We found that RODI is also affected by employee flows. When employees that attend the program lose the effects from training, the costs and benefits associated with the program change over time. Our research shows that employees that attended the program are expected to be affected for an average of ", ym, ". This means that when employees do not attend this program, the RODI loss associated with each employee is ", formatted_adjusted_per_hire_utility, " or ", formatted_adjusted_per_year_utility, " per year. Considering this along with all previous factors, our analysis has found that failing to implement this program will result in a total RODI loss of ", formatted_adjusted_utility, "."
                               )
      
      output$trainingText1 <- renderText({
        training_text1
      })
      output$trainingText2 <- renderText({
        training_text2
      })
      output$trainingText3 <- renderText({
        training_text3
      })
      
      tt1wrap <- strwrap(training_text1, width = 97, indent = 8, simplify = TRUE)
      tt1wrap <- paste(tt1wrap, collapse = "\n")
      tt2wrap <- strwrap(training_text2, width = 97, indent = 8, simplify = TRUE)
      tt2wrap <- paste(tt2wrap, collapse = "\n")
      tt3wrap <- strwrap(training_text3, width = 97, indent = 8, simplify = TRUE)
      tt3wrap <- paste(tt3wrap, collapse = "\n")
      output$trainingText <- renderText({
        training_text
      })
      
      output$training_download <- downloadHandler(
        filename = function() {
          "training_report.pdf"  # Set the filename for the downloaded file
        },
        
        content = function(file) {
          titleWrap <- strwrap(paste0("With an effect size of ", dt, ", failing to enact this program will result in ", u3, "% less job performance than if enacted."), width = 90, simplify = TRUE)
          titleWrap <- paste(titleWrap, collapse = "\n")
          capWrap <- strwrap(paste0("The above graph shows the comparison of the effect size of a trained group to an untrained group. 
             With an effect size of ", input$dTrain2_1, ", ", u3, "% of the untrained group will be below the
             mean job performance value of the trained group, and there is a ", sup, "% chance that a person picked at
             random from the untrained group will have a lower job performance value than a person picked at random
             from the trained group."), width = 97, simplify = TRUE)
          capWrap <- paste(capWrap, collapse = "\n")
          
          plot <- gg +
            labs(
              caption = capWrap
            ) +
            theme(
              plot.caption = element_text(
                hjust = 0,
                margin = margin(t = 10, unit = "pt"),
                size = 14
              ),
              plot.margin = margin(2,1,1,1, "in")
            ) +
            labs(
              y = "",
              x = "Standardized Job Performance"
            ) +
            ggtitle(paste0("Figure 1\n", titleWrap))
          title <- textGrob(
            "Training Report",
            x = 0.5, y = unit(0.5, "in"),
            gp = gpar(fontsize = 20),
          )
          
          par1 <- textGrob(
            tt1wrap,
            x = unit(1, "in"), y = .6,
            gp = gpar(fontsize = 14),
            hjust = 0
          )
          
          par2 <- textGrob(
            tt2wrap,
            x = unit(1, "in"), y = 0.1,
            gp = gpar(fontsize = 14),
            hjust = 0
          )
          
          par3 <- textGrob(
            tt3wrap,
            x = unit(1, "in"), y = -0.6,
            gp = gpar(fontsize = 14),
            hjust = 0
          )
          
          plot_text <- arrangeGrob(title, par1, par2, par3, plot, nrow= 5, heights = unit(c(1, 1, 1, 1, 5), "null"))
          
          ggsave(
            file,
            plot_text,
            device = "pdf",
            width = 10.625,  
            height = 13.75,  
            units = "in",  
            dpi = 300  
          )
        }
      )
      
    }
    else{
      
      workOut <- (input$sdP/100)*input$dTrain2
      workOutPer <- round(100*workOut, 1)
      empred <- round((1 - (1/(1+workOut)))*100,1)
      
      
      output$training_utility <- renderText({
        paste("Failing to use this program will have total lost production costs to the company of:", 
              formatted_adjusted_utility,
              "in production value.")
      })
      output$training_utility_per_employee <- renderText({
        paste("Failing to use this program will have lost production costs to the company of:", 
              formatted_adjusted_per_hire_utility,
              "in production value per employee.")
      })
      output$training_utility_per_employee_per_year <- renderText({
        paste("Failing to use this program will have lost production costs to the company of:", 
              formatted_adjusted_per_year_utility,
              "in production value per employee per year.")
      })
      
      goal_text1 <- paste0("An analysis was conducted in order to assess the production value of an employee goal setting program.
               The program consisted of ", input$nTrain, " employees who participated in setting goals for their production. Research showed that ",
                          u3, "% of employees who did not participate in the program were less productive than those who did participate."
      )
      
      goal_text2 <- paste0("Our cost analysis found that costs associated with the goal setting program are $", input$costTrain2, " per employee. To make this cost estimate more accurate, we've applied three financial adjustments to our SDy figure of $", input$sdyTrain, ". The first is a variable cost adjustment and refers to costs that increase with higher production, such as materials cost. These are expected to offset production associated with training by ", input$vrateTrain, "%. Second, as higher performers should increase profits, returns should be taxed at the company's effective tax rate, which is ", input$taxTrain, "%. Lastly, the cash value of increased production over time must be discounted to the present to approximate the net present value of the program. The discount rate applied to this program is ", input$discTrain, "%. Since ", input$addTrain, " employees will use goal setting each year, and the goal setting program is expected to be used for at least ", input$lengthTrain," years, implementing the goal setting program is expected to cost our company ", formatted_trainCost, " per year."
      )
      goal_text3 <- paste0("We found that production value is also affected by employee flows. When employees that use the goal setting program lose the effects from goal setting, the costs and benefits associated with the program change over time. Our research shows that employees that use the goal setting program are expected to be affected for an average of ", input$tTrain, " years. This means that when employees do not use this program, the production value loss associated with each employee is ", formatted_adjusted_per_hire_utility, " or ", formatted_adjusted_per_year_utility, " per year. Considering this along with all previous factors, our analysis has found that failing to implement this program will result in a total production value loss of ", formatted_adjusted_utility, ". Failure to enact the goal setting program will also impact labor costs. Employees that are" , workOutPer, "% more productive means that we will require ", empred, "% fewer workers to meet production demands."
      )
      
      output$trainingText1 <- renderText({
        goal_text1
      })
      output$trainingText2 <- renderText({
        goal_text2
      })
      output$trainingText3 <- renderText({
        goal_text3
      })
      
      gt1wrap <- strwrap(goal_text1, width = 97, indent = 8, simplify = TRUE)
      gt1wrap <- paste(gt1wrap, collapse = "\n")
      gt2wrap <- strwrap(goal_text2, width = 97, indent = 8, simplify = TRUE)
      gt2wrap <- paste(gt2wrap, collapse = "\n")
      gt3wrap <- strwrap(goal_text3, width = 97, indent = 8, simplify = TRUE)
      gt3wrap <- paste(gt3wrap, collapse = "\n")
      output$trainingText <- renderText({
        goal_text
      })
      
      output$training_download <- downloadHandler(
        filename = function() {
          "training_report.pdf"  # Set the filename for the downloaded file
        },
        
        content = function(file) {
          titleWrap <- strwrap(paste0("With an effect size of ", dt, ", failing to enact this program will result in ", u3, "% less production value than if enacted."), width = 90, simplify = TRUE)
          titleWrap <- paste(titleWrap, collapse = "\n")
          capWrap <- strwrap(paste0("The above graph shows the comparison of the effect size of a trained group to an untrained group. 
             With an effect size of ", input$dTrain2_1, ", ", u3, "% of the untrained group will be below the
             mean production value of the untrained group, and there is a ", sup, "% chance that a person picked at
             random from the untrained group will have a lower production value than a person picked at random
             from the trained group."), width = 97, simplify = TRUE)
          capWrap <- paste(capWrap, collapse = "\n")
          
          plot <- gg +
            labs(
              caption = capWrap
            ) +
            theme(
              plot.caption = element_text(
                hjust = 0,
                margin = margin(t = 10, unit = "pt"),
                size = 14
              ),
              plot.margin = margin(2,1,1,1, "in")
            )+
            labs(
              y = "",
              x = "Standardized Production"
            ) +
            ggtitle(paste0("Figure 1\n", titleWrap))
          title <- textGrob(
            "Goal Setting Report",
            x = 0.5, y = unit(.5, "in"),
            gp = gpar(fontsize = 20),
          )
          
          par1 <- textGrob(
            gt1wrap,
            x = unit(1, "in"), y = 0.8,
            gp = gpar(fontsize = 14),
            hjust = 0
          )
          
          par2 <- textGrob(
            gt2wrap,
            x = unit(1, "in"), y = 0.5,
            gp = gpar(fontsize = 14),
            hjust = 0
          )
          
          par3 <- textGrob(
            gt3wrap,
            x = unit(1, "in"), y = -0.35,
            gp = gpar(fontsize = 14),
            hjust = 0
          )
          
          plot_text <- arrangeGrob(title, par1, par2, par3, plot, nrow= 5, heights = unit(c(1, 1, 1, 1, 5), "null"))
          
          ggsave(
            file,
            plot_text,
            device = "pdf",
            width = 10.625,  
            height = 13.75,  
            units = "in",  
            dpi = 300  
          )
        }
      )
      
    }
    
  })
  
}

# Navbar
ui <- navbarPage(
  "UA+",
  tabPanel("Opening Page", opening_ui),
  tabPanel("Staffing Utility", main_ui),
  tabPanel("Training Utility", training_ui),
  tabPanel("Glossary", glossary_ui),
  tabPanel("References", reference_ui)
)

shinyApp(ui, main_server)