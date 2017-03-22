library(shiny)
library(dplyr)
library(reshape2)
library(grDevices)
library(ggplot2)
library(stringr)
library(lubridate)
library(gridExtra)
library(shinydashboard)
library(shinyBS)
library(shinyjs)
library(tidyr)
library(scales)


shinyUI(tagList(
  useShinyjs(),
  tags$head(tags$style(
    HTML('nav .container:first-child {margin-left:10px; width: 100%;}')
  )),
  fluidRow(column(
    12,
    align = "center",
    div(
      id = "loading-content",
      theme = 'flatly.css',
      icon("spinner",
           class = c("fa-spin fa-5x")),
      style = "position: absolute;
      left: 50%;
      margin-top: 50px;"
    )
    )),
  hidden(div(
    id = "NotAuthorized",
    h2("Sorry, you are not authorized to view this application.")
  )),
  hidden(div(
    id = "UtilRecon",
    navbarPage(
      title = "Shiny Demo: Skeleton",
      id = "navbarpanels",
      theme = 'flatly.css',
      
      ## Overview tab ####
      
      tabPanel(
        title = "Overview",
        h2("Title"),
        p("Content"),
        h3("SubTitle 1"),
        p("Content"),
        fixedRow(column(
          5,
          p(actionLink("link_to_timingexposure",
                       HTML(paste0("<b>", "Hyperlink 1:", "</b>")),
                       style = "color: #FFA200;")),
          p(actionLink("link_to_timingbivariateexposure",
                       HTML(paste0("<b>", "Hyperlink 2:", "</b>")),
                       style = "color: #FFA200;")),
          p(actionLink("link_to_timingdeferralcurves",
                       HTML(paste0("<b>", "Hyperlink 3:", "</b>")),
                       style = "color: #FFA200;")),
          p(actionLink("link_to_timingvariableimportance",
                       HTML(paste0("<b>", "Hyperlink 4:", "</b>")),
                       style = "color: #0081E3;")),
          p(actionLink("link_to_timingpredictions",
                       HTML(paste0("<b>", "Hyperlink 5:", "</b>")),
                       style = "color: #0081E3;")),
          p(actionLink("link_to_timingAEdeferralcurves",
                       HTML(paste0("<b>", "Hyperlink 6:", "</b>")),
                       style = "color: #64A623;")),
          p(actionLink("link_to_timingAEcommencementrates",
                       HTML(paste0("<b>", "Hyperlink 7:", "</b>")),
                       style = "color: #64A623;"))
        ),
        column(
          7,
          img(src = "skeleton.jpg", style = "width: 178px; height: 150px")
        )),
        h3("SubTitle 2"),
        p("Content"),
        fixedRow(column(
          5,
          p(actionLink("link_to_efficiencyexposure",
                       HTML(paste0("<b>", "Hyperlink 1:", "</b>")),
                       style = "color: #FFA200;")),
          p(actionLink("link_to_efficiencypredictions",
                       HTML(paste0("<b>", "Hyperlink 2:", "</b>")),
                       style = "color: #0081E3;")),
          p(actionLink("link_to_efficiencyAEprobabilities",
                       HTML(paste0("<b>", "Hyperlink 3:", "</b>")),
                       style = "color: #64A623;"))
        ),
        column(
          7,
          img(src = "skeleton.jpg", style = "width: 178px; height: 150px")
        )),
        br(),
        p(
          br(),
          a("Copyright information", href = "http://milliman.aiprx.com/legal/copyright/"),
          ' | ',
          a("Terms of use", href = "http://milliman.aiprx.com/legal/terms/"),
          ' | ',
          a("Privacy policy", href = "http://milliman.aiprx.com/legal/privacy/")
        ),
        HTML("&#169;", "2017 Milliman. All rights reserved."),
        br()
      ),
      
      ## Explore Data tabs ####
      
      navbarMenu(
        'Explore Data',
        
        ## Timing: Data: Exposure ####
        
        tabPanel(
          title = "Data: Exposure", icon = icon("bar-chart-o"), value = "timingexposure",
          sidebarLayout(
            sidebarPanel(
              helpText("Helptext"),
              selectInput("exptimingvar", label = "Variable", choices = c(1, 2, 3, 4), selected = 1)
            ),
            mainPanel(
              bsCollapsePanel(
                title = div(icon("bar-chart-o"), "Warning Tab"), style = "warning", "Content"),
              tabsetPanel(
                tabPanel(title = "Industry", img(src = "skeleton.jpg")),
                tabPanel(title = "Company", img(src = "skeleton.jpg"))
              ),
              p(
                br(),
                br(),
                br(),
                a("Copyright information", href =
                    "http://milliman.aiprx.com/legal/copyright/"),
                ' | ',
                a("Terms of use", href = "http://milliman.aiprx.com/legal/terms/"),
                ' | ',
                a("Privacy policy", href = "http://milliman.aiprx.com/legal/privacy/")
              ),
              HTML("&#169;", "2017 Milliman. All rights reserved."),
              br()
            )
          )
        ),
        
        ## Timing: Data: Bivariate exposure ####
        
        tabPanel(
          title = "Data: Bivariate exposure", icon = icon("bar-chart-o"), value = "timingbivariateexposure",
          sidebarLayout(
            sidebarPanel(
              helpText("HelpText"),
              selectInput("bivexptimingvar", label = "Variable", choices = c(1, 2, 3, 4), selected = 1)
            ),
            mainPanel(
              bsCollapsePanel(
                title = div(icon("bar-chart-o"), "Warning Tab"), style = "warning", "Content"),
              tabsetPanel(
                tabPanel(title = 'Industry', img(src = 'skeleton.jpg')),
                tabPanel(title = 'Company', img(src = 'skeleton.jpg'))
              ),
              p(
                br(),
                br(),
                br(),
                a("Copyright information", href =
                    "http://milliman.aiprx.com/legal/copyright/"),
                ' | ',
                a("Terms of use", href = "http://milliman.aiprx.com/legal/terms/"),
                ' | ',
                a("Privacy policy", href = "http://milliman.aiprx.com/legal/privacy/")
              ),
              HTML("&#169;", "2017 Milliman. All rights reserved."),
              br()
            )
          )
        ),
        
        ## Timing: Data: Deferral curves ####
        
        tabPanel(
          title = "Data: Deferral curves",
          icon = icon("bar-chart-o"),
          value = "timingdeferralcurves",
          sidebarLayout(
            sidebarPanel(helpText("HelpText"),
                         fluidRow(
                           column(
                             6,
                             checkboxGroupInput(
                               "deferralIssAgeBuckets",
                               label = "Variable 1:",
                               choices = c(1, 2, 3, 4),
                               selected = 1
                             )
                           ),
                           column(
                             6,
                             radioButtons(
                               inputId = "deferralSEind",
                               label = 'Variable 2:',
                               choices = c(1, 2),
                               selected = 1
                             ),
                             checkboxGroupInput(
                               "deferralqualvalue",
                               label = "Variable 3:",
                               choices = c(1, 2),
                               selected = 1
                             ),
                             checkboxGroupInput(
                               "deferraljointvalue",
                               label = "Variable 4:",
                               choices = c(1, 2),
                               selected = 1
                             ),
                             div(style = "display:inline-block; float:right",
                                 actionButton(
                                   "DefCurvesGo",
                                   label = "Plot",
                                   icon = icon("picture-o")
                                 ))
                           )
                         )),
            
            
            mainPanel(
              bsCollapsePanel(
                title = div(icon("bar-chart-o"), "Warning Tab"),
                style = "warning",
                "Content"
              ),
              tabsetPanel(
                tabPanel(title = "Industry",
                         img(src = 'skeleton.jpg')),
                tabPanel(title = "Company",
                         img(src = 'skeleton.jpg'))
              ),
              p(
                br(),
                br(),
                br(),
                a("Copyright information", href =
                    "http://milliman.aiprx.com/legal/copyright/"),
                ' | ',
                a("Terms of use", href = "http://milliman.aiprx.com/legal/terms/"),
                ' | ',
                a("Privacy policy", href = "http://milliman.aiprx.com/legal/privacy/")
              ),
              HTML("&#169;", "2017 Milliman. All rights reserved."),
              br()
            )
          )
        ),
        
        ## Timing: Model: Variable importance ####
        
        tabPanel(
          title = "Model: Variable importance",
          icon = icon("cog", class = "fa-spin"),
          value = "timingvariableimportance",
          sidebarLayout(
            sidebarPanel(
              helpText("HelpText"),
              icon = icon("cog", class = "fa-spin"),
              checkboxGroupInput(
                "TornadoCategories",
                label = "Variable 1:",
                choices = c(1, 2, 3, 4, 5, 6),
                selected = 1
              )
            ),
            mainPanel(
              bsCollapsePanel(
                title = div(icon("cog", class = "fa-spin"), "Information Tab"),
                style = "info",
                "Content"
              ),
              img(src = 'skeleton.jpg'),
              p(
                br(),
                br(),
                br(),
                a("Copyright information", href =
                    "http://milliman.aiprx.com/legal/copyright/"),
                ' | ',
                a("Terms of use", href = "http://milliman.aiprx.com/legal/terms/"),
                ' | ',
                a("Privacy policy", href = "http://milliman.aiprx.com/legal/privacy/")
              ),
              HTML("&#169;", "2017 Milliman. All rights reserved."),
              br()
            )
          )
        ),
        
        ## Timing: Model: Predictions ####
        
        tabPanel(
          title = "Model: Predictions",
          icon = icon("cog", class = "fa-spin"),
          value = "timingpredictions",
          sidebarLayout(
            sidebarPanel(
              helpText("HelpText"),
              fluidRow(column(
                4,
                # Model select
                radioButtons(
                  inputId = 'modelchoicepredtab',
                  label = 'Select model:',
                  choices = c('Baseline', 'Expanded'),
                  selected = 'Baseline'
                )
              ),
              column(
                8,
                
                # Split by...
                selectInput(
                  inputId = 'splitbypredvar',
                  label = 'Variable:',
                  choices = c(1, 2, 3, 4),
                  selected = 1
                )
              )),
              
              helpText("HelpText"),
              
              fluidRow(
                column(
                  6,
                  
                  # Issue age
                  numericInput(
                    inputId = 'issagepredvalue',
                    label = 'Variable 1:',
                    value = 50,
                    min = 0,
                    max = 100
                  ),
                  
                  conditionalPanel(
                    condition = "input.modelchoicepredtab == 'Expanded'",
                    
                    # Account value
                    numericInput(
                      'avpredvalue',
                      label = 'Variable 2:',
                      value = 50,
                      min = 0,
                      max = 100
                    ),
                    
                    # ITM
                    numericInput(
                      'itmpredvalue',
                      label = 'Variable 3:',
                      value = 50,
                      min = 0,
                      max = 100
                    )
                  )
                ),
                column(
                  6,
                  
                  # GLWB minimum age
                  numericInput(
                    inputId = 'minagepredvalue',
                    label = 'Variable 4:',
                    value = 50,
                    min = 0,
                    max = 100
                  ),
                  
                  conditionalPanel(
                    condition = "input.modelchoicepredtab == 'Expanded'",
                    
                    # Rollup period
                    numericInput(
                      "rollupdurpredvalue",
                      label = 'Variable 5:',
                      value = 50,
                      min = 0,
                      max = 100
                    ),
                    
                    # MAW% Potential
                    numericInput(
                      'mawapotentialpredvalue',
                      label = 'Variable 6:',
                      value = 50,
                      min = 0,
                      max = 100,
                      step = 1
                    )
                  )
                )
              ),
              
              conditionalPanel(
                condition = "input.modelchoicepredtab == 'Expanded'",
                
                # Withdrawals to date
                numericInput(
                  'wdtodatepredvalue',
                  label = 'Variable 7:',
                  value = 0,
                  min = 0,
                  max = 100000
                )
              ),
              
              fluidRow(
                column(
                  6,
                  
                  # Tax qualification status
                  selectInput(
                    "qualpredvalue",
                    label = 'Variable 8:',
                    choices = c(1, 2),
                    selected = 1
                  ),
                  
                  conditionalPanel(
                    condition = "input.modelchoicepredtab == 'Expanded'",
                    
                    # Distribution
                    selectInput(
                      'distpredvalue',
                      label = 'Variable 9:',
                      choices = c(1, 2, 3, 4),
                      selected = 1
                    )
                  )
                ),
                column(
                  6,
                  
                  # Joint status
                  selectInput(
                    "jointpredvalue",
                    label = 'Variable 10:',
                    choices = c(1, 2),
                    selected = 1
                  ),
                  
                  conditionalPanel(
                    condition = "input.modelchoicepredtab == 'Expanded'",
                    
                    # Ratchet indicator
                    selectInput(
                      'ratchetfreqpredvalue',
                      label = 'Variable 11:',
                      choices = c(1, 2),
                      selected = 1
                    )
                  )
                )
              ),
              fluidRow(
                div(style = "display:inline-block; float:right",
                    actionButton(
                      "PredGo",
                      label = "Plot",
                      icon = icon("picture-o")
                    ))
              )
            ),
            
            # Produce prediction plot
            mainPanel(
              bsCollapsePanel(
                title = div(icon("cog", class = "fa-spin"), "Info Tab"),
                style = "info",
                "Content"
              ),
              tabsetPanel(tabPanel(title = 'Plot',
                                   img(src = 'skeleton.jpg')),
                          tabPanel(title = 'Table',
                                   img(src = 'skeleton.jpg'))),
              p(
                br(),
                br(),
                br(),
                a("Copyright information", href =
                    "http://milliman.aiprx.com/legal/copyright/"),
                ' | ',
                a("Terms of use", href = "http://milliman.aiprx.com/legal/terms/"),
                ' | ',
                a("Privacy policy", href = "http://milliman.aiprx.com/legal/privacy/")
              ),
              HTML("&#169;", "2017 Milliman. All rights reserved."),
              br()
            )
          )
        ),
        
        ## Timing: Validation: A+E deferral curves ####
        
        tabPanel(
          title = "Validation: A vs. E deferral curves",
          icon = icon("check-square-o"),
          value = "timingAEdeferralcurves",
          sidebarLayout(
            sidebarPanel(
              helpText("HelpText"),
              fluidRow(
                column(
                  6,
                  checkboxGroupInput(
                    "deferralAEIssAgeBuckets",
                    label = "Variable 1:",
                    #choices = c('[0,50)', '[50,55)', '[55,60)', '[60,65)', '[65,70)', '[70,75)', '[75,80)', '[80,110)'),
                    #selected = c('[0,50)', '[50,55)', '[55,60)', '[60,65)', '[65,70)', '[70,75)', '[75,80)', '[80,110)')),
                    choices = c(1, 2, 3, 4),
                    selected = 1
                  )
                ),
                column(
                  6,
                  radioButtons(
                    inputId = 'deferralAEPreds',
                    label = 'Variable 2:',
                    choices = c(1, 2),
                    selected = 1
                  ),
                  checkboxGroupInput(
                    "deferralAEqualvalue",
                    label = "Variable 3:",
                    choices = c(1, 2),
                    selected = 1
                  ),
                  checkboxGroupInput(
                    "deferralAEjointvalue",
                    label = "Variable 4:",
                    choices = c(1, 2),
                    selected = 1
                  )
                )
              ),
              fluidRow(
                div(
                  style = "display:inline-block; float:right",
                  actionButton(
                    "AEDefCurvesGo",
                    label = "Plot",
                    icon = icon("picture-o")
                  )
                )
              )
            ),
            # radioButtons(inputId = "deferralAESEind",
            #              label = 'Standard errors:',
            #              choices = c(Show = TRUE, Hide = FALSE),
            #              selected = TRUE)),
            
            mainPanel(
              bsCollapsePanel(
                title = div(icon("check-square-o"), "Success Tab"),
                style = "success",
                "Content"
              ),
              img(src = 'skeleton.jpg'),
              
              p(
                br(),
                br(),
                br(),
                a("Copyright information", href =
                    "http://milliman.aiprx.com/legal/copyright/"),
                ' | ',
                a("Terms of use", href = "http://milliman.aiprx.com/legal/terms/"),
                ' | ',
                a("Privacy policy", href = "http://milliman.aiprx.com/legal/privacy/")
              ),
              HTML("&#169;", "2017 Milliman. All rights reserved."),
              br()
            )
          )
        ),
        
        ## Timing: Validation: A+E commencement rates ####
        
        tabPanel(
          title = "Validation: A vs. E commencement rates",
          icon = icon("check-square-o"),
          value = "timingAEcommencementrates",
          sidebarLayout(
            sidebarPanel(
              helpText("HelpText"),
              radioButtons(
                'commenceAEmodel',
                label = 'Variable 1:',
                choices = c(1, 2),
                selected = 1
              ),
              radioButtons('AECompType', 'Variable 2:', c(1, 2, 3), selected = 1),
              helpText("HelpText"),
              selectInput(
                "commenceAEvar",
                label = "Variable 3:",
                choices = c(1, 2, 3, 4),
                selected = 1
              )
            ),
            mainPanel(
              bsCollapsePanel(
                title = div(icon("check-square-o"), "Success Tab"),
                style = "success",
                "COntent"
              ),
              img(src = 'skeleton.jpg'),
              
              p(
                br(),
                br(),
                br(),
                a("Copyright information", href =
                    "http://milliman.aiprx.com/legal/copyright/"),
                ' | ',
                a("Terms of use", href = "http://milliman.aiprx.com/legal/terms/"),
                ' | ',
                a("Privacy policy", href = "http://milliman.aiprx.com/legal/privacy/")
              ),
              HTML("&#169;", "2017 Milliman. All rights reserved."),
              br()
            )
          )
        )
      ),
      
      ## Explore Segments Tabs ####
      
      navbarMenu(
        "Explore Segments",
        
        ## Efficiency: Data: Exposure ####
        
        tabPanel(
          title = "Data: Exposure",
          icon = icon("bar-chart-o"),
          value = "efficiencyexposure",
          sidebarLayout(
            sidebarPanel(
              helpText("HelpText"),
              selectInput(
                "EffExpvar1",
                label = "Variable",
                choices = c(1, 2, 3, 4),
                selected = 1
              )
            ),
            mainPanel(
              bsCollapsePanel(
                title = div(icon("bar-chart-o"), "Warning Tab"),
                style = "warning",
                "Content"
              ),
              tabsetPanel(
                tabPanel(title = "Industry",
                         img(src = 'skeleton.jpg')),
                tabPanel(title = "Company",
                         img(src = 'skeleton.jpg'))
              ),
              p(
                br(),
                br(),
                br(),
                a("Copyright information", href =
                    "http://milliman.aiprx.com/legal/copyright/"),
                ' | ',
                a("Terms of use", href = "http://milliman.aiprx.com/legal/terms/"),
                ' | ',
                a("Privacy policy", href = "http://milliman.aiprx.com/legal/privacy/")
              ),
              HTML("&#169;", "2017 Milliman. All rights reserved."),
              br()
            )
          )
        ),
        
        ## Efficiency: Model: Predictions ####
        
        tabPanel(
          title = "Model: Predictions",
          icon = icon("cog", class = "fa-spin"),
          value = "efficiencypredictions",
          sidebarLayout(
            sidebarPanel(
              helpText("HelpText"),
              selectInput(
                "Effvar1",
                label = "Variable 1:",
                choices = c(1, 2),
                selected = 1
              ),
              selectInput(
                "Effvar2",
                label = "Variable 2:",
                choices = c(1, 2),
                selected = 1
              )
            ),
            mainPanel(
              bsCollapsePanel(
                title = div(icon("cog", class = "fa-spin"), "Info Tab"),
                style = "info",
                "COntent"
              ),
              tabsetPanel(tabPanel(title = "Plot",
                                   img(src = 'skeleton.jpg')),
                          tabPanel(title = "Table",
                                   img(src = 'skeleton.jpg'))),
              p(
                br(),
                br(),
                br(),
                a("Copyright information", href =
                    "http://milliman.aiprx.com/legal/copyright/"),
                ' | ',
                a("Terms of use", href = "http://milliman.aiprx.com/legal/terms/"),
                ' | ',
                a("Privacy policy", href = "http://milliman.aiprx.com/legal/privacy/")
              ),
              HTML("&#169;", "2017 Milliman. All rights reserved."),
              br()
            )
          )
        ),
        
        ## Efficiency: Validation: A+E probabilities ####
        
        tabPanel(
          title = "Validation: A vs. E probabilities",
          icon = icon("check-square-o"),
          value = "efficiencyAEprobabilities",
          sidebarLayout(
            sidebarPanel(
              helpText("HelpText"),
              checkboxGroupInput(
                "EfficiencyAEVars",
                label = "Variable 1:",
                choices = c(1, 2, 3, 4),
                selected = 1
              )
            ),
            mainPanel(
              bsCollapsePanel(
                title = div(icon("check-square-o"), "Success Tab"),
                style = "success",
                "Content"
              ),
              tags$style(
                type = "text/css",
                ".shiny-output-error { visibility: hidden; }",
                ".shiny-output-error:before { visibility: hidden; }"
              ),
              tabsetPanel(
                tabPanel(title = "Actual/expected ratio",
                         img(src = 'skeleton.jpg')),
                tabPanel(title = "Actual-expected difference",
                         img(src = 'skeleton.jpg')),
                tabPanel(title = "Actual and expected",
                         img(src = 'skeleton.jpg'))
              ),
              p(
                br(),
                br(),
                br(),
                a("Copyright information", href =
                    "http://milliman.aiprx.com/legal/copyright/"),
                ' | ',
                a("Terms of use", href = "http://milliman.aiprx.com/legal/terms/"),
                ' | ',
                a("Privacy policy", href = "http://milliman.aiprx.com/legal/privacy/")
              ),
              HTML("&#169;", "2017 Milliman. All rights reserved."),
              br()
            )
          )
        )
      ),
      
      ## Explore Lapse Models ####
      
      navbarMenu(
        "Explore Lapse Models",
        
        ## Explore Lapse Models: Data: Exposure ####
        
        tabPanel(
          title = "Data: Exposure", icon = icon("bar-chart-o"), value = "efficiencyexposure",
          sidebarLayout(
            sidebarPanel(
              helpText("HelpText"),
              selectInput("EffExpvar11", label = "Variable", choices = c(1, 2, 3, 4), selected = 1)
            ),
            mainPanel(
              bsCollapsePanel(
                title = div(icon("bar-chart-o"), "Warning Tab"), style = "warning", "Content"),
              tabsetPanel(
                tabPanel(title = "Industry", img(src = 'skeleton.jpg')),
                tabPanel(title = "Company", img(src = 'skeleton.jpg'))
              ),
              p(
                br(),
                br(),
                br(),
                a("Copyright information", href =
                    "http://milliman.aiprx.com/legal/copyright/"),
                ' | ',
                a("Terms of use", href = "http://milliman.aiprx.com/legal/terms/"),
                ' | ',
                a("Privacy policy", href = "http://milliman.aiprx.com/legal/privacy/")
              ),
              HTML("&#169;", "2017 Milliman. All rights reserved."),
              br()
            )
          )
        ),
        
        ## Explore Lapse Models: Model: Predictions ####
        
        tabPanel(
          title = "Model: Predictions",
          icon = icon("cog", class = "fa-spin"), value = "efficiencypredictions",
          sidebarLayout(
            sidebarPanel(
              helpText("HelpText"),
              selectInput("Effvar11", label = "Variable 1:", choices = c(1, 2), selected = 1),
              selectInput("Effvar12", label = "Variable 2:", choices = c(1, 2), selected = 1)
            ),
            mainPanel(
              bsCollapsePanel(
                title = div(icon("cog", class = "fa-spin"), "Info Tab"), style = "info", "COntent"),
              tabsetPanel(
                tabPanel(title = "Plot", img(src = 'skeleton.jpg')),
                         tabPanel(title = "Table", img(src = 'skeleton.jpg'))),
              p(
                br(),
                br(),
                br(),
                a("Copyright information", href =
                    "http://milliman.aiprx.com/legal/copyright/"),
                ' | ',
                a("Terms of use", href = "http://milliman.aiprx.com/legal/terms/"),
                ' | ',
                a("Privacy policy", href = "http://milliman.aiprx.com/legal/privacy/")
              ),
              HTML("&#169;", "2017 Milliman. All rights reserved."),
              br()
            )
          )
        ),
        
        ## Explore Lapse Models: Validation: A+E probabilities ####
        
        tabPanel(
          title = "Validation: A vs. E probabilities", icon = icon("check-square-o"), value = "efficiencyAEprobabilities",
          sidebarLayout(
            sidebarPanel(
              helpText("HelpText"),
              checkboxGroupInput("EfficiencyAEVars1", label = "Variable 1:", choices = c(1, 2, 3, 4), selected = 1)
            ),
            mainPanel(
              bsCollapsePanel(
                title = div(icon("check-square-o"), "Success Tab"), style = "success", "Content"
              ),
              tags$style(
                type = "text/css",
                ".shiny-output-error { visibility: hidden; }",
                ".shiny-output-error:before { visibility: hidden; }"
              ),
              tabsetPanel(
                tabPanel(title = "Actual/expected ratio", img(src = 'skeleton.jpg')),
                tabPanel(title = "Actual-expected difference", img(src = 'skeleton.jpg')),
                tabPanel(title = "Actual and expected", img(src = 'skeleton.jpg'))
              ),
              p(
                br(),
                br(),
                br(),
                a("Copyright information", href =
                    "http://milliman.aiprx.com/legal/copyright/"),
                ' | ',
                a("Terms of use", href = "http://milliman.aiprx.com/legal/terms/"),
                ' | ',
                a("Privacy policy", href = "http://milliman.aiprx.com/legal/privacy/")
              ),
              HTML("&#169;", "2017 Milliman. All rights reserved."),
              br()
            )
          )
        )
      ),
      
      ## Explore Profitability Models ####
      
      navbarMenu(
        "Explore Profitability Models",
        
        ## Explore Profitability Models: Data: Exposure ####
        
        tabPanel(
          title = "Data: Exposure", icon = icon("bar-chart-o"), value = "efficiencyexposure",
          sidebarLayout(
            sidebarPanel(
              helpText("HelpText"),
              selectInput("EffExpvar21", label = "Variable", choices = c(1, 2, 3, 4), selected = 1)
            ),
            mainPanel(
              bsCollapsePanel(
                title = div(icon("bar-chart-o"), "Warning Tab"), style = "warning", "Content"),
              tabsetPanel(
                tabPanel(title = "Industry", img(src = 'skeleton.jpg')),
                tabPanel(title = "Company", img(src = 'skeleton.jpg'))
              ),
              p(
                br(),
                br(),
                br(),
                a("Copyright information", href =
                    "http://milliman.aiprx.com/legal/copyright/"),
                ' | ',
                a("Terms of use", href = "http://milliman.aiprx.com/legal/terms/"),
                ' | ',
                a("Privacy policy", href = "http://milliman.aiprx.com/legal/privacy/")
              ),
              HTML("&#169;", "2017 Milliman. All rights reserved."),
              br()
            )
          )
        ),
        
        ## Explore Profitability Models: Model: Predictions ####
        
        tabPanel(
          title = "Model: Predictions",
          icon = icon("cog", class = "fa-spin"), value = "efficiencypredictions",
          sidebarLayout(
            sidebarPanel(
              helpText("HelpText"),
              selectInput("Effvar21", label = "Variable 1:", choices = c(1, 2), selected = 1),
              selectInput("Effvar22", label = "Variable 2:", choices = c(1, 2), selected = 1)
            ),
            mainPanel(
              bsCollapsePanel(
                title = div(icon("cog", class = "fa-spin"), "Info Tab"), style = "info", "COntent"),
              tabsetPanel(
                tabPanel(title = "Plot", img(src = 'skeleton.jpg')),
                tabPanel(title = "Table", img(src = 'skeleton.jpg'))),
              p(
                br(),
                br(),
                br(),
                a("Copyright information", href =
                    "http://milliman.aiprx.com/legal/copyright/"),
                ' | ',
                a("Terms of use", href = "http://milliman.aiprx.com/legal/terms/"),
                ' | ',
                a("Privacy policy", href = "http://milliman.aiprx.com/legal/privacy/")
              ),
              HTML("&#169;", "2017 Milliman. All rights reserved."),
              br()
            )
          )
        ),
        
        ## Explore Profitability Models: Validation: A+E probabilities ####
        
        tabPanel(
          title = "Validation: A vs. E probabilities", icon = icon("check-square-o"), value = "efficiencyAEprobabilities",
          sidebarLayout(
            sidebarPanel(
              helpText("HelpText"),
              checkboxGroupInput("EfficiencyAEVars2", label = "Variable 1:", choices = c(1, 2, 3, 4), selected = 1)
            ),
            mainPanel(
              bsCollapsePanel(
                title = div(icon("check-square-o"), "Success Tab"), style = "success", "Content"
              ),
              tags$style(
                type = "text/css",
                ".shiny-output-error { visibility: hidden; }",
                ".shiny-output-error:before { visibility: hidden; }"
              ),
              tabsetPanel(
                tabPanel(title = "Actual/expected ratio", img(src = 'skeleton.jpg')),
                tabPanel(title = "Actual-expected difference", img(src = 'skeleton.jpg')),
                tabPanel(title = "Actual and expected", img(src = 'skeleton.jpg'))
              ),
              p(
                br(),
                br(),
                br(),
                a("Copyright information", href =
                    "http://milliman.aiprx.com/legal/copyright/"),
                ' | ',
                a("Terms of use", href = "http://milliman.aiprx.com/legal/terms/"),
                ' | ',
                a("Privacy policy", href = "http://milliman.aiprx.com/legal/privacy/")
              ),
              HTML("&#169;", "2017 Milliman. All rights reserved."),
              br()
            )
          )
        )
      )
      
    )
  ))
    ))