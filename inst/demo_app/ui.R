# Define UI for application that draws a histogram
navbarPage(title = HTML(paste0('<p style="margin-top: 0.05cm;">', paste0(rep("&nbsp;", 25), collapse = ""), '&nbspshinymodules</p>')), id = "nav-id", collapsible = TRUE,
           position = "fixed-top", theme = "css/custom.css",
           header = div(
             br(), br(), br(), br(),
             a(href = "https://www.datastorm.fr",
               target = "_blank", img(src = "img/img-datastorm-logo-white.png", class = "ribbon", style = "margin-left: 0cm;margin-top: 0.1cm;height: 55px")),
             a(href = "https://github.com/datastorm-open/shinymodules",
               target = "_blank", img(src = "img/github.png", class = "ribbon", style = "margin-left: 3cm;margin-top: 0cm;height: 60px")),
             # footer
             div(class = "ds_app_footer", div(p("copyright © Datastorm 2021", style = "color:white"), align = "center")),
           ),
           windowTitle = "shinymodules",
           tabPanel("Introduction",
                    includeMarkdown("www/script/intro.md")
           ),
           tabPanel("Data exploration",
                    tabsetPanel(
                      tabPanel("Demo",
                               # filter
                               filter_data_UI(id = "filter", titles = TRUE),
                               hr(),
                               # summary
                               summary_data_UI(id = "sum_data"),
                               hr(),
                               h2("Visualization"),
                               # graphics
                               conditionalPanel(condition = "output.enable_esquisse !== undefined && output.enable_esquisse",
                                                esquisse_ui(id = "esquisse",
                                                            header = FALSE,
                                                            controls = c("labs", "parameters", "appearance", "code"),
                                                            container = esquisseContainer(height = "700px"))
                                                
                                                
                               ),
                               conditionalPanel(condition = "output.enable_esquisse !== undefined && output.enable_esquisse === false",
                                                div(h3("Too much rows (> 50K) in filtering data for esquisse module", style = "color : red"), align = "center")
                               )
                      ),
                      tabPanel("Code", includeMarkdown("www/script/data_explor.md"))
                    )
                    
           ),
           tabPanel("Monitoring",
                    tabsetPanel(
                      tabPanel("Demo",
                               monitoring_data_UI(id = "monitoring")
                      ),
                      tabPanel("Code", includeMarkdown("www/script/monitoring.md"))
                    )
           ),
           br(), br(), br()
)
           