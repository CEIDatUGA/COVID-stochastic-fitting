# Load packages
library(dplyr)
library(tidyr)
library(readr)
library(here)
library(shiny)
library(shinyWidgets)
library(ggplot2)
library(plotly)
library(RColorBrewer)

#################################
# Load all data
# should be online (e.g. in Github repo) so things update automatically
# for speed, we only get data from the online source if the data is old, otherwise we load locally
#################################

# to ensure data gets refreshed on server, we need this
get_data <- function()
{
  #if the data file for today is here, load then return from function
  filename = here("data",paste0("clean_data_",Sys.Date(),'.rds')) 
  if (file.exists(filename)) {
     all_data <- readRDS(filename)    
     return(all_data)  
  }
  #if data file is not here, go through all of the below
  #not much needed here, just reading of data and a bit of processing
  #data for population size for each state/country so we can compute cases per 100K
  us_popsize <- readRDS(here("data","us_popsize.rds")) %>% rename(state_abr = state, state = state_full)
  
  all_data = list() #will save and return all datasets as list
  
  #################################
  # pull data from our repository and process
  #################################
  #get list of files
  
  #for each state, get latest file
  
  #download all files, combine contents
  
  raw_data <- readr::read_csv("https://raw.githubusercontent.com/CEIDatUGA/covid-19-data/ /.rds")

  #save the data
  saveRDS(all_data, filename)    
  
  return(all_data)
  
}  

###########################################
# function that re-reads the data every so often
###########################################
all_data_reactive <- reactivePoll(intervalMillis = 1000*60*60*3, # pull new data every N hours
                         session = NULL,
                         checkFunc = function() {Sys.time()}, #this will always return a different value, which means at intervals specified by intervalMillis the new data will be pulled
                         valueFunc = function() {get_data()} )

#read data is reactive, doesn't work for rest below 
all_data = isolate(all_data_reactive())


#################################
# Define UI
#################################
ui <- fluidPage(
  #tags$head(includeHTML(here("www","google-analytics.html"))), #this is for Google analytics tracking.
  includeCSS(here("www","appstyle.css")),
  #main tabs
  navbarPage( title = "COVID-19 Forecast", id = 'alltabs', selected = "us", header = "",
              tabPanel(title = "US", value = "us",
                       sidebarLayout(
                         sidebarPanel(
                           shinyWidgets::pickerInput("state_selector", "Select State(s)", state_var, multiple = TRUE,options = list(`actions-box` = TRUE), selected = c("Georgia","California","Washington") ),
                           shiny::selectInput( "case_death",   "Outcome",c("Cases" = "Cases", "Hospitalizations" = "Hospitalized", "Deaths" = "Deaths")),
                           shiny::div("Modify the top plot to display cases, hospitalizations, or deaths."),
                           br(),
                           shiny::selectInput("daily_tot", "Daily or cumulative numbers", c("Daily" = "Daily", "Total" = "Total" )),
                           shiny::div("Modify all three plots to show daily or cumulative data."),
                           br(),
                           shiny::selectInput( "absolute_scaled","Absolute or scaled values",c("Absolute Number" = "actual", "Per 100,000 persons" = "scaled") ),
                           shiny::div("Modify the top two plots to display total counts or values scaled by the state/territory population size."),
                           br(),
                           shiny::selectInput("xscale", "Set x-axis to calendar date or days since a specified total number of cases/hospitalizations/deaths", c("Calendar Date" = "x_time", "Days since N cases/hospitalizations/deaths" = "x_count")),
                           sliderInput(  inputId = "count_limit", "Choose the number of cases/hospitalizations/deaths at which to start graphs", min = 1,  max = 500, value = 10 ),
                           shiny::div("Modify all three plots to show data with x-axis as calender date or days since a state reported a specified total number of cases/hospitalizations/deaths, specified by the slider above. The slider above does not have an impact for calendar date on the x-axis."),
                           br(),
                           shiny::selectInput(  "yscale", "Y-scale", c("Linear" = "lin", "Logarithmic" = "log")),
                           shiny::div("Modify the top two plots to show data on a linear or logarithmic scale."),
                           br()
                         ),         #end sidebar panel
                         # Output:
                         mainPanel(
                           #change to plotOutput if using static ggplot object
                           plotlyOutput(outputId = "case_death_plot", height = "300px")
                         ) #end main panel
                       ) #end sidebar layout     
              ), #close US tab
              
              tabPanel( title = "About", value = "about",
                        tagList(    
                          fluidRow( #all of this is the header
                            tags$div(
                              id = "bigtext",
                              "This COVID-19 forecasting app is brought to you by the",
                              a("Center for the Ecology of Infectious Diseases",  href = "https://ceid.uga.edu", target = "_blank" ),
                              "and the",
                              a("College of Public Health", href = "https://publichealth.uga.edu", target = "_blank"),
                              "at the",
                              a("University of Georgia.", href = "https://www.uga.edu", target = "_blank"),
                              "It was developed by",
                              a("John Drake,", href = "https://daphnia.ecology.uga.edu/drakelab/", target =  "_blank"),
                              a("William Norfolk", href = "https://github.com/williamnorfolk", target = "_blank"),
                              "and ",
                              a("Andreas Handel.", href = "https://www.andreashandel.com/", target = "_blank"),
                              'Source code for this project can be found',
                              a( "in this GitHub repository.", href = "https://github.com/CEIDatUGA/COVID-shiny-tracker", target = "_blank" ),
                              'We welcome feedback and feature requests, please send them as a',
                              a( "GitHub Issue", href = "https://github.com/CEIDatUGA/COVID-shiny-tracker/issues", target = "_blank" ),
                              'or contact',
                              a("Andreas Handel.", href = "https://www.andreashandel.com/", target = "_blank")
                            ),# and tag
                            tags$div(
                              id = "bigtext",
                              "The main underlying data for is sourced from",
                              a("The Covid Tracking Project.",  href = "https://covidtracking.com/", target = "_blank" ),
                              "This data source reports all and positive tests, hospitalizations and deaths for each state. We interpret positive tests as corresponding to new cases. Additional US data (cases and deaths only) is sourced from the",
                              a("New York Times (NYT),", href = "https://github.com/nytimes/covid-19-data", target = "_blank" ),
                              "from",
                              a("USA Facts", href = "https://usafacts.org/visualizations/coronavirus-covid-19-spread-map/", target = "_blank" ),
                              "and from the",
                              a("Johns Hopkins University Center for Systems Science and Engineering (JHU).", href = "https://github.com/CSSEGISandData/COVID-19", target = "_blank" )
                              ),              
                            tags$div(
                              id = "bigtext",
                              a( "The Center for the Ecology of Infectious Diseases", href = "https://ceid.uga.edu", target = "_blank" ),
                              'has several additional projects related to COVID-19, which can be found on the',
                              a( "CEID COVID-19 Portal.", href = "http://2019-coronavirus-tracker.com/", target = "_blank" )
                            ), #Close the bigtext text div
                            tags$div(
                              id = "bigtext",
                              "If you are interested in learning more about infectious disease epidemiology and modeling, check out", 
                              a("our (slightly advanced) interactive modeling software and tutorial.", href = "https://shiny.ovpr.uga.edu/DSAIDE/", target = "_blank" )
                            ) #Close the bigtext text div
                          ), #close fluidrow
                          fluidRow( #all of this is the footer
                            column(3,
                                   a(href = "https://ceid.uga.edu", tags$img(src = "ceidlogo.png", width = "100%"), target = "_blank"),
                            ),
                            column(6,
                                   p('All text and figures are licensed under a ',
                                     a("Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License.",
                                       href = "http://creativecommons.org/licenses/by-nc-sa/4.0/", target = "_blank"),
                                     'Software/Code is licensed under ',
                                     a("GPL-3.", href = "https://www.gnu.org/licenses/gpl-3.0.en.html" , target =  "_blank"),
                                     'See source data sites for licenses governing data.',
                                     a("UGA's Privacy Policy.", href = "https://eits.uga.edu/access_and_security/infosec/pols_regs/policies/privacy/" , target =  "_blank"),
                                     align = "center",
                                     style = "font-size:small"
                                   ) #end paragraph
                            ), #end middle column
                            column(3,
                                   a(href = "https://publichealth.uga.edu", tags$img(src = "cphlogo.png", width = "100%"), target = "_blank")
                            ) #end left column
                          ) #end fluidrow
                        ) #end taglist
              ) #close about tab
  ) #close NavBarPage
) #end fluidpage and UI part of shiny app
#end UI of shiny app
###########################################


###########################################
# Define server functions
###########################################
server <- function(input, output, session) {

  ###########################################
  # function that takes UI settings and produces data for each plot
  ###########################################
  set_outcome <- function(plot_dat,case_death,daily_tot,absolute_scaled,xscale,count_limit,selected_tab,location_selector)
  {
    
    out_type = paste(daily_tot,case_death,sep='_') #make string from UI inputs that correspond with variable names
    plot_dat <- plot_dat %>%   filter(Location %in% location_selector) %>%      #Only process data for locations that are  selected
                               mutate(outcome = get(out_type)) #pick output based on variable name created from UI
    # do testing data for US 
    if (selected_tab == "us")  
    {
      test_out_type = paste(daily_tot,'Test_All',sep='_')
      test_pos_type = paste(daily_tot,'Test_Positive',sep='_')
      plot_dat <- plot_dat %>% mutate(test_outcome = get(test_out_type)) 
      plot_dat <- plot_dat %>% mutate(test_frac_outcome = get(test_pos_type)/get(test_out_type))
    }
 
    #set labels and tool tips based on input - entries 2 and 3 are ignored for world plot
    y_labels <- c("Cases", "Tests", "Positive Test Proportion")
    y_labels[1] <- case_death #fill that automatically with either Case/Hosp/Death
    y_labels <- paste(daily_tot, y_labels, sep = " ")
    
    tool_tip <- c("Date", "Cases", "Tests", "Positive Test Proportion")
    tool_tip[2] <- case_death #fill that automatically with either Case/Hosp/Death
    
       
    #if we want scaling by 100K, do extra scaling 
    if (absolute_scaled == 'scaled')
    {
      plot_dat <- plot_dat %>% mutate(outcome = outcome / Population_Size * 100000) 
      y_labels[1] <- paste0(y_labels[1], " per 100K")
      y_labels[2] <- paste0(y_labels[2], " per 100K")
      if (selected_tab == "us" )  
      {
        plot_dat <- plot_dat %>%  mutate(test_outcome = test_outcome / Population_Size * 100000)
      }
    }
     
    #adjust data to align for plotting by cases on x-axis. 
    if (xscale == 'x_count')
    {
      #Takes plot_dat and filters counts by the predetermined count limit from the reactive above
      #Created the time variable (which represents the day number of the outbreak) from the date variable
      #Will plot the number of days since the selected count_limit or the date
      
      out_type2 = paste0("Total_",case_death) #make string from UI inputs that correspond to total and selected outcome
      plot_dat <- plot_dat %>% 
        filter(get(out_type2) >= count_limit) %>%  
        mutate(Time = as.numeric(Date)) %>%
        group_by(Location) %>% 
        mutate(Time = Time - min(Time))
      
    }
    else
    {
      plot_dat <- plot_dat %>% mutate(Time = Date)
    }
    #sort dates for plotting
    plot_dat <- plot_dat %>% group_by(Location) %>% arrange(Time) %>% ungroup()
    
    list(plot_dat, y_labels, tool_tip) #return list
  } #end function that produces output for plots
  
  ###########################################
  # function that takes data generated by above function and makes plots
  # uses plotly
  ###########################################
  make_plotly <- function(plot_list, location_selector, yscale, xscale, ylabel, outname, otherdata)
  {
    tool_tip <- plot_list[[3]]
    plot_dat <- data.frame(plot_list[[1]]) #need the extra data frame conversion from tibble to get tooltip_text line to work
    linesize = 2
    
    
    if (otherdata == "Yes")
    {
      if ( ( tool_tip[2] == "Cases" || tool_tip[2] == "Deaths") && outname == "outcome" && ylabel == 1)  #add NYT and JHU lines to case/death plot
      {
        p_dat <- plot_dat 
        ncols = max(3,length(unique(p_dat$Location)))
        tooltip_text = paste(paste0("Location: ", p_dat$Location), paste0(tool_tip[1], ": ", p_dat$Date), paste0(tool_tip[ylabel+1],": ", p_dat[,outname]), sep ="\n") 
        pl <- plotly::plot_ly(p_dat) %>% plotly::add_trace(x = ~Time, y = ~get(outname), type = 'scatter', mode = 'lines+markers', color = ~Location, linetype = ~source,
                                 line = list( width = linesize), text = tooltip_text, colors = brewer.pal(ncols, "Dark2")) %>%
                                 layout(yaxis = list(title=plot_list[[2]][ylabel], type = yscale, size = 18)) 
     } else { #the other plots should not change
        p_dat <- plot_dat %>% filter(source == "COVIDTracking")
        #browser()
        tooltip_text = paste(paste0("Location: ", p_dat$Location), paste0(tool_tip[1], ": ", p_dat$Date), paste0(tool_tip[ylabel+1],": ", p_dat[,outname]), sep ="\n") 
        ncols = max(3,length(unique(p_dat$Location)))
        pl <- p_dat %>%
          plotly::plot_ly() %>%  
          add_trace(x = ~Time, y = ~get(outname), type = 'scatter', mode = 'lines+markers', color = ~Location, linetype = ~Location, 
                    line = list( width = linesize), text = tooltip_text, colors = brewer.pal(ncols, "Dark2")) %>%
          layout(  yaxis = list(title=plot_list[[2]][ylabel], type = yscale, size = 18)) 
      }
              
    } else #if otherdata is no, also no changes
    {
      p_dat <- plot_dat
      tooltip_text = paste(paste0("Location: ", p_dat$Location), paste0(tool_tip[1], ": ", p_dat$Date), paste0(tool_tip[ylabel+1],": ", p_dat[,outname]), sep ="\n") 
      ncols = max(3,length(unique(p_dat$Location)))
      pl <- p_dat %>%
        plotly::plot_ly() %>%  
        add_trace(x = ~Time, y = ~get(outname), type = 'scatter', mode = 'lines+markers', color = ~Location, linetype = ~Location, 
                  line = list(width = linesize), text = tooltip_text,  colors = brewer.pal(ncols, "Dark2")) %>%
        layout(  yaxis = list(title=plot_list[[2]][ylabel], type = yscale, size = 18)) 
    }
    return(pl)
  }
  
   
  ###########################################
  #function that checks if us tab is selected and generates UI
  ###########################################
  observeEvent( input$alltabs == 'us', 
  {
    #create data
    plot_dat <- reactive({
      us_dat <- us_ct_clean      
      if (input$otherdata == "Yes") #add NYT data if selected
      {
        us_dat <- dplyr::bind_rows(us_ct_clean, us_nyt_clean, us_jhu_clean, usafct_clean)
      }
      set_outcome(us_dat,input$case_death,input$daily_tot,input$absolute_scaled,input$xscale,input$count_limit,input$alltabs,input$state_selector)
    })
    
    #create data
    #plot_dat <- reactive({
     #             set_outcome(us_clean,input$case_death,input$daily_tot,input$absolute_scaled,input$xscale,input$count_limit,input$alltabs,input$state_selector)
  #                })
    
    #make the plot for cases/deaths for US data
    output$case_death_plot <- renderPlotly({
      #create plot
      pl <- make_plotly(plot_dat(), location_selector = input$state_selector, yscale = input$yscale, xscale = input$xscale, ylabel = 1, outname = 'outcome',otherdata = input$otherdata)
      #pl <- make_plot(plot_dat, location_selector = input$state_selector, yscale = input$yscale, xscale = input$xscale, ylabel = 1) 
    }) #end function making case/deaths plot
    
    #make the testing plot 
    output$testing_plot <- renderPlotly({
      #create plot
      pl <- make_plotly(plot_dat(), location_selector = input$state_selector, yscale = input$yscale, xscale = input$xscale, ylabel = 2, outname = 'test_outcome',otherdata = input$otherdata)
      #pl <- make_plot(plot_dat, location_selector = input$state_selector, yscale = input$yscale, xscale = input$xscale, ylabel = 2) 
    }) #end function making testing plot
    
    #make the fraction positive testing plot 
    output$testing_frac_plot <- renderPlotly({
      #create plot
      pl <- make_plotly(plot_dat(), location_selector = input$state_selector, yscale = "identity", xscale = input$xscale, ylabel = 3, outname = 'test_frac_outcome',otherdata = input$otherdata)
      #pl <- make_plot(plot_dat, location_selector = input$state_selector, yscale = input$yscale, xscale = input$xscale, ylabel = 3)
    }) #end function making testing plot
    
    
  }) #end observer listening to US tab choice


} #end server function

# Create Shiny object
shinyApp(ui = ui, server = server)