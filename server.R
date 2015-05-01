
library(shiny)
library(grid)
library(scales)

shinyServer(function(input, output, session) {
 
###### Define Controls ####### 
 
output$c1 <- renderUI({ checkboxGroupInput(inputId="ScenarioO", label = h4("AEO Scenarios"), 
             choices = unique(regionData$Scenario),
             selected="Reference")
}) 
 
output$c2 <- renderUI({ sliderInput(inputId="TrnSld", label = h4("Transportation"), min = 0, max = .8, value = 0, step = .1, width="50%") })
output$c3 <- renderUI({ sliderInput(inputId="ElcSld", label = h4("Electricity"),  min = 0, max = .8, value = 0, step = .1, width="50%") })
output$c4 <- renderUI({ sliderInput(inputId="ResSld", label = h4("Residential"),  min = 0, max = .8, value = 0, step = .1, width="50%") })
output$c5 <- renderUI({ sliderInput(inputId="ComSld", label = h4("Commercial"),  min = 0, max = .8, value = 0, step = .1, width="50%") })
output$c6 <- renderUI({ sliderInput(inputId="IndSld", label = h4("Industrial"),  min = 0, max = .8, value = 0, step = .1, width="50%") })

 
 
###### Define Data and PlotOutput ########### 
 
observe({ 

chooseScen <- reactive({ 
 
     aeoScen <- regionData %>% filter(Scenario %in% input$ScenarioO)
     return(aeoScen)
 
})
 


data_values <- function(x) {
 
     if(is.null(x)) return(NULL)
     if(is.null(x$ID)) return(NULL)
 
     all_data <- isolate(chooseScen())
     data <- all_data[all_data$ID == x$ID,]
 
     paste0("<b>", data$Scenario, "</b><br>", data$Year, "<br>", 
     format(data$ghgT,digits=0), collapse = "<br />")

} 
 
 
sPlot1 <- reactive({
     
     chooseScen %>% ggvis(~Year,~ghgT,stroke = ~Scenario) %>% layer_lines(strokeWidth := 2.5,strokeWidth.hover := 5) %>% 
     layer_points(size := 25, size.hover := 100,fillOpacity := 0.1, fillOpacity.hover := 0.25,key := ~ID) %>%
     layer_points(x = ~x, y = ~y, size := 75,stroke := "blue", fillOpacity := 0.1, data = refPoint) %>%
     layer_points(x = ~x, y = ~y, size := 75,stroke := "red", fillOpacity := 0.1, data = refPoint1) %>%
     add_tooltip(data_values, "hover") %>% scale_numeric("y",domain = c(10,225)) %>% scale_numeric("x",domain = c(1990,2050)) %>%
     add_axis("y",format = "d",title = "Million Metric Tons",title_offset = 50,properties = axis_props(labels = list(fontSize = 15))) %>% 
     add_axis("x",format = "d",title = "",properties = axis_props(labels = list(angle = -45,fontSize = 15,align = "right"))) %>% 
     set_options(width = 600,height = 600/1.333, padding = padding(25, 225, 75, 75)) %>%
     layer_paths(x = ~x, y = ~y, stroke := "black",strokeWidth := 2.5, opacity := .5, data = guideLine) %>% 
     layer_paths(x = ~year, y = ~goal, stroke := "red",strokeWidth := 2.5,strokeDash := 2, data = goalLine1) %>%
     layer_paths(x = ~year, y = ~goal, stroke := "red",strokeWidth := 2.5,strokeDash := 2, data = goalLine2) %>%
     layer_paths(x = ~year, y = ~goal, stroke := "red",strokeWidth := 2.5,strokeDash := 2, data = goalLine3)
  
 
})
 
sPlot1 %>% bind_shiny("plot1") 

})
 
observe({ 

chooseWedge <- reactive({ 
 
     WesgeScen <- sGhg[Res == input$ResSld & Com == input$ComSld & Ind == input$IndSld & Trn == input$TrnSld & Elc == input$ElcSld,]
     return(WesgeScen)
 
})
 


output$gridPlot <- renderPlot({
       
      ggplot(chooseWedge(), aes(x = Year, y = Ghg, group = Sector)) +
      stat_smooth(mapping = aes(fill = Sector), geom="area", position="stack", method="gam", formula = y~s(x)) +
      scale_fill_manual(values = colors) +
      scale_x_continuous(breaks=c(2013,2015,2020,2025,2030)) +
      geom_line(data = lines, aes(x = x, y = y),color = "red", size = 1.5) +
      geom_hline(yintercept=127, color = "black", size = 1, linetype = 5 ) +
      labs(x=NULL,y="Million Metric Tons") + 
      theme_bw() + theme(legend.position="bottom", legend.text = element_text(size = 14, face = "bold"), axis.title = element_text(size=14), legend.key.width = unit(3,"line")) + 
      guides(fill=guide_legend(title=NULL)) +
      theme(plot.title=element_text(face="bold", hjust=0)) +
      theme(panel.grid=element_blank()) + theme(panel.border=element_blank())

})
     
   
 
})
 

}) 
 



















