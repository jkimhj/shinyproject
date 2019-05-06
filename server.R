
shinyServer(function(input, output) {
  react1 = reactive({

      switch(input$rate, 
             if (input$rate =="viol"){
        df.boro%>% filter(!is.na(violation.rate.percent)) %>% group_by(borough) %>% summarise(Violation_Rate = mean(violation.rate.percent))
      } else if (input$rate =="phh_viol") {
        df.boro%>% filter(!is.na(public.health.hazard.violation.rate)) %>% group_by(borough) %>% summarise(Violation_Rate = mean(public.health.hazard.violation.rate))
      } else if (input$rate == "crit_viol"){
        df.boro%>% filter(!is.na(critical.violation.rate)) %>% group_by(borough) %>% summarise(Violation_Rate = mean(critical.violation.rate))
      }  )
   
  })
  output$boro = renderPlotly({
    ggplot(data=react1(), aes(x= borough, y=Violation_Rate, fill=borough)) +
      geom_bar(stat="identity") +
      labs(title='Violations Rate by Borough',
           x='Borough',
           y= 'Rate (%)') +
      scale_fill_brewer(palette='Set1') +
      theme_bw() +
      theme(legend.key=element_blank())
  }
)

  react2 = reactive({
    program_table = df.program %>% filter(program.type==input$program) %>% group_by(program.type) %>%
      select(violation.rate.percent, public.health.hazard.violation.rate, critical.violation.rate) %>%
      summarise(viol_rate=mean(violation.rate.percent), phh_rate = mean(public.health.hazard.violation.rate), 
                crit_rate=mean(critical.violation.rate))
    pt_melt = melt(program_table, id.vars = "program.type")
   
  })
  output$program_plot = renderPlotly({
    p <- ggplot(data=react2(), aes(x=variable, y=value)) +
      geom_bar(stat="identity", position="dodge")+
      labs(title='Violations Rate by Program Type',
           x='Violation Type',
           y= 'Rate (%)') +
      scale_fill_brewer(palette='Set1') +
      theme_bw() +
      theme(legend.key=element_blank())
   p2 <- ggplotly(p)
   })
  react3 = reactive({
    facility_table = df.facility %>% filter(facility.type==input$f_type) %>% group_by(facility.type) %>%
      select(violation.rate.percent, public.health.hazard.violation.rate, critical.violation.rate) %>%
      summarise(viol_rate=mean(violation.rate.percent), phh_rate = mean(public.health.hazard.violation.rate), 
                crit_rate=mean(critical.violation.rate))
    ft_melt = melt(facility_table, id.vars = "facility.type")
  })
  
  output$facility_plot = renderPlotly({
    p <- ggplot(data=react2(), aes(x=variable, y=value)) +
      geom_bar(stat="identity", position="dodge")+
      labs(title='Violations Rate by Facility Type',
           x='Violation Type',
           y= 'Rate (%)') +
      scale_fill_brewer(palette='Set1') +
      theme_bw() +
      theme(legend.key=element_blank())
    p2 <- ggplotly(p)
  })
  
  output$centers_map = renderLeaflet({
    leaflet(df.zipcode) %>% addTiles() %>%
      addAwesomeMarkers(lng=df.zipcode$longitude, lat=df.zipcode$latitude,
                        label = centerTooltip,icon=icons
                        # clusterOptions=markerClusterOptions()
                        )
    })
  
  react4 = reactive({
           if (input$viol_choice =="viol"){
             yt_melted_1 = yt_melted[yt_melted["variable"]=="viol_rate",]
           } else if (input$viol_choice =="phh_viol") {
             yt_melted_1 = yt_melted[yt_melted["variable"]=="phh_rate",]
           } else if (input$viol_choice == "crit_viol"){
             yt_melted_1 = yt_melted[yt_melted["variable"]=="crit_rate",]
           }  
 })
  output$year_plot = renderPlotly({
    ggplot(data=react4(), aes(x=year, y=value, color=borough)) +
      geom_line()+
      labs(title='Violations Rate by Year',
           x='Year',y= 'Rate (%)') +
      scale_fill_brewer(palette='Set1') +
      theme_bw() 
  })
  
  output$table = DT::renderDataTable({
    datatable(center_table, rownames=FALSE, options=list(scrollX=TRUE))
  })
})




