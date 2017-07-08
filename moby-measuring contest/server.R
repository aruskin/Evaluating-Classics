function(input, output, session) {
  
  tables <- reactiveValues(lhs_table=NULL, rhs_table=NULL)
  
  observeEvent(input$go.lhs, {
    lhs.field <- input$search.type.lhs
    lhs.query <- input$search.lhs
    lhs.choices <- get_relevant_works(lhs.field, lhs.query)
    updateSelectInput(session, 'options.lhs', choices = lhs.choices)
  })
  
  observeEvent(input$go.rhs, {
    rhs.field <- input$search.type.rhs
    rhs.query <- input$search.rhs
    rhs.choices <- get_relevant_works(rhs.field, rhs.query)
    updateSelectInput(session, 'options.rhs', choices = rhs.choices)
  })
  
  output$lhs.table <- renderTable({
    id <- input$options.lhs
    if(id != ""){
      my_table <- get_book_summary(id)
      colnames(my_table) <- 'Book 1'
      tables$lhs_table <- my_table
      my_table
    }
  }, rownames = TRUE)
  
  output$rhs.table <- renderTable({
    id <- input$options.rhs
    print(id)
    if(id != ""){
      my_table <- get_book_summary(id)
      colnames(my_table) <- 'Book 2'
      tables$rhs_table <- my_table
      my_table
    }
  }, rownames = TRUE)
  
  output$comparison <- renderText({
    if(!(is.null(tables$lhs_table) | is.null(tables$rhs_table))){
      lhs_table <- isolate(tables$lhs_table)
      rhs_table <- isolate(tables$rhs_table)
      wordcount_ratio <- lhs_table[1,]/rhs_table[1,]
      #wordlength_ratio <- lhs_table[3,]/rhs_table[3,]
      wps_ratio <-lhs_table[5,]/rhs_table[5,]
      
      paste0(paste0("Book 1 has ", round(wordcount_ratio, 2), "x as many words as Book 2.\n"),
            paste0("On average, sentences in Book 1 are ", round(wps_ratio, 2), 
                   "x as long as sentences in Book 2."))
    }
  })
}