library(data.table)
library(xtable)
function(input, output, session) {
  
  tables <- reactiveValues(lhs_table=NULL, rhs_table=NULL)
  
  observeEvent(input$go.lhs, {
    lhs.field <- input$search.type.lhs
    lhs.query <- input$search.lhs
    lhs.choices <- get_relevant_works(lhs.field, lhs.query)
    updateSelectInput(session, 'options.lhs', choices=lhs.choices,
                      selected=NULL)
  })
  
  observeEvent(input$go.rhs, {
    rhs.field <- input$search.type.rhs
    rhs.query <- input$search.rhs
    rhs.choices <- get_relevant_works(rhs.field, rhs.query)
    updateSelectInput(session, 'options.rhs', choices=rhs.choices,
                      selected=NULL)
  })
  
  observeEvent(input$options.lhs, {
    id.lhs <- isolate(input$options.lhs)
    if(id.lhs == ""){
      tables$lhs_table <- NULL
    }else{
      progress <- shiny::Progress$new()
      progress$set(message = "Summarizing Book 1...", value = 0)
      tables$lhs_table <- get_book_summary(id.lhs)
      progress$set(message = "Done!", value = 1)
      progress$close()
    }
  })
  
  observeEvent(input$options.rhs, {
    id.rhs <- isolate(input$options.rhs)
    if(id.rhs == ""){
      tables$rhs_table <- NULL
    }else{
      progress <- shiny::Progress$new()
      progress$set(message = "Summarizing Book 2...", value = 0)
      tables$rhs_table <- get_book_summary(id.rhs)
      progress$set(message = "Done!", value = 1)
      progress$close()
    }
  })

  output$my.table <- renderTable({
    if(!(is.null(tables$lhs_table) | is.null(tables$rhs_table))){
      out.table <- rbind(tables$lhs_table[,1], tables$rhs_table[,1]) %>%
        as.data.frame
      out.table[,1] <- prettyNum(out.table[,1], big.mark = ",") 
      out.table[,2] <- prettyNum(out.table[,2], big.mark = ",")
      colnames(out.table) <- rownames(tables$lhs_table)
      rownames(out.table) <- c("Book 1", "Book 2")
      out.table
    }
  }, rownames = TRUE, digits=2, align = 'r')
  
  output$comparison <- renderText({
    if(!(is.null(tables$lhs_table) | is.null(tables$rhs_table))){
      lhs_table <- isolate(tables$lhs_table)
      rhs_table <- isolate(tables$rhs_table)
      wordcount_ratio <- lhs_table[1,]/rhs_table[1,]
      wps_ratio <-lhs_table[5,]/rhs_table[5,]
      
      paste0(paste0("Book 1 has ", round(wordcount_ratio, 2), "x as many words as Book 2.\n"),
            paste0("On average, sentences in Book 1 are ", round(wps_ratio, 2), 
                   "x as long as sentences in Book 2."))
    }else{
      "Make selections for Book 1 and Book 2"
    }
  })
}