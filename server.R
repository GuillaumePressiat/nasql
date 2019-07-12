
options(shiny.maxRequestSize=100*1024^2) 
library(pmeasyr)
library(dplyr)

#input <- list(finess = '750712184', anno = 2019, mese1 = 5, path = '~/Documents/data/mco/')

function(input, output) { 
  
  
  df <- eventReactive(input$button_i, {
    
    if (is.null(input$button_i)){ return(NULL)}
    
    if (input$button_i){
      p <- noyau_pmeasyr(
        finess = as.character(input$finess),
        annee  = as.integer(input$annee),
        mois   = as.integer(input$mois),
        path   = input$path,
        n_max = input$max_rows,
        tolower_names = TRUE,
        lib = FALSE,
        progress = FALSE
      )
      
      if (!input$monet){
      r <- irsa(p, typi   = 6) %>% #$rsa
        prepare_rsa()
      
      r$rsa <- r$rsa %>%
        inner_tra(itra(p))
      }
      # if (input$monet){
      #   con <- MonetDBLite::src_monetdblite(dbdir = input$path, create = FALSE)
      #   r <- collect_rsa_from_db(con, substr(input$annee,3,4)) %>%
      #     prepare_rsa()
      # }
      return(r)
    }
  })

  output$rsa <-  renderDataTable({
    #%>% select(cle_rsa)
    df()$rsa }, rownames=FALSE, extensions = 'Buttons', filter = 'top', 
    options = list(lengthChange = FALSE, dom = 'Bfrtip', 
                   buttons = c('copy', 'excel', 'colvis'),
                   scrollY = 600, scrollX = TRUE,
                   scroller = TRUE, server = FALSE)# , server = TRUE
  )

  
  
   df_requ <- eventReactive(input$lance_r, {
     
     if (input$lance_r == FALSE){return(NULL)}
     
     
     liste_r <- purrr::map(input$requ, get_liste)
     
     r2 <- lancer_requete(df(), liste_r, vars = c('nohop', 'nas', 'duree', 'diags', 'actes', 'ghm', 'agean', 'rsatype'))
     
     return(r2)})
     


  output$rsa_requ <- renderDataTable({
    df_requ() }, rownames=FALSE, extensions = 'Buttons', filter = 'top', 
    options = list(dom = 'Blfrtip', buttons = c('copy', 'csv', 'excel', 'colvis')
    ))
  
  output$nb_requ <- renderText({paste0("Il reste ", nrow(df_requ()), ' rsa suite à ces requêtes')})
  output$nb_rsa <- renderText({paste0("", nrow(df()$rsa), ' rsa importés, fichier : ', input$finess, '.', input$annee, '.', input$mois, '.rsa')})
  
  
  output$p1 <- renderPlotly({
    ggplot(data = df()$rsa, aes(x = duree, fill = rsatype)) + geom_bar() + coord_flip() +
      ggthemes::scale_fill_tableau() +
      theme_light()
      plotly::ggplotly()
  }) 
  output$p2 <- renderPlotly({
    ggplot(data = df_requ(), aes(x = duree, fill = rsatype)) + geom_bar() + coord_flip() +
      ggthemes::scale_fill_pander() +
      facet_wrap(~ Requete) + 
      theme_light()
      plotly::ggplotly()
  }) 
  
}


