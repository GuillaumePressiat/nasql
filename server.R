# listes_api <- referime::get_table('listes_api') %>%
#   filter(row_number() < 1)

function(input, output) { 
  
  listes_api <- struc_listes()
  
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
        withProgress(
          message="Import des données",{
            incProgress(1/3, detail="Import des rsa")
      r <- irsa(p, typi   = 6) 
      
        incProgress(1/3, detail="Préparation des RSA")
        r <- r %>%
        prepare_rsa()
      
      incProgress(1/3, detail="Ajout des NAS")
      r$rsa <- r$rsa %>%
        inner_tra(itra(p))
          })
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
     withProgress(
       message="Requêtes en cours...",{
         incProgress(1, detail="")
     r2 <- lancer_requete(df(), liste_r, vars = c('nohop', 'nas', 'duree', 'diags', 'actes', 'ghm', 'agean', 'rsatype'))
       })
     
     return(r2)})
     


  output$rsa_requ <- renderDataTable({
    df_requ() }, rownames=FALSE, extensions = 'Buttons', filter = 'top', 
    options = list(dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel', 'colvis'), 
                   scrollY = 600, scrollX = TRUE,
                   scroller = TRUE, server = TRUE
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
    if (nrow(df_requ())){
    ggplot(data = df_requ(), aes(x = duree, fill = rsatype)) + geom_bar() + coord_flip() +
      ggthemes::scale_fill_pander() +
      facet_wrap(~ Requete) + 
      theme_light()
      
      plotly::ggplotly()
    }
  }) 


  dfw = callModule(editableDT, "table1", dataname = reactive(input$mydata), inputwidth=reactive(500))
  

  output$test=renderTable({
    dfw()
  })
  
  # df_requ_adhoc <- eventReactive(input$lance_r2, {
    df_requ_adhoc <- eventReactive(input$lance_r2, {
    # cat('ok\n')
    ttabb <- dfw()
    # cat('ok\n')
    # 
    # return(ttabb)
    #ttabb <- u[122:123,]
    jsonify <- function(i) {
      temp <- ttabb[i,]
      as.list(as.data.frame(temp)) ->temp2
      temp2$diags <- stringr::str_split(temp2$diags, "\\, ", simplify = T)[1,]
      temp2$ghm_exclus <- stringr::str_split(temp2$ghm_exclus, "\\, ", simplify = T)[1,]
      temp2$ghm <- stringr::str_split(temp2$ghm, "\\, ", simplify = T)[1,]
      temp2$actes <- stringr::str_split(temp2$actes, "\\, ", simplify = T)[1,]
      temp2[is.na(temp2)] <- NULL
      temp2[temp2 == ""] <- NULL
      temp2
    }
    
    
    if (nrow(ttabb) > 0){
     lrbis <- 1:nrow(ttabb) %>% purrr::map(jsonify) %>% rlist::list.append()
    
    #return(lrbis)
     
    #return(requete(df(), list(thematique = "", requete = "", nom = "", actes = 'EBLA003')))
    return(requete(df(), lrbis[[1]], vars = c('nohop', 'nas', 'ghm', 'actes', 'diags', 'duree', 'agean')))
     # return(lancer_requete(df(), lrbis))
    }
    # } else {
    # NULL
    # }
    
    #return(lancer_requete(df(), lrbis))
    # } else {
    #   return(tibble(nok = "nok ::"))
    #}
    
  })
    #output$rsa_requ_main <- renderPrint({print(df_requ_adhoc()[[1]]$actes)})
    
    output$rsa_requ_main <- renderDataTable({
      df_requ_adhoc()}, rownames=FALSE, extensions = 'Buttons', filter = 'top',
      options = list(dom = 'Bfrtip',
                     buttons = c('copy', 'excel', 'colvis'),
                     scrollY = 600, scrollX = TRUE,
                     scroller = TRUE, server = TRUE))

}


