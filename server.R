# listes_api <- referime::get_table('listes_api') %>%
#   filter(row_number() < 1)

function(input, output) { 
  
  # listes_api <- struc_listes()
  
  
  l <- observeEvent(input$button_a1,{
    if (input$button_a1){
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
      withProgress(
        message="Dézippage des fichiers",{
          incProgress(1/1, detail="Dézippage")
      adezip(p, type = "out")
        })

    }
    return(NULL)
    })
    
    l2 <- observeEvent(input$button_a0,{
    if (input$button_a0){
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
      withProgress(
        message="Suppression des fichiers",{
          incProgress(1/1, detail="Efface files")
      adelete(p)
        })

    }
    return(NULL)
      }
    )
      
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
        if (!grepl('Recette', input$checkgroup3)){
          r$valo <- tibble(cle_rsa = "", type_fin = 0, rec_totale = 0, rec_bee = 0, rec_exb = 0)
        } 
        
        if ('Recette BEE' %in% input$checkgroup3){
          withProgress(
            message="Imports pour la valorisation",{
          incProgress(1/2, detail="Import rsa pour la valo")
          # r$rsa_v <- vvr_rsa(p)
          # r$ano <- vvr_ano_mco(p)
          tarifs_ghs <- dplyr::distinct(get_table('tarifs_mco_ghs'), ghs, anseqta, .keep_all = TRUE)
          
          vrsa <- vvr_rsa(p)
          incProgress(1/2, detail="Import ano pour la valo")
          vano <- vvr_ano_mco(p)
            
          
          r$valo <- vvr_mco(
            vvr_ghs_supp(rsa = vrsa, ano =  vano, tarifs = tarifs_ghs),
            vvr_mco_sv(vrsa, vano)
          ) %>% 
            inner_join(r$rsa %>% select(cle_rsa, nas, nohop))
            })
        }          
        if ('Recette BEE + suppléments' %in% input$checkgroup3){
          withProgress(
            message="Imports pour la valorisation",{
              incProgress(1/2, detail="Import rsa pour la valo")
              # r$rsa_v <- vvr_rsa(p)
              # r$ano <- vvr_ano_mco(p)
              tarifs_ghs <- dplyr::distinct(get_table('tarifs_mco_ghs'), ghs, anseqta, .keep_all = TRUE)
              
              vrsa <- vvr_rsa(p)
              incProgress(1/2, detail="Import ano pour la valo")
              vano <- vvr_ano_mco(p)
              
              
              r$valo <- vvr_mco(
                vvr_ghs_supp(rsa = vrsa, ano =  vano, tarifs = tarifs_ghs,
                             supplements = get_table('tarifs_mco_supplements'), bee = FALSE, full = TRUE),
                vvr_mco_sv(vrsa, vano)) %>%  # , porg = ipo(p)
                inner_join(r$rsa %>% select(cle_rsa, nas, nohop))
            })
        }       

      }
      # if (input$monet){
      #   con <- MonetDBLite::src_monetdblite(dbdir = input$path, create = FALSE)
      #   r <- collect_rsa_from_db(con, substr(input$annee,3,4)) %>%
      #     prepare_rsa()
      # }
      return(r)
    }
  })

  
  
  output$sv <-  renderDataTable({
    datatable(df()$valo %>% epmsi_mco_sv(), 
      rownames=FALSE, extensions = 'Buttons', filter = 'top', class = 'white-space: nowrap',
    options = list(lengthChange = FALSE, dom = 'Bfrtip', pageLength = 12,
                   buttons = c('copy', 'excel', 'colvis'),
                   scrollY = 200, scrollX = TRUE,
                   scroller = TRUE, server = FALSE)) %>%
    formatStyle(0, target= 'row', lineHeight='80%')}
  )
  
  output$rav <-  renderDataTable({
    #%>% select(cle_rsa)
    datatable(df()$valo %>% epmsi_mco_rav(), rownames=FALSE, extensions = 'Buttons', filter = 'top', class = 'white-space: nowrap',
    options = list(lengthChange = FALSE, dom = 'Bfrtip', pageLength = 20,
                   buttons = c('copy', 'excel', 'colvis'),
                   scrollY = 300, scrollX = TRUE,
                   scroller = TRUE, server = FALSE)) %>%
      formatStyle(0, target= 'row', lineHeight='80%')}
  )
  
  output$rsa <-  renderDataTable({
    #%>% select(cle_rsa)
    datatable(df()$rsa, rownames=FALSE, extensions = 'Buttons', filter = 'top', class = 'white-space: nowrap',
    options = list(lengthChange = FALSE, dom = 'Bfrtip',  pageLength = 100,
                   buttons = c('copy', 'excel', 'colvis'),
                   scrollY = 600, scrollX = TRUE,
                   scroller = TRUE, server = FALSE)) %>%
      formatStyle(0, target= 'row', lineHeight='80%')}
  )

  output$rsa_valo <-  renderDataTable({
    #%>% select(cle_rsa)
    datatable(df()$valo , rownames=FALSE, extensions = 'Buttons', filter = 'top', class = 'white-space: nowrap',
    options = list(lengthChange = FALSE, dom = 'Bfrtip', pageLength = 100,
                   buttons = c('copy', 'excel', 'colvis'),
                   scrollY = 600, scrollX = TRUE,
                   scroller = TRUE, server = FALSE)) %>%
      formatStyle(0, target= 'row', lineHeight='80%')})
  
   df_requ <- eventReactive(input$lance_r, {
     
     if (input$lance_r == FALSE){return(NULL)}
     
     
     liste_r <- purrr::map(input$requ, get_liste)
     withProgress(
       message="Requêtes en cours...",{
         incProgress(1, detail="")
     r2 <- lancer_requete(df(), liste_r, vars = c('nohop', 'nas', 'duree', 'diags', 'actes', 'ghm', 'agean', 'rsatype')) %>% 
                            select(Thematique, Requete, everything())
       })
     
     return(r2)})
     


  output$rsa_requ <- renderDataTable({
    datatable(df_requ(), rownames=FALSE, extensions = 'Buttons', filter = 'top', class = 'white-space: nowrap',
    options = list(dom = 'Bfrtip',
                   buttons = c('copy', 'excel', 'colvis'),
                   scrollY = 600, scrollX = TRUE,
                   scroller = TRUE, server = TRUE, pageLength = nrow(df_requ()))) %>%
    formatStyle(0, target= 'row', lineHeight='80%')})
  
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
  
  #librairie_manuelle <- eventReactive(input$filejson, {
  librairie_import <- eventReactive(input$filejson, {
    req(input$filejson)
    
    if (is.null(input$filejson)){return(dfw())}
    
    tryCatch(
      {
        dftemp <- jsonlite::read_json(input$filejson$datapath, simplifyVector = TRUE) %>%
          as_tibble()
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    dftemp

  })
  
  
  output$lib_requ_imports <- renderDataTable(datatable(librairie_import(), class = 'white-space: nowrap', rownames = FALSE,
                                                       options = list(dom = 't', scrollX = TRUE, scroller = TRUE)))
  
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
     lrbis <- 1:nrow(ttabb) %>% purrr::map(jsonify) %>% rlist::list.append() %>% purrr::map(function(x)x)
    
    #return(lrbis)
     
    #return(requete(df(), list(thematique = "", requete = "", nom = "", actes = 'EBLA003')))
    return(lancer_requete(df(), lrbis, vars = c('nohop', 'nas', 'ghm', 'actes', 'diags', 'duree', 'agean')) %>% 
             select(Thematique, Requete, everything())) #[[1]]
     # return(lancer_requete(df(), lrbis))
    }
    })
    
    # df_requ_adhoc <- eventReactive(input$lance_r2, {
    df_requ_import <- eventReactive(input$lance_r3, {
      # cat('ok\n')
      ttabb <- librairie_import()
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
        lrbis <- 1:nrow(ttabb) %>% purrr::map(jsonify) %>% rlist::list.append() %>% purrr::map(function(x)x)
        
        #return(lrbis)
        
        #return(requete(df(), list(thematique = "", requete = "", nom = "", actes = 'EBLA003')))
        return(lancer_requete(df(), lrbis, vars = c('nohop', 'nas', 'ghm', 'actes', 'diags', 'duree', 'agean')) %>% 
                 select(Thematique, Requete, everything())) #[[1]]
        # return(lancer_requete(df(), lrbis))
      }
    })

    #output$rsa_requ_main <- renderPrint({print(df_requ_adhoc()[[1]]$actes)})
    lum <- nomensland::get_table('lib_mco_um') %>%
      mutate(libelle_code_um2_abrege = ifelse(code_um2 == '29', 'Méd adultes', libelle_code_um2_abrege)) %>%
      mutate(libelle_code_um2_abrege = ifelse(code_um2 == '52', 'Chir péd', libelle_code_um2_abrege)) %>%
      mutate(libelle_code_um2_abrege = ifelse(code_um2 == '53', 'Chir ad', libelle_code_um2_abrege))
    
    
    output$rsa_requ_main <- renderDataTable({
      datatable(df_requ_adhoc(), rownames=FALSE, extensions = 'Buttons', filter = 'top', class = 'white-space: nowrap',
      options = list(dom = 'Bfrtip',
                     buttons = c('copy', 'excel', 'colvis'),
                     scrollY = 600, scrollX = TRUE,
                     scroller = TRUE, server = TRUE, pageLength = nrow(df_requ_adhoc()))) %>%
        formatStyle(0, target= 'row', lineHeight='80%')})
    
    output$rsa_requ_import <- renderDataTable({
      datatable(df_requ_import(), rownames=FALSE, extensions = 'Buttons', filter = 'top', class = 'white-space: nowrap',
      options = list(dom = 'Bfrtip',
                     buttons = c('copy', 'excel', 'colvis'),
                     scrollY = 600, scrollX = TRUE,
                     scroller = TRUE, server = TRUE, pageLength = nrow(df_requ_import()))) %>%
      formatStyle(0, target= 'row', lineHeight='80%')})
    
    custom.message = "function (d) {
root = d;
while (root.parent) {
root = root.parent
}
p = (100*d.value/root.value).toPrecision(2);
msg = '<small>' + p+' %<br/>'+d.value+ ' rsa' + '</small>';
return msg;
}"



data_for_parcours <- eventReactive(input$parc_cible, {
  if (input$parc_cible == "RSA"){
    return(df()$rsa %>% select(cle_rsa))
  }
  if (input$parc_cible == "via librairie de requêtes"){
    return(df_requ())
  }
  if (input$parc_cible == "via requête saisie"){
    return(df_requ_adhoc())
  }
  if (input$parc_cible == "via requête importée"){
    return(df_requ_import())
  }
  
})


output$parc <- sunburstR::renderSunburst({
  
  u <- data_for_parcours() %>%
    inner_join(df()$rsa_um, by = 'cle_rsa') %>%
    mutate(um = substr(typaut1,1,2)) %>%
    left_join(lum %>% select(code_um2, libelle_code_um2_abrege), by = c('um' = 'code_um2')) %>%
    distinct(cle_rsa, libelle_code_um2_abrege) %>%
    group_by(cle_rsa) %>%
    summarise(lums = paste0(libelle_code_um2_abrege, collapse = "-")) %>%
    count(lums) %>%
    ungroup() %>%
    as.data.frame()
  
  u %>%
    sunburstR::sunburst(width = "80%", explanation = custom.message, legend = list(w = 200))
})


    output$parc_dat <- renderDataTable({

      u <- data_for_parcours() %>%
        inner_join(df()$rsa_um, by = 'cle_rsa') %>%
        mutate(um = substr(typaut1,1,2)) %>%
        left_join(lum %>% select(code_um2, libelle_code_um2_abrege), by = c('um' = 'code_um2')) %>%
        distinct(cle_rsa, libelle_code_um2_abrege) %>%
        group_by(cle_rsa) %>%
        summarise(lums = paste0(libelle_code_um2_abrege, collapse = "-")) %>%
        count(lums) %>%
        ungroup() %>%
        as.data.frame() %>%
        arrange(desc(n))
      u
      
    }, rownames=FALSE, extensions = 'Buttons', filter = 'top', 
    options = list(lengthChange = FALSE, dom = 'Bfrtip', 
                   buttons = c('copy', 'excel', 'colvis'),
                   scrollY = 600, scrollX = TRUE,
                   scroller = TRUE, server = FALSE, pageLength = 400))
    

   
    encode_xls_csv <- ifelse(grepl('1252', sessionInfo()$locale), 'latin1', 'UTF-8')
      
      output$download11 <- downloadHandler(
        filename = function(){paste0(input$name_down1, '.xls')}, 
        content = function(fname){
          WriteXLS::WriteXLS(df_requ(), fname, Encoding = encode_xls_csv)
        })
      
      output$download12 <- downloadHandler(
        filename = function(){paste0(input$name_down1, '.json')}, 
        content = function(fname){
          jsonlite::write_json(df_requ(), fname)
        })
      
      output$download13 <- downloadHandler(
        filename = function(){paste0(input$name_down1, '.csv')}, 
        content = function(fname){
          write.csv2(df_requ(), fname, row.names = FALSE, quote = TRUE)
        })
      
      output$download21 <- downloadHandler(
        filename = function(){paste0(input$name_down2, '.xls')}, 
        content = function(fname){
          WriteXLS::WriteXLS(df_requ_adhoc(), fname, Encoding = encode_xls_csv)
        })
      
      output$download22 <- downloadHandler(
        filename = function(){paste0(input$name_down2, '.json')}, 
        content = function(fname){
          jsonlite::write_json(df_requ_adhoc(), fname)
        })
      
      output$download23 <- downloadHandler(
        filename = function(){paste0(input$name_down2, '.csv')}, 
        content = function(fname){
          write.csv2(df_requ_adhoc(), fname, row.names = FALSE, quote = TRUE)
        })

      output$download51 <- downloadHandler(
        filename = function(){paste0(input$name_down2, '.xls')}, 
        content = function(fname){
          WriteXLS::WriteXLS(df_requ_import(), fname, Encoding = encode_xls_csv)
        })
      
      output$download52 <- downloadHandler(
        filename = function(){paste0(input$name_down2, '.json')}, 
        content = function(fname){
          jsonlite::write_json(df_requ_import(), fname)
        })
      
      output$download53 <- downloadHandler(
        filename = function(){paste0(input$name_down2, '.csv')}, 
        content = function(fname){
          write.csv2(df_requ_import(), fname, row.names = FALSE, quote = TRUE)
        })
      
      output$download31 <- downloadHandler(
        filename = function(){paste0(input$name_down3, '.xls')}, 
        content = function(fname){
          WriteXLS::WriteXLS(dfw(), fname, Encoding = encode_xls_csv)
        })
      
      output$download32 <- downloadHandler(
        filename = function(){paste0(input$name_down3, '.json')}, 
        content = function(fname){
          jsonlite::write_json(dfw(), fname)
        })
      output$download33 <- downloadHandler(
        filename = function(){paste0(input$name_down3, 'csv')}, 
        content = function(fname){
          write.csv2(dfw(), fname, row.names = FALSE, quote = TRUE)
        })

      output$download41 <- downloadHandler(
        filename = function(){paste0(input$name_down4, '.xls')}, 
        content = function(fname){
          WriteXLS::WriteXLS(df(), fname, Encoding = encode_xls_csv)
        })
      
      output$download42 <- downloadHandler(
        filename = function(){paste0(input$name_down4, '.json')}, 
        content = function(fname){
          jsonlite::write_json(df(), fname)
        })
      
        output$download43 <- downloadHandler(
        filename = function(){paste0(input$name_down4, '.zip')},
        content = function(fname) {
          fs <- names(df())
          tmpdir <- tempdir()
          setwd(tempdir())
          for (i in 1:length(fs)){
            path <- paste0(input$name_down4, "_", fs[i] , ".csv")
            #fs <- c(fs, path)
            write.csv2(df()[[i]], path, quote = TRUE, row.names = FALSE)
          }
          zip(zipfile=fname, files=paste0(input$name_down4, '_', names(df()), '.csv'))
        },
        contentType = "application/zip"
        )
        # content = function(fname){
        #   write.csv2(df()$rsa, fname, quote = TRUE, row.names = FALSE)
        # })

          observe({ 
            if (any(lapply(df(), nrow) > 65535) | any(lapply(df(), ncol) > 256)) {
              disable('download41')
            } else {
              enable('download41')
            }
          })
          
          observe({ 
            if (any(nrow(df_requ_adhoc()) > 65535) | ncol(df_requ_adhoc()) > 256) {
              disable('download21')
            } else {
              enable('download21')
            }
          })
          
          observe({
            if (nrow(df_requ()) > 65535 | ncol(df_requ()) > 256){
              disable('download11')
            } else {
              enable('download11')
            }
          })
          
          shinyjs::hide('mydata')
}


