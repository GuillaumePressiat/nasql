
library(nomensland)
dico <- get_dictionnaire_listes()
lr <- dico$nom_abrege
th <- dico$thematique %>% unique
names(lr) <- dico$nom_liste

lr <- list()
# i <- "Recours Exceptionnel"
for (i in th){
  te <- dico %>% filter(thematique == i)
  
  temp <- list(setNames(te$nom_abrege, te$nom_liste))
  names(temp) <- i
  lr <- append(lr, temp)
}

# listes_api <- referime::get_table('listes_api')
# tab_vide <- get_liste('chip') %>% 
#   as_tibble() %>%
#   filter(row_number() == 0)


dashboardPagePlus(
  

  
  dashboardHeaderPlus(title="Find my NASql", #titleWidth = 350,   
                      enable_rightsidebar = TRUE,
                      rightSidebarIcon = "gears"),
  dashboardSidebar(
    #width = 350,
    sidebarMenu(
      id="menuchoice",
      menuItem("Rsa import / explore", tabName="app"),
      menuItem("Librairie de requêtes", tabName="app2"),
      menuItem("Requête saisie", tabName="app3"))),
  
  
  
  #selectInput("requ", "Requête", choices = as.list(lr), selected = "bpco_exacerbee"),
  rightsidebar = rightSidebar(title = "Saisie de paramètres et lancement de l'import",
                              
                              selectInput("annee", "Selection de l'année", choices = setNames(2016:2019, 2016:2019), selected = 2018),
                              selectInput("mois", "Mois de remontée", choices = setNames(1:12, 1:12), selected = 12),
                              
                              selectInput("finess", "Finess", choices = list("750712184"), selected = 1),
                              textInput("path", "path", '~/Documents/data/mco'),
                              
                              shinyWidgets::prettySwitch(
                                inputId = "monet",
                                label = "db ? (non actif)"),
                              
                              shinyWidgets::knobInput(
                                inputId = "max_rows",
                                label = "Nb RSA à importer (n_max)",
                                value = 1e3,
                                min = 1,
                                max = 1.5e6,
                                displayPrevious = TRUE, 
                                lineCap = "default",
                                fgColor = "#3c8dbc",
                                inputColor = "#3c8dbc"
                              )),
  
  dashboardBody(
    tags$head(tags$style(HTML('
                                /* logo */
                                .skin-blue .main-header   .logo {
                                background-color: #383838;
                                }

                                .box-header  {
                                background-color: #383838;
                                }

                                .skin-blue .box-header {
                                background-color: #383838;
                                }
                                
                                /* navbar (rest of the header) */
                                .skin-blue .main-header .navbar {
                                background-color: #383838;
                                }

                                /* main sidebar */
                                .skin-blue .main-sidebar {
                                background-color: #383838;
                                }

                                /* active selected tab in the sidebarmenu */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                                background-color: #707070;
                                }

                                /* other links in the sidebarmenu */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                                background-color: #383838;
                                color: white;
                                }

                                /* other links in the sidebarmenu when hovered */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                                color: sienna;
                                }
                                /* toggle button when hovered  */
                                .skin-blue .main-header .navbar .sidebar-toggle:hover{
                                background-color: #a0522d;
                                }

                                /* body */
                                .content-wrapper, .right-side {
                                background: linear-gradient(to top left, #3c8dbc 0%, #a0522d 100%);
                                }

                                '))),
    fluidRow(
      tabItems(
        tabItem(tabName = "app", 
                shinydashboard::box(shinyWidgets::actionBttn(
                  inputId = "button_i",
                  label = "Importer les rsa",
                  style = "float", 
                  color = "success", size = "s"
                ), em('Il est possible de choisir les paramètres en affichant la barre latérale droite'),
                h5(textOutput("nb_rsa")),
                plotlyOutput('p1'), 
                width = 12, solidHeader = TRUE, collapsible = TRUE, 
                collapsed = FALSE, title = "Description des rsa", status = "primary"),
        
                shinydashboard::box(shinyWidgets::addSpinner(dataTableOutput("rsa")), 
                                    width = 12, 
                                    solidHeader = TRUE, collapsible = TRUE, 
                                    collapsed = TRUE, title = "Mes rsa", status = "primary")),
        
        
        tabItem(tabName = "app2", 
                shinydashboard::box(
                  # actionButton("button_i", "Importer les rsa !"),
                  selectizeInput("requ", "Requête", choices = as.list(lr), selected = "chip", multiple = TRUE),
                  # shinyWidgets::materialSwitch(label = "Requêtage actif", inputId = "lance_r", value = FALSE, right = TRUE)),
                  shinyWidgets::actionBttn(
                    inputId = "lance_r",
                    label = "Lancer la requête",
                    style = "float", 
                    color = "success", size = "xs"
                  ),
                  h5(textOutput("nb_requ")),
                  plotlyOutput('p2'), 
                  width = 12, solidHeader = TRUE, collapsible = TRUE, 
                  collapsed = FALSE, title = "Description de ces rsa requêtés", status = "primary"),
                shinydashboard::box(shinyWidgets::addSpinner(dataTableOutput("rsa_requ")), 
                                    width = 12, solidHeader = TRUE, collapsible = TRUE, 
                                    collapsed = FALSE, title = 'Rsa requêtés', status = "primary")),
        tabItem(tabName = "app3", 
                shinydashboard::box(
                  textInput('mydata', label = 'Nom de la table', value = 'listes_api'),
                  h5("Saisie / édition d'une requête"),
                  editableDTUI("table1"),
                  shinyWidgets::actionBttn(
                    inputId = "lance_r2",
                    label = "Lancer la requête",
                    style = "float", 
                    color = "success", size = "xs"),
                  # verbatimTextOutput("rsa_requ_main"),
                  dataTableOutput("rsa_requ_main"),
                  width = 12, solidHeader = TRUE, collapsible = TRUE, 
                  collapsed = FALSE, title = 'Saisir une requête ad hoc', status = "primary")    
        )))))


