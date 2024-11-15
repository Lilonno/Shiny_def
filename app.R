library(shiny)
library(readxl)
library(survival)
library(survminer)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(gridExtra)  

# Importer les bases de données
dataDEF <- read_excel("dataDEF.xlsx")

# Traitement des données
dataDEF$Generation <- as.Date(dataDEF$Generation)

# Ajouter des colonnes pour les secteurs d'activité
dataDEF$Groupe <- with(dataDEF, ifelse(grepl("agri|pêche|forêt", `Secteur d'activité`, ignore.case = TRUE), "Primaire",
                                       ifelse(grepl("construction|industrie|production", `Secteur d'activité`, ignore.case = TRUE), "Secondaire", "Tertiaire")))

# Ajouter des colonnes pour TPE, PME...
dataDEF$Taille <- with(dataDEF, ifelse(grepl("Plus de 200 salariés", `Tranche d'effectifs`, ignore.case = TRUE), "GE",
                                       ifelse(grepl("50 - 200 salariés", `Tranche d'effectifs`, ignore.case = TRUE), "ETI",
                                              ifelse(grepl("10 - 50 salariés", `Tranche d'effectifs`, ignore.case = TRUE), "PME", "TPE"))))

# Ajouter une colonne avec l'année
library(dplyr)
library(lubridate)

dataDEF <- dataDEF %>%
  mutate(
    Generation = as.Date(Generation),  # Convertir Generation en date
    Année = year(Generation)            # Extraire l'année de Generation
  )

colnames(dataDEF)

# Créer une nouvelle base de données avec les colonnes désirées
data_nouvelle <- dataDEF %>%
  select(Année, Generation, `Secteur d'activité`, Groupe, Taille, `Taux de défaillances 3 mois`, `Taux de défaillances 6 mois`) %>%
  group_by(Taille, `Secteur d'activité`, Groupe, Année, Generation) %>%
  summarise(
    taux_defaillance_moyen_3_mois = mean(`Taux de défaillances 3 mois`, na.rm = TRUE),
    taux_defaillance_moyen_6_mois = mean(`Taux de défaillances 6 mois`, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  distinct() %>%
  group_by(Groupe, Taille, Année) %>%
  mutate(
    taux_defaillance_groupe_3_mois = mean(taux_defaillance_moyen_3_mois, na.rm = TRUE),
    taux_defaillance_groupe_6_mois = mean(taux_defaillance_moyen_6_mois, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  group_by(Groupe, `Secteur d'activité`) %>%
  mutate(
    taux_groupe_3mois = mean(taux_defaillance_moyen_3_mois, na.rm = TRUE),
    taux_groupe_6mois = mean(taux_defaillance_moyen_6_mois, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  group_by(Année) %>%
  mutate(
    taux_generale_3mois = mean(taux_defaillance_moyen_3_mois, na.rm = TRUE),
    taux_generale_6mois = mean(taux_defaillance_moyen_6_mois, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  group_by(Groupe) %>%
  mutate(
    taux_generale_3mois = mean(taux_defaillance_moyen_3_mois, na.rm = TRUE),
    taux_generale_6mois = mean(taux_defaillance_moyen_6_mois, na.rm = TRUE)
  ) %>%
  ungroup()

colnames(data_nouvelle)
# Créer une nouvelle base de données avec les colonnes désirées pour semestre
data_nouvelle1 <- dataDEF %>%
  select(Année, Generation, `Secteur d'activité`, Groupe, Taille, `Taux de défaillances 3 mois`, `Taux de défaillances 6 mois`) %>%
  group_by(Taille, `Secteur d'activité`, Groupe, Generation) %>%
  summarise(
    taux_defaillance_moyen_3_mois = mean(`Taux de défaillances 3 mois`, na.rm = TRUE),
    taux_defaillance_moyen_6_mois = mean(`Taux de défaillances 6 mois`, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  distinct() %>%
  group_by(Groupe, Taille, Generation) %>%
  mutate(
    taux_defaillance_groupe_3_mois = mean(taux_defaillance_moyen_3_mois, na.rm = TRUE),
    taux_defaillance_groupe_6_mois = mean(taux_defaillance_moyen_6_mois, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  group_by(Groupe, `Secteur d'activité`) %>%
  mutate(
    taux_groupe_3mois = mean(taux_defaillance_moyen_3_mois, na.rm = TRUE),
    taux_groupe_6mois = mean(taux_defaillance_moyen_6_mois, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  group_by(Generation) %>%
  mutate(
    taux_generale_3mois = mean(taux_defaillance_moyen_3_mois, na.rm = TRUE),
    taux_generale_6mois = mean(taux_defaillance_moyen_6_mois, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  group_by(Groupe) %>%
  mutate(
    taux_generale_3mois = mean(taux_defaillance_moyen_3_mois, na.rm = TRUE),
    taux_generale_6mois = mean(taux_defaillance_moyen_6_mois, na.rm = TRUE)
  ) %>%
  ungroup()


# Interface utilisateur
ui <- fluidPage(
  titlePanel("Analyse des Défaillances", windowTitle = "Analyse des Défaillances"),
  
  # Onglets pour séparer les sections
  tabsetPanel(
    tabPanel("Analyse des Défaillances",
             div(style = "margin: 20px;",
                 sidebarLayout(
                   sidebarPanel(
                     h3("Filtres de sélection", style = "color: #444444; margin-bottom: 15px;"),
                     selectInput("groupe_general", "Sélectionnez un groupe :", 
                                 choices = c("Tout", unique(data_nouvelle$Groupe)), selected = "Tout"),
                     selectInput("secteur_general", "Sélectionnez un secteur d'activité :", 
                                 choices = NULL)  # Choix mis à jour dynamiquement
                   ),
                   
                   mainPanel(
                     tabsetPanel(
                       tabPanel("Données en semestre",
                                tabsetPanel(
                                  tabPanel("Graphique à 3 mois", 
                                           plotOutput("plot3mois_semestre"),
                                           br(),
                                           h4("Taux de défaillance moyen à 3 mois (semestre)", style = "text-align: center; color: #444444; margin-top: 10px;"),
                                           textOutput("analyse3mois_semestre")  # Espace pour l'analyse
                                  ),
                                  tabPanel("Graphique à 6 mois", 
                                           plotOutput("plot6mois_semestre"),
                                           br(),
                                           h4("Taux de défaillance moyen à 6 mois (semestre)", style = "text-align: center; color: #444444; margin-top: 10px;"),
                                           textOutput("analyse6mois_semestre")  # Espace pour l'analyse
                                  ),
                                  tabPanel("Comparaison", 
                                           plotOutput("plot_comparaison_semestre"),  # Graphiques côte à côte
                                           br(),
                                           h4("Comparaison des taux de défaillance (semestre)", style = "text-align: center; color: #444444; margin-top: 10px;"),
                                           textOutput("analysecomp_semestre")
                                  ),
                                  tabPanel("Tableau Récapitulatif", 
                                           tableOutput("tableau_recap_semestre"),
                                           br(),
                                           h4("Tableau récapitulatif des taux de défaillance (semestre)", style = "text-align: center; color: #444444; margin-top: 10px;")
                                  )
                                )
                       ),
                       tabPanel("Données en année",
                                tabsetPanel(
                                  tabPanel("Graphique à 3 mois", 
                                           plotOutput("plot3mois"),
                                           br(),
                                           h4("Taux de défaillance moyen à 3 mois", style = "text-align: center; color: #444444; margin-top: 10px;"),
                                           textOutput("analyse3mois")  # Espace pour l'analyse
                                  ),
                                  tabPanel("Graphique à 6 mois", 
                                           plotOutput("plot6mois"),
                                           br(),
                                           h4("Taux de défaillance moyen à 6 mois", style = "text-align: center; color: #444444; margin-top: 10px;"),
                                           textOutput("analyse6mois")  # Espace pour l'analyse
                                  ),
                                  tabPanel("Comparaison", 
                                           plotOutput("plot_comparaison"),  # Graphiques côte à côte
                                           br(),
                                           h4("Comparaison des taux de défaillance", style = "text-align: center; color: #444444; margin-top: 10px;"),
                                           textOutput("analysecomp")
                                  ),
                                  tabPanel("Tableau Récapitulatif", 
                                           tableOutput("tableau_recap"),
                                           br(),
                                           h4("Tableau récapitulatif des taux de défaillance", style = "text-align: center; color: #444444; margin-top: 10px;")
                                  )
                                )
                       )
                     )
                   )
                 )
             )
    )
  )
)


server <- function(input, output, session) {
  
  # Met à jour le filtre secteur en fonction du groupe sélectionné
  observeEvent(input$groupe_general, {
    secteurs_filtrés <- data_nouvelle %>%
      filter(Groupe == input$groupe_general | input$groupe_general == "Tout") %>%
      pull(`Secteur d'activité`) %>%
      unique()
    
    secteurs_filtrés <- c("Tout", secteurs_filtrés)
    
    updateSelectInput(session, "secteur_general", choices = secteurs_filtrés, selected = "Tout")
  })
  
  # Données filtrées réactives pour tous les graphiques et le tableau
  filtered_data <- reactive({
    data_nouvelle %>%
      filter(
        (input$groupe_general == "Tout" | Groupe == input$groupe_general) &
          (input$secteur_general == "Tout" | `Secteur d'activité` == input$secteur_general)
      )
  })
  
  # Graphique 3 mois
  output$plot3mois <- renderPlot({
    data <- filtered_data() %>%
      group_by(Année, Taille) %>%
      summarise(taux_defaillance_3mois = mean(taux_defaillance_moyen_3_mois, na.rm = TRUE), .groups = "drop")
    
    ggplot(data, aes(x = Année, y = taux_defaillance_3mois, color = Taille)) +
      geom_line(size = 1.2) +
      labs(title = "Taux de défaillance moyen à 3 mois", x = "Année", y = "Taux de défaillance à 3 mois") +
      theme_minimal() +
      theme(legend.position = "top", plot.title = element_text(color = "#444444"))
  })
  
  output$analyse6mois <- renderText({
    ""
  })
  
  # Graphique 3 mois trimestre
  output$plot3mois_semestre <- renderPlot({
    data <- filtered_data() %>%
      group_by(Generation, Taille) %>%
      summarise(taux_defaillance_3mois = mean(taux_defaillance_moyen_3_mois, na.rm = TRUE), .groups = "drop")
    
    ggplot(data, aes(x = Generation, y = taux_defaillance_3mois, color = Taille)) +
      geom_line(size = 1.2) +
      labs(title = "Taux de défaillance moyen à 3 mois", x = "Generation", y = "Taux de défaillance à 3 mois") +
      theme_minimal() +
      theme(legend.position = "top", plot.title = element_text(color = "#444444")) +
      annotate("rect", xmin = as.Date("2020-03-01"), xmax = as.Date("2020-05-30"), ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "blue") +
      annotate("rect", xmin = as.Date("2020-10-01"), xmax = as.Date("2020-12-15"), ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "blue") +
      annotate("rect", xmin = as.Date("2021-04-01"), xmax = as.Date("2021-05-30"), ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "blue") +
      annotate("rect", xmin = as.Date("2022-02-24"), xmax = as.Date("2022-03-31"), ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "red")
  })
  
  output$analyse3mois_semestre <- renderText({
    "Les périodes de confinement liées à la COVID-19 sont représentées en bleu, tandis que le début de la guerre en Ukraine est indiqué en rouge"
  })
  
  
  # Graphique 6 mois
  output$plot6mois <- renderPlot({
    data <- filtered_data() %>%
      group_by(Année, Taille) %>%
      summarise(taux_defaillance_6mois = mean(taux_defaillance_moyen_6_mois, na.rm = TRUE), .groups = "drop")
    
    ggplot(data, aes(x = Année, y = taux_defaillance_6mois, color = Taille)) +
      geom_line(size = 1.2) +
      labs(title = "Taux de défaillance moyen à 6 mois", x = "Année", y = "Taux de défaillance à 6 mois") +
      theme_minimal() +
      theme(legend.position = "top", plot.title = element_text(color = "#444444"))
  })
  
  output$analyse6mois <- renderText({
    ""
  })
  
  
  # Graphique 6 mois trimestre
  output$plot6mois_semestre <- renderPlot({
    data <- filtered_data() %>%
      group_by(Generation, Taille) %>%
      summarise(taux_defaillance_6mois = mean(taux_defaillance_moyen_6_mois, na.rm = TRUE), .groups = "drop")
    
    ggplot(data, aes(x = Generation, y = taux_defaillance_6mois, color = Taille)) +
      geom_line(size = 1.2) +
      labs(title = "Taux de défaillance moyen à 6 mois", x = "Generation", y = "Taux de défaillance à 6 mois") +
      theme_minimal() +
      theme(legend.position = "top", plot.title = element_text(color = "#444444")) +
      annotate("rect", xmin = as.Date("2020-03-01"), xmax = as.Date("2020-05-30"), ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "blue") +
      annotate("rect", xmin = as.Date("2020-10-01"), xmax = as.Date("2020-12-15"), ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "blue") +
      annotate("rect", xmin = as.Date("2021-04-01"), xmax = as.Date("2021-05-30"), ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "blue") +
      annotate("rect", xmin = as.Date("2022-02-24"), xmax = as.Date("2022-03-31"), ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "red")
  })
  
  output$analyse6mois_semestre <- renderText({
    "Les périodes de confinement liées à la COVID-19 sont représentées en bleu, tandis que le début de la guerre en Ukraine est indiqué en rouge"
  })
  
  # Graphique de comparaison
  output$plot_comparaison <- renderPlot({
    data <- filtered_data() %>%
      group_by(Année) %>%
      summarise(taux_defaillance_3mois = mean(taux_defaillance_moyen_3_mois, na.rm = TRUE),
                taux_defaillance_6mois = mean(taux_defaillance_moyen_6_mois, na.rm = TRUE), .groups = "drop")
    
    data_long <- data %>%
      pivot_longer(cols = c(taux_defaillance_3mois, taux_defaillance_6mois), 
                   names_to = "taux", values_to = "valeur")
    
    ggplot(data_long, aes(x = Année, y = valeur, color = taux)) +
      geom_line(size = 1.2) +
      labs(title = "Comparaison des taux de défaillance", x = "Année", y = "Taux de défaillance") +
      theme_minimal() +
      theme(legend.position = "top", plot.title = element_text(color = "#444444"))
  })
  
  
  output$analyse6mois <- renderText({
    ""
  })
  
  
  # Graphique de comparaison semestre
  output$plot_comparaison_semestre <- renderPlot({
    data <- filtered_data() %>%
      group_by(Generation) %>%
      summarise(taux_defaillance_3mois = mean(taux_defaillance_moyen_3_mois, na.rm = TRUE),
                taux_defaillance_6mois = mean(taux_defaillance_moyen_6_mois, na.rm = TRUE), .groups = "drop")
    
    data_long <- data %>%
      pivot_longer(cols = c(taux_defaillance_3mois, taux_defaillance_6mois), 
                   names_to = "taux", values_to = "valeur")
    
    ggplot(data_long, aes(x = Generation, y = valeur, color = taux)) +
      geom_line(size = 1.2) +
      labs(title = "Comparaison des taux de défaillance", x = "Année", y = "Taux de défaillance") +
      theme_minimal() +
      theme(legend.position = "top", plot.title = element_text(color = "#444444"))
    
    ggplot(data_long, aes(x = Generation, y = valeur, color = taux)) +
      geom_line(size = 1.2) +
      labs(title = "Comparaison des taux de défaillance", x = "Année", y = "Taux de défaillance") +
      theme_minimal() +
      theme(legend.position = "top", plot.title = element_text(color = "#444444")) +
      annotate("rect", xmin = as.Date("2020-03-01"), xmax = as.Date("2020-05-30"), ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "blue") +
      annotate("rect", xmin = as.Date("2020-10-01"), xmax = as.Date("2020-12-15"), ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "blue") +
      annotate("rect", xmin = as.Date("2021-04-01"), xmax = as.Date("2021-05-30"), ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "blue") +
      annotate("rect", xmin = as.Date("2022-02-24"), xmax = as.Date("2022-03-31"), ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "red")
  })
  
  output$analysecomp_semestre <- renderText({
    "Les périodes de confinement liées à la COVID-19 sont représentées en bleu, tandis que le début de la guerre en Ukraine est indiqué en rouge"
  })
  
  
  #Tableau de comparaison année
  output$tableau_recap <- renderTable({
    data <- filtered_data() %>%
      group_by(Groupe, Taille) %>%
      summarise(
        taux_3mois = mean(taux_defaillance_moyen_3_mois, na.rm = TRUE),
        taux_6mois = mean(taux_defaillance_moyen_6_mois, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(desc(taux_3mois), desc(taux_6mois))
    
    # Formater les taux pour afficher 2 décimales
    data$taux_3mois <- format(data$taux_3mois, nsmall = 2)
    data$taux_6mois <- format(data$taux_6mois, nsmall = 2)
    
    data
  })
  
  
  #Tableau recap semestre
  output$tableau_recap_semestre <- renderTable({
    data <- filtered_data() %>%
      group_by(Groupe, Taille) %>%
      summarise(
        taux_3mois = mean(taux_defaillance_moyen_3_mois, na.rm = TRUE),
        taux_6mois = mean(taux_defaillance_moyen_6_mois, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(desc(taux_3mois), desc(taux_6mois))
    
    # Formater les taux pour afficher 2 décimales
    data$taux_3mois <- format(data$taux_3mois, nsmall = 2)
    data$taux_6mois <- format(data$taux_6mois, nsmall = 2)
    
    data
  })
}



# Exécution de l'application
shinyApp(ui = ui, server = server)



