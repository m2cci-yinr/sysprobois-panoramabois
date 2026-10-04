# Run from the project root: shiny::runApp("scripts")
library(shiny)
root <- normalizePath(file.path(getwd(), ".."), winslash="/", mustWork=TRUE)
read_data <- function(name) {
 path <- file.path(root,"data",name)
 if (!file.exists(path)) stop("Fichier manquant : ",name,". Exécuter sankey_aura_1.R pour préparer les résultats.")
 read.csv(path,encoding="UTF-8",check.names=FALSE,stringsAsFactors=FALSE)
}
source(file.path(root,"scripts","flux_interactive.R"),encoding="UTF-8",local=TRUE)
map_files <- c(interactive_flux_choices,
 "Localisation du projet · AURA"="data/carte_aura_heatmap_nombre_projets.html",
 "Acteurs · localisation du projet"="data/carte_aura_heatmap_acteurs.html")
sankey_file <- "data/sankey_aura_filiere_bois_provenance_projets_carte.html"
figures <- c("Systèmes constructifs et essences"="correlation_systemes_constructifs_essences.png",
 "Types de bâtiments et systèmes"="correlation_types_batiments_systemes_constructifs.png",
 "Essences et provenances"="correlation_essences_provenances.png",
 "Distances entre groupes"="distances_acteurs_entre_groupes.png",
 "Localité des acteurs"="localite_de_acteur.png")
# Serve only presentation files, never the source datasets or the project root.
public <- tempfile("aura-public-"); dir.create(public)
for (f in c(setdiff(unname(map_files),unname(interactive_flux_choices)),sankey_file,unname(figures))) {
 src <- file.path(root,f)
 if (!file.exists(src)) src <- file.path(root,"figure",f)
 if (file.exists(src)) file.copy(src,public,overwrite=TRUE)
}
for (d in c("sankey_aura_filiere_bois_assets","carte_aura_heatmap_acteurs_files",
 "carte_aura_heatmap_nombre_projets_files","carte_aura_heatmap_projets_acteurs_assets")) {
 if (dir.exists(file.path(root,"data",d))) file.copy(file.path(root,"data",d),public,recursive=TRUE)
}
# Rename presentation labels in embedded widgets without altering source data or scripts.
file.copy(file.path(root,"scripts","dashboard_labels.js"),public,overwrite=TRUE)
for(f in c(setdiff(unname(map_files),unname(interactive_flux_choices)),sankey_file)) {
 path<-file.path(public,f)
 if(file.exists(path)) {
  html<-paste(readLines(path,encoding="UTF-8",warn=FALSE),collapse="\n")
  html<-sub("</body>",'<script src="dashboard_labels.js"></script></body>',html,fixed=TRUE)
  writeLines(enc2utf8(html),path,useBytes=TRUE)
 }
}
addResourcePath("aura-results",public)
counts <- read_data("participations_entreprises_par_role_departement.csv")
distances <- read_data("distances_acteurs_projets_detail_roles.csv")
distances <- distances[!tolower(trimws(distances$groupe_acteur)) %in% c("autre","autres"),,drop=FALSE]
counts$dept_acteur <- dept_name(counts$dept_acteur)
ui <- navbarPage("Filière bois · AURA",id="section",
 header=tags$head(tags$style(HTML("body{background:#f5f6f8} .container-fluid{max-width:1600px} .well{background:white;border-color:#e2e5e9} iframe{background:white;border:1px solid #e2e5e9;border-radius:8px} .shiny-plot-output{background:white} h3{color:#25465a} .flux-map-wrap{position:relative} .flux-legend summary{cursor:pointer;padding:4px} .flux-filter-box .form-group{margin-bottom:10px} .flux-legend-control{background:#f7f7f7;padding:8px;max-width:285px;box-shadow:0 1px 5px #999;border-radius:4px} .flux-legend h5{margin:10px 0 3px;font-weight:bold} .leaflet-control-layers-expanded{border:2px solid #aaa;border-radius:5px;padding:5px 7px;max-height:42vh;overflow:auto;font-size:13px} .leaflet-control-layers-overlays label{margin:2px 0;font-weight:normal} .flux-legend summary{font-size:16px;font-weight:600} .flux-legend-body{max-height:310px;overflow:auto;font-size:12px;color:#444} .flux-legend section{margin:8px 0 12px} .flux-legend h4{font-size:13px;font-weight:600;margin:0 0 6px} .legend-note{font-size:11px;color:#666;margin:4px 0 8px} .legend-row{display:flex;align-items:center;gap:7px;margin:4px 0} .legend-line{display:inline-block;width:45px;flex-shrink:0;border-top-style:solid;border-top-color:#555} .legend-swatch{display:inline-block;width:20px;height:20px;flex-shrink:0} .flux-legend footer{border-top:1px solid #ddd;padding-top:7px} .legend-boundaries{display:flex;gap:18px;margin-bottom:8px} .legend-region,.legend-dept{display:inline-block;width:34px;vertical-align:middle;margin-right:5px;border-top:3px dashed #444} .legend-dept{border-top:1px dashed #666}"))),
 tabPanel("Sankey",h3("De la provenance à la localisation du projet"),
 p("Survolez les liens pour consulter les valeurs et déplacez les nœuds pour explorer la filière."),uiOutput("sankey")),
 tabPanel("Cartes",sidebarLayout(sidebarPanel(width=3,
 selectInput("map","Carte",map_files),
 uiOutput("map_controls")),
 mainPanel(width=9,uiOutput("map_view")))),
 tabPanel("Statistiques",sidebarLayout(sidebarPanel(width=3,
 selectInput("metric","Indicateur",c("Participations par département"="counts","Distances acteurs–localisation du projet"="distance")),
 uiOutput("role_select"),downloadButton("download_csv","Télécharger les données affichées")),
 mainPanel(width=9,plotOutput("stat_plot",height="800px"),tableOutput("stat_table")))),
 tabPanel("Figures existantes",selectInput("figure","Figure",figures),uiOutput("figure_view"),downloadButton("download_figure","Télécharger le PNG")),
 tabPanel("À propos",h3("Sources et actualisation"),
 p("Cette application reprend le Sankey et les résultats de sankey_aura_1.R, ainsi que les flux interactifs reconstruits à partir des tables complètes de participations."),
 p("Les filtres de statistiques s’appliquent uniquement à l’onglet Statistiques. Les cartes de flux, localisations du projet et acteurs sont interactives."),
 p("Après une mise à jour des données, régénérez les résultats et les données de flux, puis redémarrez l’application.")))
# Cartes 03/04 : seuil fixe ≥ 2, quel que soit le niveau de zoom.
fixed_threshold_maps <- c("03_flux_construction_projets_default","04_flux_transformation_construction_default")
server <- function(input,output,session) {
 frame <- function(f) {
  public_name <- basename(f)
  validate(need(file.exists(file.path(public,public_name)),paste("Résultat manquant :",f)))
  tags$iframe(src=paste0("aura-results/",public_name),title=f,width="100%",height="800px")
 }
 output$sankey <- renderUI(frame(sankey_file))
 output$map_controls <- renderUI({
 req(input$map %in% map_files)
 if(input$map %in% fixed_threshold_maps) tagList(
 p(strong("Seuil : ≥ 2 participations"),"(fixe pour cette carte, zoomée ou non sur AURA)."),
 p("Cochez les rôles dans le contrôle de la carte : une liaison apparaît lorsque ses deux rôles sont cochés."))
 else if(input$map %in% interactive_flux_choices) tagList(
 radioButtons("flux_threshold","Version — seuil uniforme",choices=c("≥ 5 participations"=5,"≥ 2 participations"=2),selected=5),
 p("Cochez les rôles dans le contrôle de la carte : une liaison apparaît lorsque ses deux rôles sont cochés."))
 else p("Carte interactive : utilisez les couches et les commandes de zoom.")
 })
 output$map_view <- renderUI({
 req(input$map %in% map_files)
 if(input$map %in% interactive_flux_choices)
   tagList(h3(names(interactive_flux_choices)[match(input$map,interactive_flux_choices)]),
     leaflet::leafletOutput("flux_map",height="850px"))
 else frame(input$map)
 })
 output$flux_map <- leaflet::renderLeaflet({
 req(input$map %in% interactive_flux_choices)
 if(input$map %in% fixed_threshold_maps) return(build_flux_map(input$map,2))
 req(input$flux_threshold)
 build_flux_map(input$map,as.numeric(input$flux_threshold))
 })
 output$figure_view <- renderUI({req(input$figure %in% figures)
 validate(need(file.exists(file.path(public,input$figure)),"Cette figure n’a pas encore été exportée."))
 tags$img(src=paste0("aura-results/",input$figure),style="max-width:100%;height:auto",alt=names(figures)[match(input$figure,figures)])})
 output$role_select <- renderUI({
 roles <- if(input$metric=="counts") counts$role else distances$groupe_acteur
 selectInput("role","Rôle",c("Tous",ordered_roles(roles)))
 })
 filtered <- reactive({
 req(input$role)
 d <- if(input$metric=="counts") counts else distances
 col <- if(input$metric=="counts") "role" else "groupe_acteur"
 if(input$role!="Tous") d <- d[d[[col]]==input$role,,drop=FALSE]
 d
 })
 summary_data <- reactive({
 d <- filtered();validate(need(nrow(d)>0,"Aucune donnée pour cette sélection."))
 if(input$metric=="counts") {
 a <- aggregate(d$nb_participations,list(Département=d$dept_acteur),sum)
 names(a)[2] <- "Participations"; a[order(-a$Participations),]
 } else {
 x <- d$distance_km[is.finite(d$distance_km)]
 validate(need(length(x)>0,"Aucune distance disponible."))
 data.frame(Indicateur=c("Liaisons","Médiane (km)","Moyenne (km)","Maximum (km)"),Valeur=c(length(x),round(median(x),2),round(mean(x),2),round(max(x),2)))
 }
 })
 output$stat_plot <- renderPlot({
 d <- filtered();validate(need(nrow(d)>0,"Aucune donnée."))
 par(mar=c(5,12,3,1))
 if(input$metric=="counts") {
 a <- summary_data();barplot(rev(a$Participations),names.arg=rev(a$Département),horiz=TRUE,las=1,col="#356b83",border=NA,xlab="Participations",main="Participations par département")
 } else {
 x <- d$distance_km[is.finite(d$distance_km)];validate(need(length(x)>0,"Aucune distance disponible."))
 hist(x,breaks=30,col="#356b83",border="white",xlab="Distance (km)",ylab="Nombre de liaisons",main="Distances acteurs–localisation du projet")
 }
 },res=110)
 output$stat_table <- renderTable(summary_data(),striped=TRUE)
 output$download_csv <- downloadHandler(filename=function()paste0("aura_",input$metric,".csv"),content=function(file)write.csv(filtered(),file,row.names=FALSE,fileEncoding="UTF-8"))
 output$download_figure <- downloadHandler(filename=function()basename(input$figure),content=function(file){req(input$figure %in% figures);file.copy(file.path(root,"figure",input$figure),file)})
}
shinyApp(ui,server)

