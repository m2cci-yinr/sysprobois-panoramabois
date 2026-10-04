# sysprobois-panoramabois

Scripts, données et visualisations associés au mémoire de fin d’études GEOMAS sur les systèmes productifs de la construction bois.

## Organisation du projet

```text
sysprobois-panoramabois/
├── data/       fichiers CSV, Excel, GeoJSON et ressources des cartes
├── figure/     figures PNG exportées
├── HTML/       visualisations HTML autonomes
└── scripts/    notebook Python et scripts R
```

Les commandes doivent être exécutées depuis le dossier racine du projet :

```text
E:/S10/stage/python/sysprobois-panoramabois
```

Si le projet est copié ailleurs, remplacer ce chemin par le nouveau chemin local. Les chemins contenant des espaces ou des caractères accentués doivent être placés entre guillemets.

## Environnement Python

Le notebook `scripts/01_collecte_preparation.ipynb` utilise `requests`, `beautifulsoup4`, `pandas`, `numpy`, `folium` et `jupyter`.

```powershell
py -m pip install requests beautifulsoup4 pandas numpy folium jupyter
py -m jupyter notebook scripts/01_collecte_preparation.ipynb
```

Le notebook collecte des données en ligne et peut réécrire plusieurs fichiers CSV. Vérifier le dossier de travail avant d’exécuter les cellules de collecte.

## Environnement R

Les scripts R nécessitent R 4.5 ou une version récente et les packages suivants :

```r
install.packages(c(
  "shiny", "dplyr", "readr", "stringr", "stringi", "tidyr",
  "tidygeocoder", "jsonlite", "networkD3", "leaflet",
  "leaflet.extras", "leaflet.extras2", "htmlwidgets", "htmltools",
  "corrplot", "readxl", "sf", "base64enc"
))
```

Depuis la racine du projet :

```powershell
Rscript scripts/02_analyse_visualisation.R
```

Si R n’est pas dans le `PATH`, utiliser le chemin complet vers `Rscript.exe`, par exemple :

```powershell
& "D:/app/R-4.5.1/bin/Rscript.exe" scripts/02_analyse_visualisation.R
```

## Application Shiny

Depuis la racine du projet :

```powershell
Rscript -e "shiny::runApp('scripts')"
```

L’application dépend des résultats présents dans `data/` et des fichiers HTML/PNG générés dans `HTML/` et `figure/`. En cas de fichier absent, vérifier le dossier de travail et les chemins définis dans `scripts/app.R` et `scripts/flux_interactive_data.R`.

## Ordre conseillé

1. Exécuter le notebook Python pour télécharger ou actualiser les données.
2. Vérifier les fichiers produits dans `data/`.
3. Exécuter `scripts/02_analyse_visualisation.R`.
4. Lancer l’application avec `shiny::runApp("scripts")`.

Les fichiers déjà présents dans `data/`, `figure/` et `HTML/` permettent de consulter les résultats sans relancer la collecte.

## Vérification rapide

```powershell
Rscript -e "parse(file='scripts/02_analyse_visualisation.R'); parse(file='scripts/app.R'); parse(file='scripts/flux_interactive.R'); parse(file='scripts/flux_interactive_data.R')"
node --check scripts/dashboard_labels.js
```

## Données et reproductibilité

Les données téléchargées, les caches géographiques et les fichiers générés peuvent évoluer. Avant toute nouvelle collecte, conserver une copie des résultats actuels ou travailler sur une branche Git dédiée. Les appels au site Panoramabois et aux services géographiques nécessitent une connexion Internet.
