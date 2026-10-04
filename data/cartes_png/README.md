# Cartes AURA — exports PNG

10 cartes, 4200 × 2760 pixels, 300 dpi.

Les données proviennent des HTML actuellement filtrés ; les flux ont au moins 5 participations. Les cartes par rôle utilisent la même emprise et conservent toutes leurs localisations.

Il s’agit de versions statiques redessinées : les cartes de participants utilisent des points individuels à la place des grappes interactives. Les cartes de flux réutilisent les sprites Leaflet et les glyphes Font Awesome du HTML, avec les couleurs et rotations de chaque marqueur, à leurs coordonnées originales. Les couleurs grid7, limites, courbes et boucles sont conservées.

| Fichier | Carte | Marqueurs | Liens |
|---|---|---:|---:|
| 01_flux_ensemble_icons_france.png | Participations des entreprises AURA par département | 41 | 54 |
| 02_flux_provenance_transformation_icons_france.png | Provenance vers entreprises de transformation | 22 | 26 |
| 03_flux_construction_projets_icons.png | Entreprises de construction vers projets | 18 | 13 |
| 04_flux_transformation_construction_icons.png | Entreprises de transformation vers charpentiers | 19 | 14 |
| 05_projets_AURA.png | Carte des projets AURA | 177 | 0 |
| 06_acteurs_tous_roles.png | Acteurs des projets AURA — tous les rôles | 801 | 0 |
| 07_acteurs_maitres_ouvrage_france_centree.png | Acteurs des projets AURA — Maître d'ouvrage | 131 | 0 |
| 08_acteurs_etudes_ingenieries_france_centree.png | Acteurs des projets AURA — Études et ingénieries | 322 | 0 |
| 09_acteurs_construction_france_centree.png | Acteurs des projets AURA — Entreprise de construction | 200 | 0 |
| 10_acteurs_transformation_france_centree.png | Acteurs des projets AURA — Entreprises de transformation | 148 | 0 |

Fond : Esri World Light Gray Base, © Esri, DeLorme, NAVTEQ. Grille de densité : cache local grid7 INSEE/ANCT.

Régénération : exécuter export_cartes_png.py depuis le dossier du projet (Python avec Pillow ; R avec sf, ggplot2, jsonlite, png et ragg). Le script relit les HTML à chaque export et réutilise les tuiles en cache.

Mise à jour : les cartes 07–10 sont centrées sur la France métropolitaine, Corse incluse, avec une emprise commune. Toutes les localisations sont conservées. Les anciennes images sont conservées séparément.

Les cartes de flux 01 et 02 utilisent également une emprise commune couvrant toute la France métropolitaine, Corse incluse, avec leurs icônes originales.
