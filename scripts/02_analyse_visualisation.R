
# ============================================================================
# Analyse de la filière bois en Auvergne-Rhône-Alpes
# ============================================================================

# Encodage : UTF-8. Exécution : Rscript sankey_aura_1.R
# Les données sources et les sorties se trouvent dans le dossier du script.
# Les caches géographiques sont mis à jour pendant la préparation des données.
# Organisation : configuration, préparation, visualisations, CSV, PNG, HTML.

# ============================================================================
# 1. Configuration et fonctions utilitaires
# ============================================================================

packages <- c(
  "dplyr", "readr", "stringr", "stringi",
  "tidyr", "tidygeocoder", "jsonlite", "networkD3", "leaflet",
  "leaflet.extras", "leaflet.extras2", "htmlwidgets", "htmltools", "corrplot",
  "readxl", "sf"
)

library(dplyr)
library(readr)
library(stringr)
library(stringi)
library(tidyr)
library(tidygeocoder)
library(jsonlite)
library(networkD3)
library(leaflet)
library(leaflet.extras)
library(leaflet.extras2)
library(htmlwidgets)
library(htmltools)
library(corrplot)
library(readxl)
library(sf)

get_script_dir <- function() {
  cmd_file <- commandArgs(trailingOnly = FALSE)
  file_arg <- "--file="
  file_match <- grep(file_arg, cmd_file, value = TRUE)
  if (length(file_match) > 0) {
    return(dirname(normalizePath(sub(file_arg, "", file_match[1]), winslash = "/")))
  }
  
  frame_files <- vapply(sys.frames(), function(x) {
    if (!is.null(x$ofile)) x$ofile else NA_character_
  }, character(1))
  frame_files <- frame_files[!is.na(frame_files)]
  if (length(frame_files) > 0) {
    return(dirname(normalizePath(frame_files[length(frame_files)], winslash = "/")))
  }
  
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    active_file <- rstudioapi::getActiveDocumentContext()$path
    if (!is.null(active_file) && active_file != "") {
      return(dirname(normalizePath(active_file, winslash = "/")))
    }
  }
  
  normalizePath(getwd(), winslash = "/", mustWork = FALSE)
}

data_file <- function(filename) {
  candidates <- c(
    file.path(get_script_dir(), filename),
    file.path(get_script_dir(), "..", "data", filename),
    file.path(getwd(), filename)
  )
  found <- candidates[file.exists(candidates)]
  
  if (length(found) == 0) {
    stop(
      "Fichier introuvable: ", filename, "\n",
      "Dossiers testes:\n- ", paste(dirname(candidates), collapse = "\n- "),
      call. = FALSE
    )
  }
  
  found[1]
}

output_file <- function(filename) {
  data_dir <- file.path(get_script_dir(), "..", "data")
  if (!dir.exists(data_dir)) dir.create(data_dir, recursive = TRUE)
  file.path(data_dir, filename)
}

# Périmètre de l'analyse : projets situés en Auvergne-Rhône-Alpes.
target_region <- "Auvergne-Rhone-Alpes"

clean_text <- function(x) {
  x %>%
    as.character() %>%
    str_replace_all("\u00a0", " ") %>%
    str_squish()
}

clean_commune <- function(x) {
  x %>%
    clean_text() %>%
    str_replace_all("[._/]", " ") %>%
    str_replace_all("\\s*-\\s*", "-") %>%
    str_to_lower(locale = "fr") %>%
    stringi::stri_trans_general("Latin-ASCII") %>%
    str_to_title(locale = "fr")
}

clean_ascii <- function(x) {
  x %>%
    clean_text() %>%
    stringi::stri_trans_general("Latin-ASCII")
}

node_name <- function(stage, label) {
  paste0(stage, " | ", label)
}

display_name <- function(x) {
  str_replace(x, "^.* \\| ", "")
}

haversine_km <- function(lon1, lat1, lon2, lat2) {
  radius_km <- 6371.0088
  to_rad <- pi / 180
  lat1_rad <- lat1 * to_rad
  lat2_rad <- lat2 * to_rad
  delta_lat <- (lat2 - lat1) * to_rad
  delta_lon <- (lon2 - lon1) * to_rad
  
  a <- sin(delta_lat / 2)^2 +
    cos(lat1_rad) * cos(lat2_rad) * sin(delta_lon / 2)^2
  2 * radius_km * atan2(sqrt(a), sqrt(1 - a))
}

actor_key <- function(id_acteur, nom_acteur) {
  id_acteur <- na_if(clean_text(id_acteur), "")
  nom_acteur <- na_if(clean_ascii(nom_acteur), "")
  if_else(
    !is.na(id_acteur),
    paste0("id:", id_acteur),
    if_else(!is.na(nom_acteur), paste0("nom:", nom_acteur), NA_character_)
  )
}

normalise_groupe_analyse <- function(groupe_acteur) {
  groupe_acteur %>%
    clean_ascii() %>%
    str_remove("\\s*\\([0-9]+\\)$") %>%
    recode(
      "Maitre d'ouvrage" = "Maître d'ouvrage",
      "Etudes et ingenieries" = "Études et ingénieries",
      "Entreprises des lots bois" = "Entreprise de construction",
      "Fournisseurs du bois" = "Entreprises de transformation"
    )
}

make_flow_curve <- function(lon_source, lat_source, lon_target, lat_target,
                            bend = 0.18, side = 1, points = 36) {
  t <- seq(0, 1, length.out = points)
  dx <- lon_target - lon_source
  dy <- lat_target - lat_source
  control_lon <- (lon_source + lon_target) / 2 - side * bend * dy
  control_lat <- (lat_source + lat_target) / 2 + side * bend * dx
  
  tibble(
    longitude = (1 - t)^2 * lon_source +
      2 * (1 - t) * t * control_lon + t^2 * lon_target,
    latitude = (1 - t)^2 * lat_source +
      2 * (1 - t) * t * control_lat + t^2 * lat_target
  )
}

make_flow_loop <- function(lon_anchor, lat_anchor, radius = 0.08,
                           start_angle = 0, points = 72) {
  angles <- seq(start_angle, start_angle + 2 * pi, length.out = points)
  latitude_radius <- radius
  longitude_radius <- radius / pmax(cos(lat_anchor * pi / 180), 0.2)
  center_lon <- lon_anchor - longitude_radius * cos(start_angle)
  center_lat <- lat_anchor - latitude_radius * sin(start_angle)
  
  tibble(
    longitude = center_lon + longitude_radius * cos(angles),
    latitude = center_lat + latitude_radius * sin(angles)
  )
}

# Cadrer sur les geometries des flux (courbes et boucles incluses),
# independamment de l'etendue des contours administratifs.
fit_flow_bounds <- function(widget, flow_geometries) {
  points <- dplyr::bind_rows(flow_geometries)
  if (nrow(points) == 0) {
    return(setView(widget, lng = 4.70, lat = 45.45, zoom = 7))
  }
  points <- points[is.finite(points$longitude) & is.finite(points$latitude), ]
  if (nrow(points) == 0) {
    return(setView(widget, lng = 4.70, lat = 45.45, zoom = 7))
  }
  fitBounds(
    widget,
    lng1 = min(points$longitude), lat1 = min(points$latitude),
    lng2 = max(points$longitude), lat2 = max(points$latitude),
    options = list(padding = c(40, 40), maxZoom = 11)
  )
}

extract_departement <- function(code_postal, acteur_nom) {
  cp <- str_extract(as.character(code_postal), "\\d{5}")
  dept_cp <- case_when(
    !is.na(cp) & str_detect(cp, "^(97|98)") ~ str_sub(cp, 1, 3),
    !is.na(cp) ~ str_sub(cp, 1, 2),
    TRUE ~ NA_character_
  )
  
  dept_nom <- str_match(clean_ascii(acteur_nom), "\\(([A-Za-z0-9]{2,3})\\)\\s*$")[, 2]
  
  coalesce(dept_cp, dept_nom, "ND")
}

# Une ligne entreprise = une participation (projet, identite entreprise, role).
# Les identifiants de participation restent presents jusqu'aux agregations.
stage_entries <- function(data, node_column) {
  entry <- if ("participation_id" %in% names(data)) {
    data$participation_id
  } else {
    paste(data$id_projet, data[[node_column]], sep = "::")
  }
  tibble(
    id_projet = as.character(data$id_projet),
    node = data[[node_column]],
    entry_id = entry,
    is_actor = "participation_id" %in% names(data)
  ) %>% distinct(id_projet, entry_id, .keep_all = TRUE)
}

make_stage_pairs <- function(source_data, target_data) {
  source_entries <- stage_entries(source_data, "node_source") %>%
    rename(node_source = node, source_entry = entry_id, source_is_actor = is_actor)
  target_entries <- stage_entries(target_data, "node_target") %>%
    rename(node_target = node, target_entry = entry_id, target_is_actor = is_actor)
  # Co-presence dans un projet, pas une relation commerciale verifiee.
  inner_join(source_entries, target_entries, by = "id_projet", relationship = "many-to-many")
}

make_stage_links <- function(source_data, target_data) {
  pairs <- make_stage_pairs(source_data, target_data)
  source_counts <- stage_entries(source_data, "node_source") %>% count(id_projet, name = "n_source")
  target_counts <- stage_entries(target_data, "node_target") %>% count(id_projet, name = "n_target")
  pairs %>%
    left_join(source_counts, by = "id_projet") %>%
    left_join(target_counts, by = "id_projet") %>%
    mutate(
      # Co-presence dans les projets, pas une relation commerciale verifiee.
      # Provenance -> entreprise : une participation cible repartie entre les provenances.
      # Entreprise -> entreprise : une participation source repartie entre les cibles.
      association_weight = if_else(source_is_actor, 1 / n_target, 1 / n_source)
    ) %>%
    group_by(source = node_source, target = node_target) %>%
    summarise(value = sum(association_weight), .groups = "drop")
}
make_project_links <- function(source_data, target_data) {
  make_stage_pairs(source_data, target_data) %>%
    # Chaque entreprise-role compte une fois dans chacun de ses projets.
    count(source = node_source, target = node_target, name = "value")
}

as_flow_pairs <- function(pairs) {
  pairs %>% transmute(
    id_projet, source = node_source, target = node_target,
    source_entry, target_entry, source_is_actor, target_is_actor,
    # La carte compte les participations sources, sauf a l'entree
    # Provenance -> entreprise ou elle compte les participations cibles.
    participation_id = if_else(source_is_actor, source_entry, target_entry)
  )
}

geocode_provenance_centers <- function(provenances) {
  cache_path <- output_file("provenance_geocodes_cache.csv")
  places <- tibble(provenance = sort(unique(clean_ascii(provenances)))) %>%
    filter(!is.na(provenance), provenance != "", provenance != "Aucune")
  
  cached <- if (file.exists(cache_path)) {
    read_csv(cache_path, show_col_types = FALSE) %>%
      select(provenance, geocode_query, latitude, longitude) %>%
      filter(!is.na(latitude), !is.na(longitude))
  } else {
    tibble(
      provenance = character(), geocode_query = character(),
      latitude = double(), longitude = double()
    )
  }
  
  missing_places <- places %>%
    anti_join(cached, by = "provenance") %>%
    mutate(
      geocode_query = case_when(
        provenance == "Auvergne-Rhone-Alpes" ~ "Auvergne-Rhone-Alpes, France",
        provenance == "Bourgogne-Franche-Comte" ~ "Bourgogne-Franche-Comte, France",
        provenance == "Bretagne" ~ "Bretagne, France",
        provenance == "Centre-Val-de-Loire" ~ "Centre-Val de Loire, France",
        provenance == "Grand-Est" ~ "Grand Est, France",
        provenance == "Nouvelle-Aquitaine" ~ "Nouvelle-Aquitaine, France",
        provenance == "Provence-Alpes-Cote-d'Azur" ~ "Provence-Alpes-Cote d'Azur, France",
        provenance == "Asie" ~ "Asia",
        provenance == "Autres" ~ NA_character_,
        TRUE ~ provenance
      )
    )
  
  to_geocode <- missing_places %>% filter(!is.na(geocode_query))
  unresolved <- missing_places %>%
    filter(is.na(geocode_query)) %>%
    mutate(latitude = NA_real_, longitude = NA_real_)
  
  geocoded <- if (nrow(to_geocode) > 0) {
    tryCatch(
      tidygeocoder::geocode(
        to_geocode,
        address = geocode_query,
        method = "osm",
        lat = latitude,
        long = longitude,
        limit = 1,
        min_time = 1
      ),
      error = function(e) {
        warning(
          "Geocodage des provenances impossible: ", conditionMessage(e),
          ". Les valeurs deja presentes dans le cache restent disponibles."
        )
        to_geocode %>% mutate(latitude = NA_real_, longitude = NA_real_)
      }
    )
  } else {
    to_geocode %>% mutate(latitude = double(), longitude = double())
  }
  
  updated_cache <- bind_rows(cached, geocoded, unresolved) %>%
    distinct(provenance, .keep_all = TRUE) %>%
    arrange(provenance)
  
  write_csv(updated_cache, cache_path, na = "")
  
  updated_cache %>%
    filter(!is.na(latitude), !is.na(longitude)) %>%
    select(provenance, latitude, longitude)
}

geocode_department_centers <- function(departments) {
  cache_path <- output_file("departement_geocodes_cache.csv")
  places <- tibble(dept = sort(unique(clean_ascii(departments)))) %>%
    filter(!is.na(dept), dept != "", dept != "ND")
  
  cached <- if (file.exists(cache_path)) {
    read_csv(
      cache_path,
      col_types = cols(
        dept = col_character(),
        geocode_query = col_character(),
        latitude = col_double(),
        longitude = col_double()
      ),
      show_col_types = FALSE
    ) %>%
      select(dept, geocode_query, latitude, longitude) %>%
      filter(!is.na(latitude), !is.na(longitude))
  } else {
    tibble(
      dept = character(), geocode_query = character(),
      latitude = double(), longitude = double()
    )
  }
  
  missing_places <- places %>% anti_join(cached, by = "dept")
  
  department_reference <- if (nrow(missing_places) > 0) {
    tryCatch(
      jsonlite::fromJSON(
        "https://geo.api.gouv.fr/departements?fields=nom,code",
        simplifyDataFrame = TRUE
      ) %>%
        as_tibble() %>%
        transmute(dept = as.character(code), department_name = clean_text(nom)),
      
      error = function(e) {
        warning("Impossible de recuperer les noms des departements: ", conditionMessage(e))
        tibble(dept = character(), department_name = character())
      }
    )
  } else {
    tibble(dept = character(), department_name = character())
  }
  
  missing_places <- missing_places %>%
    left_join(department_reference, by = "dept") %>%
    mutate(
      # Seuls les codes qui ressemblent reellement a un departement francais
      # (numerique, ou 2A/2B) sont geocodes comme "X, France" en repli : un
      # code comme "DE" (Allemagne) n'est pas un departement francais et ne
      # doit pas etre force dans une requete "DE, France", au risque d'etre
      # resolu par erreur vers un lieu francais sans rapport (cf. cache).
      geocode_query = case_when(
        !is.na(department_name) ~ paste0(department_name, ", France"),
        str_detect(dept, "^([0-9]{2,3}|2A|2B)$") ~ paste0(dept, ", France"),
        TRUE ~ NA_character_
      )
    ) %>%
    select(dept, geocode_query)
  
  to_geocode <- missing_places %>% filter(!is.na(geocode_query))
  unresolved <- missing_places %>%
    filter(is.na(geocode_query)) %>%
    mutate(latitude = NA_real_, longitude = NA_real_)
  
  geocoded <- if (nrow(to_geocode) > 0) {
    tryCatch(
      tidygeocoder::geocode(
        to_geocode,
        address = geocode_query,
        method = "osm",
        lat = latitude,
        long = longitude,
        limit = 1,
        min_time = 1
      ),
      error = function(e) {
        warning("Geocodage des departements impossible: ", conditionMessage(e))
        to_geocode %>% mutate(latitude = NA_real_, longitude = NA_real_)
      }
    )
  } else {
    to_geocode %>% mutate(latitude = double(), longitude = double())
  }
  
  updated_cache <- bind_rows(cached, geocoded, unresolved) %>%
    distinct(dept, .keep_all = TRUE) %>%
    arrange(dept)
  
  write_csv(updated_cache, cache_path, na = "")
  
  updated_cache %>%
    filter(!is.na(latitude), !is.na(longitude)) %>%
    select(dept, latitude, longitude)
}

get_department_names <- function(departments, fallback_reference = NULL) {
  cache_path <- output_file("departement_names_cache.csv")
  places <- tibble(dept = sort(unique(clean_ascii(departments)))) %>%
    filter(!is.na(dept), dept != "")

  cached <- if (file.exists(cache_path)) {
    read_csv(
      cache_path,
      col_types = cols(dept = col_character(), department_name = col_character()),
      show_col_types = FALSE
    ) %>%
      filter(!is.na(department_name), department_name != "")
  } else {
    tibble(dept = character(), department_name = character())
  }

  fallback_reference <- if (is.null(fallback_reference)) {
    tibble(dept = character(), department_name = character())
  } else {
    fallback_reference %>%
      transmute(
        dept = clean_ascii(dept),
        department_name = clean_text(department_name)
      ) %>%
      filter(dept != "", department_name != "") %>%
      distinct(dept, .keep_all = TRUE)
  }

  missing_french_codes <- places %>%
    anti_join(cached, by = "dept") %>%
    filter(str_detect(dept, "^([0-9]{2,3}|2A|2B)$"))

  official_reference <- if (nrow(missing_french_codes) > 0) {
    tryCatch(
      jsonlite::fromJSON(
        "https://geo.api.gouv.fr/departements?fields=nom,code",
        simplifyDataFrame = TRUE
      ) %>%
        as_tibble() %>%
        transmute(
          dept = as.character(code),
          department_name = clean_text(nom)
        ),
      error = function(e) {
        warning("Impossible de recuperer les noms des departements: ", conditionMessage(e))
        tibble(dept = character(), department_name = character())
      }
    )
  } else {
    tibble(dept = character(), department_name = character())
  }

  updated_cache <- places %>%
    left_join(cached, by = "dept") %>%
    left_join(
      official_reference %>% rename(official_name = department_name),
      by = "dept"
    ) %>%
    left_join(
      fallback_reference %>% rename(fallback_name = department_name),
      by = "dept"
    ) %>%
    mutate(
      department_name = coalesce(
        official_name,
        department_name,
        fallback_name,
        if_else(dept == "ND", "Département non renseigné", dept)
      )
    ) %>%
    select(dept, department_name) %>%
    distinct(dept, .keep_all = TRUE) %>%
    arrange(dept)

  write_csv(updated_cache, cache_path, na = "")
  updated_cache
}

department_name_from_code <- function(department_codes, department_reference) {
  normalized_codes <- clean_ascii(department_codes)
  matched_names <- department_reference$department_name[
    match(normalized_codes, department_reference$dept)
  ]
  coalesce(matched_names, normalized_codes)
}

get_department_regions <- function(departments) {
  cache_path <- output_file("departement_regions_cache.csv")
  places <- tibble(dept = sort(unique(clean_ascii(departments)))) %>%
    filter(!is.na(dept), dept != "", dept != "ND")
  
  cached <- if (file.exists(cache_path)) {
    read_csv(
      cache_path,
      col_types = cols(
        dept = col_character(),
        region_code = col_character(),
        region = col_character()
      ),
      show_col_types = FALSE
    ) %>%
      filter(!is.na(region), region != "")
  } else {
    tibble(dept = character(), region_code = character(), region = character())
  }
  
  missing_places <- places %>% anti_join(cached, by = "dept")
  
  fetched <- if (nrow(missing_places) > 0) {
    tryCatch(
      {
        departments_reference <- jsonlite::fromJSON(
          "https://geo.api.gouv.fr/departements?fields=code,codeRegion",
          simplifyDataFrame = TRUE
        ) %>%
          as_tibble() %>%
          transmute(dept = as.character(code), region_code = as.character(codeRegion))
        
        regions_reference <- jsonlite::fromJSON(
          "https://geo.api.gouv.fr/regions?fields=code,nom",
          simplifyDataFrame = TRUE
        ) %>%
          as_tibble() %>%
          transmute(region_code = as.character(code), region = clean_ascii(nom))
        
        missing_places %>%
          left_join(departments_reference, by = "dept") %>%
          left_join(regions_reference, by = "region_code") %>%
          select(dept, region_code, region)
      },
      error = function(e) {
        warning("Impossible de recuperer les regions des departements: ", conditionMessage(e))
        missing_places %>% mutate(region_code = NA_character_, region = NA_character_)
      }
    )
  } else {
    tibble(dept = character(), region_code = character(), region = character())
  }
  
  updated_cache <- bind_rows(cached, fetched) %>%
    distinct(dept, .keep_all = TRUE) %>%
    arrange(dept)
  
  write_csv(updated_cache, cache_path, na = "")
  
  updated_cache %>%
    filter(!is.na(region), region != "") %>%
    select(dept, region_code, region)
}

# Contours administratifs locaux, sans telechargement de limites.
read_administrative_boundaries <- function(filename) {
  sf::st_read(data_file(filename), quiet = TRUE, stringsAsFactors = FALSE) %>%
    sf::st_transform(4326)
}

# Effectifs classes : memes seuils et largeurs sur toutes les cartes.
# Ne conserver que les noeuds relies aux flux effectivement dessines.
connected_flow_nodes <- function(lines) {
  visible <- lines %>% filter(nb_participations >= 5)
  bind_rows(
    visible %>% transmute(flow_stage = flow_stage_source,
                          latitude = latitude_source, longitude = longitude_source),
    visible %>% transmute(flow_stage = flow_stage_target,
                          latitude = latitude_target, longitude = longitude_target)
  ) %>% distinct()
}

flow_line_width <- function(n) {
  c(1, 2, 4, 7, 11)[findInterval(n, c(1, 2, 5, 10, 25))]
}
flow_width_legend <- function() {
  labels <- c("5–9", "10–24", "25 et plus")
  widths <- c(4, 7, 11)
  paste0(vapply(seq_along(widths), function(i) paste0(
    "<div style='display:flex;align-items:center;gap:8px;height:22px'>",
    "<span style='display:inline-block;width:48px;border-top:", widths[i],
    "px solid #555'></span>", labels[i], " participations</div>"
  ), character(1)), collapse = "")
}
# Une seule boite pour la lecture des flux et des limites administratives.
add_flow_legend <- function(widget, quantity_rows, type_rows) {
  widget %>%
    removeControl("map-context") %>%
    addControl(
      html = HTML(paste0(
        "<div style='max-width:min(600px,calc(100vw - 80px));max-height:42vh;overflow:auto;font:13px/1.4 Arial;color:#333;'>",
        "<div style='font-size:15px;font-weight:700;margin-bottom:8px;'>Légende</div>",
        "<div style='display:flex;flex-wrap:wrap;gap:14px 22px;'>",
        "<section style='flex:1 1 220px;'><b>Participations entreprises</b><br><small>Classes d’effectifs · liens ≥ 5 uniquement</small>",
        quantity_rows, "</section>",
        "<section style='flex:1 1 250px;min-width:0;'><b>Types de liaison</b><br>",
        "<small>Flèche : source → destination</small>", type_rows, "</section></div>",
        "<div style='border-top:1px solid #ddd;margin-top:10px;padding-top:8px;'>",
        "<span style='border-top:2px dashed #2F2F2F;display:inline-block;width:24px;vertical-align:middle;'></span> Région &nbsp;&nbsp;",
        "<span style='border-top:1px dashed #666;display:inline-block;width:24px;vertical-align:middle;'></span> Département",
        "<div style='font-size:10px;color:#666;margin-top:5px;'>Sources : fichiers locaux projets/acteurs ; contours administratifs.</div>",
        "</div></div>"
      )), position = "bottomleft", layerId = "flow-legend", className = "info legend"
    )
}

add_cartographic_context <- function(widget) {
  widget %>%
    addScaleBar(position = "bottomleft", options = scaleBarOptions(imperial = FALSE)) %>%
    addControl(
      html = HTML("<div style='font:600 14px Arial;text-align:center;' aria-label='Nord'>N<br>&uarr;</div>"),
      position = "topleft", layerId = "north-arrow"
    ) %>%
    addControl(
      html = HTML(paste0(
        "<div style='font:11px Arial;max-width:260px;color:#444;'>",
        "<span style='border-top:2px dashed #2F2F2F;display:inline-block;width:20px;'></span> Région &nbsp;",
        "<span style='border-top:1px dashed #666;display:inline-block;width:20px;'></span> Département",
        "<br>Sources : fichiers locaux projets/acteurs ; contours administratifs.</div>"
      )), position = "bottomright", layerId = "map-context"
    )
}

add_administrative_boundaries <- function(widget) {
  widget %>%
    addMapPane("administrative_boundaries", zIndex = 410) %>%
    addPolygons(
      data = department_boundaries,
      group = "Limites departements",
      stroke = TRUE, color = "#666666", weight = 1, opacity = 0.75,
      dashArray = "4, 4", fill = FALSE,
      options = pathOptions(pane = "administrative_boundaries", interactive = FALSE)
    ) %>%
    addPolygons(
      data = region_boundaries,
      group = "Limites regions",
      stroke = TRUE, color = "#2F2F2F", weight = 2, opacity = 0.9,
      dashArray = "8, 5", fill = FALSE,
      options = pathOptions(pane = "administrative_boundaries", interactive = FALSE)
    ) %>%
    add_cartographic_context()
}

# Grille communale de densite (INSEE/ANCT): le niveau 1 est le plus dense,
# le niveau 7 le moins dense. Les contours proviennent du shapefile local et
# sont simplifies puis mis en cache pour garder une carte Leaflet fluide.
population_density_layer <- "Grille communale de densité (grid7)"
population_density_colors <- c(
  "1" = "#D73027",
  "2" = "#F46D43",
  "3" = "#FDAE61",
  "4" = "#FEE08B",
  "5" = "#A6D96A",
  "6" = "#66BD63",
  "7" = "#1A9850"
)

population_density_labels <- c(
  "1" = "Grands centres urbains",
  "2" = "Centres urbains intermédiaires",
  "3" = "Petites villes",
  "4" = "Ceintures urbaines",
  "5" = "Bourgs ruraux",
  "6" = "Rural à habitat dispersé",
  "7" = "Rural à habitat très dispersé"
)

decorate_population_density_communes <- function(data) {
  data %>%
    mutate(
      color = unname(population_density_colors[gridens7]),
      density_label = unname(population_density_labels[gridens7]),
      popup = paste0(
        "<b>", htmlEscape(commune), "</b><br>",
        "Grille communale de densité : ", htmlEscape(density_label)
      )
    )
}

# Communes et classes grid7 issues du cache local simplifie.
add_commune_density <- function(widget, legend = FALSE) {
  if (is.null(population_density_communes) || nrow(population_density_communes) == 0) {
    return(widget)
  }
  widget <- widget %>%
    addPolygons(
      data = population_density_communes,
      group = population_density_layer,
      stroke = FALSE, fill = TRUE,
      fillColor = ~color, fillOpacity = 0.65,
      popup = ~popup, smoothFactor = 1.5,
      options = pathOptions(interactive = TRUE)
    )
  if (legend) {
    widget <- widget %>% addLegend(
      position = "topleft", colors = unname(population_density_colors),
      labels = unname(population_density_labels), opacity = 0.7,
      title = "Densité des communes (grid7)", group = population_density_layer
    )
  }
  widget
}

get_population_density_communes <- function() {
  cache_path <- output_file("grid7_densite_population_communes_simplifie.rds")
  
  if (file.exists(cache_path)) {
    return(decorate_population_density_communes(readRDS(cache_path)))
  }
  
  density_grid <- tryCatch(
    readxl::read_excel(
      data_file("grid7.xlsx"),
      sheet = "Data",
      skip = 4,
      col_types = c("text", "text", "text")
    ) %>%
      transmute(
        codgeo = clean_text(codgeo),
        gridens7 = clean_text(gridens7)
      ) %>%
      filter(codgeo != "", gridens7 %in% names(population_density_colors)) %>%
      distinct(codgeo, .keep_all = TRUE),
    error = function(e) {
      warning("Lecture de grid7.xlsx impossible: ", conditionMessage(e))
      NULL
    }
  )
  
  if (is.null(density_grid) || nrow(density_grid) == 0) {
    return(NULL)
  }
  
  # Couche optionnelle : comme pour les autres ressources externes du script
  # (geocodage, limites regionales), un shapefile local manquant, illisible ou
  # incomplet ne doit pas arreter tout le script (Sankey, cartes, CSV) mais
  # seulement desactiver cette couche de densite.
  if (!requireNamespace("sf", quietly = TRUE)) {
    warning(
      "Le package R 'sf' est requis pour lire le shapefile des communes ",
      "(couche de densite de population ignoree)."
    )
    return(NULL)
  }

  density_communes <- tryCatch(
    {
      shapefile_path <- data_file("communes-20220101-shp/communes-20220101.shp")
      message("Lecture du shapefile communal local pour la densite de population...")
      communes <- sf::st_read(shapefile_path, quiet = TRUE, stringsAsFactors = FALSE)

      if (!"insee" %in% names(communes)) {
        stop("Champ 'insee' introuvable dans le shapefile des communes.", call. = FALSE)
      }

      message("Jointure grid7 et simplification des contours communaux...")
      communes %>%
        transmute(
          codgeo = clean_text(insee),
          commune = clean_text(nom),
          geometry = geometry
        ) %>%
        left_join(density_grid, by = "codgeo") %>%
        filter(!is.na(gridens7)) %>%
        sf::st_as_sf() %>%
        sf::st_transform(3857) %>%
        sf::st_simplify(dTolerance = 250, preserveTopology = TRUE) %>%
        sf::st_transform(4326) %>%
        decorate_population_density_communes()
    },
    error = function(e) {
      warning(
        "Lecture du shapefile communal impossible: ", conditionMessage(e),
        " (couche de densite de population ignoree)."
      )
      NULL
    }
  )

  if (is.null(density_communes)) {
    return(NULL)
  }

  saveRDS(density_communes, cache_path)
  message("Couche de densite de population mise en cache: ", basename(cache_path))
  density_communes
}


# ============================================================================
# 2. Import des données et préparation des indicateurs
# ============================================================================

# Fichiers sources produits par le notebook.
produits <- read_csv(
  data_file("projets_produits_techniques_essence_provenance_completes.csv"),
  show_col_types = FALSE
)

projets <- read_csv(
  data_file("projets_geocodes_complet.csv"),
  show_col_types = FALSE
)

acteurs <- read_csv(
  data_file("acteurs_projets_geocodes.csv"),
  show_col_types = FALSE
)

techniques <- read_csv(
  data_file("projets_infos_techniques.csv"),
  show_col_types = FALSE
)

# Les fichiers sources peuvent contenir quelques projets hors périmètre :
# on restreint systématiquement tous les jeux de données à l'AURA.
aura_project_ids <- projets %>%
  transmute(
    id_projet = as.character(projet_id),
    region_projet = clean_ascii(region)
  ) %>%
  filter(region_projet == target_region, !is.na(id_projet), id_projet != "") %>%
  pull(id_projet)

if (length(aura_project_ids) == 0) {
  stop("Aucun projet Auvergne-Rhone-Alpes trouvé dans projets_geocodes_complet.csv.", call. = FALSE)
}

produits <- produits %>% filter(as.character(id_projet) %in% aura_project_ids)
projets <- projets %>% filter(as.character(projet_id) %in% aura_project_ids)
acteurs <- acteurs %>% filter(as.character(id_projet_fusion) %in% aura_project_ids)
techniques <- techniques %>% filter(as.character(id_projet) %in% aura_project_ids)

# Exclure tous les particuliers avant de vérifier les rôles de la filière.
# La même population alimente ensuite le Sankey, les cartes et les statistiques.
acteurs <- acteurs %>%
  mutate(exclure_particulier = str_detect(
    str_to_lower(clean_ascii(paste(
      coalesce(nom_acteur_final, ""), coalesce(role_acteur_fusion, ""),
      coalesce(role_acteur_carte, "")
    ))),
    "\\bparticuliers?\\b"
  ))
acteurs_particuliers_exclus <- acteurs %>% filter(exclure_particulier)
acteurs <- acteurs %>% filter(!exclure_particulier) %>% select(-exclure_particulier)
filiere_project_actor_flags <- acteurs %>%
  transmute(
    id_projet = as.character(id_projet_fusion),
    role = clean_ascii(role_acteur_fusion)
  ) %>%
  filter(!is.na(id_projet), id_projet != "") %>%
  group_by(id_projet) %>%
  summarise(
    has_scierie_industriel = any(role %in% c("Scierie", "Industriel"), na.rm = TRUE),
    has_bois_pose = any(role %in% c("Charpentier", "Constructeur bois", "Menuisier"), na.rm = TRUE),
    .groups = "drop"
  )

filiere_project_ids <- filiere_project_actor_flags %>%
  filter(has_scierie_industriel, has_bois_pose) %>%
  pull(id_projet)

projets_retires_filiere <- projets %>%
  transmute(
    id_projet = as.character(projet_id),
    nom_projet = clean_text(nom_projet),
    commune = clean_commune(commune),
    departement = clean_ascii(departement)
  ) %>%
  left_join(filiere_project_actor_flags, by = "id_projet") %>%
  mutate(
    has_scierie_industriel = coalesce(has_scierie_industriel, FALSE),
    has_bois_pose = coalesce(has_bois_pose, FALSE),
    raison_retrait = case_when(
      !has_scierie_industriel & !has_bois_pose ~
        "Aucun acteur Scierie/Industriel ni Charpentier/Constructeur bois/Menuisier",
      !has_scierie_industriel ~ "Aucun acteur Scierie/Industriel",
      !has_bois_pose ~ "Aucun acteur Charpentier/Constructeur bois/Menuisier",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(raison_retrait))

produits <- produits %>%
  filter(as.character(id_projet) %in% filiere_project_ids)

projets <- projets %>%
  filter(as.character(projet_id) %in% filiere_project_ids)

acteurs <- acteurs %>%
  filter(as.character(id_projet_fusion) %in% filiere_project_ids)

techniques <- techniques %>%
  filter(as.character(id_projet) %in% filiere_project_ids)

provenance_centers <- geocode_provenance_centers(produits$provenance)

acteur_departments_for_region <- acteurs %>%
  transmute(
    id_projet = as.character(id_projet_fusion),
    id_acteur = as.character(id_acteur_fusion),
    nom_acteur = clean_text(nom_acteur_final),
    role_acteur = coalesce(clean_ascii(role_acteur_carte), clean_ascii(role_acteur_fusion)),
    groupe_acteur = coalesce(
      clean_ascii(groupe_acteur_carte),
      clean_ascii(groupe_acteur_fusion)
    ),
    is_particulier = str_detect(
      str_to_lower(paste(
        coalesce(nom_acteur_final, ""),
        coalesce(role_acteur_fusion, ""),
        sep = " "
      )),
      "particulier"
    ),
    dept_acteur = extract_departement(code_postal_acteur, nom_acteur_final)
  ) %>%
  filter(!is.na(id_projet), id_projet != "", !is_particulier)

project_regions <- projets %>%
  transmute(
    id_projet = as.character(projet_id),
    nom_projet = clean_text(nom_projet),
    commune_projet = clean_commune(commune),
    departement_projet = clean_ascii(departement),
    dept_projet = extract_departement(code_postal, departement),
    region_projet = clean_ascii(region)
  ) %>%
  filter(!is.na(id_projet), id_projet != "")

department_regions_for_actors <- get_department_regions(acteur_departments_for_region$dept_acteur)

acteurs_projets_regions <- acteur_departments_for_region %>%
  left_join(department_regions_for_actors, by = c("dept_acteur" = "dept")) %>%
  rename(region_acteur = region, region_code_acteur = region_code) %>%
  inner_join(project_regions, by = "id_projet") %>%
  mutate(
    is_same_region = !is.na(region_acteur) &
      !is.na(region_projet) &
      region_acteur == region_projet,
    is_same_departement = !is.na(dept_acteur) &
      
      dept_acteur != "ND" &
      dept_acteur == dept_projet
  ) %>%
  distinct(
    id_projet,
    id_acteur,
    nom_acteur,
    role_acteur,
    groupe_acteur,
    dept_acteur,
    .keep_all = TRUE
  )

project_same_region_actor_counts <- acteurs_projets_regions %>%
  group_by(id_projet, nom_projet, commune_projet, departement_projet, region_projet) %>%
  summarise(
    nb_acteurs_total = n_distinct(coalesce(id_acteur, nom_acteur), na.rm = TRUE),
    nb_acteurs_region_connue = n_distinct(
      coalesce(id_acteur[!is.na(region_acteur)], nom_acteur[!is.na(region_acteur)]),
      na.rm = TRUE
    ),
    nb_acteurs_meme_region = n_distinct(
      coalesce(id_acteur[is_same_region], nom_acteur[is_same_region]),
      na.rm = TRUE
    ),
    nb_acteurs_region_inconnue = n_distinct(
      coalesce(id_acteur[is.na(region_acteur)], nom_acteur[is.na(region_acteur)]),
      na.rm = TRUE
    ),
    part_acteurs_meme_region = round(nb_acteurs_meme_region / nb_acteurs_region_connue, 3),
    .groups = "drop"
  ) %>%
  mutate(
    part_acteurs_meme_region = if_else(
      nb_acteurs_region_connue > 0,
      part_acteurs_meme_region,
      NA_real_
    )
  ) %>%
  arrange(desc(part_acteurs_meme_region), desc(nb_acteurs_region_connue), nom_projet)

project_same_region_actor_counts_by_role <- acteurs_projets_regions %>%
  filter(!is.na(role_acteur), role_acteur != "") %>%
  group_by(id_projet, nom_projet, region_projet, role_acteur) %>%
  summarise(
    nb_acteurs_total = n_distinct(coalesce(id_acteur, nom_acteur), na.rm = TRUE),
    nb_acteurs_region_connue = n_distinct(
      coalesce(id_acteur[!is.na(region_acteur)], nom_acteur[!is.na(region_acteur)]),
      na.rm = TRUE
    ),
    nb_acteurs_meme_region = n_distinct(
      coalesce(id_acteur[is_same_region], nom_acteur[is_same_region]),
      na.rm = TRUE
    ),
    part_acteurs_meme_region = round(nb_acteurs_meme_region / nb_acteurs_region_connue, 3),
    .groups = "drop"
  ) %>%
  mutate(
    part_acteurs_meme_region = if_else(
      nb_acteurs_region_connue > 0,
      part_acteurs_meme_region,
      NA_real_
    )
  ) %>%
  arrange(id_projet, desc(part_acteurs_meme_region), role_acteur)

# Matrices de presence au niveau projet : un projet ne compte qu'une fois par modalite.
make_presence_matrix <- function(data, id_col, category_col, min_projects = 3) {
  records <- data %>%
    transmute(
      id_projet = as.character({{ id_col }}),
      categorie = clean_text({{ category_col }})
    ) %>%
    filter(!is.na(id_projet), id_projet != "", !is.na(categorie), categorie != "") %>%
    distinct(id_projet, categorie)
  
  kept_categories <- records %>%
    count(categorie, name = "nb_projets") %>%
    filter(nb_projets >= min_projects) %>%
    pull(categorie)
  
  records <- records %>% filter(categorie %in% kept_categories)
  if (nrow(records) == 0) {
    return(matrix(numeric(), nrow = 0, ncol = 0))
  }
  
  wide <- records %>%
    mutate(presence = 1) %>%
    pivot_wider(
      id_cols = id_projet,
      names_from = categorie,
      values_from = presence,
      values_fill = 0
    ) %>%
    arrange(id_projet)
  
  result <- as.matrix(wide %>% select(-id_projet))
  rownames(result) <- wide$id_projet
  storage.mode(result) <- "numeric"
  result
}

# Comptage distinct par projet et paire de categories observee.
make_global_khi2 <- function(records, left_label, right_label) {
  records <- records %>%
    transmute(id_projet = clean_text(id_projet), gauche = clean_text(gauche), droite = clean_text(droite)) %>%
    filter(!is.na(id_projet), id_projet != "", !is.na(gauche), gauche != "", !is.na(droite), droite != "") %>%
    distinct(id_projet, gauche, droite)
  counts <- records %>% group_by(gauche, droite) %>%
    summarise(nb_projets = n_distinct(id_projet), .groups = "drop")
  tab <- as.matrix(xtabs(nb_projets ~ gauche + droite, data = counts))
  stat <- p <- v <- expected_min <- NA_real_
  df <- n_low <- NA_integer_
  expected <- matrix(NA_real_, nrow(tab), ncol(tab), dimnames = dimnames(tab))
  valid <- nrow(tab) >= 2 && ncol(tab) >= 2 && sum(tab) > 0
  if (valid) {
    test <- suppressWarnings(chisq.test(tab, correct = FALSE))
    stat <- unname(test$statistic)
    df <- as.integer(test$parameter)
    p <- test$p.value
    v <- sqrt(stat / (sum(tab) * min(nrow(tab) - 1, ncol(tab) - 1)))
    expected <- test$expected
    expected_min <- min(expected)
    n_low <- sum(expected < 5)
  }
  summary <- tibble(
    famille_gauche = left_label, famille_droite = right_label,
    n_projets = n_distinct(records$id_projet), n_associations = sum(tab),
    khi2 = stat, ddl = df, p_value = p, cramer_v = v, significatif_005 = p < 0.05,
    effectif_attendu_min = expected_min, nb_cellules_attendues_inf_5 = n_low,
    proportion_cellules_attendues_inf_5 = if (length(tab)) n_low / length(tab) else NA_real_,
    approximation_fragile = if (valid) expected_min < 1 || mean(expected < 5) > 0.2 else NA,
    methode = "Khi2 sans correction; V de Cramer",
    unite = "Projet-paire distinct; plusieurs paires possibles par projet",
    interpretation = "Exploratoire : dependance possible entre paires du meme projet"
  )
  list(summary = summary, contingency = as.data.frame.matrix(addmargins(tab, FUN = list(Total = sum))) %>% tibble::rownames_to_column("categorie_gauche"),
       counts = counts, records = records, table = tab,
       subtitle = paste0("khi2 = ", round(stat, 2), " ; df = ", df,
                         " ; p = ", format.pval(p, digits = 3),
                         " ; cramer_v = ", round(v, 3), " ; N = ", summary$n_projets, " projets"))
}
# L'ouverture d'un peripherique png() echoue parfois de maniere transitoire
# sous Windows (antivirus ou autre processus qui verrouille brievement le
# fichier qui vient d'etre ecrit) : on reessaie plusieurs fois avant d'abandonner,
# plutot que de faire echouer tout le script pour un verrou temporaire.
# Le peripherique png() par defaut sous Windows ("windows") anti-crenele mal le
# texte incline (labels de colonnes a 45 degres) : les lettres se chevauchent
# et paraissent ecrasees. Le peripherique "cairo" rend ce texte correctement,
# mais il n'ouvre reellement le fichier qu'au premier trace (pas a l'appel
# png() lui-meme) : un verrou transitoire sous Windows (antivirus ou autre
# processus qui verrouille brievement le fichier qui vient d'etre ecrit) ne se
# voit donc qu'a ce moment-la. On reessaie l'ouverture ET le premier trace
# ensemble, plutot que de faire echouer tout le script pour un verrou temporaire.
open_png_device_with_retry <- function(filename, width, height, res,
                                        max_attempts = 5, wait_seconds = 1) {
  for (attempt in seq_len(max_attempts)) {
    opened <- tryCatch({
      grDevices::png(filename, width = width, height = height, res = res)
      TRUE
    }, error = function(e) FALSE, warning = function(w) FALSE)
    if (isTRUE(opened)) return(invisible(TRUE))
    if (attempt < max_attempts) Sys.sleep(wait_seconds)
  }
  stop("Impossible d'ouvrir le périphérique PNG : ", filename, call. = FALSE)
}

save_correlation_plot <- function(
    correlation_matrix,
    filename,
    title,
    subtitle = NULL,
    x_axis_title = NULL,
    y_axis_title = NULL,
    sample_size_matrix = NULL,
    cell_labels = NULL,
    strip_axis_prefix = TRUE,
    show_values = TRUE,
    number_digits = 3,
    is_corr = TRUE,
    value_limits = c(-1, 1),
    legend_title = NULL,
    text_scale = 1,
    color_palette = colorRampPalette(c("#2B6CB0", "#F7FAFC", "#C53030"))(200),
    plain_numbers = FALSE, show_color_legend = TRUE
) {
  if (nrow(correlation_matrix) == 0 || all(is.na(correlation_matrix))) {
    warning("Correlation non produite: donnees insuffisantes pour ", title)
    return(invisible(FALSE))
  }

  plot_matrix <- correlation_matrix
  sample_size_plot <- sample_size_matrix
  cell_labels_plot <- cell_labels
  if (!is.null(sample_size_plot) && !identical(dim(sample_size_plot), dim(plot_matrix))) {
    stop("sample_size_matrix doit avoir les mêmes dimensions que correlation_matrix.")
  }
  if (!is.null(cell_labels_plot) && !identical(dim(cell_labels_plot), dim(plot_matrix))) {
    stop("cell_labels doit avoir les mêmes dimensions que correlation_matrix.")
  }
  if (strip_axis_prefix) {
    rownames(plot_matrix) <- str_replace(rownames(plot_matrix), "^.*? \\| ", "")
    colnames(plot_matrix) <- str_replace(colnames(plot_matrix), "^.*? \\| ", "")
    if (!is.null(sample_size_plot)) {
      rownames(sample_size_plot) <- rownames(plot_matrix)
      colnames(sample_size_plot) <- colnames(plot_matrix)
    }
    if (!is.null(cell_labels_plot)) {
      rownames(cell_labels_plot) <- rownames(plot_matrix)
      colnames(cell_labels_plot) <- colnames(plot_matrix)
    }
  }

  # type = "upper" suppose une matrice carree et symetrique (ex : distances
  # entre groupes) ; sinon la matrice complete est affichee. Aucun graphique
  # n'est reordonne : l'ordre d'origine des variables est toujours conserve,
  # pour que tous les graphiques du script partagent le meme format.
  is_square_symmetric <- nrow(plot_matrix) == ncol(plot_matrix) &&
    identical(rownames(plot_matrix), colnames(plot_matrix)) &&
    isTRUE(all.equal(
      unname(plot_matrix),
      unname(t(plot_matrix)),
      tolerance = 1e-8,
      check.attributes = FALSE
    ))
  plot_type <- if (is_square_symmetric) "upper" else "full"

  # Répartir les longs libellés sur deux lignes pour rapprocher la matrice.
  wrap_label <- function(x, width) paste(strwrap(x, width = width), collapse = "\n")
  rownames(plot_matrix) <- vapply(rownames(plot_matrix), wrap_label, character(1), width = 32)
  compact_columns <- ncol(plot_matrix) <= 6 && text_scale <= 1
  if (!is.null(sample_size_plot)) dimnames(sample_size_plot) <- dimnames(plot_matrix)
  if (!is.null(cell_labels_plot)) dimnames(cell_labels_plot) <- dimnames(plot_matrix)
  longest_row_label <- max(nchar(unlist(strsplit(rownames(plot_matrix), "\n"))), na.rm = TRUE)
  longest_column_label <- max(nchar(colnames(plot_matrix)), na.rm = TRUE)
  # Les libelles portent parfois le prefixe de famille : la taille du texte
  # s'adapte au nombre de variables affichees, pour tous les graphiques.
  label_cex <- if (max(dim(plot_matrix)) > 24) {
    0.75
  } else if (max(dim(plot_matrix)) > 16) {
    0.82
  } else {
    0.92
  }
  # Taille fixe (independante des labels, de la taille de la matrice et de
  # text_scale) pour que les valeurs numeriques des cellules aient toujours
  # la meme taille sur tous les PNG.
  number_cex <- 0.9

  # corrplot reserve lui-meme, a l'interieur du systeme de coordonnees, la place
  # des libelles de lignes et de colonnes. Lui ajouter de larges marges revient a
  # reserver cet espace deux fois : c'est ce qui creait la bande blanche au-dessus
  # de la matrice. Les marges internes restent donc minimales, seules les marges
  # externes (oma) accueillent les titres.
  plot_margins <- c(0.2, 0.2, 0.2, 0.2)
  outer_left_margin <- if (!is.null(y_axis_title) && y_axis_title != "") 2.8 else 0.5
  title_lines <- strwrap(title, width = 62)
  subtitle_lines <- if (!is.null(subtitle)) strwrap(subtitle, width = 85) else character()
  axis_lines <- if (!is.null(x_axis_title)) strwrap(x_axis_title, width = 85) else character()
  outer_top_margin <- 0.8 + 1.7 * length(title_lines) +
    1.1 * length(subtitle_lines) + 1.3 * length(axis_lines)

  # A res = 160, une ligne de marge vaut 0.2 pouce (32 px) et un caractere de
  # libelle environ 16 * tl.cex pixels de large.
  line_height_px <- 32
  cell_size_px <- if (compact_columns) 130 else 110
  label_char_px <- 16 * label_cex
  # Largeur : matrice + libelles de lignes + echelle de couleurs.
  # Hauteur : matrice + libelles de colonnes inclines a 45 degres.
  extra_columns <- longest_row_label * label_char_px / cell_size_px + 2
  extra_rows <- longest_column_label * label_char_px * sin(pi / 4) / cell_size_px

  image_width <- min(
    3400,
    max(
      1000,
      ceiling(
        cell_size_px * (ncol(plot_matrix) + extra_columns) +
          (plot_margins[2] + plot_margins[4] + outer_left_margin + 0.4) * line_height_px
      )
    )
  )
  image_height <- min(
    3400,
    max(
      620,
      ceiling(
        cell_size_px * (nrow(plot_matrix) + extra_rows) +
          (plot_margins[1] + plot_margins[3] + outer_top_margin + 0.4) * line_height_px
      )
    )
  )
  open_png_device_with_retry(filename, width = image_width, height = image_height, res = 160)
  on.exit(grDevices::dev.off(), add = TRUE)
  par(
    family = "sans",
    bg = "white",
    mar = plot_margins,
    oma = c(0.4, outer_left_margin, outer_top_margin, if (plain_numbers) 8 else if (is_corr) 0.4 else 2.8),
    xpd = NA
  )
  corrplot_result <- corrplot::corrplot(
    plot_matrix,
    method = "color",
    type = plot_type,
    order = "original",
    is.corr = is_corr,
    na.label = " ",
    tl.col = "black",
    tl.cex = label_cex * text_scale,
    tl.srt = 45,
    tl.pos = "lt",
    col = color_palette,
    col.lim = if (is_corr) c(-1, 1) else value_limits,
    cl.cex = 0.9 * text_scale,
    cl.pos = if (show_color_legend) "r" else "n",
    cl.length = 5,
    cl.align.text = "l",
    addCoef.col = if (show_values && !plain_numbers && is.null(sample_size_plot) && is.null(cell_labels_plot)) "#202020" else NULL,
    number.cex = number_cex,
    number.digits = number_digits,
    mar = plot_margins
  )
  if (show_values && plain_numbers) {
    positions <- corrplot_result$corrPos
    ri <- match(positions$yName, rownames(plot_matrix))
    ci <- match(positions$xName, colnames(plot_matrix))
    values <- plot_matrix[cbind(ri, ci)]
    # Les marges Total ont un fond fixe; seule la matrice interieure definit l'echelle.
    is_total <- positions$yName == "Total" | positions$xName == "Total"
    inner_max <- max(c(1, values[!is_total & is.finite(values)]))
    fill_index <- 1L + round((length(color_palette) - 1L) * pmin(1, pmax(0, values / inner_max)))
    fills <- color_palette[fill_index]
    fills[is_total] <- "#C9D4DF"
    graphics::rect(positions$x - 0.5, positions$y - 0.5,
                   positions$x + 0.5, positions$y + 0.5,
                   col = fills, border = "white", lwd = 0.6)

    graphics::text(positions$x, positions$y,
                   labels = formatC(values, format = "f", digits = number_digits),
                   cex = number_cex, col = "#202020")
  }

  if (show_values && !is.null(sample_size_plot)) {
    positions <- corrplot_result$corrPos
    if (!is.null(positions) && nrow(positions) > 0) {
      row_index <- match(positions$yName, rownames(plot_matrix))
      column_index <- match(positions$xName, colnames(plot_matrix))
      valid_position <- !is.na(row_index) & !is.na(column_index)
      n_values <- rep(NA_integer_, nrow(positions))
      n_values[valid_position] <- as.integer(sample_size_plot[cbind(
        row_index[valid_position],
        column_index[valid_position]
      )])
      # positions$corr vient de corrplot et est renormalise en interne quand
      # is.corr = FALSE (ex: distances en km) : ce n'est pas la vraie valeur.
      # On relit la valeur reelle directement dans plot_matrix, comme pour
      # n_values ci-dessus, pour que le texte affiche la bonne unite.
      cell_values <- rep(NA_real_, nrow(positions))
      cell_values[valid_position] <- plot_matrix[cbind(
        row_index[valid_position],
        column_index[valid_position]
      )]
      labels <- ifelse(
        is.finite(cell_values) & !is.na(n_values) & n_values > 0,
        formatC(cell_values, format = "f", digits = number_digits),
        ""
      )
      graphics::text(
        positions$x,
        positions$y,
        labels = labels,
        cex = number_cex,
        col = ifelse(is.finite(cell_values) &
          if (is_corr) abs(cell_values) > 0.65 else cell_values > value_limits[1] + 0.65 * diff(value_limits), "white", "#202020")
      )
    }
  }
  if (show_values && !is.null(cell_labels_plot)) {
    positions <- corrplot_result$corrPos
    if (!is.null(positions) && nrow(positions) > 0) {
      row_index <- match(positions$yName, rownames(plot_matrix))
      column_index <- match(positions$xName, colnames(plot_matrix))
      valid_position <- !is.na(row_index) & !is.na(column_index)
      labels <- rep("", nrow(positions))
      labels[valid_position] <- cell_labels_plot[cbind(
        row_index[valid_position],
        column_index[valid_position]
      )]
      graphics::text(
        positions$x,
        positions$y,
        labels = labels,
        cex = number_cex,
        col = ifelse(plot_matrix[cbind(row_index, column_index)] >
          value_limits[1] + 0.65 * diff(value_limits), "white", "#202020")
      )
    }
  }
  if (show_color_legend && !is_corr && !is.null(legend_title)) {
    mtext(legend_title, side = 4, outer = TRUE, line = 1, cex = 0.8)
  }
  annotation_line <- outer_top_margin - 2
  for (title_line in title_lines) {
    mtext(title_line, side = 3, outer = TRUE, line = annotation_line,
          cex = 1.2 * sqrt(text_scale), font = 2, col = "#151515")
    annotation_line <- annotation_line - 1.7
  }
  for (subtitle_line in subtitle_lines) {
    mtext(subtitle_line, side = 3, outer = TRUE, line = annotation_line,
          cex = 0.95 * sqrt(text_scale), col = "#555555")
    annotation_line <- annotation_line - 1.1
  }
  for (axis_line in axis_lines) {
    mtext(axis_line, side = 3, outer = TRUE, line = annotation_line,
          cex = 0.95 * sqrt(text_scale), font = 2, col = "#202020")
    annotation_line <- annotation_line - 1.3
  }
  if (!is.null(y_axis_title) && y_axis_title != "") {
    # Titre vertical, centre sur le cote gauche du panneau.
    mtext(
      y_axis_title,
      side = 2,
      outer = TRUE,
      line = 1.15,
      cex = 0.95 * sqrt(text_scale),
      font = 2,
      las = 0,
      col = "#202020"
    )
  }
  invisible(TRUE)
}

project_actor_locality <- acteurs_projets_regions %>%
  mutate(acteur_id = coalesce(na_if(id_acteur, ""), na_if(nom_acteur, ""))) %>%
  group_by(id_projet, nom_projet, commune_projet, departement_projet, region_projet) %>%
  summarise(
    nb_acteurs_departement_connu = n_distinct(
      acteur_id[!is.na(dept_acteur) & dept_acteur != "ND"],
      na.rm = TRUE
    ),
    nb_acteurs_meme_departement = n_distinct(
      acteur_id[is_same_departement],
      na.rm = TRUE
    ),
    nb_acteurs_region_connue = n_distinct(
      acteur_id[!is.na(region_acteur)],
      na.rm = TRUE
    ),
    nb_acteurs_meme_region = n_distinct(
      acteur_id[is_same_region],
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  mutate(
    part_acteurs_meme_departement = if_else(
      nb_acteurs_departement_connu > 0,
      nb_acteurs_meme_departement / nb_acteurs_departement_connu,
      NA_real_
    ),
    part_acteurs_meme_region = if_else(
      nb_acteurs_region_connue > 0,
      nb_acteurs_meme_region / nb_acteurs_region_connue,
      NA_real_
    )
  )

project_systemes_constructifs <- produits %>%
  transmute(id_projet = as.character(id_projet), systeme_constructif = sous_categorie) %>%
  # « Liste des produits » est un libellé de rubrique, pas un système constructif.
  filter(clean_ascii(systeme_constructif) != "Liste des produits")
project_essences <- produits %>%
  transmute(id_projet = as.character(id_projet), essence = essence) %>%
  # « Tempéré » décrit un climat et ne constitue pas une essence de bois.
  filter(clean_ascii(essence) != "Tempere")
project_provenances <- produits %>%
  transmute(id_projet = as.character(id_projet), provenance = provenance) %>%
  filter(clean_ascii(provenance) != "Aucune", clean_ascii(provenance) != "Autres")
project_types_batiments <- techniques %>%
  transmute(id_projet = as.character(id_projet), type_batiment = type_batiment) %>%
  separate_rows(type_batiment, sep = ",") %>%
  mutate(type_batiment = clean_text(type_batiment)) %>%
  # Certaines lignes sources contiennent seulement une surface dans ce champ.
  filter(!str_detect(type_batiment, "^[0-9 .]+(m.?( SHON)?)?$"))

systemes_matrix <- make_presence_matrix(project_systemes_constructifs, id_projet, systeme_constructif)
essences_matrix <- make_presence_matrix(project_essences, id_projet, essence)
provenances_matrix <- make_presence_matrix(project_provenances, id_projet, provenance)
types_batiments_matrix <- make_presence_matrix(
  project_types_batiments,
  id_projet,
  type_batiment
)

# Associations effectivement renseignees dans les lignes produits.
# Meme selection de modalites que les graphiques ; pas de produit cartesien
# entre essences et provenances presentes dans un meme projet.
khi2_systemes_essences <- make_global_khi2(
  produits %>% transmute(id_projet, gauche = sous_categorie, droite = essence) %>%
    filter(clean_text(gauche) %in% colnames(systemes_matrix),
           clean_text(droite) %in% colnames(essences_matrix)),
  "Parties d'ouvrage", "Essence"
)
khi2_types_batiments_systemes <- make_global_khi2(
  project_types_batiments %>%
    inner_join(project_systemes_constructifs, by = "id_projet", relationship = "many-to-many") %>%
    transmute(id_projet, gauche = type_batiment, droite = systeme_constructif) %>%
    filter(clean_text(gauche) %in% colnames(types_batiments_matrix),
           clean_text(droite) %in% colnames(systemes_matrix)),
  "Type de bâtiment", "Parties d'ouvrage"
)
khi2_essences_provenances <- make_global_khi2(
  produits %>% transmute(id_projet, gauche = essence, droite = provenance) %>%
    filter(clean_text(gauche) %in% colnames(essences_matrix),
           clean_text(droite) %in% colnames(provenances_matrix)),
  "Essence", "Provenance"
)
# ============================================================================
# 3. Construction du Sankey et des cartes HTML
# ============================================================================

provenances_exclues_filiere <- c("Autres", "France")

stage_provenance <- produits %>%
  mutate(
    id_projet = as.character(id_projet),
    provenance = clean_ascii(provenance),
    node = node_name("01", provenance)
  ) %>%
  filter(
    !is.na(provenance),
    provenance != "",
    provenance != "Aucune",
    !provenance %in% provenances_exclues_filiere
  ) %>%
  select(id_projet, node)

stage_acteurs <- acteurs %>%
  mutate(
    id_projet = as.character(id_projet_fusion),
    id_acteur = na_if(clean_text(id_acteur_fusion), ""),
    role = clean_ascii(role_acteur_fusion),
    nom_acteur = clean_text(nom_acteur_final),
    commune_acteur = clean_commune(commune_acteur),
    dept_acteur = extract_departement(code_postal_acteur, nom_acteur_final),
    latitude = suppressWarnings(as.numeric(latitude)),
    longitude = suppressWarnings(as.numeric(longitude))
  ) %>%
  filter(!is.na(id_projet), id_projet != "") %>%
  mutate(
    stage = case_when(
      role == "Scierie" ~ "Scierie",
      role == "Industriel" ~ "Industriel",
      role %in% c("Charpentier", "Constructeur bois", "Menuisier") ~ role,
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(stage)) %>%
  mutate(
    # Une identite de repli est necessaire pour les entreprises sans fiche ID.
    acteur_id = if_else(
      !is.na(id_acteur), paste0("id:", id_acteur),
      coalesce(na_if(clean_text(cle_acteur), ""),
               paste0("nom:", str_to_lower(clean_ascii(nom_acteur))))
    ),
    identite_source = if_else(!is.na(id_acteur), "id_acteur", "cle_ou_nom_sans_id"),
    participation_id = paste(id_projet, acteur_id, stage, sep = "::")
  ) %>%
  distinct(id_projet, acteur_id, stage, .keep_all = TRUE)

project_departments <- projets %>%
  transmute(
    id_projet = as.character(projet_id),
    dept = extract_departement(code_postal, rep(NA_character_, n()))
  ) %>%
  filter(!is.na(dept), dept != "", dept != "ND") %>%
  distinct()

department_centers <- geocode_department_centers(
  c(stage_acteurs$dept_acteur, project_departments$dept)
)

# Pour les acteurs dont le departement n'a pas pu etre determine (ex. "ND"),
# on dispose deja de leurs coordonnees reelles issues du geocodage de leur
# adresse (stage_acteurs$latitude/longitude). On les moyenne par departement
# extrait pour servir de position de repli plus fidele que le centre de la
# France, notamment pour les acteurs situes hors de France (ex. Allemagne).
actor_real_centers <- stage_acteurs %>%
  filter(!is.na(latitude), !is.na(longitude)) %>%
  group_by(dept_acteur) %>%
  summarise(
    real_latitude = mean(latitude, na.rm = TRUE),
    real_longitude = mean(longitude, na.rm = TRUE),
    .groups = "drop"
  )

resolve_dept_latitude <- function(dept) {
  coalesce(
    department_centers$latitude[match(dept, department_centers$dept)],
    actor_real_centers$real_latitude[match(dept, actor_real_centers$dept_acteur)],
    fallback_latitude
  )
}

resolve_dept_longitude <- function(dept) {
  coalesce(
    department_centers$longitude[match(dept, department_centers$dept)],
    actor_real_centers$real_longitude[match(dept, actor_real_centers$dept_acteur)],
    fallback_longitude
  )
}

project_department_name_reference <- projets %>%
  transmute(
    dept = extract_departement(code_postal, rep(NA_character_, n())),
    department_name = clean_text(departement)
  ) %>%
  filter(!is.na(dept), dept != "", !is.na(department_name), department_name != "") %>%
  distinct(dept, .keep_all = TRUE)

department_names <- get_department_names(
  c(stage_acteurs$dept_acteur, project_departments$dept),
  fallback_reference = project_department_name_reference
)

valid_project_ids <- intersect(
  unique(stage_provenance$id_projet),
  unique(stage_acteurs %>% filter(stage %in% c("Scierie", "Industriel")) %>% pull(id_projet))
)

valid_project_ids <- Reduce(
  intersect,
  list(
    valid_project_ids,
    unique(stage_acteurs %>% filter(stage %in% c("Charpentier", "Constructeur bois", "Menuisier")) %>% pull(id_projet)),
    unique(projets %>% transmute(id_projet = as.character(projet_id)) %>% pull(id_projet))
  )
)

stage_provenance <- stage_provenance %>%
  filter(id_projet %in% valid_project_ids) %>%
  distinct(id_projet, node)

stage_scierie_industriel <- stage_acteurs %>%
  filter(stage %in% c("Scierie", "Industriel"), id_projet %in% valid_project_ids) %>%
  transmute(id_projet, id_acteur, acteur_id, role = stage, participation_id,
            node = node_name("02", paste0(stage, " - Dept ", dept_acteur))) %>%
  distinct(id_projet, acteur_id, role, .keep_all = TRUE)

# stage_bois_real et stage_bois etaient auparavant deux copies strictement
# identiques (une pour les cartes/totaux, une pour les sources/cibles du
# Sankey) : fusionnees ici pour eviter que les deux divergent silencieusement
# si l'une des deux est modifiee sans l'autre.
stage_bois_real <- stage_acteurs %>%
  filter(stage %in% c("Charpentier", "Constructeur bois", "Menuisier"), id_projet %in% valid_project_ids) %>%
  transmute(id_projet, id_acteur, acteur_id, role = stage, participation_id,
            node = node_name("03", paste0(stage, " - Dept ", dept_acteur))) %>%
  distinct(id_projet, acteur_id, role, .keep_all = TRUE)

stage_bois <- stage_bois_real

stage_projet <- projets %>%
  transmute(
    id_projet = as.character(projet_id),
    dept = extract_departement(code_postal, rep(NA_character_, n())),
    node = node_name("04", paste0("Projet - Dept ", dept))
  ) %>%
  filter(id_projet %in% valid_project_ids, !is.na(dept), dept != "", dept != "ND") %>%
  distinct(id_projet, node)

# Ensemble de reference unique pour toutes les visualisations situees apres le
# Sankey : seuls les projets ayant les quatre etapes effectivement tracees.
sankey_project_ids <- Reduce(
  intersect,
  list(
    unique(stage_provenance$id_projet),
    unique(stage_scierie_industriel$id_projet),
    unique(stage_bois_real$id_projet),
    unique(stage_projet$id_projet)
  )
)

valid_project_ids <- sankey_project_ids
stage_provenance <- stage_provenance %>% filter(id_projet %in% sankey_project_ids)
stage_scierie_industriel <- stage_scierie_industriel %>% filter(id_projet %in% sankey_project_ids)
stage_bois_real <- stage_bois_real %>% filter(id_projet %in% sankey_project_ids)
stage_bois <- stage_bois_real
stage_projet <- stage_projet %>% filter(id_projet %in% sankey_project_ids)

# Indicateurs spatiaux : même cohorte que les quatre étapes du Sankey.
# Les effectifs par cellule restent ceux des observations valides.
distance_groupes_acteurs <- c(
  "Maître d'ouvrage",
  "Études et ingénieries",
  "Entreprise de construction",
  "Entreprises de transformation"
)

# Une ligne source peut représenter un rôle plutôt qu'un acteur unique. La table
# détaillée conserve les rôles, puis deux tables dédupliquées imposent les grains
# analytiques : projet-acteur pour le global et projet-acteur-groupe par métier.
distances_acteurs_projets_detail <- acteurs %>%
  filter(as.character(id_projet_fusion) %in% sankey_project_ids) %>%
  transmute(
    id_projet = as.character(id_projet_fusion),
    id_acteur = as.character(id_acteur_fusion),
    nom_acteur = clean_text(nom_acteur_final),
    role_acteur = coalesce(clean_ascii(role_acteur_carte), clean_ascii(role_acteur_fusion)),
    groupe_acteur = normalise_groupe_analyse(coalesce(
      groupe_acteur_carte,
      groupe_acteur_fusion
    )),
    commune_acteur = clean_commune(commune_acteur),
    code_postal_acteur = as.character(code_postal_acteur),
    latitude_acteur = suppressWarnings(as.numeric(latitude)),
    longitude_acteur = suppressWarnings(as.numeric(longitude))
  ) %>%
  mutate(acteur_id = actor_key(id_acteur, nom_acteur)) %>%
  inner_join(
    projets %>%
      transmute(
        id_projet = as.character(projet_id),
        nom_projet = clean_text(nom_projet),
        commune_projet = clean_commune(commune),
        departement_projet = clean_ascii(departement),
        code_postal_projet = as.character(code_postal),
        latitude_projet = suppressWarnings(as.numeric(latitude)),
        longitude_projet = suppressWarnings(as.numeric(longitude))
      ),
    by = "id_projet"
  ) %>%
  filter(
    !is.na(acteur_id),
    !is.na(latitude_acteur), !is.na(longitude_acteur),
    !is.na(latitude_projet), !is.na(longitude_projet)
  ) %>%
  mutate(
    distance_km = haversine_km(
      longitude_acteur, latitude_acteur,
      longitude_projet, latitude_projet
    )
  ) %>%
  distinct(id_projet, acteur_id, role_acteur, groupe_acteur, .keep_all = TRUE)

distances_acteurs_projets <- distances_acteurs_projets_detail %>%
  group_by(
    id_projet, acteur_id, id_acteur, nom_acteur, commune_acteur,
    code_postal_acteur, latitude_acteur, longitude_acteur,
    nom_projet, commune_projet, departement_projet, code_postal_projet,
    latitude_projet, longitude_projet, distance_km
  ) %>%
  summarise(
    role_acteur = paste(sort(unique(role_acteur[!is.na(role_acteur) & role_acteur != ""])), collapse = " | "),
    groupe_acteur = paste(sort(unique(groupe_acteur[!is.na(groupe_acteur) & groupe_acteur != ""])), collapse = " | "),
    .groups = "drop"
  )

distances_acteurs_projets_groupes <- distances_acteurs_projets_detail %>%
  filter(groupe_acteur %in% distance_groupes_acteurs) %>%
  distinct(id_projet, acteur_id, groupe_acteur, .keep_all = TRUE)

summarise_distances <- function(data, ...) {
  data %>%
    group_by(...) %>%
    summarise(
      nb_liaisons = n(),
      nb_acteurs = n_distinct(acteur_id, na.rm = TRUE),
      nb_projets = n_distinct(id_projet),
      distance_min_km = round(min(distance_km, na.rm = TRUE), 2),
      distance_q25_km = round(quantile(distance_km, 0.25, na.rm = TRUE), 2),
      distance_mediane_km = round(median(distance_km, na.rm = TRUE), 2),
      distance_q75_km = round(quantile(distance_km, 0.75, na.rm = TRUE), 2),
      distance_max_km = round(max(distance_km, na.rm = TRUE), 2),
      .groups = "drop"
    )
}

distance_stats_global <- distances_acteurs_projets %>%
  mutate(type_statistique = "Tous les acteurs") %>%
  summarise_distances(type_statistique)

distance_stats_roles <- distances_acteurs_projets_groupes %>%
  summarise_distances(groupe_acteur) %>%
  arrange(match(groupe_acteur, distance_groupes_acteurs))

distance_stats_projets <- distances_acteurs_projets %>%
  summarise_distances(id_projet, nom_projet, commune_projet, departement_projet) %>%
  arrange(desc(distance_mediane_km), desc(nb_liaisons))

distance_stats_roles_top <- distance_stats_roles

# Distances entre chaque paire d'acteurs participant au même projet. Les acteurs
# sont uniques dans le projet ; l'ordre lexical de la clé évite A-B et B-A.
acteurs_paires_gauche <- distances_acteurs_projets %>%
  transmute(
    id_projet, nom_projet, commune_projet, departement_projet,
    acteur_id_a = acteur_id,
    id_acteur_a = id_acteur,
    nom_acteur_a = nom_acteur,
    groupes_acteur_a = groupe_acteur,
    latitude_acteur_a = latitude_acteur,
    longitude_acteur_a = longitude_acteur
  )

acteurs_paires_droite <- distances_acteurs_projets %>%
  transmute(
    id_projet,
    acteur_id_b = acteur_id,
    id_acteur_b = id_acteur,
    nom_acteur_b = nom_acteur,
    groupes_acteur_b = groupe_acteur,
    latitude_acteur_b = latitude_acteur,
    longitude_acteur_b = longitude_acteur
  )

distances_acteurs_acteurs <- suppressWarnings(
  inner_join(acteurs_paires_gauche, acteurs_paires_droite, by = "id_projet")
) %>%
  filter(acteur_id_a < acteur_id_b) %>%
  mutate(
    distance_km = haversine_km(
      longitude_acteur_a, latitude_acteur_a,
      longitude_acteur_b, latitude_acteur_b
    )
  ) %>%
  arrange(id_projet, acteur_id_a, acteur_id_b)

distance_actor_counts_by_project <- distances_acteurs_projets %>%
  count(id_projet, nom_projet, commune_projet, departement_projet, name = "nb_acteurs")

distance_stats_actor_pairs_project <- distance_actor_counts_by_project %>%
  left_join(
    distances_acteurs_acteurs %>%
      group_by(id_projet) %>%
      summarise(
        nb_paires = n(),
        distance_min_km = min(distance_km, na.rm = TRUE),
        distance_q25_km = quantile(distance_km, 0.25, na.rm = TRUE),
        distance_mediane_km = median(distance_km, na.rm = TRUE),
        distance_q75_km = quantile(distance_km, 0.75, na.rm = TRUE),
        distance_max_km = max(distance_km, na.rm = TRUE),
        .groups = "drop"
      ),
    by = "id_projet"
  ) %>%
  mutate(
    nb_paires = coalesce(nb_paires, 0L),
    across(starts_with("distance_"), ~ round(.x, 2))
  ) %>%
  arrange(desc(distance_mediane_km), desc(nb_acteurs))

# Paires de groupes : médiane des paires d'acteurs dans chaque projet, puis
# médiane entre projets. Cette double agrégation empêche les grands projets de
# dominer la comparaison entre métiers.
groupes_paires_gauche <- distances_acteurs_projets_groupes %>%
  transmute(
    id_projet,
    acteur_id_a = acteur_id,
    groupe_a = groupe_acteur,
    latitude_a = latitude_acteur,
    longitude_a = longitude_acteur
  )

groupes_paires_droite <- distances_acteurs_projets_groupes %>%
  transmute(
    id_projet,
    acteur_id_b = acteur_id,
    groupe_b = groupe_acteur,
    latitude_b = latitude_acteur,
    longitude_b = longitude_acteur
  )

distance_group_pairs_project <- suppressWarnings(
  inner_join(groupes_paires_gauche, groupes_paires_droite, by = "id_projet")
) %>%
  filter(acteur_id_a < acteur_id_b) %>%
  mutate(
    ordre_a = match(groupe_a, distance_groupes_acteurs),
    ordre_b = match(groupe_b, distance_groupes_acteurs),
    groupe_1 = if_else(ordre_a <= ordre_b, groupe_a, groupe_b),
    groupe_2 = if_else(ordre_a <= ordre_b, groupe_b, groupe_a),
    distance_km = haversine_km(longitude_a, latitude_a, longitude_b, latitude_b)
  ) %>%
  group_by(id_projet, groupe_1, groupe_2) %>%
  summarise(
    nb_paires_acteurs = n(),
    distance_mediane_projet_km = median(distance_km, na.rm = TRUE),
    .groups = "drop"
  )

distance_stats_group_pairs <- distance_group_pairs_project %>%
  group_by(groupe_1, groupe_2) %>%
  summarise(
    nb_projets = n(),
    nb_paires_acteurs = sum(nb_paires_acteurs),
    distance_q25_km = round(quantile(distance_mediane_projet_km, 0.25, na.rm = TRUE), 2),
    distance_mediane_km = round(median(distance_mediane_projet_km, na.rm = TRUE), 2),
    distance_q75_km = round(quantile(distance_mediane_projet_km, 0.75, na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  arrange(match(groupe_1, distance_groupes_acteurs), match(groupe_2, distance_groupes_acteurs))

distance_group_matrix <- matrix(
  NA_real_,
  nrow = length(distance_groupes_acteurs),
  ncol = length(distance_groupes_acteurs),
  dimnames = list(distance_groupes_acteurs, distance_groupes_acteurs)
)
distance_group_n_matrix <- matrix(
  0L,
  nrow = length(distance_groupes_acteurs),
  ncol = length(distance_groupes_acteurs),
  dimnames = list(distance_groupes_acteurs, distance_groupes_acteurs)
)
for (i in seq_len(nrow(distance_stats_group_pairs))) {
  groupe_1 <- distance_stats_group_pairs$groupe_1[i]
  groupe_2 <- distance_stats_group_pairs$groupe_2[i]
  distance_group_matrix[groupe_1, groupe_2] <- distance_stats_group_pairs$distance_mediane_km[i]
  distance_group_matrix[groupe_2, groupe_1] <- distance_stats_group_pairs$distance_mediane_km[i]
  distance_group_n_matrix[groupe_1, groupe_2] <- distance_stats_group_pairs$nb_projets[i]
  distance_group_n_matrix[groupe_2, groupe_1] <- distance_stats_group_pairs$nb_projets[i]
}

diag(distance_group_matrix) <- NA_real_
diag(distance_group_n_matrix) <- 0L
distance_group_values <- distance_group_matrix[is.finite(distance_group_matrix)]
distance_group_limits <- range(distance_group_values)
if (diff(distance_group_limits) == 0) {
  distance_group_limits <- distance_group_limits + c(-0.5, 0.5)
}
distance_group_sample_sizes <- distance_group_n_matrix[
  upper.tri(distance_group_n_matrix) & distance_group_n_matrix > 0
]


# Proximité géographique : les colonnes mesurent la distance médiane de chaque
# groupe au projet ; les lignes mesurent la distance médiane entre deux groupes
# d'acteurs du même projet. Les deux axes contiennent donc des familles de
# variables distinctes et aucune autocorrélation n'est affichée.
distance_project_metrics_long <- distances_acteurs_projets_groupes %>%
  group_by(id_projet, groupe_acteur) %>%
  summarise(
    distance_mediane_km = median(distance_km, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    groupe_affiche = groupe_acteur,
    indicateur_distance = paste("Distance médiane", groupe_affiche, sep = " | ")
  ) %>%
  select(id_projet, indicateur_distance, distance_mediane_km)

distance_indicator_order_compact <- paste(
  "Distance médiane",
  unname(distance_groupes_acteurs),
  sep = " | "
)

complete_numeric_columns <- function(data, expected_columns) {
  missing_columns <- setdiff(expected_columns, names(data))
  for (column_name in missing_columns) {
    data[[column_name]] <- NA_real_
  }
  data %>% select(id_projet, any_of(expected_columns))
}

distance_project_metrics <- distance_project_metrics_long %>%
  mutate(
    indicateur_distance = factor(
      indicateur_distance,
      levels = distance_indicator_order_compact
    )
  ) %>%
  pivot_wider(
    id_cols = id_projet,
    names_from = indicateur_distance,
    values_from = distance_mediane_km,
    names_expand = TRUE,
    names_sort = FALSE,
    values_fn = median
  ) %>%
  complete_numeric_columns(distance_indicator_order_compact)

distance_role_pair_definitions <- tibble(
  groupe_1 = c(
    "Maître d'ouvrage", "Maître d'ouvrage", "Maître d'ouvrage",
    "Études et ingénieries", "Études et ingénieries",
    "Entreprise de construction"
  ),
  groupe_2 = c(
    "Études et ingénieries", "Entreprise de construction", "Entreprises de transformation",
    "Entreprise de construction", "Entreprises de transformation",
    "Entreprises de transformation"
  )
) %>%
  mutate(
    paire_affichee = paste(
      groupe_1,
      groupe_2,
      sep = " – "
    ),
    indicateur_paire = paste(
      "Distance médiane entre acteurs",
      paire_affichee,
      sep = " | "
    )
  )

actor_pair_indicator_order <- distance_role_pair_definitions$indicateur_paire

distance_actor_pair_metrics_long <- distance_group_pairs_project %>%
  filter(groupe_1 != groupe_2) %>%
  inner_join(
    distance_role_pair_definitions %>%
      select(groupe_1, groupe_2, indicateur_paire),
    by = c("groupe_1", "groupe_2")
  ) %>%
  select(id_projet, indicateur_paire, distance_mediane_projet_km)

distance_actor_pair_metrics <- distance_actor_pair_metrics_long %>%
  mutate(
    indicateur_paire = factor(
      indicateur_paire,
      levels = actor_pair_indicator_order
    )
  ) %>%
  pivot_wider(
    id_cols = id_projet,
    names_from = indicateur_paire,
    values_from = distance_mediane_projet_km,
    names_expand = TRUE,
    names_sort = FALSE,
    values_fn = median
  ) %>%
  complete_numeric_columns(actor_pair_indicator_order)

correlation_proximite_input <- distance_actor_pair_metrics %>%
  full_join(distance_project_metrics, by = "id_projet")

correlation_proximite <- matrix(
  NA_real_,
  nrow = length(actor_pair_indicator_order),
  ncol = length(distance_indicator_order_compact),
  dimnames = list(actor_pair_indicator_order, distance_indicator_order_compact)
)
correlation_proximite_n <- matrix(
  0L,
  nrow = length(actor_pair_indicator_order),
  ncol = length(distance_indicator_order_compact),
  dimnames = list(actor_pair_indicator_order, distance_indicator_order_compact)
)

for (vertical_indicator in actor_pair_indicator_order) {
  for (horizontal_indicator in distance_indicator_order_compact) {
    x <- correlation_proximite_input[[horizontal_indicator]]
    y <- correlation_proximite_input[[vertical_indicator]]
    complete <- is.finite(x) & is.finite(y)
    correlation_proximite_n[vertical_indicator, horizontal_indicator] <- sum(complete)
    if (sum(complete) >= 8 && sd(x[complete]) > 0 && sd(y[complete]) > 0) {
      correlation_proximite[vertical_indicator, horizontal_indicator] <- cor(
        x[complete], y[complete], method = "spearman"
      )
    }
  }
}

correlation_pair_rows_retained <- rownames(correlation_proximite)[
  apply(correlation_proximite, 1, function(x) any(is.finite(x)))
]
correlation_project_columns_retained <- colnames(correlation_proximite)[
  apply(correlation_proximite, 2, function(x) any(is.finite(x)))
]
correlation_proximite <- correlation_proximite[
  correlation_pair_rows_retained,
  correlation_project_columns_retained,
  drop = FALSE
]
correlation_proximite_n <- correlation_proximite_n[
  correlation_pair_rows_retained,
  correlation_project_columns_retained,
  drop = FALSE
]

correlation_proximite_indices <- expand.grid(
  ligne = seq_len(nrow(correlation_proximite)),
  colonne = seq_len(ncol(correlation_proximite)),
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE
)
correlation_proximite_index_matrix <- as.matrix(
  correlation_proximite_indices[, c("ligne", "colonne"), drop = FALSE]
)

correlation_proximite_table <- tibble(
  distance_entre_groupes = rownames(correlation_proximite)[
    correlation_proximite_indices$ligne
  ],
  distance_groupe_projet = colnames(correlation_proximite)[
    correlation_proximite_indices$colonne
  ],
  correlation_spearman = round(
    as.numeric(correlation_proximite[correlation_proximite_index_matrix]),
    3
  ),
  n_projets = as.integer(correlation_proximite_n[
    correlation_proximite_index_matrix
  ])
) %>%
  arrange(desc(abs(correlation_spearman)))

correlation_proximite_n_non_nuls <- correlation_proximite_n[
  correlation_proximite_n > 0
]
correlation_proximite_n_min <- min(correlation_proximite_n_non_nuls)
correlation_proximite_n_max <- max(correlation_proximite_n_non_nuls)


# Tableau simple de localite par groupe d'acteurs : la part "meme region"
# reste une localite administrative, distincte de la distance reelle, et n'est
# affichee qu'a titre indicatif (elle ne remplace pas les indicateurs de
# distance). Seule la part "meme region" est retenue, la part "meme
# departement" faisant doublon avec elle.
localite_stats_input <- distances_acteurs_projets_groupes %>%
  mutate(dept_acteur = extract_departement(code_postal_acteur, nom_acteur)) %>%
  left_join(
    project_regions %>% select(id_projet, region_projet),
    by = "id_projet"
  )

localite_dept_regions <- get_department_regions(localite_stats_input$dept_acteur)

localite_stats_input <- localite_stats_input %>%
  left_join(
    localite_dept_regions %>% select(dept_acteur = dept, region_acteur = region),
    by = "dept_acteur"
  ) %>%
  mutate(
    is_same_region_acteur = !is.na(region_acteur) &
      !is.na(region_projet) &
      region_acteur == region_projet
  )

localite_stats_roles <- localite_stats_input %>%
  group_by(groupe_acteur) %>%
  summarise(
    n_liaisons = n(),
    distance_mediane_km = median(distance_km, na.rm = TRUE),
    part_50km = mean(distance_km <= 50, na.rm = TRUE),
    part_100km = mean(distance_km <= 100, na.rm = TRUE),
    n_region_connue = sum(!is.na(region_acteur)),
    part_meme_region = if_else(
      n_region_connue > 0,
      sum(is_same_region_acteur, na.rm = TRUE) / n_region_connue,
      NA_real_
    ),
    .groups = "drop"
  ) %>%
  arrange(match(groupe_acteur, distance_groupes_acteurs))

# Meme moteur de rendu que les autres matrices (save_correlation_plot), pour
# un format identique : la couleur est normalisee colonne par colonne (0 a 1,
# bleu foncé = le plus local), car les colonnes melangent des km et des parts ;
# les cellules affichent la valeur reelle (cell_labels), pas la valeur
# normalisee utilisee pour la couleur.
localite_normalize <- function(x, invert = FALSE) {
  span <- diff(range(x, na.rm = TRUE))
  normalised <- if (is.finite(span) && span > 0) {
    (x - min(x, na.rm = TRUE)) / span
  } else {
    rep(0.5, length(x))
  }
  if (invert) 1 - normalised else normalised
}

localite_row_names <- localite_stats_roles$groupe_acteur
localite_column_names <- c(
  "Distance médiane au projet",
  "Part ≤ 50 km",
  "Part ≤ 100 km",
  "Part même région (indicatif)"
)

localite_color_matrix <- 100 * with(localite_stats_roles, cbind(
  localite_normalize(distance_mediane_km, invert = TRUE),
  localite_normalize(part_50km),
  localite_normalize(part_100km),
  localite_normalize(part_meme_region)
))
dimnames(localite_color_matrix) <- list(localite_row_names, localite_column_names)

localite_labels_matrix <- with(localite_stats_roles, cbind(
  paste0(formatC(distance_mediane_km, format = "f", digits = 1), " km"),
  paste0(formatC(100 * part_50km, format = "f", digits = 0), " %"),
  paste0(formatC(100 * part_100km, format = "f", digits = 0), " %"),
  if_else(
    is.na(part_meme_region),
    "n.d.",
    paste0(formatC(100 * part_meme_region, format = "f", digits = 0), " %")
  )
))
dimnames(localite_labels_matrix) <- dimnames(localite_color_matrix)



stage_provenance <- stage_provenance %>% rename(node_source = node)
stage_scierie_industriel_source <- stage_scierie_industriel %>% rename(node_source = node)
stage_scierie_industriel_target <- stage_scierie_industriel %>% rename(node_target = node)
stage_bois_source <- stage_bois %>% rename(node_source = node)
stage_bois_target <- stage_bois %>% rename(node_target = node)
stage_projet <- stage_projet %>% rename(node_target = node)

# Registre auditable : une ligne par projet-entreprise-role dans le meme
# echantillon que le Sankey (les criteres de selection ne changent pas).
participations_entreprises <- stage_acteurs %>%
  filter(id_projet %in% sankey_project_ids) %>%
  transmute(id_projet, id_acteur, acteur_id, nom_acteur, role = stage,
            dept_acteur, identite_source, participation_id) %>%
  arrange(id_projet, acteur_id, role)
stopifnot(!anyDuplicated(participations_entreprises$participation_id))
participations_par_projet_role <- participations_entreprises %>%
  group_by(id_projet, role) %>%
  summarise(nb_participations = n(), nb_entreprises = n_distinct(acteur_id), .groups = "drop")
participations_par_projet <- participations_entreprises %>%
  group_by(id_projet) %>%
  summarise(nb_participations = n(), nb_entreprises = n_distinct(acteur_id), .groups = "drop")
participations_par_role_departement <- participations_entreprises %>%
  group_by(role, dept_acteur) %>%
  summarise(nb_participations = n(), nb_entreprises = n_distinct(acteur_id),
            nb_projets = n_distinct(id_projet),
            nb_participations_sans_id = sum(identite_source != "id_acteur"), .groups = "drop")

links_plot <- bind_rows(
  make_stage_links(stage_provenance, stage_scierie_industriel_target),
  make_stage_links(stage_scierie_industriel_source, stage_bois_target),
  make_project_links(stage_bois_source, stage_projet)
)

sankey_link_nodes <- unique(c(links_plot$source, links_plot$target))

stage_summary <- tibble(
  stage = c(
    "Provenance (associations)",
    "Scierie / Industriel (participations entreprises)",
    "Charpentier / Constructeur bois / Menuisier (participations entreprises)",
    "Projet (projets uniques)"
  ),
  value = c(
    nrow(stage_provenance),
    nrow(stage_scierie_industriel),
    nrow(stage_bois_real),
    n_distinct(stage_projet$id_projet)
  )
)

stage_summary_text <- stage_summary %>%
  mutate(label = paste0(stage, ": ", value)) %>%
  pull(label) %>%
  paste(collapse = " | ")

node_totals <- bind_rows(
  stage_provenance %>% transmute(name = node_source),
  stage_scierie_industriel_source %>% transmute(name = node_source),
  stage_bois_source %>% transmute(name = node_source),
  stage_projet %>% transmute(name = node_target)
) %>%
  group_by(name) %>%
  summarise(node_value = n(), .groups = "drop")

nodes <- tibble(name = unique(c(
  stage_provenance$node_source,
  stage_scierie_industriel_source$node_source,
  stage_bois_source$node_source,
  stage_projet$node_target
))) %>%
  mutate(
    raw_label = display_name(name),
    stage_order = case_when(
      str_starts(name, "01") ~ 1L,
      str_starts(name, "02") ~ 2L,
      str_starts(name, "03") ~ 3L,
      TRUE ~ 4L
    ),
    group = case_when(
      str_starts(name, "01") ~ "Provenance",
      str_starts(name, "02") ~ "Scierie / Industriel",
      str_starts(name, "03") ~ "Charpentier / Constructeur / Menuisier",
      TRUE ~ "Projet"
    ),
    node_area = case_when(
      str_starts(name, "01") ~ raw_label,
      str_detect(raw_label, " - Dept ") ~ str_replace(raw_label, "^.* - Dept ", ""),
      TRUE ~ raw_label
    ),
    node_role = case_when(
      str_starts(name, "01") ~ "Provenance",
      str_detect(raw_label, " - Dept ") ~ str_replace(raw_label, " - Dept .*$", ""),
      str_starts(name, "04") ~ "Projet",
      TRUE ~ raw_label
    ),
    department_display = department_name_from_code(node_area, department_names),
    label = case_when(
      str_detect(raw_label, " - Dept ") ~ paste0(node_role, " - ", department_display),
      TRUE ~ raw_label
    ),
    count_unit = case_when(
      stage_order %in% c(2L, 3L) ~ "participations entreprises (projet-entreprise-role)",
      stage_order == 1L ~ "associations projet-provenance",
      TRUE ~ "projets distincts"
    )
  ) %>%
  left_join(node_totals, by = "name") %>%
  mutate(node_value = coalesce(node_value, 0))

node_area_totals <- nodes %>%
  group_by(stage_order, node_area) %>%
  summarise(area_value = sum(node_value, na.rm = TRUE), .groups = "drop")

nodes <- nodes %>%
  left_join(node_area_totals, by = c("stage_order", "node_area")) %>%
  mutate(area_value = coalesce(area_value, 0)) %>%
  arrange(stage_order, desc(area_value), node_area, desc(node_value), node_role, label)

links_plot <- links_plot %>%
  mutate(
    IDsource = match(source, nodes$name) - 1,
    IDtarget = match(target, nodes$name) - 1,
    group = case_when(
      str_starts(source, "01") ~ "Provenance",
      str_starts(source, "02") ~ "Scierie / Industriel",
      str_starts(source, "03") ~ "Charpentier / Constructeur / Menuisier",
      TRUE ~ "Projet"
    )
  )

colour_scale <- 'd3.scaleOrdinal()
  .domain(["Provenance", "Scierie / Industriel", "Charpentier / Constructeur / Menuisier", "Projet"])
  .range(["#6B8E23", "#5C677D", "#84A59D", "#F28482"])'

p <- sankeyNetwork(
  Links = links_plot,
  Nodes = nodes,
  Source = "IDsource",
  Target = "IDtarget",
  Value = "value",
  NodeID = "label",
  NodeGroup = "group",
  LinkGroup = "group",
  colourScale = colour_scale,
  sinksRight = TRUE,
  nodeWidth = 22,
  nodePadding = 12,
  fontSize = 11,
  iterations = 0,
  width = 1600,
  height = 950,
  margin = list(left = 20, right = 260, top = 55, bottom = 10)
)

# networkD3 ne conserve pas les colonnes supplementaires de Nodes.
# On transmet donc explicitement les effectifs pour les etiquettes et infobulles.
p$x$node_counts <- nodes$node_value
p$x$node_units <- nodes$count_unit

p <- htmlwidgets::prependContent(
  p,
  tags$div(
    style = "font-family:Arial, sans-serif; margin:12px 0 4px 0;",
    tags$h2(
      "Filière bois : participations des entreprises par rôle et département",
      style = "margin:0 0 6px 0; font-size:22px; font-weight:600;"
    ),
    tags$div(
      stage_summary_text,
      style = "font-size:13px; color:#333; margin-top:4px; font-weight:600;"
    )
  )
)

p <- htmlwidgets::onRender(
  p,
  "
  function(el, x) {
    var marginLeft = 20;
    var marginTop = 55;
    var labels = [
      ['Provenance'],
      ['Scierie / Industriel'],
      ['Charpentier / Constructeur bois', '/ Menuisier'],
      ['Projet']
    ];

    var svg = d3.select(el).select('svg');
    svg.selectAll('.sankey-stage-title').remove();
    var counts = Array.isArray(x.node_counts) ? x.node_counts : [x.node_counts];
    var units = Array.isArray(x.node_units) ? x.node_units : [x.node_units];
    svg.selectAll('.node').each(function(d, i) {
      d.participation_count = counts[i];
      d.count_unit = units[i];
      var node = d3.select(this);
      node.select('text').text(d.name + ' (' + counts[i] + ')');
      var tip = d.name + '\\n' + counts[i] + ' ' + units[i];
      node.selectAll('title').text(tip);
      node.selectAll('foreignObject').selectAll('body').text(tip);
    });
    svg.selectAll('.link').each(function(d) {
      var tip = d.source.name + ' → ' + d.target.name + '\\n' +
        d3.format('.2f')(d.value) + ' associations pondérées';
      var link = d3.select(this);
      link.selectAll('title').text(tip);
      link.selectAll('foreignObject').selectAll('body').text(tip);
    });

    var columns = {};
    svg.selectAll('.node').each(function(d) {
      if (!d) return;

      var x0 = d.x0 !== undefined ? d.x0 : d.x;
      var x1 = d.x1 !== undefined ? d.x1 : x0 + (d.dx || 0);

      if (x0 === undefined || x1 === undefined) {
        var transform = d3.select(this).attr('transform') || '';
        var match = /translate\\(([-0-9.]+),\\s*([-0-9.]+)\\)/.exec(transform);
        if (match) {
          x0 = parseFloat(match[1]);
          x1 = x0 + 22;
        }
      }

      if (x0 === undefined || isNaN(x0)) return;

      var key = Math.round(x0);
      if (!columns[key]) columns[key] = {x0: x0, x1: x1};
      columns[key].x0 = Math.min(columns[key].x0, x0);
      columns[key].x1 = Math.max(columns[key].x1, x1);
    });

    Object.keys(columns)
      .map(Number)
      .sort(function(a, b) { return a - b; })
      .slice(0, labels.length)
      .forEach(function(key, i) {
        var col = columns[key];
        var title = svg.append('text')
          .attr('class', 'sankey-stage-title')
          .attr('x', marginLeft + (col.x0 + col.x1) / 2)
          .attr('y', marginTop - 24)

          .attr('text-anchor', 'middle')
          .style('font-family', 'Arial, sans-serif')
          .style('font-size', '14px')
          .style('font-weight', '700')
          .style('fill', '#333');

        labels[i].forEach(function(line, j) {
          title.append('tspan')
            .attr('x', marginLeft + (col.x0 + col.x1) / 2)
            .attr('dy', j === 0 ? 0 : 16)
            .text(line);
        });
      });

    // networkD3 place margin.right (260 px) a gauche du viewBox : on recale
    // le diagramme a 10 px du bord gauche pour supprimer le grand vide.
    var svgNode = svg.node();
    var vb = (svgNode.getAttribute('viewBox') || '').split(/[\\s,]+/).map(Number);
    var content = svg.select('g').node();
    if (vb.length === 4 && content) {
      var left = content.getBBox().x + marginLeft;
      svg.selectAll('.sankey-stage-title').each(function() {
        left = Math.min(left, this.getBBox().x);
      });
      vb[0] = left - 10;
      svgNode.setAttribute('viewBox', vb.join(','));
    }
  }
  "
)

fallback_latitude <- 46.603354
fallback_longitude <- 1.8883335

map_provenance_data <- stage_provenance %>%
  filter(node_source %in% sankey_link_nodes) %>%
  transmute(
    id_projet,
    provenance = display_name(node_source)
  ) %>%
  distinct() %>%
  mutate(
    center_index = match(provenance, provenance_centers$provenance),
    latitude = coalesce(provenance_centers$latitude[center_index], fallback_latitude),
    longitude = coalesce(provenance_centers$longitude[center_index], fallback_longitude)
  ) %>%
  group_by(type = "Provenance", commune = provenance, dept = provenance) %>%
  summarise(
    latitude = first(latitude),
    longitude = first(longitude),
    nb_acteurs = NA_integer_,
    nb_projets = n_distinct(id_projet),
    .groups = "drop"
  ) %>%
  mutate(
    nom = paste0("Provenance - ", commune),
    popup = paste0(
      "<b>Provenance</b><br>",
      htmlEscape(commune), "<br>",
      "Projets liés à ce point : ", nb_projets
    )
  ) %>%
  select(type, nom, commune, dept, latitude, longitude, nb_acteurs, nb_projets, popup)

map_actor_data <- bind_rows(
  stage_scierie_industriel %>%
    filter(node %in% sankey_link_nodes) %>%
    transmute(id_projet, node, acteur_id, participation_id, type = "Scierie / Industriel"),
  stage_bois_real %>%
    filter(node %in% sankey_link_nodes) %>%
    transmute(id_projet, node, acteur_id, participation_id, type = "Charpentier / Constructeur / Menuisier")
) %>%
  mutate(
    label = display_name(node),
    dept = str_replace(label, "^.* - Dept ", ""),
    map_latitude = resolve_dept_latitude(dept),
    map_longitude = resolve_dept_longitude(dept)
  ) %>%
  group_by(type, dept) %>%
  summarise(
    latitude = first(map_latitude),
    longitude = first(map_longitude),
    nb_acteurs = n_distinct(acteur_id),
    nb_participations = n_distinct(participation_id),
    nb_projets = n_distinct(id_projet),
    .groups = "drop"
  ) %>%
  mutate(
    commune = paste0("Centre du departement ", dept),
    nom = paste0(type, " - Dept ", dept),
    popup = paste0(
      "<b>", htmlEscape(type), "</b><br>",
      "Centre du departement: ", htmlEscape(dept), "<br>",
      "Participations entreprises : ", nb_participations, "<br>",
      "Entreprises distinctes : ", nb_acteurs, "<br>",
      "Projets distincts : ", nb_projets
    )
  ) %>%
  select(type, nom, commune, dept, latitude, longitude, nb_acteurs, nb_participations, nb_projets, popup) %>%
  distinct()

map_project_data <- stage_projet %>%
  filter(node_target %in% sankey_link_nodes) %>%
  transmute(id_projet, node = node_target) %>%
  mutate(
    dept = project_departments$dept[match(id_projet, project_departments$id_projet)],
    dept = coalesce(na_if(dept, ""), "ND"),
    center_index = match(dept, department_centers$dept),
    map_latitude = coalesce(department_centers$latitude[center_index], fallback_latitude),
    map_longitude = coalesce(department_centers$longitude[center_index], fallback_longitude)
  ) %>%
  group_by(type = "Projet", dept) %>%
  summarise(
    latitude = first(map_latitude),
    longitude = first(map_longitude),
    nb_acteurs = NA_integer_,
    nb_projets = n_distinct(id_projet),
    .groups = "drop"
  ) %>%
  mutate(
    commune = paste0("Centre du departement ", dept),
    nom = paste0("Projet - Dept ", dept),
    popup = paste0(
      "<b>Projet</b><br>",
      "Centre du departement: ", htmlEscape(dept), "<br>",
      "Projets liés à ce point : ", nb_projets
    )
  ) %>%
  select(type, nom, commune, dept, latitude, longitude, nb_acteurs, nb_projets, popup) %>%
  distinct()

map_data <- bind_rows(map_provenance_data, map_actor_data, map_project_data)

map_stage_totals <- map_data %>%
  group_by(type) %>%
  summarise(value = sum(coalesce(nb_participations, nb_projets)), .groups = "drop")

expected_map_stage_totals <- tibble(
  type = c(
    "Provenance",
    "Scierie / Industriel",
    "Charpentier / Constructeur / Menuisier",
    "Projet"
  ),
  expected = c(
    nrow(stage_provenance %>% distinct(id_projet, node_source)),
    nrow(stage_scierie_industriel),
    nrow(stage_bois_real),
    nrow(
      stage_projet %>%
        transmute(
          id_projet,
          dept = str_replace(display_name(node_target), "^.* - Dept ", "")
        ) %>%
        distinct(id_projet, dept)
    )
  )
) %>%
  mutate(actual = map_stage_totals$value[match(type, map_stage_totals$type)])

if (any(abs(expected_map_stage_totals$actual - expected_map_stage_totals$expected) > 1e-8)) {
  stop(
    "Totaux des noeuds de la carte incoherents: ",
    paste0(
      expected_map_stage_totals$type, "=",
      expected_map_stage_totals$actual, " (attendu ",
      expected_map_stage_totals$expected, ")",
      collapse = " | "
    ),
    call. = FALSE
  )
}

map_marker_data <- map_data %>%
  mutate(
    stage_order = match(
      type,
      c(
        "Provenance",
        "Scierie / Industriel",
        "Charpentier / Constructeur / Menuisier",
        "Projet"
      )
    ),
    coordinate_lat = round(latitude, 6),
    coordinate_lng = round(longitude, 6)
  ) %>%
  group_by(coordinate_lat, coordinate_lng) %>%
  arrange(stage_order, .by_group = TRUE) %>%
  mutate(
    marker_index = row_number(),
    marker_count = n(),
    marker_rotation_class = if_else(
      marker_count > 1,
      paste0("stage-rotate-", marker_index),
      "stage-rotate-0"
    ),
    marker_latitude = latitude,
    marker_longitude = longitude
  ) %>%
  ungroup() %>%
  select(-stage_order, -coordinate_lat, -coordinate_lng, -marker_index, -marker_count)

heatmap_actor_layer_labels <- c(
  "Maitre d'ouvrage" = "Maître d'ouvrage",
  "Etudes et ingenieries" = "Études et ingénieries",
  "Entreprises des lots bois" = "Entreprise de construction",
  "Fournisseurs du bois" = "Entreprises de transformation"
)

project_heatmap_data <- projets %>%
  transmute(
    heatmap_type = "Projet",
    layer = "Nombre de projets",
    id_projet = as.character(projet_id),
    id_acteur = NA_character_,
    nom = clean_text(nom_projet),
    commune = clean_commune(commune),
    latitude = suppressWarnings(as.numeric(latitude)),
    longitude = suppressWarnings(as.numeric(longitude))
  ) %>%
  filter(
    id_projet %in% sankey_project_ids,
    !is.na(latitude), !is.na(longitude)
  ) %>%
  distinct(id_projet, latitude, longitude, .keep_all = TRUE) %>%
  group_by(heatmap_type, layer, commune, latitude, longitude) %>%
  summarise(
    nb_projets = n_distinct(id_projet),
    nb_acteurs = NA_integer_,
    noms = paste(head(sort(unique(nom[!is.na(nom) & nom != ""])), 5), collapse = "<br>"),
    .groups = "drop"
  ) %>%
  mutate(
    weight = nb_projets,
    popup = paste0(
      "<b>Projets</b><br>",
      htmlEscape(commune), "<br>",
      "Nombre de projets: ", nb_projets,
      if_else(noms != "", paste0("<br><br>", htmlEscape(noms)), "")
    )
  )

actor_heatmap_data <- acteurs %>%
  transmute(
    heatmap_type = "Acteur",
    layer = clean_text(
      coalesce(
        groupe_acteur_carte,
        groupe_acteur_fusion,
        "Acteur"
      )
    ),
    id_projet = as.character(id_projet_fusion),
    id_acteur = as.character(id_acteur_fusion),
    nom = clean_text(nom_acteur_final),
    commune = clean_commune(commune_acteur),
    latitude = suppressWarnings(as.numeric(latitude)),
    longitude = suppressWarnings(as.numeric(longitude))
  ) %>%
  mutate(
    layer = str_remove(layer, "\\s*\\([0-9]+\\)\\s*$"),
    layer_key = clean_ascii(layer)
  ) %>%
  filter(
    id_projet %in% sankey_project_ids,
    !is.na(latitude), !is.na(longitude),
    layer_key %in% names(heatmap_actor_layer_labels)
  ) %>%
  mutate(
    layer = unname(heatmap_actor_layer_labels[layer_key])
  ) %>%
  distinct(layer, id_projet, id_acteur, latitude, longitude, .keep_all = TRUE) %>%
  group_by(heatmap_type, layer, commune, latitude, longitude) %>%
  summarise(
    nb_projets = n_distinct(id_projet),
    nb_acteurs = n_distinct(coalesce(id_acteur, nom)),
    noms = paste(head(sort(unique(nom[!is.na(nom) & nom != ""])), 5), collapse = "<br>"),
    .groups = "drop"
  ) %>%
  mutate(
    weight = nb_acteurs,
    popup = paste0(
      "<b>", htmlEscape(layer), "</b><br>",
      htmlEscape(commune), "<br>",
      "Acteurs: ", nb_acteurs, "<br>",
      "Projets lies: ", nb_projets,
      if_else(noms != "", paste0("<br><br>", htmlEscape(noms)), "")
    )
  )

heatmap_data <- bind_rows(project_heatmap_data, actor_heatmap_data) %>%
  mutate(weight = coalesce(weight, 0))

# Donnees individuelles utilisees par MarkerCluster. Contrairement a la
# precedente densite par points, les coordonnees ne sont plus artificiellement
# dispersees : Leaflet regroupe les marqueurs proches et les deploie au zoom.
project_cluster_data <- projets %>%
  transmute(
    id_projet = as.character(projet_id),
    nom = clean_text(nom_projet),
    commune = clean_commune(commune),
    latitude = suppressWarnings(as.numeric(latitude)),
    longitude = suppressWarnings(as.numeric(longitude)),
    url = clean_text(url_projet)
  ) %>%
  filter(
    id_projet %in% sankey_project_ids,
    !is.na(latitude), !is.na(longitude)
  ) %>%
  distinct(id_projet, latitude, longitude, .keep_all = TRUE) %>%
  mutate(
    popup = paste0(
      "<b>", htmlEscape(if_else(nom == "", paste("Projet", id_projet), nom)), "</b><br>",
      htmlEscape(commune),
      if_else(
        !is.na(url) & url != "",
        paste0(
          "<br><a href='", htmlEscape(url),
          "' target='_blank' rel='noopener noreferrer'>Voir le projet</a>"
        ),
        ""
      )
    )
  )

actor_cluster_data <- acteurs %>%
  transmute(
    layer = clean_text(
      coalesce(
        groupe_acteur_carte,
        groupe_acteur_fusion,
        "Acteur"
      )
    ),
    id_projet = as.character(id_projet_fusion),
    id_acteur = as.character(id_acteur_fusion),
    nom = clean_text(nom_acteur_final),
    commune = clean_commune(commune_acteur),
    latitude = suppressWarnings(as.numeric(latitude)),
    longitude = suppressWarnings(as.numeric(longitude)),
    url = clean_text(url_acteur_final),
    nom_projet = clean_text(nom_projet_fusion)
  ) %>%
  mutate(
    layer = str_remove(layer, "\\s*\\([0-9]+\\)\\s*$"),
    layer_key = clean_ascii(layer),
    actor_key = coalesce(
      na_if(id_acteur, ""),
      if_else(nom != "", paste0("nom:", clean_ascii(nom)), NA_character_)
    )
  ) %>%
  filter(
    id_projet %in% sankey_project_ids,
    !is.na(latitude), !is.na(longitude),
    !is.na(actor_key),
    layer_key %in% names(heatmap_actor_layer_labels)
  ) %>%
  mutate(layer = unname(heatmap_actor_layer_labels[layer_key])) %>%
  distinct(layer, actor_key, id_projet, latitude, longitude, .keep_all = TRUE) %>%
  group_by(layer, actor_key, nom, commune, latitude, longitude) %>%
  summarise(
    url = first(url[!is.na(url) & url != ""], default = ""),
    nb_projets = n_distinct(id_projet),
    projets = paste(
      head(sort(unique(nom_projet[!is.na(nom_projet) & nom_projet != ""])), 5),
      collapse = " · "
    ),
    .groups = "drop"
  ) %>%
  mutate(
    popup = paste0(
      "<b>", htmlEscape(nom), "</b><br>",
      htmlEscape(layer), "<br>",
      htmlEscape(commune), "<br>",
      "Projets lies: ", nb_projets,
      if_else(projets != "", paste0("<br><br>", htmlEscape(projets)), ""),
      if_else(
        url != "",
        paste0(
          "<br><br><a href='", htmlEscape(url),
          "' target='_blank' rel='noopener noreferrer'>Voir l'acteur</a>"
        ),
        ""
      )
    )
  )

project_marker_style <- list(
  icon = "building",
  marker_color = "darkblue",
  cluster_hue = 216,
  legend_color = "#174A8B"
)

actor_marker_styles <- tibble::tribble(
  ~layer, ~icon, ~marker_color, ~cluster_hue, ~legend_color,
  "Maître d'ouvrage",             "home",   "blue",   211, "#2474B5",
  "Études et ingénieries",        "pencil", "purple", 278, "#7A4AA5",
  "Entreprise de construction",   "wrench", "orange",  28, "#D86E16",
  "Entreprises de transformation","tree",   "green",  135, "#27854A"
)

cluster_max_radius <- 55

department_boundaries <- read_administrative_boundaries("departements-50m.geojson")
region_boundaries <- read_administrative_boundaries("regions-50m.geojson")
population_density_communes <- get_population_density_communes()
heatmap_base_overlay_groups <- c(
  "Limites departements", "Limites regions",
  if (!is.null(population_density_communes) && nrow(population_density_communes) > 0) {
    population_density_layer
  }
)

# Les flux cartographiques reutilisent exactement les noeuds et valeurs du
# Sankey. Une position de repli au centre de la France conserve les associations
# sans coordonnees (notamment la provenance "Autres" et les departements ND).

flow_provenance_locations <- stage_provenance %>%
  transmute(
    name = node_source,
    flow_stage = "Provenance",
    commune = display_name(node_source),
    dept = display_name(node_source)
  ) %>%
  distinct() %>%
  mutate(
    center_index = match(commune, provenance_centers$provenance),
    latitude = coalesce(provenance_centers$latitude[center_index], fallback_latitude),
    longitude = coalesce(provenance_centers$longitude[center_index], fallback_longitude)
  ) %>%
  select(name, flow_stage, commune, dept, latitude, longitude)

flow_actor_locations <- bind_rows(
  stage_scierie_industriel_source %>%
    transmute(name = node_source, flow_stage = "Scierie / Industriel"),
  stage_bois_source %>%
    transmute(name = node_source, flow_stage = "Charpentier / Constructeur / Menuisier")
) %>%
  distinct() %>%
  mutate(
    label = display_name(name),
    dept = str_replace(label, "^.* - Dept ", ""),
    latitude = resolve_dept_latitude(dept),
    longitude = resolve_dept_longitude(dept),
    commune = if_else(
      dept == "ND",
      "Departement non renseigne",
      paste0("Centre du departement ", dept)
    )
  ) %>%
  select(name, flow_stage, commune, dept, latitude, longitude)


flow_project_locations <- stage_projet %>%
  transmute(id_projet, name = node_target) %>%
  mutate(
    dept = project_departments$dept[match(id_projet, project_departments$id_projet)],
    center_index = match(dept, department_centers$dept),
    latitude = coalesce(department_centers$latitude[center_index], fallback_latitude),
    longitude = coalesce(department_centers$longitude[center_index], fallback_longitude),
    flow_stage = "Projet",
    commune = if_else(
      is.na(dept) | dept == "" | dept == "ND",
      "Departement non renseigne",
      paste0("Centre du departement ", dept)
    ),
    dept = coalesce(na_if(dept, ""), "ND")
  ) %>%
  group_by(name, flow_stage) %>%
  summarise(
    commune = first(commune),
    dept = first(dept),
    latitude = first(latitude),
    longitude = first(longitude),
    .groups = "drop"
  )

flow_node_locations <- bind_rows(
  flow_provenance_locations,
  flow_actor_locations,
  flow_project_locations
) %>%
  distinct(name, .keep_all = TRUE)

flow_segment_domain <- c(
  "Provenance -> Scierie / Industriel",
  "Scierie / Industriel -> Charpentier / Constructeur / Menuisier",
  "Charpentier / Constructeur / Menuisier -> Projet"
)
flow_segment_groups <- c(
  "Flux Provenance-Scierie",
  "Flux Scierie-Charpentier",
  "Flux Charpentier-Projet"
)
flow_segment_palette <- colorFactor(
  palette = c("#2F6690", "#3C7A57", "#A9562B"),
  domain = flow_segment_domain,
  na.color = "#4A4A4A"
)

# La carte compte les participations (projet-entreprise-role) sources, ou cibles
# pour Provenance -> entreprise. Le nombre de projets reste un champ distinct.
# Une participation peut apparaitre sur plusieurs destinations ; les liens
# ne sont donc pas additifs. Le Sankey utilise des associations ponderees.
flow_project_pairs <- bind_rows(
  make_stage_pairs(stage_provenance, stage_scierie_industriel_target) %>%
    as_flow_pairs(),
  make_stage_pairs(stage_scierie_industriel_source, stage_bois_target) %>%
    as_flow_pairs(),
  make_stage_pairs(stage_bois_source, stage_projet) %>%
    as_flow_pairs()
) %>%
  distinct(id_projet, source, target, source_entry, target_entry, .keep_all = TRUE)

flow_project_routes <- flow_project_pairs %>%
  mutate(
    source_index = match(source, flow_node_locations$name),
    target_index = match(target, flow_node_locations$name),
    flow_stage_source = flow_node_locations$flow_stage[source_index],
    commune_source = flow_node_locations$commune[source_index],
    dept_source = flow_node_locations$dept[source_index],
    latitude_source = flow_node_locations$latitude[source_index],
    longitude_source = flow_node_locations$longitude[source_index],
    flow_stage_target = flow_node_locations$flow_stage[target_index],
    commune_target = flow_node_locations$commune[target_index],
    dept_target = flow_node_locations$dept[target_index],
    latitude_target = flow_node_locations$latitude[target_index],
    longitude_target = flow_node_locations$longitude[target_index],
    segment = paste0(flow_stage_source, " -> ", flow_stage_target)
  ) %>%
  filter(
    !is.na(flow_stage_source), !is.na(flow_stage_target),
    !is.na(latitude_source), !is.na(longitude_source),
    !is.na(latitude_target), !is.na(longitude_target)
  )

flow_lines <- flow_project_routes %>%
  group_by(
    segment,
    flow_stage_source, commune_source, dept_source,
    latitude_source, longitude_source,
    flow_stage_target, commune_target, dept_target,
    latitude_target, longitude_target
  ) %>%
  summarise(
      nb_projets = n_distinct(id_projet),
      nb_participations = n_distinct(participation_id),
      nb_participations_source = n_distinct(source_entry[source_is_actor]),
      nb_participations_cible = n_distinct(target_entry[target_is_actor]),
      .groups = "drop"
    ) %>%
  mutate(
    is_intradepartement = (
      flow_stage_source != "Provenance" & dept_source == dept_target
    )
  ) %>%
  mutate(
    line_weight = flow_line_width(nb_participations),
    line_opacity = 0.8,
    route_rank = dense_rank(paste(segment, dept_source, dept_target, sep = "|")),
    curve_side = if_else(route_rank %% 2 == 0, -1, 1),
    curve_bend = 0.10 + 0.015 * (route_rank %% 4),
    segment_rank = dense_rank(segment),
    loop_radius = 2.5 * (0.055 + 0.012 * pmin(segment_rank, 3)),
    loop_start_angle = (segment_rank - 1) * pi / 3,
    flow_geometry = if_else(is_intradepartement, "boucle-intradepartement", "arc-inter-departements"),
    flow_group = flow_segment_groups[match(segment, flow_segment_domain)],
    line_color = flow_segment_palette(segment),
    popup = paste0(
      "<b>", nb_participations, " participations entreprises</b><br>",
      "Projets distincts : ", nb_projets, "<br>",
      "Comptage : ", if_else(flow_stage_source == "Provenance", "entreprises cibles", "entreprises sources"), "<br>",
      if_else(is_intradepartement, "<b>Boucle dans le même département</b><br>", ""),
      htmlEscape(flow_stage_source), ": ", htmlEscape(commune_source), "<br>",
      htmlEscape(flow_stage_target), ": ", htmlEscape(commune_target)
    ),
    hover_label = paste0(
      nb_participations, " participations entreprises"
    )
  ) %>%
  arrange(nb_projets)

expected_flow_totals <- flow_project_pairs %>%
  mutate(
    segment = case_when(
      str_starts(source, "01") ~ flow_segment_domain[1],
      str_starts(source, "02") ~ flow_segment_domain[2],
      str_starts(source, "03") ~ flow_segment_domain[3],
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(segment)) %>%
  group_by(segment) %>%
  summarise(expected = n_distinct(id_projet), .groups = "drop")

mapped_flow_totals <- flow_project_routes %>%
  group_by(segment) %>%
  summarise(actual = n_distinct(id_projet), .groups = "drop")

flow_total_check <- expected_flow_totals %>%
  mutate(actual = mapped_flow_totals$actual[match(segment, mapped_flow_totals$segment)])

if (any(abs(flow_total_check$actual - flow_total_check$expected) > 1e-8)) {
  stop(
    "Totaux des flux cartographiques incoherents: ",
    paste0(
      flow_total_check$segment, "=", round(flow_total_check$actual, 6),
      " (attendu ", round(flow_total_check$expected, 6), ")",
      collapse = " | "
    ),
    call. = FALSE
  )
}

flow_legend_rows <- flow_width_legend()

# Legende explicite pour conserver l'ordre logique des etapes, du haut vers le bas.
flow_stage_legend_rows <- paste0(
  vapply(seq_along(flow_segment_domain), function(index) {
    paste0(
      "<div style='display:flex;align-items:center;gap:7px;margin:4px 0;'>",
      "<span style='display:inline-block;flex:0 0 14px;width:14px;height:14px;background:",
      flow_segment_palette(flow_segment_domain[index]), ";'></span>",
      htmlEscape(flow_segment_domain[index]),
      "</div>"
    )
  }, character(1)),
  collapse = ""
)

map_marker_data <- map_marker_data %>%
  semi_join(connected_flow_nodes(flow_lines),
            by = c("type" = "flow_stage", "latitude", "longitude"))

map_widget <- leaflet(map_data, width = "100%", height = 720) %>%
  addTiles(
    urlTemplate = "https://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}",
    attribution = "Tiles &copy; Esri &mdash; Esri, DeLorme, NAVTEQ", group = "Fond gris clair",
    options = tileOptions(maxZoom = 19, maxNativeZoom = 16)
  )

map_widget <- add_administrative_boundaries(map_widget)
map_flow_geometries <- vector("list", nrow(flow_lines))

if (nrow(flow_lines) > 0) {
  for (i in seq_len(nrow(flow_lines))) {
    if (flow_lines$nb_participations[i] < 5) next
    flow_points_to_draw <- if (isTRUE(flow_lines$is_intradepartement[i])) {
      make_flow_loop(
        flow_lines$longitude_source[i],
        flow_lines$latitude_source[i],
        radius = flow_lines$loop_radius[i],
        start_angle = flow_lines$loop_start_angle[i]
      )
    } else {
      make_flow_curve(
        flow_lines$longitude_source[i],
        flow_lines$latitude_source[i],
        flow_lines$longitude_target[i],
        flow_lines$latitude_target[i],
        bend = flow_lines$curve_bend[i],
        side = flow_lines$curve_side[i]
      )
    }
    
    map_flow_geometries[[i]] <- flow_points_to_draw
    map_widget <- map_widget %>%
      leaflet.extras2::addArrowhead(
        lng = flow_points_to_draw$longitude,
        lat = flow_points_to_draw$latitude,
        group = flow_lines$flow_group[i],
        color = flow_lines$line_color[i],
        weight = flow_lines$line_weight[i],
        opacity = flow_lines$line_opacity[i],
        smoothFactor = 0.5,
        noClip = TRUE,
        popup = flow_lines$popup[i],
        label = flow_lines$hover_label[i],
        options = leaflet.extras2::arrowheadOptions(
          yawn = 52,
          size = paste0(round(8 + flow_lines$line_weight[i]), "px"),
          frequency = "endonly",
          offsets = list(end = "16px")
        ),
        highlightOptions = highlightOptions(
          weight = flow_lines$line_weight[i] + 2,
          opacity = 1,
          bringToFront = TRUE
        )
      )
  }
}

map_widget <- fit_flow_bounds(map_widget, map_flow_geometries)

for (stage_type in c("Provenance", "Scierie / Industriel", "Charpentier / Constructeur / Menuisier", "Projet")) {
  stage_points <- map_marker_data %>% filter(type == stage_type)
  if (nrow(stage_points) > 0) {
    stage_icon <- switch(
      stage_type,
      "Provenance" = "leaf",
      "Scierie / Industriel" = "industry",
      "Charpentier / Constructeur / Menuisier" = "wrench",
      "Projet" = "home"
    )
    stage_marker <- switch(
      stage_type,
      "Provenance" = "green",
      "Scierie / Industriel" = "blue",
      "Charpentier / Constructeur / Menuisier" = "orange",
      "Projet" = "red"
    )
    map_widget <- map_widget %>%
      addAwesomeMarkers(
        data = stage_points,
        lng = ~marker_longitude,
        lat = ~marker_latitude,
        group = stage_type,
        icon = awesomeIcons(
          icon = stage_icon,
          library = "fa",
          markerColor = stage_marker,
          iconColor = "white",
          extraClasses = stage_points$marker_rotation_class
        ),
        popup = ~popup
      )
  }
}

map_widget <- map_widget %>%
  addLayersControl(
    overlayGroups = c("Limites departements", "Limites regions", "Provenance", "Scierie / Industriel", "Charpentier / Constructeur / Menuisier", "Projet"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  hideGroup(flow_segment_groups) %>%
  add_flow_legend(flow_legend_rows, flow_stage_legend_rows)

map_widget <- htmlwidgets::onRender(
  map_widget,
  "
  function(el, x) {
    var map = this;
    if (!map.eachLayer && map.map) map = map.map;
    if (!map.eachLayer) return;
    var markerRotations = {
      1: 0,
      2: 90,
      3: 180,
      4: 270
    };
    var flowRules = [
      {
        source: 'Provenance',
        target: 'Scierie / Industriel',
        flowGroup: 'Flux Provenance-Scierie'
      },
      {
        source: 'Scierie / Industriel',
        target: 'Charpentier / Constructeur / Menuisier',
        flowGroup: 'Flux Scierie-Charpentier'
      },
      {
        source: 'Charpentier / Constructeur / Menuisier',
        target: 'Projet',
        flowGroup: 'Flux Charpentier-Projet'
      }
    ];

    function isGroupVisible(groupName) {
      if (!map.layerManager) return false;
      var layer = map.layerManager.getLayerGroup(groupName, true);
      return !!layer && map.hasLayer(layer);
    }

    function updateFlowVisibility() {
      if (!map.layerManager) return;

      flowRules.forEach(function(rule) {
        var flowLayer = map.layerManager.getLayerGroup(rule.flowGroup, true);
        if (!flowLayer) return;

        if (isGroupVisible(rule.source) && isGroupVisible(rule.target)) {
          map.addLayer(flowLayer);
        } else {
          map.removeLayer(flowLayer);
        }
      });
    }

    // Icone + couleur devant chaque nom de couche dans le controle de calques,
    // reprenant les memes icones/couleurs que les marqueurs et le fond de
    // carte des limites regionales.
    var groupStyles = {
      'Limites regions': { icon: 'map-o', color: '#2F2F2F' },
      'Provenance': { icon: 'leaf', color: '#2E7D32' },
      'Scierie / Industriel': { icon: 'industry', color: '#2474B5' },
      'Charpentier / Constructeur / Menuisier': { icon: 'wrench', color: '#D86E16' },
      'Projet': { icon: 'home', color: '#D63E2A' }
    };

    function decorateLayerControl() {
      var spans = el.querySelectorAll('.leaflet-control-layers-overlays label span');
      spans.forEach(function(span) {
        if (span.dataset.iconized) return;
        var text = span.textContent.trim();
        var style = groupStyles[text];
        if (!style) return;
        span.dataset.iconized = '1';
        span.textContent = '';

        var badge = document.createElement('span');
        badge.style.display = 'inline-flex';
        badge.style.alignItems = 'center';
        badge.style.justifyContent = 'center';
        badge.style.width = '18px';
        badge.style.height = '18px';
        badge.style.borderRadius = '50%';
        badge.style.backgroundColor = style.color;
        badge.style.marginRight = '6px';
        badge.style.verticalAlign = 'middle';

        var icon = document.createElement('i');
        icon.className = 'fa fa-' + style.icon;
        icon.style.color = '#FFFFFF';
        icon.style.fontSize = '10px';
        badge.appendChild(icon);

        span.appendChild(badge);
        span.appendChild(document.createTextNode(text));
      });
    }

    function applyMarkerRotation() {
      map.eachLayer(function(layer) {
        if (!layer._icon) return;

        var icon = layer._icon;
        var classText = icon.className || '';
        var match = /stage-rotate-([1-4])/.exec(classText);

        if (!match) {
          var inner = icon.querySelector('[class*=stage-rotate-]');
          if (!inner) return;
          match = /stage-rotate-([1-4])/.exec(inner.className || '');
          if (!match) return;
        }

        var angle = markerRotations[match[1]];
        if (angle === undefined) return;

        var baseTransform = icon.style.transform || '';
        baseTransform = baseTransform.replace(/\\s*rotate\\([^)]*\\)/g, '');
        icon.style.transformOrigin = '50% 100%';
        icon.style.transform = baseTransform + ' rotate(' + angle + 'deg)';
      });
    }

    setTimeout(function() {
      applyMarkerRotation();
      updateFlowVisibility();
      decorateLayerControl();
    }, 0);
    map.on('layeradd', applyMarkerRotation);
    map.on('zoomend moveend', applyMarkerRotation);
    map.on('overlayadd', updateFlowVisibility);
    map.on('overlayremove', updateFlowVisibility);
  }
  "
)

heatmap_actor_layers <- c(
  unname(heatmap_actor_layer_labels)
)

heatmap_layers <- c(
  "Nombre de projets",
  heatmap_actor_layers
)

create_heatmap_base <- function() {
  widget <- leaflet(width = "100%", height = 720) %>%
    addTiles(
    urlTemplate = "https://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}",
    attribution = "Tiles &copy; Esri &mdash; Esri, DeLorme, NAVTEQ", group = "Fond gris clair",
    options = tileOptions(maxZoom = 19, maxNativeZoom = 16)
  ) %>%
    setView(lng = 4.70, lat = 45.45, zoom = 7)
  
  widget <- add_commune_density(widget)
  
  widget <- add_administrative_boundaries(widget)
  
  widget
}

add_population_density_legend <- function(widget) {
  widget %>%
    removeControl("map-context") %>%
    addLegend(
      position = "bottomright", colors = unname(population_density_colors),
      labels = unname(population_density_labels), opacity = 0.65,
      title = paste0(
        "<div style='font-size:15px;margin-bottom:8px;'>Légende</div>",
        "<div style='font:12px/1.4 Arial;font-weight:400;margin-bottom:10px;padding-bottom:8px;border-bottom:1px solid #ddd;'>",
        "<span style='border-top:2px dashed #2F2F2F;display:inline-block;width:24px;vertical-align:middle;'></span> Région &nbsp;&nbsp;",
        "<span style='border-top:1px dashed #666;display:inline-block;width:24px;vertical-align:middle;'></span> Département",
        "<div style='font-size:10px;color:#666;margin-top:5px;max-width:330px;'>Sources : fichiers locaux projets/acteurs ; contours administratifs ; INSEE/ANCT (grid7).</div></div>",
        "Densité communale — 7 classes ordonnées<br><small>Forte → faible</small>"
      ),
      group = population_density_layer, layerId = "density-legend"
    )
}

make_cluster_options <- function(hue) {
  # Badge de navigation : effectif exact ecrit, teinte reservee au role.
  cluster_icon_function <- JS(
    paste0(
      "function(cluster) {",
      "  var count = cluster.getChildCount();",
      "  var lightness = 40;",
      "  var size = 38;",
      "  var color = 'hsl(", hue, ", 72%, ' + lightness + '%)';",
      "  var textColor = lightness < 52 ? '#FFFFFF' : '#17202A';",
      "  var html = '<div style=\"width:' + size + 'px;height:' + size + 'px;",
      "border-radius:50%;display:flex;align-items:center;justify-content:center;",
      "background:' + color + ';color:' + textColor + ';",
      "border:3px solid rgba(255,255,255,0.88);",
      "box-shadow:0 1px 6px rgba(0,0,0,0.38);",
      "font:700 13px/1 Arial,sans-serif;\">' + count + '</div>';",
      "  return L.divIcon({",
      "    html: html,",
      "    className: 'adaptive-marker-cluster',",
      "    iconSize: L.point(size, size)",
      "  });",
      "}"
    )
  )
  
  markerClusterOptions(
    showCoverageOnHover = FALSE,
    zoomToBoundsOnClick = TRUE,
    spiderfyOnMaxZoom = TRUE,
    removeOutsideVisibleBounds = TRUE,
    maxClusterRadius = cluster_max_radius,
    iconCreateFunction = cluster_icon_function
  )
}

add_cluster_legend <- function(widget, title, symbol_rows, bottom_text) {
  widget %>%
    addControl(
      html = HTML(
        paste0(
          "<div class='info legend' style='font-size:13px;line-height:1.3;min-width:255px;'>",
          "<div style='font-size:18px;margin-bottom:7px;'>", htmlEscape(title), "</div>",
          symbol_rows,
          "<div style='margin-top:8px;font-weight:600;'>Nombre dans une grappe</div>",
          "<div>Le chiffre indique l’effectif exact ; regroupement selon le zoom.</div>",
          "<div style='margin-top:7px;'>", htmlEscape(bottom_text), "</div>",
          "</div>"
        )
      ),
      position = "bottomleft"
    )
}

project_symbol_legend <- paste0(
  "<div style='display:flex;align-items:center;gap:8px;margin:3px 0;'>",
  "<span style='display:inline-flex;width:22px;height:22px;border-radius:50%;",
  "align-items:center;justify-content:center;background:", project_marker_style$legend_color,
  ";color:white;'><i class='fa fa-", project_marker_style$icon, "'></i></span>",
  "<span>Projet</span></div>"
)

actor_symbol_legend <- paste(
  vapply(seq_len(nrow(actor_marker_styles)), function(i) {
    paste0(
      "<div style='display:flex;align-items:center;gap:8px;margin:3px 0;'>",
      "<span style='display:inline-flex;width:22px;height:22px;border-radius:50%;",
      "align-items:center;justify-content:center;background:",
      actor_marker_styles$legend_color[i],
      ";color:white;'><i class='fa fa-", actor_marker_styles$icon[i], "'></i></span>",
      "<span>", htmlEscape(actor_marker_styles$layer[i]), "</span></div>"
    )
  }, character(1)),
  collapse = ""
)

heatmap_projects_widget <- create_heatmap_base()

if (nrow(project_cluster_data) > 0) {
  heatmap_projects_widget <- heatmap_projects_widget %>%
    addAwesomeMarkers(
      data = project_cluster_data,
      lng = ~longitude,
      lat = ~latitude,
      icon = makeAwesomeIcon(
        icon = project_marker_style$icon,
        library = "fa",
        markerColor = project_marker_style$marker_color,
        iconColor = "white"
      ),
      popup = ~popup,
      label = ~if_else(nom == "", paste("Projet", id_projet), nom),
      group = "Nombre de projets",
      clusterOptions = make_cluster_options(project_marker_style$cluster_hue)
    )
}

heatmap_projects_widget <- heatmap_projects_widget %>%
  addLayersControl(
    overlayGroups = c(heatmap_base_overlay_groups, "Nombre de projets"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  add_population_density_legend()

heatmap_actors_widget <- create_heatmap_base()

for (layer_index in seq_along(heatmap_actor_layers)) {
  actor_layer <- heatmap_actor_layers[layer_index]
  layer_data <- actor_cluster_data %>% filter(layer == actor_layer)
  style_index <- match(actor_layer, actor_marker_styles$layer)
  
  if (nrow(layer_data) > 0 && !is.na(style_index)) {
    heatmap_actors_widget <- heatmap_actors_widget %>%
      addAwesomeMarkers(
        data = layer_data,
        lng = ~longitude,
        lat = ~latitude,
        icon = makeAwesomeIcon(
          icon = actor_marker_styles$icon[style_index],
          library = "fa",
          markerColor = actor_marker_styles$marker_color[style_index],
          iconColor = "white"
        ),
        popup = ~popup,
        label = ~nom,
        group = actor_layer,
        clusterOptions = make_cluster_options(actor_marker_styles$cluster_hue[style_index])
      )
  }
}

heatmap_actors_widget <- heatmap_actors_widget %>%
  addLayersControl(
    overlayGroups = c(heatmap_base_overlay_groups, heatmap_actor_layers),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  add_population_density_legend()

heatmap_widget <- heatmap_projects_widget

heatmap_scale_summary <- bind_rows(
  tibble(
    carte = "Heatmap du nombre de projets",
    couche = "Nombre de projets",
    methode = "MarkerCluster",
    un_marqueur_represente = "1 projet",
    symbole = project_marker_style$icon,
    rayon_max_cluster_pixels = cluster_max_radius,
    nombre_marqueurs = nrow(project_cluster_data)
  ),
  tibble(
    carte = "Heatmap des acteurs",
    couche = heatmap_actor_layers,
    methode = "MarkerCluster",
    un_marqueur_represente = "1 acteur distinct",
    symbole = actor_marker_styles$icon[
      match(heatmap_actor_layers, actor_marker_styles$layer)
    ],
    rayon_max_cluster_pixels = cluster_max_radius,
    nombre_marqueurs = vapply(
      heatmap_actor_layers,
      function(actor_layer) {
        nrow(actor_cluster_data %>% filter(layer == actor_layer))
      },
      numeric(1)
    )
  )
)

distance_global_row <- distance_stats_global %>% slice(1)
distance_role_rows <- distance_stats_roles_top %>%
  mutate(
    distance_q25_km = format(distance_q25_km, nsmall = 2),
    distance_mediane_km = format(distance_mediane_km, nsmall = 2),
    distance_q75_km = format(distance_q75_km, nsmall = 2),
    distance_max_km = format(distance_max_km, nsmall = 2)
  )

distance_stats_table <- tags$table(
  style = "border-collapse:collapse; font-family:Arial, sans-serif; font-size:13px; min-width:900px;",
  tags$thead(
    tags$tr(
      lapply(
        c("Groupe acteur", "Liaisons", "Acteurs", "Projets", "Q25 km", "Mediane km", "Q75 km", "Max km"),
        function(label) tags$th(style = "text-align:left; border-bottom:1px solid #ccc; padding:6px 8px;", label)
      )
    )
  ),
  tags$tbody(
    lapply(seq_len(nrow(distance_role_rows)), function(i) {
      tags$tr(
        tags$td(style = "padding:5px 8px; border-bottom:1px solid #eee;", distance_role_rows$groupe_acteur[i]),
        tags$td(style = "padding:5px 8px; border-bottom:1px solid #eee; text-align:right;", distance_role_rows$nb_liaisons[i]),
        tags$td(style = "padding:5px 8px; border-bottom:1px solid #eee; text-align:right;", distance_role_rows$nb_acteurs[i]),
        tags$td(style = "padding:5px 8px; border-bottom:1px solid #eee; text-align:right;", distance_role_rows$nb_projets[i]),
        tags$td(style = "padding:5px 8px; border-bottom:1px solid #eee; text-align:right;", distance_role_rows$distance_q25_km[i]),
        tags$td(style = "padding:5px 8px; border-bottom:1px solid #eee; text-align:right;", distance_role_rows$distance_mediane_km[i]),
        tags$td(style = "padding:5px 8px; border-bottom:1px solid #eee; text-align:right;", distance_role_rows$distance_q75_km[i]),
        tags$td(style = "padding:5px 8px; border-bottom:1px solid #eee; text-align:right;", distance_role_rows$distance_max_km[i])
      )
    })
  )
)

distance_actor_pair_global <- distance_stats_actor_pairs_project %>%
  filter(nb_paires > 0, !is.na(distance_mediane_km)) %>%
  summarise(
    nb_projets = n(),
    nb_paires = sum(nb_paires),
    distance_mediane_projets_km = round(median(distance_mediane_km), 2)
  )

distance_group_pair_rows <- distance_stats_group_pairs %>%
  mutate(
    distance_q25_km = format(distance_q25_km, nsmall = 2),
    distance_mediane_km = format(distance_mediane_km, nsmall = 2),
    distance_q75_km = format(distance_q75_km, nsmall = 2)
  )

distance_group_pair_table <- tags$table(
  style = "border-collapse:collapse; font-family:Arial, sans-serif; font-size:13px; min-width:820px;",
  tags$thead(
    tags$tr(
      lapply(
        c("Groupe 1", "Groupe 2", "Projets", "Paires d'acteurs", "Q25 km", "Mediane km", "Q75 km"),
        function(label) tags$th(style = "text-align:left; border-bottom:1px solid #ccc; padding:6px 8px;", label)
      )
    )
  ),
  tags$tbody(
    lapply(seq_len(nrow(distance_group_pair_rows)), function(i) {
      tags$tr(
        tags$td(style = "padding:5px 8px; border-bottom:1px solid #eee;", distance_group_pair_rows$groupe_1[i]),
        tags$td(style = "padding:5px 8px; border-bottom:1px solid #eee;", distance_group_pair_rows$groupe_2[i]),
        tags$td(style = "padding:5px 8px; border-bottom:1px solid #eee; text-align:right;", distance_group_pair_rows$nb_projets[i]),
        tags$td(style = "padding:5px 8px; border-bottom:1px solid #eee; text-align:right;", distance_group_pair_rows$nb_paires_acteurs[i]),
        tags$td(style = "padding:5px 8px; border-bottom:1px solid #eee; text-align:right;", distance_group_pair_rows$distance_q25_km[i]),
        tags$td(style = "padding:5px 8px; border-bottom:1px solid #eee; text-align:right; font-weight:600;", distance_group_pair_rows$distance_mediane_km[i]),
        tags$td(style = "padding:5px 8px; border-bottom:1px solid #eee; text-align:right;", distance_group_pair_rows$distance_q75_km[i])
      )
    })
  )
)

same_region_project_rows <- project_same_region_actor_counts %>%
  filter(!is.na(part_acteurs_meme_region)) %>%
  mutate(
    part_acteurs_meme_region = paste0(round(100 * part_acteurs_meme_region, 1), "%")
  )

same_region_project_table <- tags$table(
  style = "border-collapse:collapse; font-family:Arial, sans-serif; font-size:13px; min-width:900px;",
  tags$thead(
    tags$tr(
      lapply(
        c("Projet", "Commune", "Region projet", "Part meme region", "Acteurs region connue", "Acteurs total", "Region inconnue"),
        function(label) tags$th(style = "text-align:left; border-bottom:1px solid #ccc; padding:6px 8px;", label)
      )
    )
  ),
  tags$tbody(
    lapply(seq_len(nrow(same_region_project_rows)), function(i) {
      tags$tr(
        tags$td(style = "padding:5px 8px; border-bottom:1px solid #eee;", same_region_project_rows$nom_projet[i]),
        tags$td(style = "padding:5px 8px; border-bottom:1px solid #eee;", same_region_project_rows$commune_projet[i]),
        tags$td(style = "padding:5px 8px; border-bottom:1px solid #eee;", same_region_project_rows$region_projet[i]),
        tags$td(style = "padding:5px 8px; border-bottom:1px solid #eee; text-align:right; font-weight:600;", same_region_project_rows$part_acteurs_meme_region[i]),
        tags$td(style = "padding:5px 8px; border-bottom:1px solid #eee; text-align:right;", same_region_project_rows$nb_acteurs_region_connue[i]),
        tags$td(style = "padding:5px 8px; border-bottom:1px solid #eee; text-align:right;", same_region_project_rows$nb_acteurs_total[i]),
        tags$td(style = "padding:5px 8px; border-bottom:1px solid #eee; text-align:right;", same_region_project_rows$nb_acteurs_region_inconnue[i])
      )
    })
  )
)

# ---------------------------------------------------------------------------
# Cartes de flux complementaires, par role individuel. La carte principale
# (map_widget, ci-dessus) reste inchangee : elle regroupe "Scierie"/"Industriel"
# et "Charpentier"/"Constructeur bois"/"Menuisier" chacun en une seule couche.
# Ces deux cartes isolent chaque role separement, avec ses points et ses flux
# vers l'etape voisine (Provenance pour Scierie/Industriel, Projet pour
# Charpentier/Constructeur bois/Menuisier).
build_role_flow_map <- function(pair_data, source_locations, target_locations,
                                 role_styles, segment_colors, height = 720) {
  node_locations <- bind_rows(source_locations, target_locations) %>%
    distinct(name, .keep_all = TRUE)

  routes <- pair_data %>%
    mutate(
      source_index = match(source, node_locations$name),
      target_index = match(target, node_locations$name),
      flow_stage_source = node_locations$flow_stage[source_index],
      commune_source = node_locations$commune[source_index],
      dept_source = node_locations$dept[source_index],
      latitude_source = node_locations$latitude[source_index],
      longitude_source = node_locations$longitude[source_index],
      flow_stage_target = node_locations$flow_stage[target_index],
      commune_target = node_locations$commune[target_index],
      dept_target = node_locations$dept[target_index],
      latitude_target = node_locations$latitude[target_index],
      longitude_target = node_locations$longitude[target_index],
      segment = paste0(flow_stage_source, " -> ", flow_stage_target)
    ) %>%
    filter(
      !is.na(flow_stage_source), !is.na(flow_stage_target),
      !is.na(latitude_source), !is.na(longitude_source),
      !is.na(latitude_target), !is.na(longitude_target)
    )

  lines <- routes %>%
    group_by(
      segment,
      flow_stage_source, commune_source, dept_source,
      latitude_source, longitude_source,
      flow_stage_target, commune_target, dept_target,
      latitude_target, longitude_target
    ) %>%
    summarise(
      nb_projets = n_distinct(id_projet),
      nb_participations = n_distinct(participation_id),
      nb_participations_source = n_distinct(source_entry[source_is_actor]),
      nb_participations_cible = n_distinct(target_entry[target_is_actor]),
      .groups = "drop"
    ) %>%
    mutate(
      is_intradepartement = (flow_stage_source != "Provenance") & (dept_source == dept_target),
      line_weight = flow_line_width(nb_participations),
      line_opacity = 0.8,
      route_rank = dense_rank(paste(segment, dept_source, dept_target, sep = "|")),
      curve_side = if_else(route_rank %% 2 == 0, -1, 1),
      curve_bend = 0.10 + 0.015 * (route_rank %% 4),
      segment_rank = dense_rank(segment),
      loop_radius = 2.5 * (0.055 + 0.012 * pmin(segment_rank, 3)),
      loop_start_angle = (segment_rank - 1) * pi / 3,
      line_color = unname(segment_colors[segment]),
      popup = paste0(
        "<b>", nb_participations, " participations entreprises</b><br>",
        "Projets distincts : ", nb_projets, "<br>",
        "Comptage : ", if_else(flow_stage_source == "Provenance", "entreprises cibles", "entreprises sources"), "<br>",
        if_else(is_intradepartement, "<b>Boucle dans le même département</b><br>", ""),
        htmlEscape(flow_stage_source), ": ", htmlEscape(commune_source), "<br>",
        htmlEscape(flow_stage_target), ": ", htmlEscape(commune_target)
      ),
      hover_label = paste0(nb_participations, " participations entreprises")
    ) %>%
    arrange(nb_projets)

  # Meme technique que map_widget (carte agregee) : les marqueurs qui partagent
  # exactement le meme point ne sont pas deplaces (jitter), ils restent a la
  # coordonnee reelle et sont distingues par rotation de l'icone (classe
  # "stage-rotate-N", cf. le onRender plus bas), pour rester coherent entre
  # toutes les cartes de flux.
  marker_data <- node_locations %>%
    semi_join(connected_flow_nodes(lines), by = c("flow_stage", "latitude", "longitude")) %>%
    left_join(nodes %>% select(name, node_value, count_unit), by = "name") %>%
    mutate(
      stage_order = match(flow_stage, role_styles$stage),
      coordinate_lat = round(latitude, 6),
      coordinate_lng = round(longitude, 6)
    ) %>%
    group_by(coordinate_lat, coordinate_lng) %>%
    arrange(stage_order, .by_group = TRUE) %>%
    mutate(
      marker_index = row_number(),
      marker_count = n(),
      marker_rotation_class = if_else(
        marker_count > 1,
        paste0("stage-rotate-", marker_index),
        "stage-rotate-0"
      )
    ) %>%
    ungroup() %>%
    select(-stage_order, -coordinate_lat, -coordinate_lng, -marker_index, -marker_count) %>%
    mutate(popup = paste0("<b>", htmlEscape(flow_stage), "</b><br>", htmlEscape(commune),
                           "<br>", node_value, " ", htmlEscape(count_unit)))

  widget <- leaflet(width = "100%", height = height) %>%
    addTiles(
    urlTemplate = "https://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}",
    attribution = "Tiles &copy; Esri &mdash; Esri, DeLorme, NAVTEQ", group = "Fond gris clair",
    options = tileOptions(maxZoom = 19, maxNativeZoom = 16)
  )

  widget <- add_administrative_boundaries(widget)

  flow_geometries <- vector("list", nrow(lines))
  if (nrow(lines) > 0) {
    for (i in seq_len(nrow(lines))) {
      if (lines$nb_participations[i] < 5) next
      flow_points <- if (isTRUE(lines$is_intradepartement[i])) {
        make_flow_loop(
          lines$longitude_source[i], lines$latitude_source[i],
          radius = lines$loop_radius[i], start_angle = lines$loop_start_angle[i]
        )
      } else {
        make_flow_curve(
          lines$longitude_source[i], lines$latitude_source[i],
          lines$longitude_target[i], lines$latitude_target[i],
          bend = lines$curve_bend[i], side = lines$curve_side[i]
        )
      }
      flow_geometries[[i]] <- flow_points
      widget <- widget %>%
        leaflet.extras2::addArrowhead(
          lng = flow_points$longitude, lat = flow_points$latitude,
          group = paste0("Flux : ", lines$segment[i]),
          color = lines$line_color[i],
          weight = lines$line_weight[i],
          opacity = lines$line_opacity[i],
          smoothFactor = 0.5, noClip = TRUE,
          popup = lines$popup[i], label = lines$hover_label[i],
          options = leaflet.extras2::arrowheadOptions(
            yawn = 52,
            size = paste0(round(8 + lines$line_weight[i]), "px"),
            frequency = "endonly", offsets = list(end = "16px")
          ),
          highlightOptions = highlightOptions(
            weight = lines$line_weight[i] + 2, opacity = 1, bringToFront = TRUE
          )
        )
    }
  }

  widget <- fit_flow_bounds(widget, flow_geometries)

  for (stage_type in unique(marker_data$flow_stage)) {
    stage_points <- marker_data %>% filter(flow_stage == stage_type)
    if (nrow(stage_points) == 0) next
    style_row <- role_styles %>% filter(stage == stage_type)
    if (nrow(style_row) == 0) next
    widget <- widget %>%
      addAwesomeMarkers(
        data = stage_points,
        lng = ~longitude, lat = ~latitude,
        group = stage_type,
        icon = awesomeIcons(
          icon = style_row$icon[1],
          library = "fa",
          markerColor = style_row$marker_color[1],
          iconColor = "white",
          extraClasses = stage_points$marker_rotation_class
        ),
        popup = ~popup
      )
  }

  flow_segments_present <- sort(unique(lines$segment))

  # Memes deux legendes que map_widget (carte agregee) : epaisseur des flux
  # (participations entreprises) et etape du flux (couleur par segment).
  flow_legend_rows <- flow_width_legend()

  flow_stage_legend_rows <- paste0(
    vapply(flow_segments_present, function(seg) {
      paste0(
        "<div style='display:flex;align-items:center;gap:7px;margin:4px 0;'>",
        "<span style='display:inline-block;flex:0 0 14px;width:14px;height:14px;background:",
        unname(segment_colors[seg]), ";'></span>",
        htmlEscape(seg),
        "</div>"
      )
    }, character(1)),
    collapse = ""
  )

  # Comme sur map_widget (carte agregee), les groupes "Flux : ..." ne sont pas
  # des cases a cocher dans le controle de calques : ils restent masques par
  # defaut et ne s'affichent qu'automatiquement (cf. onRender plus bas),
  # quand les deux roles qu'ils relient sont coches. La legende "Roles" est
  # retiree : le controle de calques porte deja un rond colore + icone par
  # role (cf. decorateLayerControl dans le onRender).
  widget <- widget %>%
    addLayersControl(
      overlayGroups = c(
        "Limites departements", "Limites regions",
        role_styles$stage
      ),
      options = layersControlOptions(collapsed = FALSE)
    ) %>%
    hideGroup(paste0("Flux : ", flow_segments_present)) %>%
    add_flow_legend(flow_legend_rows, flow_stage_legend_rows)

  # Meme comportement interactif que map_widget (carte agregee) : les flux
  # restent masques par defaut, mais des qu'on coche a la fois le role source
  # et le role cible dans le controle de calques, la ligne de flux
  # correspondante s'affiche automatiquement (et inversement si on decoche).
  flow_rules_js <- paste0(
    "[",
    paste0(
      vapply(flow_segments_present, function(seg) {
        parts <- str_split_fixed(seg, " -> ", 2)
        sprintf(
          "{source: %s, target: %s, flowGroup: %s}",
          jsonlite::toJSON(parts[1, 1], auto_unbox = TRUE),
          jsonlite::toJSON(parts[1, 2], auto_unbox = TRUE),
          jsonlite::toJSON(paste0("Flux : ", seg), auto_unbox = TRUE)
        )
      }, character(1)),
      collapse = ","
    ),
    "]"
  )

  # Meme decoration que map_widget (carte agregee) : un rond colore + icone
  # blanche devant chaque nom de role dans le controle de calques, avec les
  # memes icone/couleur que les marqueurs de ce role sur la carte.
  group_styles_js <- paste0(
    "{",
    paste(
      c(
        "'Limites regions': { icon: 'map-o', color: '#2F2F2F' }",
        vapply(seq_len(nrow(role_styles)), function(i) {
          sprintf(
            "%s: { icon: %s, color: %s }",
            jsonlite::toJSON(role_styles$stage[i], auto_unbox = TRUE),
            jsonlite::toJSON(role_styles$icon[i], auto_unbox = TRUE),
            jsonlite::toJSON(role_styles$legend_color[i], auto_unbox = TRUE)
          )
        }, character(1))
      ),
      collapse = ","
    ),
    "}"
  )

  js_template <- r"(
  function(el, x) {
    var map = this;
    if (!map.eachLayer && map.map) map = map.map;
    if (!map.eachLayer) return;
    var markerRotations = { 1: 0, 2: 90, 3: 180, 4: 270 };
    var flowRules = __FLOW_RULES__;
    var groupStyles = __GROUP_STYLES__;

    function decorateLayerControl() {
      var spans = el.querySelectorAll('.leaflet-control-layers-overlays label span');
      spans.forEach(function(span) {
        if (span.dataset.iconized) return;
        var text = span.textContent.trim();
        var style = groupStyles[text];
        if (!style) return;
        span.dataset.iconized = '1';
        span.textContent = '';

        var badge = document.createElement('span');
        badge.style.display = 'inline-flex';
        badge.style.alignItems = 'center';
        badge.style.justifyContent = 'center';
        badge.style.width = '18px';
        badge.style.height = '18px';
        badge.style.borderRadius = '50%';
        badge.style.backgroundColor = style.color;
        badge.style.marginRight = '6px';
        badge.style.verticalAlign = 'middle';

        var icon = document.createElement('i');
        icon.className = 'fa fa-' + style.icon;
        icon.style.color = '#FFFFFF';
        icon.style.fontSize = '10px';
        badge.appendChild(icon);

        span.appendChild(badge);
        span.appendChild(document.createTextNode(text));
      });
    }

    function isGroupVisible(groupName) {
      if (!map.layerManager) return false;
      var layer = map.layerManager.getLayerGroup(groupName, true);
      return !!layer && map.hasLayer(layer);
    }

    function updateFlowVisibility() {
      if (!map.layerManager) return;
      flowRules.forEach(function(rule) {
        var flowLayer = map.layerManager.getLayerGroup(rule.flowGroup, true);
        if (!flowLayer) return;
        if (isGroupVisible(rule.source) && isGroupVisible(rule.target)) {
          map.addLayer(flowLayer);
        } else {
          map.removeLayer(flowLayer);
        }
      });
    }

    function applyMarkerRotation() {
      map.eachLayer(function(layer) {
        if (!layer._icon) return;
        var icon = layer._icon;
        var classText = icon.className || '';
        var match = /stage-rotate-([1-4])/.exec(classText);
        if (!match) {
          var inner = icon.querySelector('[class*=stage-rotate-]');
          if (!inner) return;
          match = /stage-rotate-([1-4])/.exec(inner.className || '');
          if (!match) return;
        }
        var angle = markerRotations[match[1]];
        if (angle === undefined) return;
        var baseTransform = icon.style.transform || '';
        baseTransform = baseTransform.replace(/\s*rotate\([^)]*\)/g, '');
        icon.style.transformOrigin = '50% 100%';
        icon.style.transform = baseTransform + ' rotate(' + angle + 'deg)';
      });
    }

    setTimeout(function() {
      applyMarkerRotation();
      updateFlowVisibility();
      decorateLayerControl();
    }, 0);
    map.on('layeradd', applyMarkerRotation);
    map.on('zoomend moveend', applyMarkerRotation);
    map.on('overlayadd', updateFlowVisibility);
    map.on('overlayremove', updateFlowVisibility);
  }
  )"

  resolved_js <- sub("__FLOW_RULES__", flow_rules_js, js_template, fixed = TRUE)
  resolved_js <- sub("__GROUP_STYLES__", group_styles_js, resolved_js, fixed = TRUE)

  htmlwidgets::onRender(widget, resolved_js)
}

role_styles_scierie_industriel <- tibble::tribble(
  ~stage,        ~icon,      ~marker_color, ~legend_color,
  "Provenance",  "leaf",     "green",       "#2E7D32",
  "Scierie",     "industry", "blue",        "#2474B5",
  "Industriel",  "cogs",     "cadetblue",   "#3E7C8C"
)

flow_actor_locations_scierie_industriel <- stage_scierie_industriel_source %>%
  transmute(name = node_source) %>%
  distinct() %>%
  mutate(
    label = display_name(name),
    flow_stage = str_replace(label, " - Dept .*$", ""),
    dept = str_replace(label, "^.* - Dept ", ""),
    latitude = resolve_dept_latitude(dept),
    longitude = resolve_dept_longitude(dept),
    commune = if_else(
      dept == "ND", "Departement non renseigne", paste0("Centre du departement ", dept)
    )
  ) %>%
  select(name, flow_stage, commune, dept, latitude, longitude)

segment_colors_scierie_industriel <- c(
  "Provenance -> Scierie" = "#E67E22",
  "Provenance -> Industriel" = "#3E7C8C"
)

map_widget_scierie_industriel <- build_role_flow_map(
  pair_data = make_stage_pairs(stage_provenance, stage_scierie_industriel_target) %>%
    as_flow_pairs(),
  source_locations = flow_provenance_locations,
  target_locations = flow_actor_locations_scierie_industriel,
  role_styles = role_styles_scierie_industriel,
  segment_colors = segment_colors_scierie_industriel
)

role_styles_bois_pose <- tibble::tribble(
  ~stage,              ~icon,      ~marker_color, ~legend_color,
  "Charpentier",       "wrench",   "orange",      "#D86E16",
  "Constructeur bois", "building", "darkred",     "#B03A2E",
  "Menuisier",         "cubes",    "darkpurple",  "#6C3483",
  "Projet",            "home",     "gray",        "#4A4A4A"
)

flow_actor_locations_bois_pose <- stage_bois_source %>%
  transmute(name = node_source) %>%
  distinct() %>%
  mutate(
    label = display_name(name),
    flow_stage = str_replace(label, " - Dept .*$", ""),
    dept = str_replace(label, "^.* - Dept ", ""),
    latitude = resolve_dept_latitude(dept),
    longitude = resolve_dept_longitude(dept),
    commune = if_else(
      dept == "ND", "Departement non renseigne", paste0("Centre du departement ", dept)
    )
  ) %>%
  select(name, flow_stage, commune, dept, latitude, longitude)

segment_colors_bois_pose <- c(
  "Charpentier -> Projet" = "#D86E16",
  "Constructeur bois -> Projet" = "#B03A2E",
  "Menuisier -> Projet" = "#6C3483"
)

map_widget_bois_pose <- build_role_flow_map(
  pair_data = make_stage_pairs(stage_bois_source, stage_projet) %>%
    as_flow_pairs(),
  source_locations = flow_actor_locations_bois_pose,
  target_locations = flow_project_locations,
  role_styles = role_styles_bois_pose,
  segment_colors = segment_colors_bois_pose,
  height = 800
)

# Carte a deux roles independants relies directement a Charpentier (et non
# une chaine sequentielle Scierie -> Industriel -> Charpentier) : Scierie et
# Industriel sont chacun connectes directement a Charpentier, sans lien
# Scierie -> Industriel.
role_styles_scierie_industriel_charpentier <- tibble::tribble(
  ~stage,        ~icon,      ~marker_color, ~legend_color,
  "Scierie",     "industry", "blue",        "#2474B5",
  "Industriel",  "cogs",     "cadetblue",   "#3E7C8C",
  "Charpentier", "wrench",   "orange",      "#D86E16"
)

stage_scierie_source_only <- stage_scierie_industriel_source %>%
  filter(str_detect(display_name(node_source), "^Scierie - Dept "))

stage_industriel_source_only <- stage_scierie_industriel_source %>%
  filter(str_detect(display_name(node_source), "^Industriel - Dept "))

stage_bois_target_charpentier <- stage_bois_target %>%
  filter(str_detect(display_name(node_target), "^Charpentier - Dept "))

flow_actor_locations_charpentier_only <- flow_actor_locations_bois_pose %>%
  filter(flow_stage == "Charpentier")

pair_data_scierie_industriel_charpentier <- bind_rows(
  make_stage_pairs(stage_scierie_source_only, stage_bois_target_charpentier) %>%
    as_flow_pairs(),
  make_stage_pairs(stage_industriel_source_only, stage_bois_target_charpentier) %>%
    as_flow_pairs()
) %>%
  distinct(id_projet, source, target, source_entry, target_entry, .keep_all = TRUE)

segment_colors_scierie_industriel_charpentier <- c(
  "Scierie -> Charpentier" = "#2474B5",
  "Industriel -> Charpentier" = "#D86E16"
)

map_widget_scierie_industriel_charpentier <- build_role_flow_map(
  pair_data = pair_data_scierie_industriel_charpentier,
  source_locations = flow_actor_locations_scierie_industriel,
  target_locations = bind_rows(
    flow_actor_locations_scierie_industriel,
    flow_actor_locations_charpentier_only
  ),
  role_styles = role_styles_scierie_industriel_charpentier,
  segment_colors = segment_colors_scierie_industriel_charpentier,
  height = 800
)

page <- browsable(
  tagList(
    p,
    tags$div(
      style = "font-family:Arial, sans-serif; margin:26px 0 8px 0;",
      tags$h2(
        "Carte des participations des entreprises AURA par département",
        style = "margin:0 0 6px 0; font-size:22px; font-weight:600;"
      )
    ),
    map_widget,
    tags$div(
      style = "font-family:Arial, sans-serif; margin:26px 0 8px 0;",
      tags$h2(
        "Carte des flux — Provenance vers entreprises de transformation",
        style = "margin:0 0 6px 0; font-size:22px; font-weight:600;"
      )
    ),
    map_widget_scierie_industriel,
    tags$div(
      style = "font-family:Arial, sans-serif; margin:26px 0 8px 0;",
      tags$h2(
        "Carte des flux — entreprise de construction vers Projet",
        style = "margin:0 0 6px 0; font-size:22px; font-weight:600;"
      )
    ),
    map_widget_bois_pose,
    tags$div(
      style = "font-family:Arial, sans-serif; margin:26px 0 8px 0;",
      tags$h2(
        "Carte des flux — entreprises de transformation vers entreprise de construction",
        style = "margin:0 0 6px 0; font-size:22px; font-weight:600;"
      )
    ),
    map_widget_scierie_industriel_charpentier,
    tags$div(
      style = "font-family:Arial, sans-serif; margin:26px 0 8px 0;",
      tags$h2(
        "Carte des projets AURA",
        style = "margin:0 0 6px 0; font-size:22px; font-weight:600;"
      )
    ),
    heatmap_projects_widget,
    tags$div(
      style = "font-family:Arial, sans-serif; margin:26px 0 8px 0;",
      tags$h2(
        "Carte des acteurs des projets AURA",
        style = "margin:0 0 6px 0; font-size:22px; font-weight:600;"
      )
    ),
    heatmap_actors_widget,
    tags$div(
      style = "font-family:Arial, sans-serif; margin:26px 0 8px 0;",
      tags$h2(
        "Statistiques sur les distances acteur-projet",
        style = "margin:0 0 6px 0; font-size:22px; font-weight:600;"
      ),
      tags$div(
        paste0(
          "Toutes liaisons: ", distance_global_row$nb_liaisons,
          " | Acteurs: ", distance_global_row$nb_acteurs,
          " | Projets: ", distance_global_row$nb_projets,
          " | Distance mediane: ", distance_global_row$distance_mediane_km, " km",
          " | Intervalle interquartile: ", distance_global_row$distance_q25_km,
          "–", distance_global_row$distance_q75_km, " km"
        ),
        style = "font-size:13px; color:#333; margin-bottom:8px; font-weight:600;"
        
      ),
      tags$div(
        style = "overflow-x:auto;",
        distance_stats_table
      )
    ),
    tags$div(
      style = "font-family:Arial, sans-serif; margin:26px 0 8px 0;",
      tags$h2(
        "Distances entre acteurs d'un même projet",
        style = "margin:0 0 6px 0; font-size:22px; font-weight:600;"
      ),
      tags$div(
        paste0(
          "Projets avec au moins deux acteurs géocodés: ", distance_actor_pair_global$nb_projets,
          " | Paires uniques: ", distance_actor_pair_global$nb_paires,
          " | Médiane des médianes par projet: ",
          distance_actor_pair_global$distance_mediane_projets_km, " km"
        ),
        style = "font-size:13px; color:#333; margin-bottom:8px; font-weight:600;"
      ),
      tags$div(
        "Les distances par groupe sont calculées dans chaque projet avant l'agrégation entre projets, afin de ne pas surpondérer les grands projets.",
        style = "font-size:13px; color:#555; margin-bottom:8px;"
      ),
      tags$div(style = "overflow-x:auto;", distance_group_pair_table)
    ),
    tags$div(
      style = "font-family:Arial, sans-serif; margin:26px 0 8px 0;",
      tags$h2(
        "Acteurs situes dans la meme region que leur projet",
        style = "margin:0 0 6px 0; font-size:22px; font-weight:600;"
      ),
      tags$div(
        "Tous les projets avec la proportion d'acteurs localises dans la meme region administrative que le projet.",
        style = "font-size:13px; color:#555; margin-bottom:8px;"
      ),
      tags$div(
        style = "overflow-x:auto;",
        same_region_project_table
      )
    )
  )
)
# ============================================================================
# 4. Export des tableaux CSV
# ============================================================================

write_csv(
  links_plot %>%
    transmute(
      source = display_name(source),
      target = display_name(target),
      value
    ),
  output_file("sankey_aura_filiere_bois_links.csv")
)

write_csv(
  nodes %>%
    transmute(
      stage = group,
      area = node_area,
      node = label,
      value_total = node_value,
      area_value_total = area_value,
      unite = count_unit
    ),
  output_file("sankey_aura_filiere_bois_nodes_ordre.csv")
)

write_csv(
  map_data,
  output_file("carte_aura_filiere_bois_points.csv")
)

write_csv(
  expected_map_stage_totals,
  output_file("carte_aura_filiere_bois_totaux_noeuds.csv")
)

write_csv(
  flow_total_check,
  output_file("carte_aura_filiere_bois_totaux_flux.csv")
)

write_csv(
  heatmap_data %>%
    transmute(
      type = heatmap_type,
      couche = layer,
      commune,
      latitude,
      longitude,
      poids = weight,
      nb_projets,
      nb_acteurs
    ),
  output_file("carte_aura_heatmap_projets_acteurs.csv")
)

write_csv(
  heatmap_scale_summary,
  output_file("carte_aura_heatmap_echelles_couleur.csv")
)

write_csv(
  projets_retires_filiere,
  output_file("projets_retires_sans_acteurs_filiere_bois.csv")
)

write_csv(
  distances_acteurs_projets %>%
    mutate(distance_km = round(distance_km, 3)) %>%
    arrange(desc(distance_km)),
  output_file("distances_acteurs_projets.csv")
)

write_csv(
  distances_acteurs_projets_detail %>%
    mutate(distance_km = round(distance_km, 3)) %>%
    arrange(id_projet, acteur_id, groupe_acteur, role_acteur),
  output_file("distances_acteurs_projets_detail_roles.csv")
)

write_csv(
  distances_acteurs_acteurs %>%
    mutate(distance_km = round(distance_km, 3)) %>%
    arrange(id_projet, acteur_id_a, acteur_id_b),
  output_file("distances_acteurs_acteurs_par_projet.csv")
)

write_csv(
  distance_stats_actor_pairs_project,
  output_file("statistiques_distances_acteurs_entre_eux_par_projet.csv")
)

write_csv(
  distance_stats_group_pairs,
  output_file("statistiques_distances_acteurs_entre_groupes.csv")
)

write_csv(
  bind_rows(
    distance_stats_global %>% rename(groupe = type_statistique),
    distance_stats_roles %>% rename(groupe = groupe_acteur)
  ),
  output_file("statistiques_distances_acteurs_par_role.csv")
)

write_csv(
  distance_stats_projets,
  output_file("statistiques_distances_acteurs_par_projet.csv")
)

write_csv(
  project_same_region_actor_counts,
  output_file("statistiques_acteurs_meme_region_par_projet.csv")
)

write_csv(
  project_same_region_actor_counts_by_role,
  output_file("statistiques_acteurs_meme_region_par_projet_role.csv")
)

write_csv(
  project_actor_locality %>%
    mutate(
      part_acteurs_meme_departement = round(part_acteurs_meme_departement, 3),
      part_acteurs_meme_region = round(part_acteurs_meme_region, 3)
    ) %>%
    arrange(desc(part_acteurs_meme_region), desc(part_acteurs_meme_departement)),
  output_file("statistiques_acteurs_locaux_par_projet.csv")
)

write_csv(
  correlation_proximite_table,
  output_file("correlation_proximite_acteurs_projets.csv")
)


write_csv(khi2_systemes_essences$summary, output_file("khi2_systemes_constructifs_essences_global.csv"))
write_csv(khi2_types_batiments_systemes$summary, output_file("khi2_types_batiments_systemes_constructifs_global.csv"))
write_csv(khi2_essences_provenances$summary, output_file("khi2_essences_provenances_global.csv"))
write_csv(khi2_systemes_essences$contingency, output_file("contingence_systemes_essences.csv"))
write_csv(khi2_types_batiments_systemes$contingency, output_file("contingence_types_batiments_systemes.csv"))
write_csv(khi2_essences_provenances$contingency, output_file("contingence_essences_provenances.csv"))



write_csv(
  acteurs_projets_regions,
  output_file("acteurs_projets_regions_detail.csv")
)

write_csv(
  flow_lines %>%
    transmute(
      segment,
      nb_projets,
      nb_participations,
      nb_participations_source,
      nb_participations_cible,
      line_weight,
      flow_geometry,
      source_stage = flow_stage_source,
      source_commune = commune_source,
      source_dept = dept_source,
      source_latitude = latitude_source,
      source_longitude = longitude_source,
      target_stage = flow_stage_target,
      target_commune = commune_target,
      target_dept = dept_target,
      target_latitude = latitude_target,
      target_longitude = longitude_target
    ),
  output_file("carte_aura_filiere_bois_flux_projets.csv")
)

# Sorties d'audit du comptage demande : projet x entreprise x role.
# Cohorte des trois figures spatiales et indicateurs permettant leur vérification.
# Le n du sous-titre désigne la cohorte ; le n des cellules désigne les cas valides.
write_csv(
  tibble(id_projet = sort(sankey_project_ids)) %>%
    mutate(distance_disponible = id_projet %in% distances_acteurs_projets$id_projet),
  output_file("cohorte_projets_figures_spatiales.csv")
)
write_csv(localite_stats_roles, output_file("statistiques_localite_acteurs_par_groupe.csv"))
write_csv(acteurs_particuliers_exclus, output_file("acteurs_particuliers_exclus.csv"))
write_csv(participations_entreprises, output_file("participations_entreprises_detail.csv"))
write_csv(participations_par_projet, output_file("participations_entreprises_par_projet.csv"))
write_csv(participations_par_projet_role, output_file("participations_entreprises_par_projet_role.csv"))
write_csv(participations_par_role_departement, output_file("participations_entreprises_par_role_departement.csv"))
# ============================================================================
# 5. Export des figures PNG
# ============================================================================




# Tableaux de contingence : nombre de projets distincts par cellule.
save_correlation_plot(
  addmargins(khi2_systemes_essences$table, FUN = list(Total = sum)),
  output_file("correlation_systemes_constructifs_essences.png"),
  "Tableau de contingence — Parties d'ouvrage et essences",
  subtitle = khi2_systemes_essences$subtitle,
  x_axis_title = "Essence de bois", y_axis_title = "Parties d'ouvrage",
  is_corr = FALSE, number_digits = 0, strip_axis_prefix = FALSE,
  show_color_legend = FALSE, plain_numbers = TRUE,
  value_limits = c(0, max(1, sum(khi2_systemes_essences$table))),
  legend_title = "Nombre de projets",
  color_palette = colorRampPalette(c("#DCEAF5", "#689BC5"))(200)
)
save_correlation_plot(
  addmargins(t(khi2_types_batiments_systemes$table), FUN = list(Total = sum)),
  output_file("correlation_types_batiments_systemes_constructifs.png"),
  "Tableau de contingence — Types de bâtiment et parties d'ouvrage",
  subtitle = khi2_types_batiments_systemes$subtitle,
  x_axis_title = "Type de bâtiment", y_axis_title = "Parties d'ouvrage",
  is_corr = FALSE, number_digits = 0, strip_axis_prefix = FALSE,
  show_color_legend = FALSE, plain_numbers = TRUE,
  value_limits = c(0, max(1, sum(khi2_types_batiments_systemes$table))),
  legend_title = "Nombre de projets", text_scale = 1.3,
  color_palette = colorRampPalette(c("#DCEAF5", "#689BC5"))(200)
)
save_correlation_plot(
  addmargins(khi2_essences_provenances$table, FUN = list(Total = sum)),
  output_file("correlation_essences_provenances.png"),
  "Tableau de contingence — Essences et provenances du bois",
  subtitle = khi2_essences_provenances$subtitle,
  x_axis_title = "Provenance du bois", y_axis_title = "Essence de bois",
  is_corr = FALSE, number_digits = 0, strip_axis_prefix = FALSE,
  show_color_legend = FALSE, plain_numbers = TRUE,
  value_limits = c(0, max(1, sum(khi2_essences_provenances$table))),
  legend_title = "Nombre de projets", text_scale = 1.3,
  color_palette = colorRampPalette(c("#DCEAF5", "#689BC5"))(200)
)

save_correlation_plot(
  distance_group_matrix,
  output_file("distances_acteurs_entre_groupes.png"),
  "Distances entre groupes d'acteurs d'un même projet",
  x_axis_title = "Groupe d'acteurs",
  y_axis_title = "Groupe d'acteurs",
  sample_size_matrix = distance_group_n_matrix,
  strip_axis_prefix = FALSE,
  number_digits = 1,
  is_corr = FALSE,
  value_limits = distance_group_limits,
  legend_title = "Distance médiane (km)",
  color_palette = colorRampPalette(c("#F7FAFC", "#2B6CB0"))(200)
)

save_correlation_plot(
  correlation_proximite,
  output_file("correlation_proximite_acteurs_projets.png"),
  "Proximité des acteurs entre eux et par rapport au projet",
  x_axis_title = "Distance médiane entre chaque groupe d'acteurs et le projet",
  y_axis_title = "Distance médiane entre deux groupes d'acteurs du même projet",
  sample_size_matrix = correlation_proximite_n
)

save_correlation_plot(
  localite_color_matrix,
  output_file("localite_de_acteur.png"),
  "Localité des acteurs par groupe, par rapport au projet",
  cell_labels = localite_labels_matrix,
  show_color_legend = FALSE,
  is_corr = FALSE,
  value_limits = c(0, 100),
  color_palette = colorRampPalette(c("#F0F5FA", "#A6C8E2", "#245A81"))(200)
)
# ============================================================================
# 6. Export des pages HTML
# ============================================================================

if (interactive()) print(page)

saveWidget(
  p,
  output_file("sankey_aura_filiere_bois_provenance_projets.html"),
  selfcontained = TRUE
)

saveWidget(
  map_widget,
  output_file("carte_aura_filiere_bois_roles.html"),
  selfcontained = TRUE
)

saveWidget(
  map_widget_scierie_industriel,
  output_file("carte_aura_filiere_bois_scierie_industriel.html"),
  selfcontained = TRUE
)

saveWidget(
  map_widget_bois_pose,
  output_file("carte_aura_filiere_bois_charpentier_constructeur_menuisier.html"),
  selfcontained = TRUE
)

saveWidget(
  map_widget_scierie_industriel_charpentier,
  output_file("carte_aura_filiere_bois_scierie_industriel_charpentier.html"),
  selfcontained = TRUE
)

save_html(
  browsable(
    tagList(
      tags$div(
        style = "font-family:Arial, sans-serif; margin:0 0 8px 0;",
        tags$h2(
          "Carte des projets AURA — regroupement MarkerCluster",
          style = "margin:0 0 6px 0; font-size:22px; font-weight:600;"
        )
      ),
      heatmap_projects_widget,
      tags$div(
        style = "font-family:Arial, sans-serif; margin:26px 0 8px 0;",
        tags$h2(
          "Carte des acteurs des projets AURA — regroupement MarkerCluster",
          style = "margin:0 0 6px 0; font-size:22px; font-weight:600;"
        )
      ),
      heatmap_actors_widget
    )
  ),
  file = output_file("carte_aura_heatmap_projets_acteurs.html"),
  libdir = output_file("carte_aura_heatmap_projets_acteurs_assets")
)

# selfcontained = TRUE (pandoc) produit une page blanche pour ces deux
# widgets : les fichiers depassent 30 Mo une fois les marqueurs et les icones
# encodes en base64, ce que pandoc n'arrive pas toujours a assembler
# correctement. Le mode non autonome (fichiers externes), deja utilise pour
# la carte combinee ci-dessus, reste fiable a cette echelle.
save_html(
  browsable(heatmap_projects_widget),
  file = output_file("carte_aura_heatmap_nombre_projets.html"),
  libdir = output_file("carte_aura_heatmap_nombre_projets_files")
)

save_html(
  browsable(heatmap_actors_widget),
  file = output_file("carte_aura_heatmap_acteurs.html"),
  libdir = output_file("carte_aura_heatmap_acteurs_files")
)

save_html(
  page,
  file = output_file("sankey_aura_filiere_bois_provenance_projets_carte.html"),
  libdir = output_file("sankey_aura_filiere_bois_assets")
)
write_csv(khi2_systemes_essences$counts, output_file("comptages_projets_systemes_essences.csv"))
write_csv(khi2_types_batiments_systemes$counts, output_file("comptages_projets_types_batiments_systemes.csv"))
write_csv(khi2_essences_provenances$counts, output_file("comptages_projets_essences_provenances.csv"))
