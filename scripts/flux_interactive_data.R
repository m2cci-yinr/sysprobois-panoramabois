# Full source rows for interactive maps; geographic location never changes the threshold.
load_interactive_specs <- function(root) {
 cache <- file.path(root,"data","cartes_png","_cache")
 specs <- jsonlite::fromJSON(file.path(cache,"map_data.json"),simplifyVector=FALSE)
 for (i in seq_along(specs)) for (j in seq_along(specs[[i]]$points)) for (k in c("icon_file","legend_file")) {
  p <- specs[[i]]$points[[j]][[k]]
  if (!is.null(p)) {
   p <- sub("^exports/cartes_png/", "data/cartes_png/", p)
   p <- sub("^cartes_png/", "data/cartes_png/", p)
   specs[[i]]$points[[j]][[k]] <- file.path(root,p)
  }
 }
 specs <- Filter(function(s) substr(s$id,1,2) %in% c("01","02","03","04"),specs)
 for(i in seq_along(specs)) {
  s <- specs[[i]]; id <- substr(s$id,1,2)
  if(id=="01") {
   d <- read.csv(file.path(root,"data","carte_aura_filiere_bois_flux_projets.csv"),colClasses="character",encoding="UTF-8")
   for(n in c("nb_participations","source_latitude","source_longitude","target_latitude","target_longitude")) d[[n]] <- as.numeric(d[[n]])
   names(d)[match(c("source_stage","target_stage","source_dept","target_dept","source_latitude","source_longitude","target_latitude","target_longitude"),names(d))] <- c("flow_stage_source","flow_stage_target","dept_source","dept_target","latitude_source","longitude_source","latitude_target","longitude_target")
   rk <- paste(d$segment,d$dept_source,d$dept_target,sep="|")
   d$route_rank <- match(rk,sort(unique(rk)));d$segment_rank <- match(d$segment,sort(unique(d$segment)))
   sm <- c("Provenance -> Scierie / Industriel"="Provenance-Scierie","Scierie / Industriel -> Charpentier / Constructeur / Menuisier"="Scierie-Charpentier","Charpentier / Constructeur / Menuisier -> Projet"="Charpentier-Projet")
   d$segment <- unname(sm[d$segment]);d <- d[!is.na(d$segment),]
   cols <- setNames(vapply(s$flows,function(f)f$color,""),vapply(s$flows,function(f)f$segment,""))
   d$line_color <- unname(cols[d$segment]);d$curve_side <- ifelse(d$route_rank%%2==0,-1,1);d$curve_bend <- .10+.015*(d$route_rank%%4)
   d$is_intradepartement <- d$flow_stage_source!="Provenance" & d$dept_source==d$dept_target
   d$loop_radius <- 2.5*(.055+.012*pmin(d$segment_rank,3));d$loop_start_angle <- (d$segment_rank-1)*pi/3
  } else d <- readRDS(file.path(cache,paste0("role_flows_full_",id,".rds")))$lines
  fs <- list();ps <- list()
  for(k in seq_len(nrow(d))) {
   r <- d[k,]; if(!is.finite(r$nb_participations)||r$nb_participations<1) next
   old <- Filter(function(f) f$segment==r$segment && f$count==r$nb_participations && abs(f$lon[[1]]-r$longitude_source)<1e-5 && abs(f$lat[[1]]-r$latitude_source)<1e-5 && abs(tail(unlist(f$lon),1)-r$longitude_target)<1e-5 && abs(tail(unlist(f$lat),1)-r$latitude_target)<1e-5,s$flows)
   if(length(old)) f <- old[[1]] else {
    if(r$is_intradepartement) {
     a<-seq(r$loop_start_angle,r$loop_start_angle+2*pi,length.out=72);lr<-r$loop_radius/max(cos(r$latitude_source*pi/180),.2)
     lon<-r$longitude_source-lr*cos(r$loop_start_angle)+lr*cos(a);lat<-r$latitude_source-r$loop_radius*sin(r$loop_start_angle)+r$loop_radius*sin(a)
    } else {
     t<-seq(0,1,length.out=36);dx<-r$longitude_target-r$longitude_source;dy<-r$latitude_target-r$latitude_source
     cx<-(r$longitude_source+r$longitude_target)/2-r$curve_side*r$curve_bend*dy;cy<-(r$latitude_source+r$latitude_target)/2+r$curve_side*r$curve_bend*dx
     lon<-(1-t)^2*r$longitude_source+2*(1-t)*t*cx+t^2*r$longitude_target;lat<-(1-t)^2*r$latitude_source+2*(1-t)*t*cy+t^2*r$latitude_target
    }
    f<-list(lon=as.list(lon),lat=as.list(lat),count=r$nb_participations,segment=r$segment,color=r$line_color)
   }
   # Rebuild loops in the map's Web Mercator plane: geographic circles become
   # distorted after projection, including the older cached geometries.
   if(isTRUE(r$is_intradepartement)) {
    radius <- 6378137*pi/180*r$loop_radius/max(cos(r$latitude_source*pi/180),.2)
    a <- seq(r$loop_start_angle,r$loop_start_angle+2*pi,length.out=181)
    x0 <- 6378137*r$longitude_source*pi/180
    y0 <- 6378137*log(tan(pi/4+r$latitude_source*pi/360))
    x <- x0+radius*(cos(a)-cos(a[1]))
    y <- y0+radius*(sin(a)-sin(a[1]))
    f$lon <- as.list(x/6378137*180/pi)
    f$lat <- as.list((2*atan(exp(y/6378137))-pi/2)*180/pi)
   }
   f$lon[[1]] <- r$longitude_source;f$lat[[1]] <- r$latitude_source
   f$lon[[length(f$lon)]] <- r$longitude_target;f$lat[[length(f$lat)]] <- r$latitude_target
   f$source_role<-r$flow_stage_source;f$target_role<-r$flow_stage_target;f$source_dept<-r$dept_source;f$target_dept<-r$dept_target
   fs[[length(fs)+1]]<-f
   for(e in list(list(r$flow_stage_source,r$longitude_source,r$latitude_source,r$dept_source),list(r$flow_stage_target,r$longitude_target,r$latitude_target,r$dept_target))) {
    tpl<-Filter(function(p)p$role==e[[1]],s$points)
    stopifnot(length(tpl)>0)
    same<-Filter(function(p)abs(p$lon-e[[2]])<1e-5&&abs(p$lat-e[[3]])<1e-5,tpl)
    p<-if(length(same))same[[1]] else tpl[[1]];p$lon<-e[[2]];p$lat<-e[[3]];p$dept<-e[[4]]
    ps[[paste(p$role,p$lon,p$lat,sep="|")]]<-p
   }
  }
  s$flows<-fs;s$points<-unname(ps);specs[[i]]<-s
 }
 aura<-specs[[3]]
 for(s in specs[1:2]){s$bbox<-aura$bbox;s$zoom_aura<-TRUE;s$title<-paste0(s$title," — zoom sur AURA");specs[[length(specs)+1]]<-s}
 specs
}
