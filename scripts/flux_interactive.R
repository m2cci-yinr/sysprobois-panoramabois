# Interactive flux maps share their data and geometry with the PNG exporter.
source(file.path(root,"scripts","flux_interactive_data.R"),encoding="UTF-8",local=TRUE)
flux_specs <- load_interactive_specs(root)
flux_key <- function(s) paste0(s$id,if(isTRUE(s$zoom_aura)) "_aura" else "_default")
role_label <- function(x) {
 x[x=="Projet"] <- "Localisation du projet"
 x[x=="Scierie / Industriel"] <- "Entreprises de transformation"
 x[x=="Charpentier / Constructeur / Menuisier"] <- "Entreprise de construction"
 x
}
role_family <- function(x) {
 ifelse(x %in% c("Scierie","Industriel","Scierie / Industriel"),"Entreprises de transformation",
 ifelse(x %in% c("Charpentier","Menuisier","Constructeur bois","Charpentier / Constructeur / Menuisier"),"Entreprise de construction",role_label(x)))
}
# One supply-chain order for controls, legends and role lists.
role_rank <- function(x) {
 ranks <- c("Provenance"=10,"Scierie / Industriel"=20,"Entreprises de transformation"=20,
 "Scierie"=21,"Industriel"=22,"Charpentier / Constructeur / Menuisier"=30,
 "Entreprise de construction"=30,"Charpentier"=31,"Menuisier"=32,"Constructeur bois"=33,
 "Projet"=40,"Localisation du projet"=40)
 r<-unname(ranks[x]);r[is.na(r)]<-50;r
}
ordered_roles <- function(x) {x<-unique(x);x[order(role_rank(x),x)]}
flux_role_choices <- function(key,pts=flux_spec(key)$points) {
 roles<-ordered_roles(vapply(pts,function(p)p$role,""))
 labels<-lapply(roles,function(r){p<-Filter(function(p)p$role==r,pts)[[1]]
 htmltools::tags$span(class="flux-role-label",htmltools::tags$img(src=flux_uri(p$legend_file),alt=""),role_label(r))})
 list(values=roles,labels=labels)
}
interactive_flux_choices <- setNames(vapply(flux_specs,flux_key,""),c(
 "Ensemble de la filière · France","Provenance → entreprises de transformation · France",
 "Entreprise de construction → localisation du projet · AURA",
 "Entreprises de transformation → charpentiers · AURA",
 "Ensemble de la filière · AURA","Provenance → entreprises de transformation · AURA"))
interactive_flux_choices <- interactive_flux_choices[c(5,6,4,3)]
flux_spec <- function(key) flux_specs[[match(key,vapply(flux_specs,flux_key,""))]]
flux_pairs <- function(key) unique(do.call(rbind,lapply(flux_spec(key)$flows,function(f)data.frame(source=f$source_role,target=f$target_role))))
select_flux <- function(key,threshold=5,source="Tous",target="Tous",roles=NULL) {
 stopifnot(threshold %in% c(2,5))
 Filter(function(f) f$count>=threshold && (source=="Tous"||f$source_role==source) && (target=="Tous"||f$target_role==target) && (is.null(roles)||(f$source_role %in% roles && f$target_role %in% roles)),flux_spec(key)$flows)
}
# Fixed classes shared with the PNG exports.
flux_classes <- function(flows,threshold=5,n=4L) {
 threshold<-as.integer(threshold)
 if(threshold==5L) data.frame(id=as.character(1:3),lo=c(5,10,25),hi=c(9,24,Inf),
  label=c("5–9","10–24","25 et plus"),weight=c(4.5,7,10.4))
 else data.frame(id=as.character(1:4),lo=c(2,5,10,25),hi=c(4,9,24,Inf),
  label=c("1–4","5–9","10–24","25 et plus"),weight=c(2.5,4.5,7,10.4))
}
flux_depts <- sf::st_read(file.path(root,"data","departements-50m.geojson"),quiet=TRUE)
flux_regions <- sf::st_read(file.path(root,"data","regions-50m.geojson"),quiet=TRUE)
flux_depts <- sf::st_transform(flux_depts,4326)
flux_regions <- sf::st_transform(flux_regions,4326)
dept_name <- function(x) {
 code<-as.character(x);numeric_code<-grepl("^[0-9]{1,2}$",code)
 code[numeric_code]<-sprintf("%02d",as.integer(code[numeric_code]))
 result<-as.character(flux_depts$nom[match(code,as.character(flux_depts$code))])
 result[is.na(result)]<-code[is.na(result)]
 result[result=="DE"]<-"Allemagne"
 result[result=="Auvergne-Rhone-Alpes"]<-"Auvergne-Rhône-Alpes"
 result
}
flux_escape <- function(x) as.character(htmltools::htmlEscape(x))
flux_uri <- function(f) base64enc::dataURI(file=f,mime="image/png")
build_flux_map <- function(key,threshold=5,source="Tous",target="Tous",classes=NULL,roles=NULL) {
 s <- flux_spec(key)
 all_flows <- select_flux(key,threshold,source,target,roles=roles)
 bins <- flux_classes(all_flows,threshold)
 class_index <- function(n) findInterval(n,bins$lo)
 weight_for <- function(n) bins$weight[class_index(n)]
 flows <- if(is.null(classes)) all_flows else Filter(function(f) bins$id[class_index(f$count)] %in% classes,all_flows)
 flows <- flows[order(vapply(flows,function(f)f$count,0))]
 # Only endpoints of the links shown at this threshold belong on the map.
 point_for <- function(role,lon,lat) {
  hits <- which(vapply(s$points,function(p)
   identical(p$role,role) && abs(p$lon-lon)<1e-4 && abs(p$lat-lat)<1e-4,TRUE))
  if(length(hits)) hits[[1]] else NA_integer_
 }
 flow_endpoints <- lapply(flows,function(f)list(
  source=point_for(f$source_role,unlist(f$lon)[1],unlist(f$lat)[1]),
  target=point_for(f$target_role,tail(unlist(f$lon),1),tail(unlist(f$lat),1))))
 point_ids <- unique(as.integer(unlist(flow_endpoints)))
 point_ids <- point_ids[!is.na(point_ids)]
 visible_points <- s$points[point_ids]
 segment_label <- function(f) paste(role_label(f$source_role),"→",role_label(f$target_role))
 france <- substr(s$id,1,2) %in% c("01","02")
 # Carte 03 : icônes réduites et grandes flèches, pointe arrêtée au bord des icônes.
 big_arrows <- substr(s$id,1,2)=="03"
 sz <- if(france) 46 else if(big_arrows) 54 else 76
 bounds <- if(france) c(-5.3,41.95,10.8,51.4) else {
  b <- unlist(s$bbox)
  xy <- sf::st_coordinates(sf::st_transform(sf::st_sfc(sf::st_point(b[1:2]),sf::st_point(b[3:4]),crs=3857),4326))
  unname(as.numeric(c(xy[1,1:2],xy[2,1:2])))  # unname() : sinon Leaflet reçoit {"X":..} et la carte reste vide
 }
 m <- leaflet::leaflet(options=leaflet::leafletOptions(preferCanvas=FALSE,minZoom=4,maxZoom=14))
 m <- leaflet::addTiles(m,urlTemplate="https://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}",attribution="Tiles © Esri — Esri, DeLorme, NAVTEQ",options=leaflet::tileOptions(maxNativeZoom=16))
 m <- leaflet::addPolygons(m,data=flux_depts,fill=FALSE,color="#666666",weight=1,opacity=.75,dashArray="4,4",group="Départements",label=~nom)
 m <- leaflet::addPolygons(m,data=flux_regions,fill=FALSE,color="#2F2F2F",weight=2,opacity=.9,dashArray="6,4",group="Régions",label=~nom)
 for(i in seq_along(flows)) {
  f <- flows[[i]]
  tip <- paste0(flux_escape(segment_label(f))," · ",flux_escape(dept_name(f$source_dept))," → ",flux_escape(dept_name(f$target_dept))," : ",f$count," participation",if(f$count>1)"s" else "")
  m <- leaflet::addPolylines(m,lng=unlist(f$lon),lat=unlist(f$lat),color=f$color,weight=weight_for(f$count),opacity=.8,
   label=tip,popup=tip,group=f$segment,layerId=paste0("flow-",i))
 }
 for(j in seq_along(point_ids)) {
  p <- visible_points[[j]]
  ic <- leaflet::makeIcon(iconUrl=flux_uri(p$icon_file),iconWidth=sz,iconHeight=sz,iconAnchorX=sz/2,iconAnchorY=sz/2)
  m <- leaflet::addMarkers(m,lng=p$lon,lat=p$lat,icon=ic,label=paste(role_label(p$role),dept_name(p$dept),sep=" · "),popup=flux_escape(paste(role_label(p$role),dept_name(p$dept),sep=" · ")),group=p$role,layerId=paste0("point-",point_ids[[j]]),options=leaflet::markerOptions(riseOnHover=TRUE))
 }
 controls <- flux_role_choices(key,visible_points)
 m <- leaflet::addLayersControl(m,
   overlayGroups=c("Départements","Régions",controls$values),
   options=leaflet::layersControlOptions(collapsed=FALSE,position="topright"))
 roles <- visible_points[!duplicated(vapply(visible_points,function(p)p$role,""))]
 roles <- roles[order(vapply(roles,function(p)role_rank(p$role),0))]
 segments <- flows[!duplicated(vapply(flows,function(f)f$segment,""))]
 segments <- segments[order(vapply(segments,function(f)role_rank(f$source_role)*100+role_rank(f$target_role),0))]
 role_html <- ""
 for(family in unique(vapply(roles,function(p)role_family(p$role),""))) {
  members <- Filter(function(p)role_family(p$role)==family,roles)
  if(any(vapply(members,function(p)role_label(p$role)!=family,TRUE))) role_html<-paste0(role_html,"<h5>",flux_escape(family),"</h5>")
  role_html<-paste0(role_html,paste(vapply(members,function(p)paste0('<div><img style="width:16px;height:21px;vertical-align:middle;margin-right:6px" src="',flux_uri(p$legend_file),'">',flux_escape(role_label(p$role)),'</div>'),""),collapse=""))
 }
 legend <- paste0('<details open class="flux-legend"><summary>Légende</summary><div class="flux-legend-body">',
 '<section><h4>Nombre de flux — classes d’effectifs</h4><p class="legend-note">Classes fixes · liens ≥ ',threshold,' partout</p>',
 paste(vapply(seq_len(nrow(bins)),function(i)paste0('<div class="legend-row"><span class="legend-line" style="border-top-width:',bins$weight[i],'px"></span><span>',bins$label[i],' participations</span></div>'),""),collapse=""),
 '</section><section><h4>Sens des flux</h4><p class="legend-note">Flèche : source → destination</p>',
 paste(vapply(segments,function(f)paste0('<div class="legend-row"><svg width="40" height="16" viewBox="0 0 40 16" style="flex-shrink:0" aria-hidden="true"><line x1="1" y1="8" x2="29" y2="8" stroke="',f$color,'" stroke-width="2"/><path d="M28 3 L39 8 L28 13 Z" fill="',f$color,'"/></svg><span>',flux_escape(segment_label(f)),'</span></div>'),""),collapse=""),
 '</section><section><h4>Maillon de la filière</h4>',role_html,'</section>',
 '<footer><div class="legend-boundaries"><span><i class="legend-region"></i> Région</span><span><i class="legend-dept"></i> Département</span></div>',
 '<p class="legend-note">Sources : fichiers locaux projets/acteurs ; contours administratifs.</p><p class="legend-note">',length(flows),' liaison(s) affichée(s)</p></footer>',
 if(!length(flows))'<p>Aucune liaison pour cette sélection.</p>' else '', '</div></details>')
 m <- leaflet::addControl(m,html=legend,position="bottomright",className="flux-legend-control")
 m <- leaflet::addControl(m,html="<div style='background:white;padding:8px;text-align:center'>N<br>↑</div>",position="topleft")
 m <- leaflet::addScaleBar(m,position="bottomleft",options=leaflet::scaleBarOptions(imperial=FALSE))

 m <- leaflet::fitBounds(m,bounds[1],bounds[2],bounds[3],bounds[4])
 # Arrow triangles are calculated in screen pixels and follow zoom, pan and layer visibility.
 htmlwidgets::onRender(m,"function(el,x,data){
 var map=this, heads=[];
 var control=el.querySelector('.leaflet-control-layers');
 if(control){L.DomEvent.disableScrollPropagation(control);}
 var roleNames=data.roles.map(function(r){return r.name;});
 var active=new Set(roleNames);
 var overlayLabels=el.querySelectorAll('.leaflet-control-layers-overlays label');
 overlayLabels.forEach(function(label){
  var input=label.querySelector('input');
  var name=input && input.nextElementSibling ? input.nextElementSibling.textContent.trim() : '';
  var match=data.roles.find(function(r){return r.name===name;});
  if(match){
   var span=input.nextElementSibling;
   span.textContent=' '+match.label;
   var icon=document.createElement('img');icon.src=match.icon;icon.alt='';
   icon.style.cssText='width:21px;height:23px;object-fit:contain;vertical-align:middle;margin:0 5px';
   span.insertBefore(icon,span.firstChild);
  }
 });
 var legend=el.querySelector('.flux-legend-control');if(legend){L.DomEvent.disableClickPropagation(legend);L.DomEvent.disableScrollPropagation(legend);}
 var style=document.createElement('style');style.textContent='#'+el.id+' .leaflet-marker-icon{filter:drop-shadow(1px 0 white) drop-shadow(-1px 0 white) drop-shadow(0 1px white) drop-shadow(0 -1px white)} #'+el.id+' .flux-legend .legend-row{margin:8px 0;line-height:1.4}';el.appendChild(style);
 data.flows.forEach(function(f,i){
 var line=map.layerManager.getLayer('shape','flow-'+(i+1));
 if(!line)return;
 var pts=line.getLatLngs(), end=pts[pts.length-1];
 var head=L.polygon([end,end,end],{color:f.color,fillColor:f.color,fillOpacity:.95,opacity:.95,weight:1,interactive:false});
 heads.push({line:line,head:head,pts:pts,len:f.len,w:f.w,gap:f.gap,source:f.source,target:f.target,sourcePoint:f.sourcePoint,targetPoint:f.targetPoint});
 });
 var points=data.points.map(function(p){return {id:p.id,marker:map.layerManager.getLayer('marker','point-'+p.id)};});
 var iconZoom=map.getZoom();
 function resizeIcons(){
  var scale=Math.max(.65,Math.min(2.4,Math.pow(1.18,map.getZoom()-iconZoom)));
  var size=Math.round(data.iconSize*scale);
  points.forEach(function(p){
   if(!p.marker||!p.marker._icon)return;
   var icon=p.marker._icon;
   icon.style.width=size+'px';icon.style.height=size+'px';
   icon.style.marginLeft=-size/2+'px';icon.style.marginTop=-size/2+'px';
  });
 }
 function syncRoles(){var connected=new Set();heads.forEach(function(h){
  var show=active.has(h.source)&&active.has(h.target);
  if(show&&!map.hasLayer(h.line))map.addLayer(h.line);
  if(!show&&map.hasLayer(h.line))map.removeLayer(h.line);
  if(show){connected.add(h.sourcePoint);connected.add(h.targetPoint);}
 });
 points.forEach(function(p){if(!p.marker)return;var show=connected.has(p.id);
  p.marker.setOpacity(show?1:0);
  if(p.marker._icon)p.marker._icon.style.pointerEvents=show?'auto':'none';
 });update();}
 map.on('overlayadd overlayremove',function(e){
  if(roleNames.indexOf(e.name)<0)return;
  if(e.type==='overlayadd')active.add(e.name);else active.delete(e.name);
  syncRoles();
 });
 function update(){resizeIcons();heads.forEach(function(h){
 if(!map.hasLayer(h.line)){if(map.hasLayer(h.head))map.removeLayer(h.head);return;}
 var pts=h.pts, e=map.latLngToLayerPoint(pts[pts.length-1]), p=map.latLngToLayerPoint(pts[pts.length-2]);
 var zoomScale=Math.max(.65,Math.min(2.4,Math.pow(1.18,map.getZoom()-iconZoom)));
 var arrowGap=h.gap*zoomScale;
 if(arrowGap>0){ // leave the connector intact; move only its arrowhead
  var end=e, j=pts.length-2, q;
  var travelled=0,prev=end;
  for(;j>=0;j--){q=map.latLngToLayerPoint(pts[j]);var step=q.distanceTo(prev);
   if(travelled+step>=arrowGap){var fraction=(arrowGap-travelled)/(step||1);e=L.point(prev.x+(q.x-prev.x)*fraction,prev.y+(q.y-prev.y)*fraction);p=q;break;}
   travelled+=step;prev=q;}
 }
 var dx=e.x-p.x,dy=e.y-p.y,n=Math.sqrt(dx*dx+dy*dy)||1,ux=dx/n,uy=dy/n,len=h.len,w=h.w;
 h.head.setLatLngs([map.layerPointToLatLng(e),map.layerPointToLatLng(L.point(e.x-len*ux-w*uy,e.y-len*uy+w*ux)),map.layerPointToLatLng(L.point(e.x-len*ux+w*uy,e.y-len*uy-w*ux))]);
 if(!map.hasLayer(h.head))h.head.addTo(map);
 });}
 map.on('zoomend moveend overlayadd overlayremove',update);syncRoles();
 }",data=list(
  roles=lapply(controls$values,function(r){p<-Filter(function(p)p$role==r,flux_spec(key)$points)[[1]]
   list(name=r,label=role_label(r),icon=flux_uri(p$legend_file))}),
  points=lapply(point_ids,function(i)list(id=i)),iconSize=sz,
  flows=lapply(seq_along(flows),function(i){f<-flows[[i]];w<-weight_for(f$count)
   shared<-list(color=f$color,source=f$source_role,target=f$target_role,
    sourcePoint=flow_endpoints[[i]]$source,targetPoint=flow_endpoints[[i]]$target)
   if(big_arrows) c(shared,list(len=min(10+1.8*w,22),w=min(5+.9*w,11),gap=sz*.46))
   else c(shared,list(len=15,w=7,gap=sz*.46))})))
}

