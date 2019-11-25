###Functions for Climmob Reporting Analysis

##List of functions
##Updated 01 May 2019
##
##scale01() : simple scaling function
##
##byfac() : produces PlackettLuce estimates and errorbars to compare between different levels of factor
##
##favourability() : calculates total wins, losses and favourability scores
##
##favourability_plot() : plot of favourability scores
##
##win_plot() : plot of win %
##
##multcompPL() : does mean seperation from PlackettLuce object with CLD grouping
##
##plot.multcompPL() : plots errorbars, confidence intervals and CLD letters
##
##concordance() : determines level of agreement between list of traits with the overall trait
##
##contests() : produce pairwise contest matrix between all varieties
##
##anova.PL() : ANOVA table from LRT test of PL Model
##
##draw.emojis() : draws emojis for the feedback forms
##
##get_ranking() : part of the favourability function, slightly modified to include second best as well


scale01 <- function(x) (x-min(x))/(max(x)-min(x))

byfac<-function(model,split){
  
  split<-as.factor(split)
  out<-NULL
  for(i in 1:nlevels(split)){
    mod_t<-update(mod1,rankings=R[split==levels(split)[i],])
    
    tmp<-data.frame(var=rownames(qvcalc(mod_t)$qvframe),split=levels(split)[i],qvcalc(mod_t)$qvframe)
    tmp$estimate_adj<-tmp$estimate-mean(tmp$estimate)
    
    
    out<-rbind(out,tmp)
  }
  out
  
  ggplot(data=out,aes(y=estimate_adj,x=var,ymax=estimate_adj+qnorm(0.92)*quasiSE,
                      ymin=estimate_adj-qnorm(0.92)*quasiSE,col=split))+
    geom_errorbar(width=0.2,position = position_dodge(width=0.25))+
    geom_point(position = position_dodge(width=0.25))
}





win_plot<-function(x){
  p1<- ggplot(data=x,aes(y=wins,fill=wins,x=var))+
    geom_bar(stat="identity",col="black")+
    coord_flip()+
    scale_y_continuous(breaks=seq(0,1,by=0.1),labels=scales::percent)+
    scale_fill_gradient2(low="red",mid="white",high="forestgreen",limits=c(0,1),midpoint=0.5)
  
  return(p1)
}

multcompPL<-function(mod,terms=NULL,threshold=0.05,Letters=letters,adjust="none"){
  
  require(qvcalc)
  require(multcompView)
  
  #get estimates with quasi-SEs
  qv1<-qvcalc(mod)$qvframe
  
  #reduce frame to only selected terms if not all comparisons are desired
  if(is.null(terms)==FALSE){
    qv1<-subset(qv1,rownames(qv1)%in%terms)
    #give error if less than 2 terms can be identified
    if(nrow(qv1)<3){
      stop("Less than 2 terms selected")
    }
  }
  
  #set up matrices for all differences and pooled errors
  diffs<-mat.or.vec(nrow(qv1),nrow(qv1))
  ses<-mat.or.vec(nrow(qv1),nrow(qv1))
  
  for(i in 1:nrow(qv1)){
    for(j in 1:nrow(qv1)){
      
      #get differences and pooled ses
      diffs[i,j]<-qv1$estimate[i]-qv1$estimate[j]
      ses[i,j]<-sqrt(qv1$quasiVar[i]+qv1$quasiVar[j])
    }
  }
  
  #calculate z scores
  z<-diffs/ses
  #TO DO: What DF to use to use here? Is it just the resid DF?
  p<-2*(1-pt(abs(z),mod$df.residual))
  
  #adjust p-value if you want to adjust. make sure to only take each p once for adjustment
  p[upper.tri(p)]<-p.adjust(p[upper.tri(p)],method = adjust)
  
  #make sure lower triangular is mirror of upper
  p[lower.tri(p)] = t(p)[lower.tri(p)]
  
  #set rownames
  rownames(p)<-colnames(p)<-rownames(qv1)
  
  #re-order qv output to ensure letters are produced in a sensible order
  qv1$term<-reorder(factor(rownames(qv1)),qv1$estimate,mean)
  qv1<-qv1[order(qv1$estimate,decreasing = TRUE),]
  
  #get mean seperation letter groupings
  qv1$.group<-multcompLetters2(estimate ~ term, p, qv1,
                               compare="<",
                               threshold=threshold,
                               Letters=Letters,
                               reversed = FALSE)$`Letters`
  return(qv1)
  
}

#simple ggplot function to plot output from multcompPL with error bars
plot.multcompPL<-function(x,level=0.95,xlab="",ylab=""){
  require(ggplot2)
  p1<- ggplot(data=x,aes(y=estimate,x=term,label=.group,ymax=estimate+qnorm(1-(1-level)/2)*quasiSE,
                         ymin=estimate-qnorm(1-(1-level)/2)*quasiSE))+
    geom_point()+
    geom_errorbar(width=0.1)+
    coord_flip()+
    geom_text(vjust=1.2)+
    xlab(xlab)+ylab(ylab)
  return(p1)
}

concordance<-function(overall_best,overall_worst,bests,worsts,names){
  require(ggplot2)
  require(dplyr)
  require(formattable)
  require(scales)
  require(tidyr)
  
  
  b1<-bests
  colnames(b1)<-paste(names,"b",sep="")
  
  w1<-worsts
  colnames(w1)<-paste(names,"w",sep="")
  
  dt1<-data.frame(overall_b=overall_best,overall_w=overall_worst,b1,w1,stringsAsFactors = FALSE)
  
  for(i in 3:(2+ncol(bests))){
    dt1[,i]<-dt1[,i]==dt1[,1]
  }
  for(i in (3+ncol(bests)):ncol(dt1)){
    dt1[,i]<-dt1[,i]==dt1[,2]
  }
  
  
  Overall<-dt1[,3:(2+ncol(bests))]==TRUE & dt1[,(3+ncol(bests)):ncol(dt1)]==TRUE
  
  plot_data<-data.frame(agreement=c(colMeans(dt1[,-(1:2)],na.rm=T),colMeans(Overall,na.rm=T)),
                        type=rep(c("best","worst","overall"),each=length(names)),
                        trait=rep(names,3))
  

    
  plot_data$trait<-reorder(factor(plot_data$trait),ifelse(plot_data$type=="overall",plot_data$agreement,0),mean)
  
  out_data<-plot_data %>% mutate(agreement=percent(agreement,1)) %>% spread(type,agreement)
  
  
  plot_data$type<-factor(plot_data$type,levels=rev(c("worst","best","overall")),
                         labels=rev(c("% Agreement with\nOverall Worst",
                                      "% Agreement with\nOverall Best",
                                      "% Agreement with\nOverall Ranking")))
  
  
  p1<-ggplot(plot_data,aes(y=agreement,x=trait,alpha=agreement,fill=type))+
    geom_bar(stat="identity",position="dodge",col=alpha("gray50",1),show.legend = FALSE)+
    facet_wrap(~type)+coord_flip()+
    geom_hline(yintercept=0:1,size=1)+
    scale_fill_manual(values=c("purple","forestgreen","red"))+
    geom_text(aes(y=agreement/2,label=formattable::percent(agreement,1)),fontface=2,size=8,alpha=1)+
    scale_y_continuous(labels=scales::percent,breaks=seq(0,1,by=0.2),limits=c(0,1))+ylab("")+xlab("")+
    theme(axis.text.y = element_text(size=15,face=2),strip.text =element_text(size=15,face=2) )
  
  
  return(list(p1,out_data))
}


contests<-function(a,b,c,best,worst,format="ABC",type="all",player=NULL){
  
  nalist<-is.na(best)==F&is.na(worst)==F&is.na(a)==F&is.na(b)==F&is.na(c)==F
  
 bw<- data.frame(favourability(a,b,c,best,worst,
                format=format,out="raw"),variety.a=a[nalist],variety.b=b[nalist],variety.c=c[nalist],stringsAsFactors = FALSE)
  
 bw<-na.omit(bw)
  require(tidyr)
  require(dplyr)
  require(scales)
  require(ggplot2)
  
  if(type=="all"){
    vars<-sort(unique(c(bw$variety.a,bw$variety.b,bw$variety.c)))
    nvars<-length(vars)
    
    out<-NULL
    for(i in 1:nvars){
      tmp<-ifelse(bw$best1==vars[i],1,
                  ifelse(bw$worst1==vars[i],3,
                         ifelse(bw[,"variety.a"]==vars[i]|
                                  bw[,"variety.b"]==vars[i]|bw[,"variety.c"]==vars[i],2,NA)))
      out<-cbind(out,tmp)
    }
    
    colnames(out)<-vars
    
    pref<-matrix(NA,nvars,nvars)
    nmat<-matrix(NA,nvars,nvars)
    for(i in 1:nvars){
      for(j in 1:nvars){
        if(i!=j){
          n<-sum(is.na(out[,i])==F&is.na(out[,j])==F)
          pij<-sum(out[,i]>out[,j],na.rm=T)
          pji<-sum(out[,i]<out[,j],na.rm=T)
          pref[i,j]<-pij/n
          pref[j,i]<-pji/n
          nmat[i,j]<-nmat[j,i]<-n
        }
      }
    }
    
    rownames(pref)<-vars
    colnames(pref)<-vars
    
    pref2<-100*round(pref[(order(rowMeans(pref,na.rm=T))),(order(rowMeans(pref,na.rm=T)))],3)
    pref2
    pref2[lower.tri(pref2)]<-NA
    
 p1<-   pref2 %>%
      data.frame(check.names = FALSE) %>%
      mutate(var1=rownames(pref2)) %>%
      gather(key="var2",value="val",-var1) %>%
      mutate(txt=ifelse(val==50,"50:50",ifelse(val<50,paste("A","\n",100-val,"%",sep=""),
                                               paste("B","\n",val,"%",sep=""))),
             var1=factor(var1,levels=rev(rownames(pref2)))) %>%
      mutate(var2=factor(var2,levels=rownames(pref2))) %>%
      ggplot(aes(y=var1,x=var2,fill=100-val,label=txt))+
      geom_tile(col="gray50")+
      geom_text(size=3,fontface=2)+
      scale_x_discrete(position="top")+
      scale_fill_gradient2(limits=c(0,100),low = alpha("red",0.75),
                           high=alpha("forestgreen",0.75),midpoint = 50)+
      xlab("B")+ylab("A")+labs(fill="% Wins for\nDominant Variety")
    
   p2<- pref %>%
      data.frame(var1=rownames(pref),check.names = FALSE) %>%
    gather(-var1,key = "var2",value="value") %>%
      mutate(var1=factor(var1,levels=rev(rownames(pref2))),
             var2=factor(var2,levels=rownames(pref2))) %>%
        ggplot(aes(y=value,x=var1))+
          geom_bar(aes(fill=value),stat="identity",col="black")+
            facet_wrap(~var2,scales="free_y",ncol=3)+
              coord_flip()+
      scale_fill_gradient2(limits=c(0,1),low = alpha("red",0.75),
                           high=alpha("forestgreen",0.75),midpoint = 0.5,labels=percent,name="% Times Preferred")+
      scale_y_continuous(labels=percent,limits=c(0,1))
    
    return(list(p1,p2,pref,nmat))
  }
}

anova.PL<-function(model){
  if(class(model)!="PlackettLuce"){
    stop("Model type is not Plackett Luce")
  }
  LLs<-c(model$null.loglik,model$loglik)
  dfs<-c(model$df.null,model$df.residual)
  df_diff<-(-1)*diff(dfs)
  df_LL<-(-1)*diff(LLs)
  p=1-pchisq(-2*df_LL,df_diff)
  
  
  x<-data.frame(model=c("NULL",deparse(substitute(model))),
                "logLikelihood"=LLs,
                DF=dfs,"Statistic"=c(NA,-2*df_LL),
                "Pr(>Chisq)"=c(NA,p),check.names = FALSE,stringsAsFactors = FALSE)
  return(x)
}



map.f <- 
  function(
    data = data4,
    lon = "lon", 
    lat = "lat",
    remove_outliers = TRUE,
    number_of_clusters = 6,
    cluster_method = "complete",
    max_cluster_dist = 250,
    min_cluster_pert = 0.05, 
    padding = NULL,
    map_provider = "Esri.WorldImagery",
    minimap_position = "bottomright"
  ){
    
    ## Take input df and create a data frame with only lon & lat
    ## Making coding easier and removing unnecessary columns
    
    df <- 
      data.frame(
        lon = data[, lon],
        lat = data[, lat],
        stringsAsFactors = FALSE
      )
    
    ## Checks if there are any NAs and removes them
    
    if(
      any(is.na(df))
    ){
      warning("Data contains ", sum(is.na(df[,"lon"])|is.na(df[,"lat"])) ,
              " missing or invalid points which have been removed \n")
      df <- na.omit(df)
    }    
    
    
    ## Checks if lon & lat are  are numeric, converts if neccesary
    ## and supresses warnings. May introduce NAs.
    
    if(
      !(is.numeric(df$lon))|!(is.numeric(df$lat))
    ){
      df[, "lon"] <- suppressWarnings(as.numeric(df[, "lon"]))
      df[, "lat"] <- suppressWarnings(as.numeric(df[, "lat"]))
    }
    
    ## Ensure data lon & lat are each within [-180, 180] and 
    ## data frame does not include the point (0,0).
    ## If not, then points are converted to NAs
    
    
    if (
      any(
        df <= -180 | df >= 180 | (df$lon == 0 & df$lat == 0)
      )
    ){
      
      df$lon[which(!between(df$lon,-180, 180))] <- NA 
      df$lat[which(!between(df$lat,-180, 180))] <- NA
      df[which(df$lon == 0 & df$lat == 0),] <- NA
    }
    
    ## Checks if there are any NAs and removes them
    
    if(
      any(is.na(df))
    ){
      warning("Data contains ", sum(is.na(df[,"lon"])|is.na(df[,"lat"])) ,
              " missing or invalid points which have been removed \n")
      df <- na.omit(df)
    }    
    
    
    
    ## Outliers are defined as points that are part of cluster that are not too 
    ## small or are not too far away from other clusters.
    ## This section will calculate clusters, test if these clusters meet the size 
    ## and distance requirements and remove any that do not meet this criteria
    ## If the number_of_clusters == 0 or remove_outliers is FALSE then the code
    ## will not run.
    
    if(remove_outliers){
      
      ## Create clusters; 
      df <- 
        df %>% 
        ## Remove duplicate lon & lat temporarily
        distinct(lon, lat, .keep_all = FALSE) %>% 
        ## Create a distance matrix of all points,
        dist() %>% 
        ## Calculate clustering information of the points with hclust
        ## using the algo specified by cluster_methods
        hclust(method = "single") %>% 
        ## Return cluster group for each point, grouping into the number of clusters 
        ## specified by number_of_points 
        cutree(h = max_cluster_dist/110) %>%
        ## Add clusters group column to the distinct lon & lat points
        {
          bind_cols(
            distinct(df, lon, lat, .keep_all = FALSE), 
            cluster = .)
        } %>%  
        ## Rejoin to main dataset
        right_join(df, by = c("lon", "lat")) 
      
      # 
      # ## Create list of clusters to include
      # 
      #     tmp <- 
      #       df %>%
      # ## Calculate centre for each cluster using mean lon and mean lat
      #       group_by(cluster) %>% 
      #       summarise(mean(lon), mean(lat)) %>% 
      #       ungroup() %>% 
      # ## Calculate distance between each cluster centre as a distance matrix
      #       dist() %>% 
      # ## Crude conversion from lon & lat to km
      #       multiply_by(110) %>% 
      # ## Set all distances between the cluster and itself to NAs
      #       as.matrix() %>% 
      #       `diag<-`(NA) %>% 
      # ## Find the minimum distance to another cluster for each cluster
      #       apply(1, min, na.rm = TRUE) %>% 
      # ## Test if mimimum distance of each cluster to the nearest cluster is below the  
      # ## the maximum boundary set by max_cluster_dist
      #       t() %>% 
      #       is_less_than(max_cluster_dist) %>% 
      # ## Return the list of all clusters the are considered valid
      #       which()
      
      tmp <-
        df %>% 
        ## Calculate percentage of points in each cluster
        group_by(cluster) %>% 
        summarise(pert = n()) %>% 
        mutate(pert = pert / sum(pert))  %>%
        ## Keep those that are larger than the minimum cluster size
        filter(pert > min_cluster_pert) %>%
        ## Combine the list of cluster with enough points with the clusters that are not
        ## too far from each other
        use_series("cluster") 
      #    plot(df$lon, df$lat, col = df$cluster)
      
      
      ## If clusters are to be removed, then create a warning message saying how many
      ## points are considered outliers
      
      if(NROW(df[!(df$cluster %in% tmp), ]) != 0){
        warning(NROW(df[!(df$cluster %in% tmp), ]),
                " records are considered outliers and have been removed \n")
      }
      
      # Subset dataset to only those that are considered vaild clusters
      df <-
        df[df$cluster %in% tmp, ]
    }
    
    ## Find the maximum distance between my points
    
    lon_dif <- dist(df$lon)
    lat_dif <- dist(df$lat)
    max_dif <- 
      max(
        c(max(lon_dif),
          max(lat_dif)
        ))
    
    ## Create the map
    
    #  return(list(df = df, map = map))
    map <-
      ## Supress messages and create base layer 
      suppressMessages(
        df %>% 
          leaflet(options = leafletOptions(maxZoom = 17)) %>% 
          ## Set rectangular view of the final map using the min and max of lon & lat
          ## padding option does not work
          fitBounds(
            lng1 = min(df$lon), lat1 = min(df$lat), 
            lng2 = max(df$lon), lat2 = max(df$lat),
            options = list(padding = padding)
          ) %>%
          ## Define the base map texture using map_provider
          addProviderTiles(map_provider, options = providerTileOptions(maxNativeZoom=17))  %>% 
          ## Add clusters markers (calculated seperate to above process)
          #addAwesomeMarkers(icon = icons, clusterOptions = markerClusterOptions()) %>% 
          ## Add point markers
          addCircleMarkers(
            radius = 4, 
            #        fillColor = "midnight blue", 
            opacity = 0.5, 
            fillOpacity = 0.5,
            fillColor = "black",
            color = "white",
            clusterOptions = markerClusterOptions()
          ) %>%
          ## Add minimap to final map, position based on minimap_position
          addMiniMap(position = minimap_position)
      )
    
    map_interval <-
      10^round(log10(max_dif))/2
    
    map_interval <-
      ifelse(round(max_dif/map_interval) == 1, 10^round(log10(max_dif))/5, 10^round(log10(max_dif))/2)
    
    
    if(abs(map_interval)<0.01){
      lon_line <-
        data.frame(
          lon = c(median(df$lon)-0.03, median(df$lon)+0.03),
          lat = c(median(df$lat))
        )
      
      lat_line <-
        data.frame(
          lon = c(median(df$lon)),
          lat = c(median(df$lat)-0.03, median(df$lat)+0.03)
        )
      
      map <-
        map %>% 
        addPolylines(data = lon_line, lng = ~lon, lat = ~lat, color = "#000", opacity = 1, weight = 3) %>% 
        addPolylines(data = lat_line, lng = ~lon, lat = ~lat, color = "#000", opacity = 1, weight = 3) %>%
        addControl(html = paste(round(median(df$lon), digits = 5), round(median(df$lat), digits = 5), sep = ", "), position = "topleft")
    } else {
      
      xticks <-
        seq(
          floor(map$x$fitBounds[[2]]/map_interval)*map_interval,
          ceiling(map$x$fitBounds[[4]]/map_interval)*map_interval,
          by=map_interval
        )
      
      yticks <-
        seq(
          floor(map$x$fitBounds[[1]]/map_interval)*map_interval,
          ceiling(map$x$fitBounds[[3]]/map_interval)*map_interval,
          by = map_interval
        )
      
      map <- 
        map %>%
        addGraticule(interval = map_interval) %>% 
        addLabelOnlyMarkers(lng=xticks-map_interval/20,lat=max(df$lat,na.rm=T),label=as.character(xticks), 
                            labelOptions = labelOptions(noHide = T, direction = 'right', textOnly = T,style = list(
                              "color" = "white",
                              "font-style" = "bold",
                              "font-size" = "12px"
                            ))) %>%
        addLabelOnlyMarkers(lng=min(df$lon,na.rm=T),lat=yticks,label=as.character(yticks), 
                            labelOptions = labelOptions(noHide = T, direction = 'right', textOnly = T,style = list(
                              "color" = "white",
                              "font-style" = "bold",
                              "font-size" = "12px"
                            )))
    }
    
    map$x$options = list("zoomControl" = FALSE)
    
    return(map = map)
    
  }


node_terminal1<-
  function (mobobj, id = TRUE, worth = TRUE, names = TRUE, abbreviate = TRUE, 
            index = TRUE, ref = TRUE, col = "black", refcol = "lightgray", 
            bg = "white", cex = 0.5, pch = 19, xscale = NULL, yscale = NULL, 
            ylines = 1.5) 
  {
    node <- nodeids(mobobj, terminal = FALSE)
    cf <- psychotree:::apply_to_models(mobobj, node, FUN = function(z) if (worth) 
      worth(z)
      else coef(z, all = FALSE, ref = TRUE))
    
    
    cf <- do.call("rbind", cf)
    rownames(cf) <- node
    cf<-cf[,order(colSums(cf))]
    
    mod <- psychotree:::apply_to_models(mobobj, node = 1L, FUN = NULL, drop = TRUE)
    if (!worth) {
      if (is.character(ref) | is.numeric(ref)) {
        reflab <- ref
        ref <- TRUE
      }
      else {
        reflab <- mod$ref
      }
      if (is.character(reflab)) 
        reflab <- match(reflab, if (!is.null(mod$labels)) 
          mod$labels
          else colnames(cf))
      cf <- cf - cf[, reflab]
    }
    if (worth) {
      cf_ref <- 1/ncol(cf)
    }
    else {
      cf_ref <- 0
    }
    if (is.character(names)) {
      colnames(cf) <- names
      
      names <- TRUE
    }
    if (is.logical(abbreviate)) {
      nlab <- max(nchar(colnames(cf)))
      abbreviate <- if (abbreviate) 
        as.numeric(cut(nlab, c(-Inf, 1.5, 4.5, 7.5, Inf)))
      else nlab
    }
    colnames(cf) <- abbreviate(colnames(cf), abbreviate)
    if (index) {
      x <- 1:NCOL(cf)
      if (is.null(xscale)) 
        xscale <- range(x) + c(-0.1, 0.1) * diff(range(x))
    }
    else {
      x <- rep(0, length(cf))
      if (is.null(xscale)) 
        xscale <- c(-1, 1)
    }
    if (is.null(yscale)) 
      yscale <- range(cf) + c(-0.1, 0.1) * diff(range(cf))
    rval <- function(node) {
      idn <- id_node(node)
      cfi <- cf[idn, ]
      top_vp <- viewport(layout = grid.layout(nrow = 2, ncol = 3, 
                                              widths = unit(c(ylines, 1, 1), c("lines", "null", 
                                                                               "lines")), heights = unit(c(1, 1), c("lines", 
                                                                                                                    "null"))), width = unit(1, "npc"), 
                         height = unit(1, "npc") - unit(2, "lines"), 
                         name = paste("node_btplot", idn, sep = ""))
      pushViewport(top_vp)
      grid.rect(gp = gpar(fill = bg, col = 0))
      top <- viewport(layout.pos.col = 2, layout.pos.row = 1)
      pushViewport(top)
      mainlab <- paste(ifelse(id, paste("Node", idn, 
                                        "(n = "), ""), info_node(node)$nobs, 
                       ifelse(id, ")", ""), sep = "")
      grid.text(mainlab)
      popViewport()
      plot_vpi <- viewport(layout.pos.col = 2, layout.pos.row = 2, 
                           xscale = xscale, yscale = yscale, name = paste("node_btplot", 
                                                                          idn, "plot", sep = ""))
      pushViewport(plot_vpi)
      grid.lines(xscale, c(cf_ref, cf_ref), gp = gpar(col = refcol), 
                 default.units = "native")
      if (index) {
        grid.lines(x, cfi, gp = gpar(col = col, lty = 2), 
                   default.units = "native")
        grid.points(x, cfi, gp = gpar(col = col, cex = cex), 
                    pch = pch, default.units = "native")
        grid.xaxis(at = x,edits = gEdit(gPath="labels", rot=90,cex=0.4),  label = if (names) 
          names(cfi)
          else x)
      }
      else {
        if (names) 
          grid.text(names(cfi), x = x, y = cfi, default.units = "native")
        else grid.points(x, cfi, gp = gpar(col = col, cex = cex), 
                         pch = pch, default.units = "native")
      }
      grid.yaxis(at = c(ceiling(yscale[1] * 100)/100, floor(yscale[2] * 
                                                              100)/100))
      grid.rect(gp = gpar(fill = "transparent"))
      upViewport(2)
    }
    return(rval)
    
  }

draw.emojis <- function(x,y,type="happy",radius=0.3, color="grey", border="black", thickness=1.5){
  draw.circle(x,y,radius,nv=100,border=color,col=color,lty=1,density=NULL,angle=45,lwd=thickness/1.5)
  segments(x0=x+radius/5, x1=x+radius/5, y0=y+radius/2.5, y1=y+radius/5, lwd = thickness*1.5, col=border)
  segments(x0=x-radius/5, x1=x-radius/5, y0=y+radius/2.5, y1=y+radius/5, lwd = thickness*1.5, col=border)
  if(type=="happy") draw.arc(x,y,radius=radius/2, deg1=200, deg2=340, col=border, lwd=thickness/1.2)
  if(type=="sad") draw.arc(x,y-radius/1.5,radius=radius/2, deg1=20, deg2=160, col=border, lwd=thickness/1.2)
  if(type=="neutral") segments(x0=x-radius/4, x1=x+radius/4, y0=y-radius/3, y1=y-radius/3, lwd = thickness, col=border)
}
draw.emojis<-Vectorize(draw.emojis)

# favourability function modified to get the second best as well
get_ranking <- function(a,b,c,best,worst,format="ABC",reorder=TRUE){
  a<-as.character(a)
  b<-as.character(b)
  c<-as.character(c)
  best<-as.character(best)
  worst<-as.character(worst)
  
  if(format=="abc"){
    best<-toupper(best)
    worst<-toupper(worst)
  }
  
  middle <- str_replace_all(str_replace_all("ABC",best,""),worst,"")
  
  best1<-ifelse(best=="A",a,
                ifelse(best=="B",b,
                       ifelse(best=="C",c,NA)))
  worst1<-ifelse(worst=="A",a,
                 ifelse(worst=="B",b,
                        ifelse(worst=="C",c,NA)))
  middle1<-ifelse(middle=="A",a,
                  ifelse(middle=="B",b,
                         ifelse(middle=="C",c,NA)))
  return(data.frame(best1, middle1, worst1))
}



favourability<-function(a,b,c,best,worst,format="ABC",reorder=TRUE,out="summary"){
  nalist<-is.na(best)==F&is.na(worst)==F&is.na(a)==F&is.na(b)==F&is.na(c)==F
  a<-as.character(a)[nalist]
  b<-as.character(b)[nalist]
  c<-as.character(c)[nalist]
  best<-as.character(best)[nalist]
  worst<-as.character(worst)[nalist]



  if(format=="abc"){
    best<-toupper(best)
    worst<-toupper(worst)
  }

  best1<-ifelse(best=="A",a,
                ifelse(best=="B",b,
                       ifelse(best=="C",c,NA)))
  worst1<-ifelse(worst=="A",a,
                 ifelse(worst=="B",b,
                        ifelse(worst=="C",c,NA)))
  if(out=="summary"){
    vars<-sort(unique(c(a,b,c)))

    inrow<-NULL
    wins<-NULL
    losses<-NULL

    for(i in 1:length(vars)){
      inrow<-cbind(inrow,ifelse(a==vars[i]|b==vars[i]|c==vars[i],1,0))
      wins<-cbind(wins,ifelse(best1==vars[i],1,0))
      losses<-cbind(losses,ifelse(worst1==vars[i],1,0))
    }
    colnames(inrow)<-paste("n",1:(length(vars)),sep="")
    colnames(wins)<-paste("b",1:(length(vars)),sep="")
    colnames(losses)<-paste("w",1:(length(vars)),sep="")




    sumstats<-data.frame(var=vars,N=colSums(inrow),
                         best_per=100*colSums(wins)/colSums(inrow),
                         worst_per=100*colSums(losses)/colSums(inrow),
                         wins=((2*colSums(wins))+colSums(inrow-wins-losses))/(2*colSums(inrow)),stringsAsFactors = FALSE)

    sumstats$Fav_Score=sumstats$best_per-sumstats$worst_per

    if(reorder==TRUE){
      sumstats$var<-reorder(sumstats$var,sumstats$Fav_Score,mean)
      sumstats<-sumstats[order(sumstats$Fav_Score),]
    }
    return(sumstats)
  }
  else{
    return( data.frame(best1,worst1,stringsAsFactors = FALSE))
  }
}

favourability_plot<-function(x){
  p1<- ggplot(data=x,aes(y=Fav_Score,fill=Fav_Score,x=var))+
    geom_hline(yintercept = 0)+
    geom_bar(stat="identity",col="black")+
    coord_flip()+
    scale_y_continuous(breaks = seq(-100,100, by = 20))+
    scale_fill_gradient2(low = "red",
                         mid = "white",
                         high = "forestgreen",
                         limits = c(-100, 100))

  return(p1)
}