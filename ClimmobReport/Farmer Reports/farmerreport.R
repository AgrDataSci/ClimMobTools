
# if no info is provided at the input stage, the back of the sheet will just show the list of options and the farmer can add info if they wish
info_table <- data.frame(options=sort(fav2$variety), info="")

# if info/expert advice about options are provided at the input stage, these will be used instead of the list of items
if(length(info.table.info)>0){
  info_table <- data.frame(options=info.table.items, info=info.table.info)
}


# additional potential parameters
trait2show<-c() #only shows overall ranking at this stage


###############################################
# create table

# make overall table
df_feedback_overall <- cbind(df, get_ranking(a=df[,vars[1]],
                                             b=df[,vars[2]],
                                             c=df[,vars[3]],
                                             best=df[,overall[1]],
                                             worst=df[,overall[2]]
))


df_feedback <- list(overall=df_feedback_overall)

# make other trait table - not used for now
for(trait_i in trait2show){
  pos_trait <- which(trait_names==trait_i)*2-1
  df_feedback_trait_i <- cbind(df, get_ranking(a=df[,vars[1]],
                                                  b=df[,vars[2]],
                                                  c=df[,vars[3]],
                                                  best=df[,pos_trait],
                                                  worst=df[,pos_trait+1]
  ))
  
  df_feedback[[trait_i]] <- df_feedback_trait_i
}

trait2show <-c("overall", trait2show)


###############################################
# sorting various information by % Top Ranked (based on data rather than model for now) - Overall only

# variety ordered by % Top Ranked
global_ranking <- fav2 %>% arrange(desc(`% Top Ranked`)) %>% pull(variety) %>% as.character()
# ordered % Top Ranked 
global_ranking_score <- fav2 %>% arrange(desc(`% Top Ranked`)) %>% pull(`% Top Ranked`)
# nb of times variety was Top Ranked ordered by % Top Ranked
global_top <- fav2 %>% mutate(N_top=as.numeric(`% Top Ranked`*N))%>% arrange(desc(`% Top Ranked`)) %>% pull(N_top)
# nb of farmers who ranked the variety, ordered by % Top Ranked
global_N <- fav2 %>% arrange(desc(`% Top Ranked`)) %>% pull(N)



###############################################
# define colours, line widths, styles etc. - you can safely play with all of it

grey_col <- colors()[230]
grey_col1 <- colors()[235]
grey_col2 <- colors()[240]
grey_col3 <- colors()[245]
grey_dark <- colors()[190]
grey_dark2 <- colors()[205]
grey_dark2 <- grey_dark
text_colors<-c(red="#B2182B", grey=colors()[200], blue="#2166AC")
emoji_colors<-c(red="#e7b9bf", grey=grey_col2, blue="#bcd1e6")
text_colors<-c(red=colors()[205], grey=colors()[195], blue="#2166AC")
emoji_colors<-c(red=grey_col2, grey=grey_col2, blue="#bcd1e6")
text_colors<-c(red=colors()[195], grey=grey_dark, blue="black")
emoji_colors<-c(red=grey_col2, grey=grey_col1, blue=grey_col)
emoji_colors<-c(red=grey_col, grey=grey_col, blue=grey_col)
textsize_1=0.6
textsize_2=1.1
textsize_3=1.3
arrowsize=3
arrowlength=0.07
boxwidth=1
emojiradius=0.4
color4you=T
transparency <-1
smiley_thickness <-1


###############################################
# some position parameters to play with the style of the form

#vertical parameters
n_toplines <- 7 #How many lines in the top box
yspace_top <- 0.6 #line height in the top box
yspace_box<-3 #space between top box and title
yspace_title<-4 #space between title and ranking info
yspace_header<-1.5 #space between header and ranking info (for the item sheet)
yspace<-1 #line height in the ranking info
htitle<-3.5 #height of the title box

n_items <- length(global_ranking) #number of varieties/items

#modify parameters to deal with high number of items
if(n_items>23){
  yspace<-.5
  textsize_2=.8
  emojiradius=0.3
  if(n_items>43){
    yspace<-.3
    textsize_2=.5
    emojiradius=0.2
    if(n_items>73){
      yspace<-.15
      textsize_2=.27
      textsize_1=.4
    }
  }
}


#horonzital parameters
xstart<-0
xend<-25
xtop<-0.5
#xtitle<-c(11, 11.2)
xtitle<-c(12.5, 12.5)
xsmileys<-4.8
xarrowVtext<-2.2
xarrowV<-5.5
xranking<-7
xarrowH<- c(15, 17)
xsmiley<-18
xranking2<-18.8
charperunit <- (xend-xstart)/(70)
xinfo<-10 #for item sheet


###############################################
# function to calculate the position of the items/varieties in relation to the title

make_y_bloc <- function(nb_items){
  return(c(title=0,header=-yspace_title+yspace_header, -yspace_title-(0:(nb_items-1))*yspace))
}

###############################################
# preparing the design of the form

#vertical positioning
ytop <- -(1:n_toplines)*yspace_top #positions of the lines in the box
ytopbox<-rev(range(ytop))+c(yspace_top,-yspace_top) #position of the box top and bottom 
y <- ytopbox[2] - yspace_box + make_y_bloc(n_items) #position of the items/varieties in the ranking
y2<- - yspace_box + make_y_bloc(nrow(info_table)) #position of the items/varieties in the info list for items sheet

#nb of characters of each item - used to calculate the pproximate width of the ranker's items arrows
global_width <- nchar(as.character(global_ranking))

#colors of the items/varieties
global_ranking_colors <- rgb(colorRamp(text_colors)(scale01(global_ranking_score)), max=255)


###############################################
# create a black arrow, saved as external file

png("Farmer Reports/png Files/mask.png")
ytmp1<-max(last(y), -35)
ytmp2<-y[3]
grid.polygon(c(-.3, .3, .3, .5, 0, -.5, -.3),
             c(ytmp1, ytmp1, ytmp2-yspace/2, ytmp2-yspace/2*1.2, ytmp2, ytmp2-yspace/2*1.2, ytmp2-yspace/2), gp=gpar(fill="black"),
             def="native",
             vp=viewport(xs=c(-.5, .5), ys=c(ytmp1, ytmp2)))
dev.off()

###############################################
# read back in the arrow as colour matrix

m <- readPNG("Farmer Reports/png Files/mask.png", native=FALSE)
mask <- matrix(rgb(m[,,1],m[,,2],m[,,3]),
               nrow=nrow(m))
rmat <- matrix(grey(seq(0,1,length=nrow(m))),
               nrow=nrow(m), ncol=ncol(m))
rmat[mask == "#FFFFFF"] <- NA






###############################################
# make the list of items/varieties with info png file

png("Farmer Reports/png Files/items.png", width= 8.27, height= 11.7, units="in", res=300)
par(mai=c(0,0,0,0), omi=c(.8,.5,.8,.5), lheight=.9)
plot.new()
plot.window(ylim=c(-35,0), xlim=c(xstart, xend))

#title
rect(xleft=xstart,ybottom=y2["title"]-htitle*1/3, xright = xend, ytop = y2["title"]+htitle/3, col=grey_dark, border=NA)#col=grey_col2
text(xtitle[2], y2["title"], paste("Information about the", options), font=2, cex=textsize_3, col="white")

#header
text(xstart,y2["header"],options, adj=c(0,NA), cex=textsize_3,  font=2)
text(xinfo,y2["header"],info.table.typeinfo, adj=c(0,NA), cex=textsize_3,  font=2)

#List items/varieties 
text(xstart,y2[-(1:2)],info_table$options, adj=c(0,NA), cex=textsize_1)
#Items/varieties info/advice
text(xinfo,y2[-(1:2)],info_table$info, adj=c(0,NA), cex=textsize_1)

dev.off()




###############################################
# make all the personalized results png files, by looping over all the ranker.ids


for(ranker_id in ranker.ids){
  
  #name of ranker
  name <- df$package_farmername[df$id==ranker_id]

  
  ###############################################
  # varieties ranked by the ranker, ordered - Overall only for now
  
  your_ranking <- df_feedback[["overall"]] %>%
    filter(id==ranker_id) %>%
    select(best1, middle1, worst1)%>%
    unlist()%>%
    as.character()
  
  
  ###############################################
  # ranker parameters
  
  y_yours <- y[-(1:2)][match(your_ranking,global_ranking)] #position of the ranker's items
  width_yours <- global_width[match(your_ranking,global_ranking)] #approximate width of the ranker's items - for the sizes of the arrows
  
  
  ###############################################
  # make the farmer result png file
  
  farmerPath <- paste0("Farmer Reports/png Files/",ranker_id,".png")
  
  png(farmerPath, width= 8.27, height= 11.7, units="in", res=300)
  par(mai=c(0,0,0,0), omi=c(.8,.5,.8,.5))
  plot.new()
  plot.window(ylim=c(-35,0), xlim=c(xstart, xend))
  
  #top_box
  rect(xleft=xstart,ybottom=ytopbox[2], xright = xend, ytop = ytopbox[1], col=NA, border = grey_col, lty = 2, lwd=boxwidth)
  text(x=xtop,y=ytop[1], paste("Certification sheet of:",name), cex=textsize_1, adj=c(0, NA))
  text(x=xtop,y=ytop[3], paste(nrow(df), rankers, "participated in this experiment."), cex=textsize_1, adj=c(0, NA))
  text(x=xtop,y=ytop[4], paste("Each of these",rankers, "ranked three", options, "out of all the", options, "of the experiment (information about the",options,"are given at the back)"),cex=textsize_1, adj=c(0, NA))
  text(x=xtop,y=ytop[5], paste0("You ranked these three ",options,": ", paste(sort(as.character(your_ranking)), collapse=", ")), cex=textsize_1, adj=c(0, NA))
  text(x=xtop,y=ytop[7], "Thank you for your participation", font=2, cex=textsize_1, adj=c(0, NA))
  
  #title
  rect(xleft=xstart,ybottom=y[1]-htitle*1/2, xright = xend, ytop = y[1]+htitle/2, col=grey_dark, border=NA)#col=grey_col2
  text(xtitle[2], y["title"]+textsize_3*.8, paste(nrow(df_feedback_overall), rankers, "(including you) were asked to rank the three",options, "they got, to answer the question"), adj=c(0.5,NA), font=1, cex=textsize_1, col="white")
  text(xtitle[2], y["title"], metadat$desc[metadat$type=="overall"][1], adj=c(0.5,NA), font=2, cex=textsize_3, col="white")
  text(xtitle[2], y["title"]-textsize_3*.8, "Adding up all the responses resulted in the following ranking:", adj=c(0.5,NA), font=1, cex=textsize_1, col="white")
  
  #global ranking left text
  text(xarrowVtext,y[3]+.6, "most often favourite", adj=c(.5,NA), cex=textsize_1*1.2, font=2)
  text(xarrowVtext,y[3], paste(first(global_N), rankers, "ranked this", option), adj=c(.5,NA), cex=textsize_1*.8)
  text(xarrowVtext,y[3]-.4, paste(first(global_top),rankers,"ranked it as the top one"), adj=c(.5,NA), cex=textsize_1*.8)
  text(xarrowVtext,last(y)+.6, "least often favourite", adj=c(.5,NA), cex=textsize_1*1.2, col=grey_dark2, font=2)
  text(xarrowVtext,last(y), paste(last(global_N), rankers, "ranked this", option), adj=c(.5,NA), cex=textsize_1*.8, col=grey_dark2)
  text(xarrowVtext,last(y)-.4, paste(last(global_top),rankers,"ranked it as the top one"), adj=c(.5,NA), cex=textsize_1*.8, col=grey_dark2)
  
  #global ranking arrow
  rasterImage(rmat, xarrowV+.5, last(y)+1, xarrowV-.5, y[3]-1)
  draw.emojis(xsmileys+c(0.3,.8,1,1.5),y[3]+c(-.1,.4,-.3,.2), radius=emojiradius*0.8, type="happy",border=grey_dark, thickness=smiley_thickness, col=emoji_colors["blue"])
  draw.emojis(xsmileys+c(0.3,.8,1,1.5),last(y)+c(-.1,.4,-.3,.2), radius=emojiradius*0.8, type="sad",border=grey_dark2, thickness=smiley_thickness, col=emoji_colors["red"])
  
  #global ranking
  text(xranking,y[-(1:2)],global_ranking, adj=c(0,NA), col=global_ranking_colors, cex=textsize_2)#*sqrt(yspace)
  
  #ranker's own ranking
  arrows(rep(xarrowH[2],3), y0= y_yours, x1=xranking+width_yours*charperunit*(textsize_2)+1, y1=y_yours,  
         lwd=arrowsize, length=arrowlength,col=if(color4you) rev(emoji_colors) else grey_col2)
  draw.emojis(xsmiley,y_yours, radius=emojiradius, type=c("happy", "neutral", "sad"),
              col=if(color4you) rev(emoji_colors) else emoji_colors["grey"], border=grey_dark, thickness=smiley_thickness)
  text(xranking2,y_yours,paste("You ranked this",option, c("as the top one", "second", "third")), 
       adj=c(0,NA), cex=textsize_1, col=if(color4you) rev(text_colors) else text_colors["grey"])
  
  dev.off()
  
}



# Some code to make sure the png files are created before moving on - but shouldn't be an issue anymore
#start_time <- Sys.time()
#while((!file.exists("./Farmer Reports/results.png") | !file.exists("./Farmer Reports/items.png"))& (Sys.time()-start_time)<15){
#  Sys.sleep(1)
#}


###############################################
# create all the farmer feedback reports, by looping over all the ranker.ids

for(ranker_id in ranker.ids){
  #ranker description
  name <- df$package_farmername[df$id==ranker_id]
  ranker_description <- paste0(name,ranker_id)
  rmarkdown::render("Farmer Reports/farmerreport.Rmd",output_dir=paste(getwd(),"Output/Farmer Reports",sep="/"),output_format=output_format,
                    output_file = paste0(nameproj,"Farmer Report",ranker_description,".",extension))
  
}


###############################################
# empty png folder

unlink("Farmer Reports/png Files/mask.png")
unlink("Farmer Reports/png Files/items.png")
for(ranker_id in ranker.ids){
  unlink(paste0("Farmer Reports/png Files/",ranker_id,".png"))
}

