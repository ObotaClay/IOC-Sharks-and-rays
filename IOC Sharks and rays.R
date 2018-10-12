#set the working directory

#setwd(here::here('IOC-Sharks-and-rays/'))

library (plyr)
library(reshape2)
library(ggplot2)
library(dplyr)
library(ggplot2)
library(tidyr)

# Effort Analysis ---------------------------------------------------------

Shark_Ray2<- read.csv("Shark_Ray2.csv",header = TRUE,stringsAsFactors = FALSE)

# long to wide ------------------------------------------------------------
#1. gear dist.
t<-data.frame(ftable(Shark_Ray2$Primary.gear.Type..code.,Shark_Ray2$Landing.Site))

t1<-spread(t,Var2,Freq)

write.csv(t1,file='gear_effort.csv',row.names = F)

# 2. vessel dist.
t2<-data.frame(ftable(Shark_Ray2$Boat..vessel.type..code.,Shark_Ray2$Landing.Site))
t3<-spread(t2,Var2,Freq)
write.csv(t3,file='vessel_effort.csv',row.names = F)

# 3. # of visitations to fishing grounds per landing site. 
t4<-data.frame(ftable(Shark_Ray2$Area.Fished..Eneo.la.uvuvi..,Shark_Ray2$Landing.Site))
t5<-spread(t4,Var2,Freq)




# Catch Analysis ----------------------------------------------------------

Shark_Ray1<- read.csv("Shark_Ray1.csv",header = TRUE,stringsAsFactors = FALSE)


#do the average of the benthic category by site and year
x<-ddply(benthic,c("Year","Site","benthic_category","benthic_code"),summarise,
         mean_cover=mean(percent_cover,na.rm = T))
#check sites with cover adding to 100%. 
test_xsum<-ddply(x,c("Year","Site"),summarise,
                 sum_cover=sum(mean_cover,na.rm = T))




# SECTION -----------------------------------------------------------------


unique(Shark1$Shark.Scientific.name)

Shark2<-Shark1[c(which(Shark1$Shark.Scientific.name=="Sphyrna lewini" &Shark1$Landing.Site=="Kipini"),
           which(Shark1$Shark.Scientific.name=="Carcharhinus amblyrhynchos" &Shark1$Landing.Site=="Kipini"),
which(Shark1$Shark.Scientific.name=="Carcharhinus falciformis"&Shark1$Landing.Site=="Ngomeni"),
which(Shark1$Shark.Scientific.name=="Sphyrna lewini" &Shark1$Landing.Site=="Ngomeni"),
which(Shark1$Shark.Scientific.name=="Taeniura lymma" &Shark1$Landing.Site=="Shimoni"),
which(Shark1$Shark.Scientific.name=="Dasyatis kuhlii" &Shark1$Landing.Site=="Shimoni")),]

# PLOTS -------------------------------------------------------------------

p<-NA
# p<-geom_hist(aes(x=Total.length..cm.),size=0.5)

p <- ggplot(Shark2, aes(x =Disc.length , group = Shark.Scientific.name ,
                     colour=Shark.Scientific.name)) +geom_histogram(colour="black",fill="blue",size=0.5, binwidth = 5)

P<- p + facet_wrap(factor(Shark2,levels = c("Sphyrna lewini","Carcharhinus leucas","Carcharhinus amblyrhynchos","Carcharhinus falciformis","Rhynchobatus djiddensis")))
  
p<- p + facet_wrap(Landing.Site~Shark.Scientific.name, ncol =2, scales = "free_y")

p<- p + theme_bw()+ theme(strip.text = element_text(size = 8),
              strip.background = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank(),text = element_text(size = 8,face = "plain"))

# remove the gridline and label the titles
# p <-p + theme_bw() + theme(panel.grid.minor=element_blank())
# p <- ggplot(Shark) + geom_histogram(aes(x = Relative abundnace, y = ,
#                                      ),size=0.5)

# p<- p + scale_x_continuous(breaks = c(5))
p<- p + theme(legend.key = element_blank(),
              legend.position="bottom",
              legend.key.height=unit(0, "cm"),
              # legend.justification="left",
              plot.margin = unit(c(0,0.2,0,0.2), "lines"),
              legend.spacing=unit(0, "cm"),
              legend.text = element_text(size=8))
   #this theme is to format the legend because it is taking up too much space
p <- p + guides(colour = guide_legend(nrow=2))    #legend in 2 rows so it fits on the grid
# p<-p + expand_limits(y=2)
p <- p+ theme(plot.title = element_text(size = 8, face = "Italics"))
p <- p + theme(panel.spacing.x=unit(1.5, "lines"))
p <- p + theme(panel.spacing.y=unit(0.1, "lines"))
p <- p + annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf,size=1)
p <- p +  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf,size=1)
p<- p+ theme(legend.position='above')

library(plyr)
s<-dlply(Shark2, .(Landing.Site), function(x) p %+% x)


#The loop below will save each graph to the folder with the correct Scientific name in the file name
h<-unique(Shark2$Landing.Site)

for (i in 1:length(h)){
  jpeg(paste(h[i]), width = 3.7, height = 3.1, units = 'in', res = 300)   #will give you high quality plots
  print(s[i])
  dev.off() 
}



