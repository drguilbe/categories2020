#Replication Script
#Guilbeault et al. (2020) Nature Communications 
rm(list=ls());gc(); options(max.print=999999); windows()
library(RCurl); library(ggplot2); library(ggridges); theme_set(theme_ridges())
library(scales); library(tidyverse); library(RVAideMemoire);
library(DescTools); library(clinfun); library(tidytext)
library(psych); library(tm); library(RColorBrewer)
min_max_norm<-function(x){(x - min(x,na.rm=TRUE))/(max(x,na.rm=TRUE) - min(x,na.rm=TRUE))}

#Build color palette for plotting
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
color_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))

#Download data from github
color_palette <- getURL("https://raw.githubusercontent.com/drguilbe/categories2020/master/color_palette.csv"); 
color_palette <- read.csv(text = color_palette, stringsAsFactors = F)
data_wide <- getURL("https://raw.githubusercontent.com/drguilbe/categories2020/master/data_wide.csv"); 
data_wide <- read.csv(text = data_wide, stringsAsFactors = F)
data_long <- getURL("https://raw.githubusercontent.com/drguilbe/categories2020/master/data_long.csv"); 
data_long <- read.csv(text = data_long, stringsAsFactors = F)
model_data <- getURL("https://raw.githubusercontent.com/drguilbe/categories2020/master/model_data.csv"); 
model_data <- read.csv(text = model_data, stringsAsFactors = F)
survey_describe_data <- getURL("https://raw.githubusercontent.com/drguilbe/categories2020/master/all_survey_describe_shape_data.csv"); 
survey_describe_data <- read.csv(text = survey_describe_data, stringsAsFactors = F)
c <- getURL("https://raw.githubusercontent.com/drguilbe/categories2020/master/n1_data.csv"); 
n1_data <- read.csv(text = n1_data, stringsAsFactors = F)
network_size_survey <- getURL("https://raw.githubusercontent.com/drguilbe/categories2020/master/network_size_survey.csv"); 
network_size_survey <- read.csv(text = network_size_survey, stringsAsFactors = F)

#Organize Data
data.wide.conf<-subset(data_wide, condition == "W.Confederates")
data.wide.noconf<-subset(data_wide, condition == "No.Confederates")
data.long.noconf<-subset(data_long, condition == "No.Confederates")

##########
#FIGURE 1#
##########
data.wide.noconf.emergent<-subset(data.wide.noconf, emergent_category)

categories.success.data<-data.long.noconf %>% group_by(N, instance_id, label, condition, emergent_category) %>% 
  dplyr::summarise(total_success = sum(success)) %>% 
  arrange(N, instance_id, desc(emergent_category), desc(total_success))

emergent.categories<-categories.success.data %>% subset(emergent_category)#view all labels and successes
head(emergent.categories)

#Plot 
condition_N<-50
plot_N<-subset(data.wide.noconf.emergent, N==condition_N)

if(condition_N == 2){
  comp_set<-c("crab", "frog", "vagina", "kiss", "kisses") #make visualization to illustrate diff. btwn. cond. 
  n2_sel<-as.data.frame(subset(emergent.categories, N==condition_N) %>%  group_by(instance_id) %>% 
                                   dplyr::summarise(use = sum(comp_set %in% label)==0))
  plot_iids<-n2_sel[n2_sel$use,]$instance_id; length(plot_iids)
  plot_N<-subset(plot_N, instance_id %in% plot_iids)
  plot_N<-subset(plot_N, instance_id %in% sample(unique(plot_N$instance_id), 15))
}

plot_N$label<-as.factor(plot_N$label)
plot_N_label_levels<-levels(plot_N$label)

plot_colorset<-c()
for(thislabel in plot_N_label_levels){
  this_color<-unique(subset(color_palette, label == thislabel)$color)
  plot_colorset<-c(plot_colorset, this_color)
}

plot_N$instance_id<-as.factor(plot_N$instance_id)
levels(plot_N$instance_id)<-1:length(plot_N$instance_id)
plot_N$true_img<-as.numeric(as.character(plot_N$true_img))

ggplot(plot_N, aes(x = true_img, y = instance_id, fill=label)) +
  geom_density_ridges(aes(scale=3.55),alpha=1, size=1.2) +
  scale_fill_manual(values=plot_colorset, aesthetics = "fill") + 
  scale_x_continuous(limits= c(-180,1680), breaks=seq(0,1500, 200)) + 
  ylab("Trials") + xlab("Image Continuum") + 
  guides(fill=guide_legend(title="Categories")) + 
  theme(legend.text=element_text(size=30),
        legend.title=element_text(size=30, face="bold"), 
        legend.position="none",
        legend.background = element_rect(colour = "white"),
        legend.box.background = element_rect(colour = "black", fill="white"),
        plot.title=element_blank(),
        axis.title.y=element_text(size = 30, vjust = 0.1, hjust = 0.45),
        axis.title.x=element_text(size = 30, vjust=-0.2, hjust = 0.45),
        axis.text.y=element_text(size = 30),
        axis.text.x=element_text(size = 30, angle = 345),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 

##########
#FIGURE 2#
##########
get_jaccard<-function(df){
  all_jaccard<-data.frame()
  
  for(thisN in unique(df$N)){
    N_df<-subset(df, N == thisN)
    N_iids=as.character(unique(N_df$instance_id))
    N_iids = N_iids[!is.na(N_iids)] 
    
    combos<-c()
    
    for(i in 1:length(N_iids)){
      iid_i<-N_iids[i]
      for(j in 1:length(N_iids)){
        if(i != j){
          iid_j<-N_iids[j]
          
          combo<-paste(min(iid_i, iid_j), max(iid_i, iid_j))
          
          if(!combo %in% combos){
            combos<-c(combos, combo)
            
            df_iid_i<-N_df[N_df$instance_id == iid_i,]
            df_iid_j<-N_df[N_df$instance_id == iid_j,]
            
            cats_i<-unique(df_iid_i$label)
            cats_j<-unique(df_iid_j$label)
            
            Jaccard<- length(intersect(cats_i, cats_j)) / (length(cats_i) + length(cats_j) -length(intersect(cats_i, cats_j)))
            
            all_jaccard<-rbind(all_jaccard, data.frame(N = thisN, iid_i = iid_i, iid_j = iid_j, Jaccard = Jaccard))
          }
        }
      }
    }
  }
  return(all_jaccard)
}

all_diversity<-subset(data.long.noconf, player_class=="hearer") %>% group_by(instance_id, N, node_id) %>% 
  dplyr::summarise(diversity = length(unique(label))) %>% group_by(instance_id, N) %>%  
  dplyr::summarise(avg_diversity = mean(diversity))

emergent_categories<-data.long.noconf %>% group_by(N, instance_id, label, condition, emergent_category) %>% 
  dplyr::summarise(total_success = sum(success)) %>% subset(emergent_category)

all_jaccard<-get_jaccard(emergent_categories)

all_jaccard_agg<-all_jaccard %>% group_by(N, iid_i) %>% 
  dplyr::summarise(Jaccard = mean(Jaccard)); colnames(all_jaccard_agg)<-c("N", "instance_id", "Jaccard"); 

all_jaccard_agg$instance_id<-as.numeric(as.character(all_jaccard_agg$instance_id))
all_jaccard_agg %>% group_by(N) %>% dplyr::summarise(Jaccard = mean(Jaccard))

all_jaccard_w_diversity<-inner_join(all_jaccard_agg, all_diversity, by = c("N", "instance_id"))
all_jaccard_w_diversity$N<-as.factor(all_jaccard_w_diversity$N)

ggplot(subset(all_jaccard_w_diversity, N %in% c(2,6,8,24,50))) + theme_bw() +
  geom_point(size=9, aes(x=avg_diversity, y=Jaccard, color = N, shape=N), stroke = 1.25) +
  geom_smooth(size=4, aes(x=avg_diversity, y=Jaccard), color="black", 
              method="lm", formula= (y ~ x + I(x^2) + I(x^3)), color=1) +
  scale_color_manual(values=c("grey45","#56B4E9", "#CC79A7", "cyan4", "darkgoldenrod2")) + 
  scale_shape_manual(values=c(16, 18, 15, 17, 19)) + 
  xlab("Number of Labels Seen by Subject") + ylab("Cross-cultural Convergence") + 
  theme(axis.text=element_text(size=40),
        plot.title = element_text(size=24,face="bold", hjust = 0.5),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position = "none",
        legend.text=element_text(size=30),
        legend.title=element_text(size=30), 
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black", fill="white", size=1.4),
        axis.text.x=element_text(size = 40, vjust=0.8), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  scale_y_continuous(labels=scales::percent_format(trim=TRUE,accuracy=1)) + 
  coord_cartesian(xlim=c(10,43), ylim=c(0,0.6)) 
  
#Statistics#
all_jaccard_agg$N<-as.numeric(all_jaccard_agg$N)
jonckheere.test(all_jaccard_agg$Jaccard, all_jaccard_agg$N)
pairwise.wilcox.test(all_jaccard$Jaccard, all_jaccard$N, p.adjust="none")
pairwise.wilcox.test(all_jaccard_agg$Jaccard, all_jaccard_agg$N, p.adjust="none")

###########
#Figure 3A#
###########
all_speaker_agg<-subset(data.long.noconf, player_class=="speaker") %>% group_by(N, instance_id, node_id, label) %>% 
  dplyr::summarise(first_spk = min(round)) %>% mutate(label=tolower(label))
all_hearer_agg<-subset(data.long.noconf, player_class=="hearer") %>% group_by(N, instance_id, node_id, label) %>% 
  dplyr::summarise(first_hear = min(round)) %>% mutate(label=tolower(label))

fig3A_org<-merge(all_speaker_agg, all_hearer_agg, by=c("N","instance_id", "node_id", "label"))
fig3A_org<-fig3A_org %>% mutate(intro = first_spk<first_hear)

freq_rank<-fig3A_org %>% group_by(label) %>% 
  dplyr::summarise(freq=length(unique(node_id[intro==TRUE]))) %>% 
  mutate(rank = dense_rank(desc(freq))) %>% arrange(rank) %>% 
  subset(freq > 0) %>% select(-rank) %>% mutate(document=1); 
colnames(freq_rank)<-c("term", "count", "document") 

freq_rank_dtm<-freq_rank %>% cast_dtm(document, term, count)
Zipf_plot(freq_rank_dtm, lwd = 3)

###########
#Figure 3B#
###########
num_subjects<-length(unique(fig3A_org$node_id))

#Run this to account for synonyms
#fig3A_org[fig3A_org$label == "rabbit",]$label<-"bunny" 
#fig3A_org[fig3A_org$label == "sofa",]$label<-"couch" 


fig3B_agg<-fig3A_org %>% group_by(label) %>% 
  dplyr::summarise(global_intro = length(unique(node_id[intro]))/num_subjects)

fig3B_agg <-fig3B_agg %>% mutate(label_type = ifelse(global_intro > 0.25, "common", "rare")) #common as critical

fig3B_final<-merge(data.frame(fig3A_org), data.frame(fig3B_agg), by="label")

fig3B_final_org<-fig3B_final %>% group_by(N, instance_id, label, label_type) %>%
  dplyr::summarise(num_network_intro=length(unique(node_id[intro]))) %>% 
  dplyr::mutate(prop_network_intro = num_network_intro/N, 
                reach_critical = prop_network_intro >= 0.2) 
  
fig3B_final_agg<-fig3B_final_org %>% group_by(N, instance_id, label_type) %>% 
  dplyr::summarise(prop_reach_critical=sum(reach_critical)/length(reach_critical))

fig3B_final_agg<-data.frame(fig3B_final_agg)

for(iid in unique(fig3B_final_agg$instance_id)){
  
  iid_df<-subset(fig3B_final_agg, instance_id == iid)
  iid_types = unique(iid_df$label_type)
  iid_N = unique(iid_df$N)
  
  if(!"common" %in% iid_types){
    fig3B_final_agg<-rbind(fig3B_final_agg, 
                            data.frame(N = iid_N, instance_id = iid, label_type = "common", prop_reach_critical = 0))
  }
  if(!"rare" %in% iid_types){
    fig3B_final_agg<-rbind(fig3B_final_agg, 
                           data.frame(N = iid_N, instance_id = iid, label_type = "rare", prop_reach_critical = 0))
  }
}

fig3B_final_plot = fig3B_final_agg %>% group_by(N, label_type) %>% 
  dplyr::summarise(
    cilow = ifelse(var(prop_reach_critical)==0, mean(prop_reach_critical), 
                   t.test(prop_reach_critical)$conf.int[1]), 
    cihi = ifelse(var(prop_reach_critical)==0, mean(prop_reach_critical), 
                  t.test(prop_reach_critical)$conf.int[2]), 
    prop_reach_critical = mean(prop_reach_critical))

fig3B_final_plot$N<-as.factor(fig3B_final_plot$N)
fig3B_final_plot$label_type<-as.factor(fig3B_final_plot$label_type)
levels(fig3B_final_plot$label_type)<-c("Common Labels", "Rare Labels")

ggplot(aes(x=N, y=prop_reach_critical, ymin=cilow, ymax=cihi, group=label_type, color=label_type), 
       data = fig3B_final_plot) + 
  geom_point(size=9) + geom_line(size=4) + 
  geom_errorbar(width = 0, size = 2) + theme_bw() +
  xlab("Population Size") + ylab("Probability of Label\n Reaching Critical Mass") + 
  scale_color_manual(values=c("Black", "dodgerblue")) +
  theme(axis.text=element_text(size=60),
        plot.title = element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.text=element_text(size=25),
        legend.title=element_blank(), 
        legend.background = element_blank(),
        #legend.position = c(0.76,0.5),
        legend.position = "none",
        legend.box.background = element_blank(),
        axis.text.x=element_text(size = 60, vjust=0.8), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  scale_y_continuous(breaks=c(0,0.2,0.4,0.6,0.8,1), labels=c(0,0.2,0.4,0.6,0.8,1)) + 
  coord_cartesian(ylim=c(0,0.98))

###########
#Figure 3C#
###########
fig3C_org<- data_long %>% group_by(N, instance_id, node_id, label) %>% 
  dplyr::summarise(round_first_speak = min(round[player_class=="speaker"]), 
                   round_first_hear = min(round[player_class=="hearer"]), 
                   Introduce = round_first_speak < round_first_hear, 
                   Adopt = round_first_speak > round_first_hear)

fig3C_org_agg<- fig3C_org %>% group_by(N, instance_id, label) %>% 
  dplyr::summarise(num_intro = length(unique(node_id[Introduce])),
                   num_adopt = length(unique(node_id[Adopt]))) %>% 
  subset(num_intro > 0);

fig3C_org_agg$prop_intro = fig3C_org_agg$num_intro/fig3C_org_agg$N
fig3C_org_agg$prop_adopt = fig3C_org_agg$num_adopt/fig3C_org_agg$N
fig3C_org_agg$N<-as.factor(fig3C_org_agg$N)

fig3C_org_plot<-fig3C_org_agg %>% group_by(instance_id, N) %>% dplyr::summarise(
  pintro_adopt_r = ifelse(var(prop_intro) == 0, 0, corr.test(prop_intro, prop_adopt, use="complete.obs", method="pearson", ci=TRUE)$r), 
  pintro_adopt_n = ifelse(var(prop_intro) == 0, 0, corr.test(prop_intro, prop_adopt, use="complete.obs", method="pearson", ci=TRUE)$n), 
  pintro_adopt_low = ifelse(var(prop_intro) == 0, 0, CorCI(pintro_adopt_r, pintro_adopt_n)[1]), 
  pintro_adopt_hi = ifelse(var(prop_intro) == 0, 0, CorCI(pintro_adopt_r, pintro_adopt_n)[2]))

fig3C_org_plot<-merge(fig3C_org_plot, all_jaccard_w_diversity, by=c("instance_id", "N"))
fig3C_org_plot$N<-as.factor(fig3C_org_plot$N)

ggplot(fig3C_org_plot) + theme_bw() +
  geom_smooth(size=5, aes(x=avg_diversity, y=pintro_adopt_r), color="black", span = 0.45) +
  geom_point(size=8, aes(x=avg_diversity, y=pintro_adopt_r, color=N, shape=N), alpha=0.8) +
  scale_color_manual(values=c("grey45","#56B4E9", "#CC79A7", "cyan4", "darkgoldenrod2")) + 
  scale_shape_manual(values=c(16, 18, 15, 17, 19)) + 
  xlab("Avg. Number of Labels\n Encountered by Subject") + ylab("Corr. Btw. Critical Mass and Adoption") + 
  theme(axis.text=element_text(size=30),
        plot.title = element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position = "none",
        legend.text=element_text(size=30),
        legend.title=element_text(size=30), 
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black", fill="white", size=1.4),
        axis.text.x=element_text(size = 30, vjust=0.8), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  coord_cartesian(ylim=c(0, 0.97), xlim=c(10, 39)) + 
  scale_y_continuous(breaks=c(0,0.2,0.4,0.6,0.8,1), labels=c(0,0.2,0.4,0.6,0.8,1)) 

##########
#Figure 4# 
##########
confed.data_wide<-subset(data_wide, condition == "W.Confederates")
confed.data_wide.clean<-subset(confed.data_wide, !(spkr_bot == 1 & hear_bot == 1))
confed.data_wide.clean<-subset(confed.data_wide.clean, label != "response_not_given")
confed.data_wide.clean.spkr<-as.data.frame(confed.data_wide.clean %>% arrange(instance_id, spkr_node, -desc(spkr_round)))

confed.data_wide.clean.spkr.agg<-confed.data_wide.clean.spkr %>% 
  group_by(instance_id, label, spkr_round, N) %>% 
  dplyr::summarise(uses = length(success), successes = sum(success), numbots =  unique(numbots)) %>% 
  arrange(instance_id, label, -desc(spkr_round)) %>%
  group_by(instance_id, label, N, numbots) %>% 
  dplyr::mutate(cumsuccess = cumsum(successes), cumuses = cumsum(uses))

confed.data_wide.clean.spkr.agg.sumo.v.crab<-subset(confed.data_wide.clean.spkr.agg, 
                                                    label %in% c("sumo", "crab") & spkr_round <=100)

max_cumsuccess<-max(confed.data_wide.clean.spkr.agg.sumo.v.crab$cumsuccess)
trial_to_view<-unique(confed.data_wide.clean.spkr.agg.sumo.v.crab$instance_id)[6]

ggplot(subset(confed.data_wide.clean.spkr.agg.sumo.v.crab, 
              instance_id == trial_to_view),  
       aes(x = spkr_round, y = cumsuccess)) + theme_bw() +
  geom_line(size=4, aes(color = label)) +
  scale_color_manual(values=c("deeppink2","black")) + 
  xlab("Rounds") + ylab("Cumulative Number of Successful Uses") + 
  theme(axis.text=element_text(size=40, face="bold"),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.text=element_text(size=30),
        legend.title=element_text(size=30), 
        legend.position = "none",
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black", fill="white", size=1.4),
        axis.text.x=element_text(size = 40, vjust=0.8), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  coord_cartesian(xlim=c(0,100), ylim=c(0,max_cumsuccess))

###############
#Supplementary#
###############
n1_data<-subset(n1_data, plot); 
n1_data$label<-as.factor(as.character(n1_data$label))
n1_data_label_levels<-levels(n1_data$label)

n1_colorset<-c()
for(thislabel in n1_data_label_levels){
  this_color<-unique(subset(color_palette, label == thislabel)$color)
  if(length(this_color)>0){
    n1_colorset<-c(n1_colorset, this_color)
  }else{
    color_vector_use<-color_vector[!color_vector %in% n1_colorset]
    if(length(color_vector_use)>0){
      n1_colorset<-c(n1_colorset, sample(color_vector_use,1))
    }else{
      n1_colorset<-c(n1_colorset, sample(color_vector,1))
    }
  }
}

n1_data$subject_id<-as.factor(as.character(n1_data$subject_id))
levels(n1_data$subject_id)<-seq(1:length(levels(n1_data$subject_id)))
n1_data$true_img<-as.numeric(as.character(n1_data$true_img))

ggplot(n1_data, aes(x = true_img, y = subject_id, fill=label)) + 
  geom_density_ridges(aes(scale=3.55),alpha=1, size=1.2) + 
  scale_fill_manual(values=n1_colorset, aesthetics = "fill") + 
  scale_x_continuous(limits= c(-180,1680), breaks=seq(0,1500, 200)) + 
  ylab("Trials") + xlab("Image Continuum") + 
  guides(fill=guide_legend(title="Categories")) + 
  theme(legend.text=element_text(size=30),
        legend.title=element_text(size=30, face="bold"), 
        legend.position="none",
        legend.background = element_rect(colour = "white"),
        legend.box.background = element_rect(colour = "black", fill="white"),
        plot.title=element_blank(),
        axis.title.y=element_text(size = 30, vjust = 0.1, hjust = 0.45),
        axis.title.x=element_text(size = 30, vjust=-0.2, hjust = 0.45),
        axis.text.y=element_text(size = 30),
        axis.text.x=element_text(size = 30, angle = 345),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(), 
        panel.background = element_blank()) 

#FigS4 (Variance of shapes seen by individuals in each condition) 
figs4<-data.long.noconf %>% group_by(instance_id, N, node_id, player_class) %>% 
  dplyr::summarise(img_var = var(true_img))

figs4_agg<-as.data.frame(figs4 %>% group_by(instance_id, N) %>% 
                           dplyr::summarise(img_var = mean(img_var, na.rm=T)))

pairwise.wilcox.test(x = figs4_agg$img_var, g = figs4_agg$N, p.adjust.method = "none")
kruskal.test(img_var ~ N, data = figs4_agg)
figs4_agg$img_var_norm<-min_max_norm(figs4_agg$img_var)
figs4_agg$N<-as.factor(figs4_agg$N)

ggplot(figs4_agg, aes(x = N, y = img_var_norm, color = N)) + 
  geom_boxplot(size = 1.5, alpha = 0.6, outlier.size = 4) + 
  scale_color_manual(values=c("grey45","#56B4E9", "#CC79A7", "cyan4", "darkgoldenrod2")) + 
  theme(plot.title = element_text(size = 30)) + 
  ylab("Variance of Images Subjects were\n asked to Categorize (Norm.)") +
  xlab("Population Size") +
  theme(axis.text=element_text(size=30),
        plot.title = element_blank(),
        axis.title.x= element_text(size=30, hjust=0.5),
        axis.title.y= element_text(size=30, hjust=0.5),
        legend.position = "none",
        legend.text=element_text(size=30),
        legend.title=element_text(size=30), 
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black", fill="white", size=1.4),
        axis.text.x=element_text(size = 30, vjust=0.8), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +  
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))

#FigS5 (Subjects Views of Population Size - Survey) 
size_survey_agg<-network_size_survey %>% group_by(instance_id, N) %>% 
  dplyr::summarise(guess = median(guess, na.rm=T))

ggplot(size_survey_agg, aes(x = N, y = guess, color = N)) + 
  geom_boxplot(size = 1.5, alpha = 0.6, outlier.size = 4) + 
  scale_color_manual(values=c("grey45","#56B4E9", "#CC79A7", "cyan4", "darkgoldenrod2")) + 
  theme(plot.title = element_text(size = 30)) + 
  ylab("Subjects' Predictions of Population Size") + xlab("Population Size")+
  theme(axis.text=element_text(size=30),
        plot.title = element_blank(),
        axis.title.x= element_text(size=30, hjust=0.5),
        axis.title.y= element_text(size=30, hjust=0.5),
        legend.position = "none",
        legend.text=element_text(size=30),
        legend.title=element_text(size=30), 
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black", fill="white", size=1.4),
        axis.text.x=element_text(size = 30, vjust=0.8), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  coord_cartesian(ylim=c(0, 15))

size_survey_agg$N<-as.factor(size_survey_agg$N)
mod<-aov(guess ~ N, size_survey_agg)
summary(mod)

#FigS6 (Model Results)
model_data$N<-as.factor(model_data$N)
model_plot<-subset(model_data, N %in% c(2, 6, 8, 24, 50))

ggplot(model_plot) + theme_bw() +
  geom_point(size=9, aes(x=Diversity, y=Jaccard, color = N, shape=N)) +
  #geom_smooth(size=4, aes(x=div, y=Jaccard), color="black", 
  #            method="lm", formula= (y ~ x + I(x^2) + I(x^3)), color=1) + 
  scale_color_manual(values=c("grey45","#56B4E9", "#CC79A7", "cyan4", "darkgoldenrod2")) + 
  scale_shape_manual(values=c(16, 18, 15, 17, 19)) + 
  xlab("Category Diversity\n(Number of Labels Seen by Subject)")+ 
  ylab("Cross-Cultural Convergence\n(Average Jaccard Index in Vocabulary)") + 
  theme(axis.text=element_text(size=40),
        plot.title = element_text(size=24,face="bold", hjust = 0.5),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position = "none",
        legend.text=element_text(size=30),
        legend.title=element_text(size=30), 
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black", fill="white", size=1.4),
        axis.text.x=element_text(size = 40, vjust=0.8), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  scale_y_continuous(breaks=seq(0, 1, 0.2), labels=scales::percent_format(trim=TRUE,accuracy=1)) + 
  coord_cartesian(xlim=c(8,43), ylim=c(0,0.58)) 

#FigS7 (Partitions Overlap)
data.long.noconf.centroid<-as.data.frame(subset(data.long.noconf, emergent_category) %>% group_by(instance_id, N, label) %>% 
                                      dplyr::summarise(start = min(true_img), end = max(true_img), 
                                                centroid = median(true_img)))

get_centroid_overlap<-function(dataset){
  
  centroid_overlap<-data.frame()
  
  for(n in unique(dataset$N)){
    ndf<-subset(dataset, N==n)
    n_iids<-unique(ndf$instance_id)
    
    for(i in 1:length(n_iids)){
      iid_i = n_iids[i]
      
      for(j in 1:length(n_iids)){
        if(i != j){
          iid_j = n_iids[j]
          
          iid_i_df<-subset(ndf, instance_id == iid_i)
          iid_j_df<-subset(ndf, instance_id == iid_j)
          
          iid_i_df$dist<-sapply(iid_i_df$centroid, function(x) min(abs(iid_j_df$centroid - x)))
          iid_j_df$dist<-sapply(iid_j_df$centroid, function(x) min(abs(iid_i_df$centroid - x)))
          
          raw_dist = mean(c(iid_i_df$dist, iid_j_df$dist))
          mean_dist = mean(c(iid_i_df$dist, iid_j_df$dist)) / 1500
          centroid_overlap<-rbind(centroid_overlap, data.frame(N = n, iid_i = iid_i, iid_j=iid_j, 
                                                               raw_dist=raw_dist, mean_dist=mean_dist))
        }
      }
    }
  }
  return(centroid_overlap)
}

data.long.noconf.centroid.overlap<-get_centroid_overlap(data.long.noconf.centroid)

data.long.noconf.centroid.overlap.plot<-as.data.frame(data.long.noconf.centroid.overlap %>% group_by(N, iid_i) %>% 
                                      dplyr::summarise(avg_cent_dist = mean(raw_dist)) %>% 
                                        mutate(avg_cent_overlap = 1500 - avg_cent_dist) )

colnames(data.long.noconf.centroid.overlap.plot)<-c("N", "instance_id", "avg_cent_dist", "avg_cent_overlap")

data.long.noconf.centroid.overlap.plot$avg_cent_overlap_norm<- min_max_norm(data.long.noconf.centroid.overlap.plot$avg_cent_overlap)

centroid_by_diversity<-inner_join(data.long.noconf.centroid.overlap.plot, all_diversity, by = c("N", "instance_id"))
centroid_by_diversity$N<-as.factor(centroid_by_diversity$N)

ggplot(centroid_by_diversity) + theme_bw() +
  geom_point(size=9, aes(x=avg_diversity, y=avg_cent_overlap_norm, color = N, shape=N), stroke = 2) + #,
  #geom_smooth(size=4, aes(x=avg_diversity, y=avg_cent_overlap_norm), color="black", span = 1) +
  #scale_color_manual(values=c("black", "cyan2", "chocolate3", "purple", "firebrick2")) + 
  scale_color_manual(values=c("grey45","#56B4E9", "#CC79A7", "cyan4", "darkgoldenrod2")) + 
  scale_shape_manual(values=c(16, 18, 15, 17, 19)) + 
  xlab("Category Diversity\n(Number of Labels Seen by Subject)")+ 
  ylab("Cross-Cultural Convergence\n(Average Centroid Overlap, Norm.)") + 
  theme(axis.text=element_text(size=40),
        plot.title = element_text(size=24, hjust = 0.5),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position = "none",
        legend.text=element_text(size=30),
        legend.title=element_text(size=30), 
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black", fill="white", size=1.4),
        axis.text.x=element_text(size = 40, vjust=0.8), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  coord_cartesian(ylim = c(0.6,1)) + 
  scale_y_continuous(labels=scales::percent_format(trim=TRUE,accuracy=1))

#Statistics#
centroid_by_diversity$N<-as.numeric(as.character(centroid_by_diversity$N))
jonckheere.test(centroid_by_diversity$avg_cent_overlap_norm, centroid_by_diversity$N)
pairwise.wilcox.test(centroid_by_diversity$avg_cent_overlap_norm, centroid_by_diversity$N, p.adjust="none")

#FigS8 (Probability Model)
prob_ofk<-function(N, K, n, k){
  x<-choose(K, k) 
  y<-choose(N - K, n - k) 
  denom<-choose(N, n)
  hyper<-(x * y)/denom
  return(hyper)
}

N<-1480 
cmass<-0.2
d<-data.frame()
for(curr_cat_prob in c(0.005, 0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.08, 0.09, 0.1, 
                       0.25, 0.26, 0.27, 0.28, 0.29, 0.3, 0.32, 0.35, 0.37, 0.4, 0.45)){
  K<-round(N * curr_cat_prob) #probability of success
  for(n in seq(0,150,2)){
    k = round(n * cmass)
    if(k<=0){k<-1}
    if(k>n){d<-rbind(d, data.frame(N=N,n=n,K=K,k=k,p=0, cmass = cmass, cat_prob=curr_cat_prob))}else{
      kset<-seq(k, n, 1)
      kset_compute<-sapply(kset, function(currk) prob_ofk(N=N, K=K, n=n, k=currk))
      p=sum(kset_compute) 
      d<-rbind(d, data.frame(N=N,n=n,K=K,k=k,p=p, cmass = cmass, cat_prob=curr_cat_prob))
    }
  }
}

d$cat_prob<-as.numeric(d$cat_prob)

ggplot(subset(d, n <=150), aes(x = n, y = p, color = factor(cat_prob))) +
  geom_hline(yintercept = 0, linetype="dotted", black = "black", size=0.6) + 
  geom_smooth(aes(x = n, y = p, color = factor(cat_prob)),size = 1.2, se=FALSE, span=0.6)+ 
  scale_color_manual(values=c("lightblue1", "lightblue2", "lightblue3", "lightcyan3","cyan1", "cyan2", "cyan3",
                              "blue1", "blue2", "blue3", "blue4", 
                              "goldenrod", "gold1", "goldenrod1", "goldenrod2", "goldenrod3", 
                              "red1", "red2",  "deeppink", "deeppink1","deeppink2","deeppink4"))+
  ylab(paste("Probability of Critical Mass", sep="")) + xlab("Population Size") + 
  labs(color = "P(Label)") + 
  theme(axis.text=element_text(size=40), 
        axis.title.x= element_text(size=30, hjust=0.5),
        axis.title.y= element_text(size=30, hjust=0.5),        
        legend.text=element_text(size=20), 
        legend.title=element_text(size=20), 
        legend.position = c(0.7,0.5)) + 
  guides(color = guide_legend(reverse = TRUE)) + 
  scale_y_continuous(breaks=c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1)) + 
  scale_x_continuous(breaks=c(0,25,50,75,100,125,150),labels=c(0,25,50,75,100,125,150)) + 
  coord_cartesian(ylim=c(0, 1.03))

#FigS10 (Partitions with Influence from the Confederates)
data.wide.w.conf.emergent<-subset(data.wide.conf, emergent_category & !(spkr_bot == 1 & hear_bot == 1))

categories.success.data.wconf<-data.wide.w.conf.emergent %>% 
  group_by(N, instance_id, label, condition, emergent_category) %>% 
  dplyr::summarise(total_success = sum(success)) %>% 
  arrange(N, instance_id, desc(emergent_category), desc(total_success))

emergent.categories.wconf<-categories.success.data.wconf %>% subset(emergent_category)

plot.figs10<-data.wide.w.conf.emergent
plot.figs10$label<-as.factor(as.character(plot.figs10$label))
plot.figs10.levels<-levels(plot.figs10$label)

plot.figs10.colorset<-c()
for(thislabel in plot.figs10.levels){
  this_color<-unique(subset(color_palette, label == thislabel)$color)
  plot.figs10.colorset<-c(plot.figs10.colorset, this_color)
}

plot.figs10$instance_id<-as.factor(plot.figs10$instance_id)
levels(plot.figs10$instance_id)<-1:length(plot.figs10$instance_id)
plot.figs10$true_img<-as.numeric(as.character(plot.figs10$true_img))

ggplot(plot.figs10, aes(x = true_img, y = instance_id, fill=label)) +
  geom_density_ridges(aes(scale=3),alpha=1, size=1.3) +
  scale_fill_manual(values=plot.figs10.colorset, aesthetics = "fill") + 
  scale_x_continuous(limits= c(-200,1700), breaks=seq(0,1500, 200)) + 
  ylab("Trials") + xlab("Image Continuum") + 
  guides(fill=guide_legend(title="Categories")) + 
  theme(legend.text=element_text(size=30),
        legend.title=element_text(size=30, face="bold"), 
        #legend.position="none",
        legend.position=c(0.5,0.5),
        legend.background = element_rect(colour = "white"),
        legend.box.background = element_rect(colour = "black", fill="white"),
        plot.title=element_blank(),
        axis.title.y=element_text(size = 30, vjust = 0.1, hjust = 0.45),
        axis.title.x=element_text(size = 30, vjust=-0.2, hjust = 0.45),
        axis.text.y=element_text(size = 30),
        axis.text.x=element_text(size = 30, angle = 345),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 

all_jaccard_wconf<-get_jaccard(emergent.categories.wconf)

all_jaccard_wconf_agg<-all_jaccard_wconf %>% group_by(N, iid_i) %>% 
  dplyr::summarise(Jaccard = mean(Jaccard)); colnames(all_jaccard_wconf_agg)<-c("N", "instance_id", "Jaccard"); 

all_jaccard_wconf_agg$instance_id<-as.numeric(as.character(all_jaccard_wconf_agg$instance_id))
all_jaccard_wconf_agg %>% group_by(N) %>% dplyr::summarise(Jaccard = mean(Jaccard))

wilcox.test(subset(all_jaccard_agg, N==24)$Jaccard, all_jaccard_wconf_agg$Jaccard)
mean(subset(all_jaccard_agg, N==24)$Jaccard)
mean(all_jaccard_wconf_agg$Jaccard)

#FigS11 & FigS12 (Influence of Confederates on Subjects' ad hoc descriptions)
Figs11<-subset(survey_describe_data, name == "describeShape0563")
Figs11_sumo<-Figs11 %>% group_by(instance_id, name,condition) %>% 
  dplyr::summarise(PropCat = sum(CatofInt == ref_cat)/length(CatofInt)) %>% 
  arrange(name, condition, instance_id) %>% mutate(reflabel="sumo")
Figs11_crab<-Figs11 %>% group_by(instance_id, name,condition) %>% 
  dplyr::summarise(PropCat = sum(CatofInt == "crab")/length(CatofInt)) %>% 
  arrange(name, condition, instance_id) %>% mutate(reflabel="crab")
Figs11_plot<-rbind(Figs11_crab, Figs11_sumo)

ggplot(Figs11_plot, aes(x = reflabel, y = PropCat, fill = condition)) + 
  geom_boxplot(size = 1, outlier.size=5, alpha = 0.6) + 
  scale_fill_manual(values=c("white", "darkgrey")) + theme(plot.title = element_text(size = 30)) + 
  ylab("Percent of Subjects Adopting\n Confederate Framing")+ xlab("Image ID.") + 
  theme(axis.text=element_text(size=50),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=50, hjust=0.5),
        legend.text=element_text(size=30),
        legend.title=element_text(size=30), 
        legend.position = "none",
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black", fill="white", size=1.4),
        axis.text.x=element_text(size = 40, vjust=0.8), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  scale_y_continuous(labels = scales::percent_format(accuracy=1))

wilcox.test(subset(Figs11_plot, condition=="No Conf." & reflabel=="crab")$PropCat, 
            subset(Figs11_plot, condition=="W. Conf." & reflabel=="crab")$PropCat)
wilcox.test(subset(Figs11_plot, condition=="No Conf." & reflabel=="sumo")$PropCat, 
            subset(Figs11_plot, condition=="W. Conf." & reflabel=="sumo")$PropCat)

FigS12<- survey_describe_data %>% group_by(instance_id, name,condition) %>% 
  dplyr::summarise(PropCat = sum(CatofInt == ref_cat)/length(CatofInt)) %>% 
  arrange(name, condition, instance_id)

wilcox.test(subset(FigS12, condition=="No Conf.")$PropCat, subset(FigS12, condition=="W. Conf.")$PropCat)
mean(subset(FigS12, condition=="No Conf.")$PropCat)
mean(subset(FigS12, condition=="W. Conf.")$PropCat)

FigS12$name_clean<-as.factor(FigS12$name)
levels(FigS12$name_clean)<-c("0001", "0304", "0563","0959", "1099", "1488")

ggplot(FigS12,aes(x = name_clean, y = PropCat, fill = condition)) + 
  geom_boxplot(size = 2, outlier.size=5, alpha = 0.6) + 
  scale_fill_manual(values=c("white", "darkgrey")) + theme(plot.title = element_text(size = 30)) + 
  ylab("Percent of Subjects Adopting\n Confederate Framing")+ xlab("Image ID.") + 
  theme(axis.text=element_text(size=50),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=50, hjust=0.5),
        legend.text=element_text(size=30),
        legend.title=element_text(size=30), 
        legend.position = "none",
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black", fill="white", size=1.4),
        axis.text.x=element_text(size = 40, vjust=0.8), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  scale_y_continuous(labels = scales::percent_format(accuracy=1))

#FigS13 (Robustness to vocabulary size)
FigS13<-data.frame()

for(vocab_size in c(1:8)){
  print(paste("vocab_size: ", vocab_size))
  for(currN in c(2,6,8,24,50)){
    Ndf<-subset(data.wide.noconf, N==currN)
    Ndf_top<-as.data.frame(Ndf %>% group_by(N, instance_id, label) %>% 
                             dplyr::summarise(total_success = sum(success)) %>% 
                             top_n(wt = total_success, vocab_size))
    Ndf_jaccard<-get_jaccard(Ndf_top)
    Ndf_jaccard_avg<-mean(Ndf_jaccard$Jaccard)
    if(var(Ndf_jaccard$Jaccard) > 0){
      cilow<-t.test(Ndf_jaccard$Jaccard)$conf.int[1]
      cihi<-t.test(Ndf_jaccard$Jaccard)$conf.int[2]
    }else{
      cilow<-Ndf_jaccard_avg
      cihi<-Ndf_jaccard_avg
    }
    FigS13<-rbind(FigS13, data.frame(N=currN, vocab_size=vocab_size, 
                                     cimin = cilow, cimax = cihi, Jaccard=Ndf_jaccard_avg))
  }
}#Make sure get_jaccard is run above before attempting to generate plot for FigS13 

FigS13$N<-as.factor(FigS13$N)

ggplot(FigS13,  aes(x = vocab_size, y = Jaccard, group=N)) + theme_bw() +
  geom_point(size=8, aes(color = N, shape=N)) +
  geom_ribbon(size=1, aes(color=N,fill = N, ymin=cimin, ymax=cimax), alpha=0.5) +
  geom_line(size=3, aes(color = N)) +
  scale_fill_manual(values=c("grey45","#56B4E9", "#CC79A7", "cyan4", "darkgoldenrod2")) + 
  scale_color_manual(values=c("grey45","#56B4E9", "#CC79A7", "cyan4", "darkgoldenrod2")) + 
  scale_shape_manual(values=c(16, 18, 15, 17, 19)) + 
  xlab("Rounds") + ylab("Cross-Cultural Convergence\n (Average Jaccard Index in Vocabulary)") + xlab("Vocabulary Size") + 
  theme(axis.text=element_text(size=40),
        axis.title.x=element_text(size=30),
        axis.title.y=element_text(size=30),
        legend.text=element_text(size=30),
        legend.title=element_text(size=30), 
        legend.position = "none",
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black", fill="white", size=1.4),
        axis.text.x=element_text(size = 40, vjust=0.8), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
  scale_x_continuous(breaks=seq(0,8,by=2))