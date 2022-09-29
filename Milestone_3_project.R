
library('MASS')
library(olsrr)
library('lme4')
library(dplyr)
library(MuMIn)
library(ggplot2)
library(interactions)
library(sjPlot)
library(jtools)
library(tab)
library(rstatix)
library(grid)
library(gridExtra)
library(cowplot)
library(ggpubr)

library(gridExtra)


# Loading the Dataset

dataset <- read.csv("KeyData.csv")


names(dataset)


# Response Variable
dataset$SR <- as.factor(dataset$SR)

# 1. Research Tactics

# Categorical Predictor Variables 
dataset$WH <- as.factor(dataset$WH)

dataset$BF <- as.factor(dataset$BF)

dataset$NP <- as.factor(dataset$NP)

dataset$FA <- as.factor(dataset$FA)

dataset$AP <- as.factor(dataset$AP)

dataset$AR <- as.factor(dataset$AR)

dataset$DWH <- as.factor(dataset$DWH)

dataset$T <- as.factor(dataset$T)

# Quantitative Predictor Variables

# TWR, DWR


# 2. Scholarly Profile

# Categorical Predictor Variables 

dataset$Rank <- as.factor(dataset$Rank)

dataset$RS <- as.factor(dataset$RS)

# Quantitative Predictor Variables

# H


# 3. Personality Variables
# Categorical Predictor Variables 

dataset$DS <- as.factor(dataset$DS)

# Quantitative Predictor Variables

# NASA, TA, E, A, C, N, O, AC, EC, TC

names(dataset)[24] <- 'T_t'


# Functions

myerror_plot<-function(mod,prd,clrs,yLab,xLab,title,xterms){
  my_plot <- effect_plot(mod, pred = !!(prd), interval = TRUE,cat.interval.geom = c( "linerange"),
                         color.class = clrs)+scale_y_continuous(
                           limits = c(0.0, 1),
                           breaks = c(0.0, 0.25, 0.50, 0.75, 1),
                           labels = c(
                             "0.00" = "0%",
                             "0.25" = "25%",
                             "0.50" = "50%",
                             "0.75" = "75%",
                             "1" = "100%"
                           ))+labs(y = yLab, x = xLab)+theme(
                             legend.position = "none",
                             panel.grid = element_blank(),
                             plot.title = element_text(size=8,face="bold",hjust=0.5),
                             panel.border = element_rect(color="black",fill=NA))+ggtitle(title)+scale_x_discrete(labels = xterms)
  return(my_plot)
}




my_cont_plot<-function(mod,trm,clr,yLab,xLab,lmt,brk,title,x_intercept){
  
  manClr = ""
  my_plot <- effect_plot(mod, pred = !!trm ,interval = TRUE)+scale_y_continuous(
    limits = c(0.0, 1),
    breaks = c(0.0, 0.25, 0.50, 0.75, 1),
    labels = c(
      "0.00" = "0%",
      "0.25" = "25%",
      "0.50" = "50%",
      "0.75" = "75%",
      "1" = "100%"
    ))+labs(y = yLab, x = xLab)+theme(
      legend.position = "none",
      panel.grid = element_blank(),
      panel.border = element_rect(color="black",fill=NA),
      plot.title=element_text(size=8,face="bold",hjust=0.5))+ggtitle(title)+geom_smooth(color=clr,size=1.5)+
    scale_x_continuous(
      limits = lmt,
      breaks=brk)+geom_vline(
        xintercept = x_intercept,
        linetype = "dashed",
        color = "gray",
        size = 1
      )
  
  return(my_plot)
}

my_empty_plot <- function(){
  p <- ggplot(dataset)+geom_blank()+theme_bw()
  return(p)
}

dummy_df <- data.frame(
  x = c(0, 1, 0, 1),
  y = c(1, 0, 0, 1),
  group = c('a', 'a', 'b', 'b')
)


SectionA_draw_dummy_plot <- function(plot_title, y_title) {
  axis_text_size <- 8
  if (plot_title != "") {
    title = element_text(hjust = 0.5,
                         size = 8,
                         face = "bold")
  }
  
  if (plot_title == "") {
    title = element_blank()
  }
  dummy_plot <- ggplot(dummy_df, aes(x = x, y = y, group = group)) +
    geom_line(scale = 0.9, color = "white") +
    theme_bw() +
    theme(
      plot.title = title,
      axis.title.x = element_text(),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.x = element_text(
        face = "bold",
        size = axis_text_size,
        angle = 30,
        hjust = 1
      ),
      axis.text.y = element_text(face = "bold", size = axis_text_size),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    ) + labs(y = element_blank(), x = "", title = plot_title) +
    scale_y_continuous(
      limits = c(0.0, 1),
      breaks = c(0.0, 0.25, 0.50, 0.75, 1),
      labels = c(
        "0.00" = "",
        "0.25" = "",
        "0.50" = "",
        "0.75" = "",
        "1" = ""
      )
    ) +
    scale_x_continuous(
      limits = c(0.0, 1),
      breaks = c(0.0, 0.25, 0.50, 0.75, 1),
      labels = c(
        "0.00" = "",
        "0.25" = "",
        "0.50" = "",
        "0.75" = "",
        "1" = ""
      )
    )
  
  return(dummy_plot)
  # dummy_plot
}
SectionB_draw_dummy_plot <-
  function(plot_title, y_title, configuration) {
    y_title = bquote( ~ italic("S"^.(paste0("G", configuration))))
    dummy_plot <- ggplot(dummy_df, aes(x = x, y = y, group = group)) +
      geom_line(scale = 0.9, color = "white") +
      theme_bw() +
      theme(
        plot.title = element_blank(),
        axis.title.x = element_text(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(
          face = "bold",
          size = 8,
          angle = 30,
          hjust = 1
        ),
        axis.text.y = element_text(face = "bold", size = 8),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      ) + labs(y = y_title, x = "", title = plot_title) +
      scale_y_continuous(
        limits = c(0.0, 1),
        breaks = c(0.0, 0.25, 0.50, 0.75, 1),
        labels = c(
          "0.00" = "0%",
          "0.25" = "25%",
          "0.50" = "50%",
          "0.75" = "75%",
          "1" = "100%"
        )
      ) +
      scale_x_continuous(
        limits = c(0.0, 1),
        breaks = c(0.0, 0.25, 0.50, 0.75, 1),
        labels = c(
          "0.00" = "",
          "0.25" = "",
          "0.50" = "",
          "0.75" = "",
          "1" = ""
        )
      )
    
    return(dummy_plot)
    dummy_plot
  }
SectionC_draw_dummy_plot <-
  function(plot_title, y_title, configuration) {
    if (plot_title != "") {
      title = element_text(hjust = 0.5,
                           size = 7,
                           face = "bold")
    }
    
    if (plot_title == "") {
      title = element_blank()
    }
    
    if (plot_title == "Trait Anxiety") {
      y_title = bquote(~ italic("S"^.(paste0("G", configuration))))
    }
    if (plot_title != "Trait Anxiety") {
      y_title = element_blank()
    }
    
    
    dummy_plot <-
      ggplot(dummy_df, aes(x = x, y = y, group = group)) +
      geom_line(scale = 0.9, color = "white") +
      theme_bw() +
      theme(
        plot.title = title,
        axis.title.x = element_text(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(
          face = "bold",
          size = 7,
          angle = 30,
          hjust = 1
        ),
        axis.text.y = element_text(face = "bold", size = 6),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      ) + labs(y = y_title, x = "", title = plot_title) +
      scale_y_continuous(
        limits = c(0.0, 1),
        breaks = c(0.0, 0.25, 0.50, 0.75, 1),
        labels = c(
          "0.00" = "0%",
          "0.25" = "25%",
          "0.50" = "50%",
          "0.75" = "75%",
          "1" = "100%"
        )
      ) + scale_x_continuous(
        limits = c(0.0, 1),
        breaks = c(0.0, 0.25, 0.50, 0.75, 1),
        labels = c(
          "0.00" = "",
          "0.25" = "",
          "0.50" = "",
          "0.75" = "",
          "1" = ""
        )
      )
    return(dummy_plot)
    # dummy_plot
  }

########################################

# Derived Models

# Model-2  - SG30
#     1         2       3              4       5       6         7
# [0 = < 10%, 10-20%, 20-30%;     1 = 30-50%, 50-75%, 75-90%, > 90%].

dataset$SG30 <- 2

for(row in 1:nrow(dataset))
  {
  if(dataset$SR[row]==1 | dataset$SR[row]==2 | dataset$SR[row]==3)
    {
    dataset$SG30[row]=0
    }
  if(dataset$SR[row]==4 | dataset$SR[row]==5 | dataset$SR[row]==6 | dataset$SR[row]==7)
  {
    dataset$SG30[row]=1
  }
  
  }


factor(dataset$SG30)

model2 <- glm(SG30 ~ WH + BF + NP + FA + AP + AR + DWH + T_t + TWR + 
                DWR  + RS + H + DS + TA + E + A + C 
              + N + O + AC + EC + TC , data = dataset, family='binomial')

summary(model2)


forward_s30 <- stepAIC(model2, direction = "forward")

backward_s30 <- stepAIC(model2, direction = "backward")

both_s30 <- stepAIC(model2, direction = "both" , trate= FALSE)
 
forward_s30$aic
backward_s30$aic
both_s30$aic

summary(backward_s30)
 
# Model after backward - NP+FA+DWH+T+RS+H+DS+TA - AIC - 424.93
# Removing each non-significant term and seeing if it reduces the AIC further

# NS - DWH,T,RS,DS



comb2 <- glm(SG30 ~ NP + FA + DWH+ RS + H + DS + TA , data = dataset, family='binomial')

summary(comb2) # AIC increases to 425.29


comb1 <- glm(SG30 ~ NP + FA + RS + H + DS + TA , data = dataset, family='binomial')

summary(comb1) # AIC increases to 425.79


comb1 <- glm(SG30 ~ NP + FA + RS + H + DS + TA , data = dataset, family='binomial')

summary(comb1) # AIC increases to 425.79


comb3 <- glm(SG30 ~ NP + FA + H + DS + TA , data = dataset, family='binomial')

summary(comb3) # AIC increases to 426.38


# finally with all the significant ones



forwards = step(zero_model,
                scope=list(lower=formula(zero_model),upper=formula(full_model)), direction="forward")

final_model_sg30 <- glm(SG30 ~ NP + FA + DWH + T_t + RS + H + DS + TA, data = dataset, family='binomial')

summary(final_model_sg30)

f_m_sg30 <- glm(SG30 ~ NP + FA + H + DS + TA , data = dataset, family='binomial')
summary(f_m_sg30)

# Table-S2

library('kableExtra')

TS2 <- data.frame(coefficients(summary(f_m_sg30)), check.names = TRUE)
TS2$Prob_wise <- exp(TS2$Estimate)/(1+exp(TS2$Estimate))

names(TS2)[1] <- 'Odds-wise'
names(TS2)[4] <- "Pr(>|z|)"

TS2 <- TS2 %>% add_significance("Pr(>|z|)")

library('rstatix')
tab_df(TS2, digits = 3,show.rownames = TRUE)

library(dplyr)

TS2  <- TS2 %>% relocate(Prob_wise, .before = `Odds-wise`)

TS2  %>% kbl() %>% kable_classic(full_width = F, html_font = "Cambria")


# Plots


np_sg30 <- myerror_plot(f_m_sg30,"NP",c("gray","red","red"),'SG30','',"Number of Proposals",c("NP1","NP2","NP2"))
fa_sg30 <- myerror_plot(f_m_sg30,"FA",c("gray","black","black",'orange','black','orange'),'','',"Funding Agency",c("NSF","NIH","DOE","DOD","NASA","OT"))+theme(axis.text.y=element_blank(),
                                                                                                                                                                     axis.ticks.y=element_blank())
bf_sg30 <- SectionC_draw_dummy_plot("Break Frequency", "")+theme(axis.text.y=element_blank(),
                                                                 axis.ticks.y=element_blank())
pr_sg30 <- SectionC_draw_dummy_plot("Pilot Research", "")+theme(axis.text.y=element_blank(),
                                                                axis.ticks.y=element_blank())
h_sg30 <- my_cont_plot(f_m_sg30,c("H"),c("skyblue"),'SG30','',c(0,200),seq(0,200,by=50),"H-index",mean(dataset$H))

ds_sg30 <- myerror_plot(f_m_sg30,c("DS"),c("gray","orange"),'',"","Deadline Stress",c("DS1","DS2"))+theme(axis.text.y=element_blank(),
                                                                                                                   axis.ticks.y=element_blank())
ta_sg30 <- my_cont_plot(f_m_sg30,c("TA"),c("skyblue"),'',
                        '',c(20,70),seq(20,70,by=10),"Trait Anxiety",
                        mean(dataset$TA))+theme(axis.text.y=element_blank(),
                                                axis.ticks.y=element_blank())
e_sg30 <- SectionC_draw_dummy_plot("Extraversion", "")+theme(axis.text.y=element_blank(),
                                                                axis.ticks.y=element_blank())
ac_sg30 <- SectionC_draw_dummy_plot("Avoidance coping", "")+theme(axis.text.y=element_blank(),
                                                                 axis.ticks.y=element_blank())


figure2_row1 <- plot_grid(np_sg30,
                          fa_sg30,
                          bf_sg30,
                          pr_sg30,nrow = 1,ncol=4)

figure2_row3 <- plot_grid(h_sg30,
                          ds_sg30,ta_sg30,e_sg30,ac_sg30,
                          nrow = 1,ncol=5)







#########################################
# Model 3 - SG50

#dataset$BF <- as.factor(dataset$BF)

#     1         2       3         4         5       6         7
# [0 = < 10%, 10-20%, 20-30%, 30-50%, ; 1 = 50-75%, 75-90%, > 90%].


dataset$SG50 <- 1

for(row in 1:nrow(dataset))
{
  if(dataset$SR[row]==1 | dataset$SR[row]==2 | dataset$SR[row]==3 | dataset$SR[row]==4)
  {
    dataset$SG50[row]=0
  }
  if(dataset$SR[row]==5 | dataset$SR[row]==6 | dataset$SR[row]==7)
  {
    dataset$SG50[row]=1
  }
  
}

factor(dataset$SG50)



model3 <- glm(dataset$SG50 ~ WH + BF + NP + FA + AP + AR + DWH + T_t + TWR + 
                DWR  + RS + H + DS + TA + E + A + C 
              + N + O + AC + EC + TC  , data = dataset, family='binomial')

summary(model3)

forward_s50 <- stepAIC(model3, direction = "forward" , trate= FALSE)

backward_s50 <- stepAIC(model3, direction = "backward" , trate= FALSE)

both_s50 <- stepAIC(model3, direction = "both" , trate= FALSE)


forward_s50$aic
backward_s50$aic
both_s50$aic

# Trying combinations for better models

summary(backward_s50) # 304.31

# NS- DS,TC

# Removing TC and checking for AIC

comb50_1 <- glm(SG50 ~ NP + FA + BF + AR + H + E + AC + DS,data = dataset,family = 'binomial')


summary(comb50_1) # 305.07

# Removing DS and checking for AIC

comb50_2 <- glm(SG50 ~ NP + FA + BF + AR + H + E + AC,data = dataset,family = 'binomial')


summary(comb50_2) # 305.07

# AC is getting . but it should have a * 


final_model_sg50 <- glm(as.factor(dataset$SG50) ~ BF + NP + FA + AR + H + DS + E + AC + TC, data=dataset,family='binomial')
summary(final_model_sg50) # 306.58


f_m_sg50 <- glm(as.factor(dataset$SG50) ~ NP + FA + BF + AR + H  + E + AC , data=dataset,family='binomial')
summary(f_m_sg50)




# Table-S3


TS3 <- data.frame(coefficients(summary(f_m_sg50)), check.names = TRUE)
TS3$Prob_wise <- exp(TS3$Estimate)/(1+exp(TS3$Estimate))

names(TS3)[1] <- 'Odds-wise'
names(TS3)[4] <- "Pr(>|z|)"

TS3 <- TS3 %>% add_significance("Pr(>|z|)")

tab_df(TS3, digits = 3,show.rownames = TRUE)


TS3  <- TS3 %>% relocate(Prob_wise, .before = `Odds-wise`)

TS3  %>% kbl() %>% kable_classic(full_width = F, html_font = "Cambria")


# AC is showing as non-significant but is it * significant

# Plots


np_sg50 <- myerror_plot(f_m_sg50,"NP",c("gray","orange","red"),'SG50','',"",c("NP1","NP2","NP2"))
fa_sg50 <- myerror_plot(f_m_sg50,"FA",c("gray","black","black",'skyblue','black','orange'),'','',"",c("NSF","NIH","DOE","DOD","NASA","OT"))+theme(axis.text.y=element_blank(),
                                                                                                                                                                      axis.ticks.y=element_blank())

bf_sg50 <- myerror_plot(f_m_sg50,"BF",c("gray",'skyblue'),'','',"",c("BF1","BF2"))+theme(axis.text.y=element_blank(),
                                                                                                              axis.ticks.y=element_blank())


ar_sg50 <- myerror_plot(f_m_sg50,"AR",c("gray","skyblue",'orange',"skyblue","skyblue"),'','',"",c("PR1","PR2","PR3","PR4","PR5"))+theme(axis.text.y=element_blank(),
                                                                                                                                                            axis.ticks.y=element_blank())




h_sg50 <- my_cont_plot(f_m_sg50,c("H"),c("Orange"),'SG50','',
                       c(0,200),seq(0,200,by=50),"",mean(dataset$H))

da_sg50 <- SectionC_draw_dummy_plot("", "")+theme(axis.text.y=element_blank(),
                                                  axis.ticks.y=element_blank())
ta_sg50 <- SectionC_draw_dummy_plot("", "")+theme(axis.text.y=element_blank(),
                                                                 axis.ticks.y=element_blank())


e_sg50 <- my_cont_plot(f_m_sg50,c("E"),c("Orange"),'',
                       '',c(2,10),seq(2,10,by=2),"",mean(dataset$E))+theme(axis.text.y=element_blank(),
                                                                                         axis.ticks.y=element_blank())


ac_sg50 <- my_cont_plot(f_m_sg50,c("AC"),c("skyblue"),
                        '','',c(10,30),seq(10,30,by=10),"",mean(dataset$AC))+theme(axis.text.y=element_blank(),
                                                                                                         axis.ticks.y=element_blank())


figure2_row2 <- plot_grid(np_sg50,
                          fa_sg50,
                          bf_sg50,
                          ar_sg50,
                          nrow = 1,ncol=4)

figure2_row4 <- plot_grid(h_sg50,
                          da_sg50,
                          ta_sg50,
                          e_sg50,
                          ac_sg50,
                          nrow = 1,ncol=5)

Research_tactics<- ggarrange(figure2_row1, figure2_row2, nrow = 2)
Research_tactics <- plot_grid(Research_tactics, grid.draw(gg), nrow = 2, rel_heights = c(1, .05))
sectionB <- ggarrange(
  # plot1,
  h_sg30,
  h_sg50,
  nrow = 2,
  ncol = 1
)

sectionC <- ggarrange(
  ds_sg30,ta_sg30,e_sg30,ac_sg30,
  
  da_sg50,
  ta_sg50,
  e_sg50,
  ac_sg50,
  nrow = 2,
  ncol = 4
)
section_BC <- plot_grid(sectionB, sectionC, rel_widths = c(1, 2), ncol = 2, labels = c("b", "c"))
final_plot <-
  plot_grid(Research_tactics,
            section_BC,
            nrow = 2,
            labels = c("a", "")) + annotation_custom(grid.polygon(
              x = c(0, 0.5, 1, 0.336, 0.336),
              y = c(0.5, 0.5, 0.5, 0, 0.5),
              id = c(1, 1, 1, 2, 2),
              gp = gpar(lwd = 1.5))) +  annotation_custom(grid.polygon(
                # c(0, 0.25, 1, 1, 1),
                y = c(0.525, 0.525, 0.525, 0.525),
                id = NULL,
                gp = gpar(lwd = 1.5)))



final_plot

filename <- "Figure2.png"
full_path <- file.path(getwd(), filename)
ggsave(
  full_path,
  final_plot,
  width = 8.5,
  height = 11,
  units = "in"
)



###############################


# Model-4 - SG75


#     1         2       3         4           5           6       
# [0 = Not funded, 1-25%, 25-50%, 55-75%, 1 = 75%-100%, Fully funded].

factor(dataset$FC)

dataset$SG75 <- 1

for(row in 1:nrow(dataset))
{
  if(dataset$FC[row]==1 | dataset$FC[row]==2 | dataset$FC[row]==3 | dataset$FC[row]==4)
  {
    dataset$SG75[row]=0
  }
  if(dataset$FC[row]==5 | dataset$FC[row]==6)
  {
    dataset$SG75[row]=1
  }
  
}


factor(dataset$SG75)
  
model4 <- glm(as.factor(dataset$SG75) ~ WH + BF + NP + FA + AP + AR + DWH + T_t + TWR + 
                DWR  + RS + H + DS + TA + E + A + C 
              + N + O + AC + EC + TC  , data = dataset, family='binomial')

summary(model4)

forward_s75 <- stepAIC(model4, direction = "forward" , trate= FALSE)

backward_s75 <- stepAIC(model4, direction = "backward" , trate= FALSE)

both_s75 <- stepAIC(model4, direction = "both" , trate= FALSE)


forward_s75$aic
backward_s75$aic
both_s75$aic

# Trying combinations to remove the non-significant variables

summary(backward_s75)

# FA + T_t + TWR + RS + H + TA + E + O # AIC - 530.29

# NS - TA 

# Removing TA and checking for AIC

comb_75_1 <- glm(SG75 ~ FA + T_t + TWR + RS + H + E + O, data = dataset, family = 'binomial')

summary(comb_75_1) - # AIC 530.56
  
# Removing E and checking for AIC

comb_75_2 <- glm(SG75 ~ FA + T_t + TWR + RS + H + O, data = dataset, family = 'binomial')

summary(comb_75_2)  # AIC 531.61
  

# After this all are significant
  


final_model_sg75 <- glm(as.factor(dataset$SG75) ~ FA + T_t + TWR + RS + H + TA + E + O, data=dataset,family='binomial')
summary(final_model_sg75)

FA + T_t + TWR + RS + H + O

f_m_sg75 <- glm(as.factor(dataset$SG75) ~ FA + T_t + TWR + RS + H + O, data=dataset,family='binomial')
summary(f_m_sg75)


# Table-S4


TS4 <- data.frame(coefficients(summary(f_m_sg75)), check.names = TRUE)
TS4$Prob_wise <- exp(TS4$Estimate)/(1+exp(TS4$Estimate))

names(TS4)[1] <- 'Odds-wise'
names(TS4)[4] <- "Pr(>|z|)"

TS4 <- TS4 %>% add_significance("Pr(>|z|)")

tab_df(TS4, digits = 3,show.rownames = TRUE)

TS4  <- TS4 %>% relocate(Prob_wise, .before = `Odds-wise`)

TS4  %>% kbl() %>% kable_classic(full_width = F, html_font = "Cambria")


# Plots


fa_sg75 <- myerror_plot(f_m_sg75,"FA",c("gray","black","skyblue",'orange','black','black'),'S$75','',"Funding Agency",c("NSF","NIH","DOE","DOD","NASA","OT"))
ts_s75 <- myerror_plot(f_m_sg75,"T_t",c("gray","skyblue"),'','',"Time of Submission",c("TS1","TS2") )+theme(axis.text.y=element_blank(),
                                                                                                            axis.ticks.y=element_blank())

twr_s75 <- my_cont_plot(f_m_sg75,c("TWR"),c("Orange"),
                        '','',c(0,100),seq(0,100,by=25),
                        "Typical Week Research",mean(dataset$TWR))+theme(axis.text.y=element_blank(),
                                                                         axis.ticks.y=element_blank())

h_sg75 <- my_cont_plot(f_m_sg75,c("H"),c("skyblue"),'S$75','',
                       c(0,100),seq(0,100,by=25),"H-index",mean(dataset$H))

rs_sg75 <- myerror_plot(f_m_sg75,"RS",c("gray","skyblue"),'','',"Research Style",c("RS1","RS2") )+theme(axis.text.y=element_blank(),
                                                                                                             axis.ticks.y=element_blank())

o_sg75 <- my_cont_plot(f_m_sg75,c("O"),c("skyblue"),'','',
                       c(2,10),seq(2,10,by=2),"Openness",mean(dataset$O))+theme(axis.text.y=element_blank(),
                                                                                axis.ticks.y=element_blank())


figure_3_row1 <- plot_grid(fa_sg75,ts_s75,twr_s75,nrow = 1,ncol=3)

figure_3_row3 <- plot_grid(h_sg75,rs_sg75,o_sg75,nrow = 1,ncol=3)



###########################################


# Model-5 - SGDD

#     1         2       3         4           5           6       
# [0 = Not funded, 1-25%, 25-50%, 55-75%, 75%-100%, 1 = Fully funded].

dataset$SGDD <- 1

for(row in 1:nrow(dataset))
{
  if(dataset$FC[row]==1 | dataset$FC[row]==2 | dataset$FC[row]==3 | dataset$FC[row]==4 | dataset$FC[row]==5)
  {
    dataset$SGDD[row]=0
  }
  if(dataset$FC[row]==6)
  {
    dataset$SGDD[row]=1
  }
  
}

model5 <- glm(as.factor(dataset$SGDD) ~ WH + BF + NP + FA + AP + AR + DWH + T_t + TWR + 
                DWR  + RS + H + DS + TA + E + A + C 
              + N + O + AC + EC + TC, data = dataset, family='binomial')

summary(model5)

forward_sdd <- stepAIC(model5, direction = "forward" , trate= FALSE)

backward_sdd <- stepAIC(model5, direction = "backward" , trate= FALSE)

both_sdd <- stepAIC(model5, direction = "both" , trate= FALSE)




# Combinations of all variables

summary(backward_sdd) 

# MODEL NOW - FA + T + TWR + RS + O + TC -> AIC - 404.88
# NS - T,RS,TC

# Removing T and checking for AIC


comb_dd_1 <- glm(as.factor(dataset$SGDD) ~ FA + TWR + RS + O + TC ,
                 data = dataset, family='binomial')

summary(comb_dd_1) # AIC 405.01

# Removing RS and checking AIC

comb_dd_2 <- glm(as.factor(dataset$SGDD) ~ FA + TWR + O + TC ,
                 data = dataset, family='binomial')

summary(comb_dd_2) # AIC 405.61

# Removing TC and checking AIC

comb_dd_3 <- glm(as.factor(dataset$SGDD) ~ FA + TWR + O,
                 data = dataset, family='binomial')

summary(comb_dd_3) # AIC 406.73

# Matches the manuscript 


final_model_sgdd <- glm(as.factor(dataset$SGDD) ~ FA + T_t + TWR + RS + O + TC, data=dataset,family='binomial')
summary(final_model_sgdd)

f_m_sgdd <- glm(as.factor(dataset$SGDD) ~ FA + TWR + O, data=dataset,family='binomial')
summary(f_m_sgdd)

# Table-S5

TS5 <- data.frame(coefficients(summary(f_m_sgdd)), check.names = TRUE)
TS5$Prob_wise <- exp(TS5$Estimate)/(1+exp(TS5$Estimate))

names(TS5)[1] <- 'Odds-wise'
names(TS5)[4] <- "Pr(>|z|)"

TS5 <- TS5 %>% add_significance("Pr(>|z|)")

tab_df(TS5, digits = 3,show.rownames = TRUE)

TS5  <- TS5 %>% relocate(Prob_wise, .before = `Odds-wise`)

TS5  %>% kbl() %>% kable_classic(full_width = F, html_font = "Cambria")


# Plots


fa_sgdd <- myerror_plot(f_m_sgdd,"FA",c("gray","black","black","red","black","black"),'S$$','',"",c("NSF","NIH","DOE","DOD","NASA","OT"))+theme(axis.text.y=element_blank(),
                                                                                                                                                 axis.ticks.y=element_blank())
ts_sgdd <- SectionC_draw_dummy_plot("", "")+theme(axis.text.y=element_blank(),
                                                  axis.ticks.y=element_blank())
                                                                                                                                         
twr_sgdd <- my_cont_plot(f_m_sgdd,c("TWR"),c("Red"),'','',c(0,100),seq(0,100,by=25),"",mean(dataset$TWR))+theme(axis.text.y=element_blank(),
                                                                                                                axis.ticks.y=element_blank())
h_sgdd <- SectionC_draw_dummy_plot("", "")+labs(y="S$$")
rs_sgdd <- SectionC_draw_dummy_plot("", "")+theme(axis.text.y=element_blank(),
                                                  axis.ticks.y=element_blank())
o_sgdd<- my_cont_plot(f_m_sgdd,c("O"),c("skyblue"),'','',c(2,10),seq(2,10,by=2),"",mean(dataset$O))+theme(axis.text.y=element_blank(),
                                                                                                          axis.ticks.y=element_blank())


figure_3_row2<- plot_grid(fa_sgdd,
                          ts_sgdd,
                          twr_sgdd,
                          nrow = 1,ncol=3)

figure_3_row4 <- plot_grid(h_sgdd,
                           rs_sgdd,
                           o_sgdd,
                           nrow = 1,ncol=3)


colors <- c("*" = "Sky blue", "**" = "orange", "***" = "red")



g <- ggplot(iris, aes(x = Sepal.Length)) +
  geom_line(aes(y = Sepal.Width, color = "Sepal Width"), size = 1.5) +
  labs(x = "Year",
       y = "(%)",
       color = "") +
  scale_color_manual(values = colors) + theme(legend.direction = "horizontal",
                                              legend.key = element_rect(fill = "transparent"),
                                              legend.key.size = unit(2, 'cm'),
                                              legend.text = element_text (size = 20))

gg <- get_legend(g)

res_tactics<- ggarrange(figure_3_row1, figure_3_row2, nrow = 2)
res_tactics <- plot_grid(res_tactics, gg, nrow = 2, rel_heights = c(1, .05))
sectionC <- ggarrange(
  # plot1,
  
  o_sg75,
  o_sgdd,
  nrow = 2,
  ncol = 1
)



sectionB <- ggarrange(
  h_sg75,rs_sg75,h_sgdd,rs_sgdd,
  nrow = 2,
  ncol = 2
)
section_BC <- plot_grid(sectionB, sectionC, rel_widths = c(2, 1), ncol = 2, labels = c("b", "c"))
final_plot_fig3 <-
  plot_grid(res_tactics,
            section_BC,
            nrow = 2,
            labels = c("a", "")) + annotation_custom(grid.polygon(
              x = c(0, 0.5, 1, 0.336, 0.336),
              y = c(0.5, 0.5, 0.5, 0, 0.5),
              id = c(1, 1, 1, 2, 2),
              gp = gpar(lwd = 1.5))) + annotation_custom(grid.polygon(
                # c(0, 0.25, 1, 1, 1),
                y = c(0.525, 0.525, 0.525, 0.525),
                id = NULL,
                gp = gpar(lwd = 1.5)))



filename <- "Figure3.png"

full_path <- file.path(getwd(), filename)
ggsave(
  full_path,
  final_plot_fig3,
  width = 8.5,
  height = 11,
  units = "in"
)









#################################
#Model - 6 - S50DD

dataset$SG50DD <- 0

for(row in 1:nrow(dataset))
{
  if(dataset$SR[row]==1 | dataset$SR[row]==2 | dataset$SR[row]==3 | dataset$SR[row]==4)
  {
    dataset$SG50DD[row]=0
  }
  if((dataset$SR[row]==5 | dataset$SR[row]==6 | dataset$SR[row]==7) & dataset$FC[row]==6)
  {
    dataset$SG50DD[row]=1
  }
  
}

factor(dataset$SG50DD)

model6 <- glm(as.factor(dataset$SG50DD) ~ WH + BF + NP + FA + AP + AR + DWH + T_t + TWR + 
                DWR  + RS + H + DS + TA + E + A + C 
              + N + O + AC + EC + TC, data = dataset, family='binomial')

summary(model6)

forward_sg50dd <- stepAIC(model6, direction = "forward" , trate= FALSE)

backward_sg50dd <- stepAIC(model6, direction = "backward" , trate= FALSE)

both_sg50dd <- stepAIC(model6, direction = "both" , trate= FALSE)

forward_sg50dd$aic
backward_sg50dd$aic
both_sg50dd$aic



# combinations of models to remove ns

summary(backward_sg50dd) 
# AIC 159.83 - NP + TWR + RS + E + A + O

# NS - RS, O

# Removing O and checking the AIC

comb_50dd_1 <- glm(as.factor(dataset$SG50DD) ~ NP + TWR + RS + E + A
                     ,data = dataset, family='binomial')

summary(comb_50dd_1) # AIC 160.48


# Removing RS and checking the AIC

comb_50dd_2 <- glm(as.factor(dataset$SG50DD) ~ NP + TWR + E + A
                   ,data = dataset, family='binomial')


summary(comb_50dd_2) # AIC 160.95

# ALL are significant  matches the manuscript


final_model <- glm(as.factor(dataset$SG50DD) ~ NP + TWR + RS + E + A + O, data=dataset,family='binomial')
summary(final_model)

f_m_s50dd <- glm(as.factor(dataset$SG50DD) ~ NP + TWR  + E + A , data=dataset,family='binomial')
summary(f_m_s50dd)



# Table S6

TS6 <- data.frame(coefficients(summary(f_m_s50dd)), check.names = TRUE)
TS6$Prob_wise <- exp(TS6$Estimate)/(1+exp(TS6$Estimate))

names(TS6)[1] <- 'Odds-wise'
names(TS6)[4] <- "Pr(>|z|)"

TS6 <- TS6 %>% add_significance("Pr(>|z|)")

tab_df(TS6, digits = 3,show.rownames = TRUE)

TS6  <- TS6 %>% relocate(Prob_wise, .before = `Odds-wise`)

TS6  %>% kbl() %>% kable_classic(full_width = F, html_font = "Cambria")



# Plots
np_sg50_dd <- myerror_plot(f_m_s50dd,"NP",c("gray","orange","orange"),'S50$$','sm',"Number of Proposals",c("NP1","NP2","NP3"))
twr_sg50_dd <- my_cont_plot(f_m_s50dd,c("TWR"),c("Orange"),'','',c(0,100),seq(0,100,by=25),"Typical Week Research",mean(dataset$TWR))+theme(axis.text.y=element_blank(), axis.ticks.y=element_blank())
e_sg50_dd <- my_cont_plot(f_m_s50dd,c("E"),c("Skyblue"),'','',c(2,10),seq(2,10,by=2),"Extraversion",mean(dataset$E))+theme(axis.text.y=element_blank(),axis.ticks.y=element_blank())
a_sg50_dd <- my_cont_plot(f_m_s50dd,c("A"),c("skyblue"),'','',c(4,10),seq(4,10,by=2),"Agreeableness",mean(dataset$A))+theme(axis.text.y=element_blank(),axis.ticks.y=element_blank())
                                                                                                                            



figure_4<-ggarrange(np_sg50_dd,
              twr_sg50_dd,
              e_sg50_dd,
              a_sg50_dd,
              nrow = 1,ncol=4)


