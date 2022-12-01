# results experiment 2 (sentences)

# import libraries
library(lme4)
library(sjPlot)
library(performance)
library(ggplot2)
library(lmerTest)
library(multcomp)
library(papaja)
# import sentences data
df = read.csv("/data/all_subjects_sentences_peak_env.csv")

colnames(df)


# declare speed,trial and subject as factors
df$suj<- as.factor(df$suj_id)
df$trial<- as.factor(df$trial)
df$vitesse2<- as.factor(df$vitesse)

# scale variables
df$acoustic_rate <-scale(df$peak_env , center = TRUE, scale = TRUE)
df$static_lexical_surprise<- scale(df$log_freqlex.s , center = TRUE, scale = TRUE)
df$phonemic_information_rate<- scale(df$phon_fl.s, center = TRUE, scale = TRUE)
df$syllabic_information_rate<- scale(df$syll_fl.s, center = TRUE, scale = TRUE)
df$phonemic_rate<- scale(df$nb_phon.s, center = TRUE, scale = TRUE)
df$syllabic_rate<- scale(df$nb_syll.s, center = TRUE, scale = TRUE)
df$contextual_lexical_surprise<- scale(df$camemBERT.s, center = TRUE, scale = TRUE)

# run the glmm modelwith binomial link function (may take a while depending on hardware) with argument weights
m_sent_all= glmer(scores ~  
                    acoustic_rate +
                    phonemic_rate +
                    syllabic_rate +
                    phonemic_information_rate +
                    syllabic_information_rate +
                    static_lexical_surprise +
                    contextual_lexical_surprise +
                    (1 |vitesse2) +(1|suj), weights=rep(7, length(df[,1])), data=df,family=binomial(link=logit))

# look at the summary and r2 of the model 
summary(m_sent_all)
r2(m_sent_all)

# plot the log ratios
set_theme(
  base = theme_classic(), 
  legend.title.face = "bold", # title font face
  legend.inside = TRUE,         # legend inside plot
  legend.color = "grey50",      # legend label color
  legend.pos = "bottom right",  # legend position inside plot
  axis.title.size = .9,
  axis.textsize = .9,
  legend.size = .7,
  legend.title.size = .8,
  geom.label.size = 3)

plot_model(m_sent_all, vline.color = "grey", sort.est = FALSE, show.values = TRUE, value.offset = .3, title ="sentences all data", transform = NULL) +ylim(-1.5, 0.6) 




# import data
df =read.csv("/data/all_subjects_isolated_words_peak_env.csv")
colnames(df)

# declare speed,trial and subject as factors
df$suj<- as.factor(df$suj_id)
df$trial<- as.factor(df$trial)
df$vitesse2<- as.factor(df$vitesse)

# scale variables
df$acoustic_rate <-scale(df$peak_env , center = TRUE, scale = TRUE)
df$static_lexical_surprise<- scale(df$log.freqlex._s , center = TRUE, scale = TRUE)
df$phonemic_information_rate<- scale(df$phon_fl_s, center = TRUE, scale = TRUE)
df$phonemic_rate<- scale(df$nb_phon_s, center = TRUE, scale = TRUE)
df$syllabic_rate<- scale(df$nb_syll_s, center = TRUE, scale = TRUE)

# run the glmm modelwith binomial link function (may take a while depending on hardware)
m_words_all = glmer(scores ~ 1 + 
                      acoustic_rate + 
                      phonemic_rate +
                      syllabic_rate +
                      phonemic_information_rate +
                      static_lexical_surprise +
                      (1 |vitesse2) +(1|suj), data=df,family=binomial(link=logit) )




tab_model(m_words_all,m_sent_all,transform = NULL,
          
        # pred.labels = c( "Acoustic modulation rate (s-1)", "Phonemic rate (s-1)", "Syllabic rate (s-1)",
          #                "Phonemic information rate (bit.s-1)", "Syllabic information rate (bit.s-1)", 
         #                "Static lexical surprise (bit.s-1)", "Contextual lexical surprise (bit.s-1)"),
          #string.pred = "Coefficient",
          string.ci = " CI (95%)", show.stat = TRUE,
          show.se = TRUE)

glmer(m_sent_all)

# plot effects
plot_model(m_sent_all, type = 'eff',vline.color = "grey", sort.est = FALSE, show.values = TRUE, value.offset = .3)

# check  model 
performance::check_model(m_sent_all)


# post hoc comparisons
colnames(model.matrix(m_sent_all))

oz <- rbind("syllabic_rate vs. contextual_lex_surp" = c(0,0,0,1,0,0,0,-1),
           "contextual_lex_surp vs. static_lex_surp " = c(0,0,0,0,0,0,-1,1),
           "static_lex_surp vs.acoustic mod rate" = c(0,-1,0,0,0,0,1,0),
           "acoustic mod rate vs. syllabic_information_rate" = c(0,1,0,0,0,-1,0,0),
           "syllabic_information_rate vs. phonemic_information_rate"= c(0,0,0,0,-1,1,0,0),
           "phonemic_information_rate vs. phonemic_rate"= c(0,0,-1,0,1,0,0,0) )

summary( glht(m_sent_all,linfct=oz), test = adjusted("holm") )

