# Example plots for README.md
# Jenna Hershberger
# jmh579@cornell.edu
# March 26, 2020
# Updated May 12, 2020

library(tidyverse)
library(ggpubr)
library(LaCroixColoR)
library(wesanderson)

load("./data/ikeogu.2017.rda")

#### Example data distribution plot for README.md ####

# Standalone plot
# ikeogu.2017 %>% dplyr::select(-starts_with("X")) %>% group_by(study.name) %>%
#   gather(trait, value, c(DMC.oven:TCC), na.rm = T) %>%
#   ggplot(aes(x= study.name, y = value, fill = study.name)) +
#   scale_fill_manual(values = lacroix_palette("PeachPear", n = 7, type = "continuous"))+
#   #scale_fill_manual(values=wes_palette(name="Zissou1", n = 7, type = "continuous"), name = "Study") +
#   facet_wrap(~ trait, scales='free_y', nrow=2) +
#   labs(title = "DMC reference value distributions for each example study", x = "Study Name") +
#   geom_violin() + #geom_boxplot(width=0.1) +
#   theme_bw() +
#   theme(legend.position = "none")

dmc.plot <- ikeogu.2017 %>%
  dplyr::select(-starts_with("X")) %>%
  group_by(study.name) %>%
  ggplot(aes(x= study.name, fill = study.name, y = DMC.oven)) +
  scale_fill_manual(values = lacroix_palette("PeachPear", n = 7, type = "continuous"))+
  theme_bw() +
  geom_violin() + geom_boxplot(width = 0.15)

dmc.plot.v <- dmc.plot +
  theme(legend.position = "none", axis.title.x=element_blank(), axis.ticks.x = element_blank()) +
  labs(y = "Root dry matter content (%)")

dmc.plot.h <- dmc.plot +
  theme(legend.position = "none") +
  labs(y = "Root dry matter content (%)", x = "Study")

tcc.plot <- ikeogu.2017 %>%
  dplyr::select(-starts_with("X")) %>%
  group_by(study.name) %>%
  ggplot(aes(x= study.name, y = TCC, fill = study.name)) +
  scale_fill_manual(values = lacroix_palette("PeachPear", n = 7, type = "continuous"))+
  labs(y = expression(paste("Total carotenoid content (", mu, "g/g)")), x = "Study") +
  geom_violin() + geom_boxplot(width = 0.15) +
  theme_bw() +
  theme(legend.position = "none")

reference.distributions.vertical <- ggarrange(dmc.plot.v + rremove("x.text"), tcc.plot ,
                                     labels = c("A", "B"),
                                     ncol = 1, nrow = 2)
ggsave(reference.distributions.vertical,
       filename = "./man/figures/example_ref_dists_v.png", width = 4, height = 7, units = "in", bg = "transparent")

reference.distributions.horizontal <- ggarrange(dmc.plot.h, tcc.plot, labels = c("A", "B"), ncol = 2, nrow = 1)

ggsave(reference.distributions.horizontal,
       filename = "./man/figures/example_ref_dists_h.png", width = 7, height = 3, units = "in", bg = "transparent")






#### Model performance figure for README.md ####
load("./data/ikeogu.2017.rda")
mypalette <- rev(lacroix_palettes$Lemon[1,])

# C16M66.DMC <- ikeogu.2017 %>% filter(study.name == "C16M66") %>% rename(reference = DMC.oven) %>%
#   rename(unique.id = sample.id) %>%
#   dplyr::select(unique.id, reference, starts_with("X")) %>% na.omit()
# test.C16M66.DMC <- TestModelPerformance(train.data = C16M66.DMC, num.iterations = 50, preprocessing = T,
#                                tune.length = 30,
#                                wavelengths = 350:2500)
# write.csv(test.C16M66.DMC, "./data-raw/C16M66_test.csv", row.names = F)

C16Mcal.DMC <- ikeogu.2017 %>% filter(study.name == "C16Mcal") %>% rename(reference = DMC.oven) %>%
  rename(unique.id = sample.id) %>%
  dplyr::select(unique.id, reference, starts_with("X")) %>% na.omit()
C16Mval.DMC <- ikeogu.2017 %>% filter(study.name == "C16Mval") %>% rename(reference = DMC.oven) %>%
  rename(unique.id = sample.id) %>%
  dplyr::select(unique.id, reference, starts_with("X")) %>% na.omit()

test.C16M.DMC <- TestModelPerformance(train.data = C16Mcal.DMC, test.data = C16Mval.DMC,
                               num.iterations = 50, preprocessing = T, output.summary = F,
                               wavelengths = 350:2500)

getmode <- function(vector.input){
  as.matrix(vector.input)
  unique.vector <- unique(vector.input)
  return(unique.vector[which.max(tabulate(match(vector.input,unique.vector)))])
}

test.C16M.DMC$Pretreatment <- factor(test.C16M.DMC$Pretreatment,
                                     levels = unique(test.C16M.DMC$Pretreatment))
test.C16M.DMC.mode <- test.C16M.DMC %>% group_by(Pretreatment) %>% summarize_all(getmode)
test.C16M.DMC.summary <- test.C16M.DMC %>% group_by(Pretreatment) %>%
  summarize_all(mean) %>% dplyr::select(-Iteration)
test.C16M.DMC.summary$Best.ncomp <- test.C16M.DMC.mode$Best.ncomp
write.csv(test.C16M.DMC.summary, "./data-raw/C16M_DMC.csv", row.names = F)
write.csv(test.C16M.DMC, "./data-raw/C16M_DMC_nonsummary.csv", row.names = F)

# testplot.DMC <- test.C16M.DMC %>% group_by(Pretreatment) %>%
#   summarize(`Median RMSE` = median(RMSE)) %>%
#   full_join(test.C16M.DMC) %>%
#   mutate(Pretreatment = recode(Pretreatment,"Raw_data" = "Raw data")) %>%
#   ggplot(aes(y=RMSE, x = Pretreatment, fill = `Median RMSE`)) +
#   geom_boxplot() + labs(title = "Root dry matter content prediction model performance") +
#   scale_fill_gradientn(colors = mypalette) +
#   #  scale_fill_gradientn(colors = wes_palette("Zissou1", type = "continuous")) +
#   #  scale_fill_gradientn(colors = lacroix_palette("PeachPear", type = "continuous"))+
#   theme_minimal() + theme(axis.text.x = element_text(angle = 45,
#                                                      size = 8,
#                                                      hjust = 0.7))
# testplot.DMC
# ggsave(testplot.DMC, filename = "./man/figures/testplot_DMC.png", width = 7, height = 4, units = "in", bg = "transparent")


# TCC
C16Mcal.TCC <- ikeogu.2017 %>% filter(study.name == "C16Mcal") %>% rename(reference = TCC) %>%
  rename(unique.id = sample.id) %>%
  dplyr::select(unique.id, reference, starts_with("X")) %>% na.omit()
C16Mval.TCC <- ikeogu.2017 %>% filter(study.name == "C16Mval") %>% rename(reference = TCC) %>%
  rename(unique.id = sample.id) %>%
  dplyr::select(unique.id, reference, starts_with("X")) %>% na.omit()

test.C16M.TCC <- TestModelPerformance(train.data = C16Mcal.TCC, test.data = C16Mval.TCC,
                                      num.iterations = 50, preprocessing = T, output.summary = F,
                                      wavelengths = 350:2500)

test.C16M.TCC$Pretreatment <- factor(test.C16M.TCC$Pretreatment,
                                     levels = unique(test.C16M.TCC$Pretreatment))
test.C16M.TCC.mode <- test.C16M.TCC %>% group_by(Pretreatment) %>% summarize_all(getmode)
test.C16M.TCC.summary <- test.C16M.TCC %>% group_by(Pretreatment) %>%
  summarize_all(mean) %>% dplyr::select(-Iteration)
test.C16M.TCC.summary$Best.ncomp <- test.C16M.TCC.mode$Best.ncomp
write.csv(test.C16M.TCC.summary, "./data-raw/C16M_TCC.csv", row.names = F)
write.csv(test.C16M.TCC, "./data-raw/C16M_TCC_nonsummary.csv", row.names = F)

# testplot.TCC <- test.C16M.TCC %>% group_by(Pretreatment) %>%
#   summarize(`Median RMSE` = median(RMSE)) %>%
#   full_join(test.C16M.TCC) %>%
#   mutate(Pretreatment = recode(Pretreatment,"Raw_data" = "Raw data")) %>%
#   ggplot(aes(y=RMSE, x = Pretreatment, fill = `Median RMSE`)) +
#   geom_boxplot() + labs(title = "Root dry matter content prediction model performance") +
#   scale_fill_gradientn(colors = mypalette) +
#   #  scale_fill_gradientn(colors = wes_palette("Zissou1", type = "continuous")) +
#   #  scale_fill_gradientn(colors = lacroix_palette("PeachPear", type = "continuous"))+
#   theme_minimal() + theme(axis.text.x = element_text(angle = 45,
#                                                      size = 8,
#                                                      hjust = 0.7))
# ggsave(testplot.TCC, filename = "./man/figures/testplot_TCC.png", width = 7, height = 4, units = "in", bg = "transparent")

test.C16M.DMC$Pheno <- "DMC"
test.C16M.TCC$Pheno <- "TCC"
testplot.joined <- rbind(test.C16M.DMC, test.C16M.TCC) %>%
  mutate(Pretreatment = recode(Pretreatment,"Raw_data" = "Raw data"))
# testplot.all <- testplot.joined %>%
#   group_by(Pretreatment, Pheno) %>%
#   summarize(`Median RMSE` = median(RMSE)) %>%
#   full_join(testplot.joined) %>%
#   ggplot(aes(y=RMSE, x = Pretreatment, fill = Pheno))+#`Median RMSE`)) +
#   geom_boxplot(position = "dodge") +
#   labs(title = "waves prediction model performance", subtitle = "PLSR with C16M dataset from Ikeogu et al., 2017") +
#   scale_fill_manual(values=mypalette[c(2,6)], name = "Phenotype") +
#   theme_minimal() + theme(axis.text.x = element_text(angle = 45,
#                                                      size = 8,
#                                                      hjust = 0.7))
# ggsave(testplot.all, filename = "./man/figures/testplot_all.png", width = 7, height = 5, units = "in", bg = "transparent")


testplot.all.R2 <- testplot.joined %>%
  mutate(Pheno = recode(Pheno,
                         "DMC" = "Root dry matter content",
                         "TCC" = "Total carotenoid content")) %>%
  group_by(Pretreatment, Pheno) %>%
  ggplot(aes(y=R2p, x = Pretreatment, fill = Pheno))+
  geom_boxplot(position = "dodge") +
  labs(y = expression("R"["p"]^2),
       x = "Pretreatment*") +
       #title = "waves prediction model performance",
       #subtitle = "PLSR with C16M datasets from Ikeogu et al. (2017)",
       #) +
  scale_fill_manual(values=mypalette[c(2,6)], name = "Phenotype") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45,
                                                     size = 8,
                                                     hjust = 0.7),
                          legend.position="bottom")
ggsave(testplot.all.R2, filename = "./man/figures/testplot_all_R2.png", width = 7, height = 6, units = "in", bg = "transparent")


# manuscript.plot <- ggarrange(testplot.DMC,
#                              testplot.TCC,
#                              # ggarrange(dmc.plot.h, tcc.plot,
#                              #           labels = c("B", "C"),
#                              #           ncol = 2, nrow = 1),
#                              nrow = 2,
#                              labels = c("A", "B"))
#
# manuscript.plot


