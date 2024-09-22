library(ggplot2)
library(tidyr)
library(locits)
library(rstatix)
library(stringr)
library(nlme)
library(lme4)
library(dplyr)
library(multcomp)

setwd("G:\\My Drive\\KL\\SLEAP\\11_16_23_LBAL_Model_Training")
list_of_files <- list(
  "20230619_105703_BR_soc_soc", "20230619_105703_TL_soc_soc", "20230619_105703_TR_soc_soc",
  "20230619_113305_TL_soc_soc", "20230621_100453_BL_soc_soc", "20230621_100453_BR_soc_soc",
  "20230621_100453_TL_soc_soc", "20230621_100453_TR_soc_soc", "20230621_103344_BL_soc_soc",
  "20230621_103344_TL_soc_soc", "20230621_103344_TR_soc_soc", "20230627_091930_BR_soc_soc",
  "20230627_091930_TL_soc_soc", "20230627_091930_TR_soc_soc", "20230627_094040_BL_soc_soc",
  "20230627_094040_BR_soc_soc", "20230627_094040_TR_soc_soc",
  "20230627_100844_TL_soc_soc", "20230627_100844_TR_soc_soc", "20230628_092349_BL_soc_soc",
  "20230628_092349_BR_soc_soc", "20230628_092349_TL_soc_soc", "20230628_092349_TR_soc_soc",
  "20230628_094632_BL_soc_soc", "20230628_094632_BR_soc_soc", "20230628_094632_TL_soc_soc",
  "20230628_094632_TR_soc_soc", "20230628_100719_BL_soc_soc", "20230628_100719_TL_soc_soc",
  "20230619_113305_BL_soc_sol", "20230619_113305_TR_soc_sol", "20230623_101414_BL_soc_sol",
  "20230623_101414_TL_soc_sol", "20230623_101414_TR_soc_sol",
  "20230623_103946_BL_soc_sol", "20230623_103946_BR_soc_sol", "20230623_103946_TL_soc_sol",
  "20230623_103946_TR_soc_sol", "20230623_110124_BL_soc_sol", "20230623_110124_TL_soc_sol",
  "20230620_100614_BL_sol_sol", "20230620_100614_BR_sol_sol", "20230620_100614_TL_sol_sol",
  "20230620_100614_TR_sol_sol", "20230620_102938_BL_sol_sol", "20230620_102938_BR_sol_sol",
  "20230620_102938_TR_sol_sol", "20230622_100409_BL_sol_sol", "20230622_100409_BR_sol_sol",
  "20230622_100409_TL_sol_sol", "20230622_100409_TR_sol_sol", "20230622_110242_BR_sol_sol",
  "20230622_110242_TL_sol_sol", "20230622_110242_TR_sol_sol", "20230622_112700_TL_sol_sol",
  "20230627_100844_BL_sol_sol", "20230627_100844_BR_sol_sol", "20230627_103010_BL_sol_sol",
  "20230627_103010_TL_sol_sol", "20230627_103010_TR_sol_sol"
)

# create data frame of all interactions across all videos
interaction_df <- data.frame()
bout_df <- data.frame()
for (i in list_of_files) {
  full_filename <- paste("interactions\\", i, ".txt", sep = "")
  video_df <- read.delim(full_filename, header = TRUE, sep = "\t", dec = ",")

  degree_filename <- paste("batch_degree\\", i, "_bouts.csv", sep = "")
  degree_df <- read.delim(degree_filename, header = TRUE, sep = ",", dec = ",")
  video_df["Bout"] <- degree_df$Bout

  split_prefix <- strsplit(i, "_")[[1]]
  video_df["Videoname Prefix"] <- rep(paste(split_prefix[1:3], collapse = "_"), times = nrow(video_df))
  video_df["Social Contrast"] <- rep(paste(split_prefix[4:5], collapse = "_"), times = nrow(video_df))

  interaction_df <- rbind(interaction_df, video_df)

  bout_vector <- numeric()
  for (j in 1:15) {
    subset_df <- video_df[video_df$Interaction.Frame >= ((j - 1) * 1200), ]
    subset_df <- subset_df[subset_df$Interaction.Frame < (j * 1200), ]

    bout_vector <- append(bout_vector, length(unique(subset_df$Bout)))
  }

  bout_vector <- append(bout_vector, paste(split_prefix[4:5], collapse = "_"))
  bout_df <- rbind(bout_df, bout_vector)
}

# create plots of total number of interactions every minute (1200 frames) ######
# first, subset the bout data by social contrast
bout_df_soc_soc <- bout_df[bout_df[, 16] == "soc_soc", ]
bout_df_soc_soc <- bout_df_soc_soc[, -16]
bout_df_soc_sol <- bout_df[bout_df[, 16] == "soc_sol", ]
bout_df_soc_sol <- bout_df_soc_sol[, -16]
bout_df_sol_sol <- bout_df[bout_df[, 16] == "sol_sol", ]
bout_df_sol_sol <- bout_df_sol_sol[, -16]

colmeans_soc_soc <- numeric()
colmeans_soc_sol <- numeric()
colmeans_sol_sol <- numeric()
for (i in 1:15) {
  colmeans_soc_soc <- append(colmeans_soc_soc, mean(as.numeric(bout_df_soc_soc[, i])))
  colmeans_soc_sol <- append(colmeans_soc_sol, mean(as.numeric(bout_df_soc_sol[, i])))
  colmeans_sol_sol <- append(colmeans_sol_sol, mean(as.numeric(bout_df_sol_sol[, i])))
}

# second, create line graphs
plot_df <- data.frame(time = c(1:30))

colors <- c(
  "soc_soc (n = 30)" = "darkred",
  "soc_sol (n = 12)" = "steelblue",
  "sol_sol (n = 20)" = "darkgreen"
)

ggplot(data = plot_df, aes(x = time)) +
  geom_jitter(aes(y = zeropad(as.numeric(bout_df_soc_soc[1, ])), color = "soc_soc (n = 30)", width = 0.15, height = 0)) +
  geom_jitter(aes(y = zeropad(as.numeric(bout_df_soc_soc[2, ])), color = "soc_soc (n = 30)", width = 0.15, height = 0)) +
  geom_jitter(aes(y = zeropad(as.numeric(bout_df_soc_soc[3, ])), color = "soc_soc (n = 30)", width = 0.15, height = 0)) +
  geom_jitter(aes(y = zeropad(as.numeric(bout_df_soc_soc[4, ])), color = "soc_soc (n = 30)", width = 0.15, height = 0)) +
  geom_jitter(aes(y = zeropad(as.numeric(bout_df_soc_soc[5, ])), color = "soc_soc (n = 30)", width = 0.15, height = 0)) +
  geom_jitter(aes(y = zeropad(as.numeric(bout_df_soc_soc[6, ])), color = "soc_soc (n = 30)", width = 0.15, height = 0)) +
  geom_jitter(aes(y = zeropad(as.numeric(bout_df_soc_soc[7, ])), color = "soc_soc (n = 30)", width = 0.15, height = 0)) +
  geom_jitter(aes(y = zeropad(as.numeric(bout_df_soc_soc[8, ])), color = "soc_soc (n = 30)", width = 0.15, height = 0)) +
  geom_jitter(aes(y = zeropad(as.numeric(bout_df_soc_soc[9, ])), color = "soc_soc (n = 30)", width = 0.15, height = 0)) +
  geom_jitter(aes(y = zeropad(as.numeric(bout_df_soc_soc[10, ])), color = "soc_soc (n = 30)", width = 0.15, height = 0)) +
  geom_jitter(aes(y = zeropad(as.numeric(bout_df_soc_soc[11, ])), color = "soc_soc (n = 30)", width = 0.15, height = 0)) +
  geom_jitter(aes(y = zeropad(as.numeric(bout_df_soc_soc[12, ])), color = "soc_soc (n = 30)", width = 0.15, height = 0)) +
  geom_jitter(aes(y = zeropad(as.numeric(bout_df_soc_soc[13, ])), color = "soc_soc (n = 30)", width = 0.15, height = 0)) +
  geom_jitter(aes(y = zeropad(as.numeric(bout_df_soc_soc[14, ])), color = "soc_soc (n = 30)", width = 0.15, height = 0)) +
  geom_jitter(aes(y = zeropad(as.numeric(bout_df_soc_soc[15, ])), color = "soc_soc (n = 30)", width = 0.15, height = 0)) +
  geom_jitter(aes(y = zeropad(as.numeric(bout_df_soc_soc[16, ])), color = "soc_soc (n = 30)", width = 0.15, height = 0)) +
  geom_jitter(aes(y = zeropad(as.numeric(bout_df_soc_soc[17, ])), color = "soc_soc (n = 30)", width = 0.15, height = 0)) +
  geom_jitter(aes(y = zeropad(as.numeric(bout_df_soc_soc[18, ])), color = "soc_soc (n = 30)", width = 0.15, height = 0)) +
  geom_jitter(aes(y = zeropad(as.numeric(bout_df_soc_soc[19, ])), color = "soc_soc (n = 30)", width = 0.15, height = 0)) +
  geom_jitter(aes(y = zeropad(as.numeric(bout_df_soc_soc[20, ])), color = "soc_soc (n = 30)", width = 0.15, height = 0)) +
  geom_jitter(aes(y = zeropad(as.numeric(bout_df_soc_soc[21, ])), color = "soc_soc (n = 30)", width = 0.15, height = 0)) +
  geom_jitter(aes(y = zeropad(as.numeric(bout_df_soc_soc[22, ])), color = "soc_soc (n = 30)", width = 0.15, height = 0)) +
  geom_jitter(aes(y = zeropad(as.numeric(bout_df_soc_soc[23, ])), color = "soc_soc (n = 30)", width = 0.15, height = 0)) +
  geom_jitter(aes(y = zeropad(as.numeric(bout_df_soc_soc[24, ])), color = "soc_soc (n = 30)", width = 0.15, height = 0)) +
  geom_jitter(aes(y = zeropad(as.numeric(bout_df_soc_soc[25, ])), color = "soc_soc (n = 30)", width = 0.15, height = 0)) +
  geom_jitter(aes(y = zeropad(as.numeric(bout_df_soc_soc[26, ])), color = "soc_soc (n = 30)", width = 0.15, height = 0)) +
  geom_jitter(aes(y = zeropad(as.numeric(bout_df_soc_soc[27, ])), color = "soc_soc (n = 30)", width = 0.15, height = 0)) +
  geom_jitter(aes(y = zeropad(as.numeric(bout_df_soc_soc[28, ])), color = "soc_soc (n = 30)", width = 0.15, height = 0)) +
  geom_jitter(aes(y = zeropad(as.numeric(bout_df_soc_soc[29, ])), color = "soc_soc (n = 30)", width = 0.15, height = 0)) +
  geom_smooth(aes(y = rep(colmeans_soc_soc, each = 2)), color = "darkred", fill = "darkred", alpha = 0.3) +
  geom_jitter(aes(y = zeropad(as.numeric(bout_df_soc_sol[1, ])), color = "soc_sol (n = 12)", width = 0.15, height = 0)) +
  geom_jitter(aes(y = zeropad(as.numeric(bout_df_soc_sol[2, ])), color = "soc_sol (n = 12)", width = 0.15, height = 0)) +
  geom_jitter(aes(y = zeropad(as.numeric(bout_df_soc_sol[3, ])), color = "soc_sol (n = 12)", width = 0.15, height = 0)) +
  geom_jitter(aes(y = zeropad(as.numeric(bout_df_soc_sol[4, ])), color = "soc_sol (n = 12)", width = 0.15, height = 0)) +
  geom_jitter(aes(y = zeropad(as.numeric(bout_df_soc_sol[5, ])), color = "soc_sol (n = 12)", width = 0.15, height = 0)) +
  geom_jitter(aes(y = zeropad(as.numeric(bout_df_soc_sol[6, ])), color = "soc_sol (n = 12)", width = 0.15, height = 0)) +
  geom_jitter(aes(y = zeropad(as.numeric(bout_df_soc_sol[7, ])), color = "soc_sol (n = 12)", width = 0.15, height = 0)) +
  geom_jitter(aes(y = zeropad(as.numeric(bout_df_soc_sol[8, ])), color = "soc_sol (n = 12)", width = 0.15, height = 0)) +
  geom_jitter(aes(y = zeropad(as.numeric(bout_df_soc_sol[9, ])), color = "soc_sol (n = 12)", width = 0.15, height = 0)) +
  geom_jitter(aes(y = zeropad(as.numeric(bout_df_soc_sol[10, ])), color = "soc_sol (n = 12)", width = 0.15, height = 0)) +
  geom_jitter(aes(y = zeropad(as.numeric(bout_df_soc_sol[11, ])), color = "soc_sol (n = 12)", width = 0.15, height = 0)) +
  geom_smooth(aes(y = rep(colmeans_soc_sol, each = 2)), color = "steelblue", fill = "steelblue", alpha = 0.3) +
  geom_jitter(aes(y = zeropad(as.numeric(bout_df_sol_sol[1, ])), color = "sol_sol (n = 20)", width = 0.15, height = 0)) +
  geom_jitter(aes(y = zeropad(as.numeric(bout_df_sol_sol[2, ])), color = "sol_sol (n = 20)", width = 0.15, height = 0)) +
  geom_jitter(aes(y = zeropad(as.numeric(bout_df_sol_sol[3, ])), color = "sol_sol (n = 20)", width = 0.15, height = 0)) +
  geom_jitter(aes(y = zeropad(as.numeric(bout_df_sol_sol[4, ])), color = "sol_sol (n = 20)", width = 0.15, height = 0)) +
  geom_jitter(aes(y = zeropad(as.numeric(bout_df_sol_sol[5, ])), color = "sol_sol (n = 20)", width = 0.15, height = 0)) +
  geom_jitter(aes(y = zeropad(as.numeric(bout_df_sol_sol[6, ])), color = "sol_sol (n = 20)", width = 0.15, height = 0)) +
  geom_jitter(aes(y = zeropad(as.numeric(bout_df_sol_sol[7, ])), color = "sol_sol (n = 20)", width = 0.15, height = 0)) +
  geom_jitter(aes(y = zeropad(as.numeric(bout_df_sol_sol[8, ])), color = "sol_sol (n = 20)", width = 0.15, height = 0)) +
  geom_jitter(aes(y = zeropad(as.numeric(bout_df_sol_sol[9, ])), color = "sol_sol (n = 20)", width = 0.15, height = 0)) +
  geom_jitter(aes(y = zeropad(as.numeric(bout_df_sol_sol[10, ])), color = "sol_sol (n = 20)", width = 0.15, height = 0)) +
  geom_jitter(aes(y = zeropad(as.numeric(bout_df_sol_sol[11, ])), color = "sol_sol (n = 20)", width = 0.15, height = 0)) +
  geom_jitter(aes(y = zeropad(as.numeric(bout_df_sol_sol[12, ])), color = "sol_sol (n = 20)", width = 0.15, height = 0)) +
  geom_jitter(aes(y = zeropad(as.numeric(bout_df_sol_sol[13, ])), color = "sol_sol (n = 20)", width = 0.15, height = 0)) +
  geom_jitter(aes(y = zeropad(as.numeric(bout_df_sol_sol[14, ])), color = "sol_sol (n = 20)", width = 0.15, height = 0)) +
  geom_jitter(aes(y = zeropad(as.numeric(bout_df_sol_sol[15, ])), color = "sol_sol (n = 20)", width = 0.15, height = 0)) +
  geom_jitter(aes(y = zeropad(as.numeric(bout_df_sol_sol[16, ])), color = "sol_sol (n = 20)", width = 0.15, height = 0)) +
  geom_jitter(aes(y = zeropad(as.numeric(bout_df_sol_sol[17, ])), color = "sol_sol (n = 20)", width = 0.15, height = 0)) +
  geom_jitter(aes(y = zeropad(as.numeric(bout_df_sol_sol[18, ])), color = "sol_sol (n = 20)", width = 0.15, height = 0)) +
  geom_jitter(aes(y = zeropad(as.numeric(bout_df_sol_sol[19, ])), color = "sol_sol (n = 20)", width = 0.15, height = 0)) +
  geom_jitter(aes(y = zeropad(as.numeric(bout_df_sol_sol[20, ])), color = "sol_sol (n = 20)", width = 0.15, height = 0)) +
  geom_smooth(aes(y = rep(colmeans_sol_sol, each = 2)), color = "darkgreen", fill = "darkgreen", alpha = 0.3) +
  scale_color_manual(name = "Legend", values = colors) +
  coord_cartesian(ylim = c(0, 10)) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(limit = c(0, 10), oob = scales::squish) +
  ggtitle("Total Interactions Per Minute") +
  labs(x = "Time (minutes)", y = "Total Interactions")

# third, perform Kruskal-Wallis test
colmeans_soc_soc_df <- data.frame(colmeans_soc_soc, rep("soc_soc", times = length(colmeans_soc_soc)))
colmeans_soc_sol_df <- data.frame(colmeans_soc_sol, rep("soc_sol", times = length(colmeans_soc_sol)))
colmeans_sol_sol_df <- data.frame(colmeans_sol_sol, rep("sol_sol", times = length(colmeans_sol_sol)))
colmeans_soc_soc_df <- colmeans_soc_soc_df[-nrow(colmeans_soc_soc_df), ]
colmeans_soc_sol_df <- colmeans_soc_sol_df[-nrow(colmeans_soc_sol_df), ]
colmeans_sol_sol_df <- colmeans_sol_sol_df[-nrow(colmeans_sol_sol_df), ]
colnames(colmeans_soc_soc_df) <- c("Bouts", "Social_Contrast")
colnames(colmeans_soc_sol_df) <- c("Bouts", "Social_Contrast")
colnames(colmeans_sol_sol_df) <- c("Bouts", "Social_Contrast")

stats_df <- rbind(colmeans_soc_soc_df, colmeans_soc_sol_df, colmeans_sol_sol_df)

print(kruskal.test(Bouts ~ Social_Contrast, data = stats_df))
print(dunn_test(Bouts ~ Social_Contrast, data = stats_df))

# create plots of total number of interactions every 5 minutes (6000 frames) ###
# first, subset the data by social contrast
soc_soc_df <- interaction_df[interaction_df$"Social Contrast" == "soc_soc", ]
soc_sol_df <- interaction_df[interaction_df$"Social Contrast" == "soc_sol", ]
sol_sol_df <- interaction_df[interaction_df$"Social Contrast" == "sol_sol", ]

# second, bin the data and store total # of interactions per videos per bin
total_bouts_soc_soc1 <- numeric()
total_bouts_soc_sol1 <- numeric()
total_bouts_sol_sol1 <- numeric()
total_bouts_soc_soc2 <- numeric()
total_bouts_soc_sol2 <- numeric()
total_bouts_sol_sol2 <- numeric()
total_bouts_soc_soc3 <- numeric()
total_bouts_soc_sol3 <- numeric()
total_bouts_sol_sol3 <- numeric()
for (i in 1:3) {
  soc_soc_subset_df <- soc_soc_df[soc_soc_df$Interaction.Frame >= ((i - 1) * 6000), ]
  soc_soc_subset_df <- soc_soc_subset_df[soc_soc_subset_df$Interaction.Frame < (i * 6000), ]


  soc_sol_subset_df <- soc_sol_df[soc_sol_df$Interaction.Frame >= ((i - 1) * 6000), ]
  soc_sol_subset_df <- soc_sol_subset_df[soc_sol_subset_df$Interaction.Frame < (i * 6000), ]


  sol_sol_subset_df <- sol_sol_df[sol_sol_df$Interaction.Frame >= ((i - 1) * 6000), ]
  sol_sol_subset_df <- sol_sol_subset_df[sol_sol_subset_df$Interaction.Frame < (i * 6000), ]

  for (j in unique(soc_soc_df$`Videoname Prefix`)) {
    subset_df <- soc_soc_subset_df[soc_soc_subset_df$`Videoname Prefix` == j, ]

    if (i == 1) {
      total_bouts_soc_soc1 <- append(total_bouts_soc_soc1, length(unique(subset_df$Bout)))
    } else if (i == 2) {
      total_bouts_soc_soc2 <- append(total_bouts_soc_soc2, length(unique(subset_df$Bout)))
    } else {
      total_bouts_soc_soc3 <- append(total_bouts_soc_soc3, length(unique(subset_df$Bout)))
    }
  }

  for (j in unique(soc_sol_df$`Videoname Prefix`)) {
    subset_df <- soc_sol_subset_df[soc_sol_subset_df$`Videoname Prefix` == j, ]

    if (i == 1) {
      total_bouts_soc_sol1 <- append(total_bouts_soc_sol1, length(unique(subset_df$Bout)))
    } else if (i == 2) {
      total_bouts_soc_sol2 <- append(total_bouts_soc_sol2, length(unique(subset_df$Bout)))
    } else {
      total_bouts_soc_sol3 <- append(total_bouts_soc_sol3, length(unique(subset_df$Bout)))
    }
  }

  for (j in unique(sol_sol_df$`Videoname Prefix`)) {
    subset_df <- sol_sol_subset_df[sol_sol_subset_df$`Videoname Prefix` == j, ]

    if (i == 1) {
      total_bouts_sol_sol1 <- append(total_bouts_sol_sol1, length(unique(subset_df$Bout)))
    } else if (i == 2) {
      total_bouts_sol_sol2 <- append(total_bouts_sol_sol2, length(unique(subset_df$Bout)))
    } else {
      total_bouts_sol_sol3 <- append(total_bouts_sol_sol3, length(unique(subset_df$Bout)))
    }
  }
}

# third, create box plots
max_length <- max(
  length(total_bouts_soc_soc1), length(total_bouts_soc_sol1), length(total_bouts_sol_sol1),
  length(total_bouts_soc_soc2), length(total_bouts_soc_sol2), length(total_bouts_sol_sol2),
  length(total_bouts_soc_soc3), length(total_bouts_soc_sol3), length(total_bouts_sol_sol3)
)

total_bouts_soc_soc1 <- c(total_bouts_soc_soc1, rep(NA, max_length - length(total_bouts_soc_soc1)))
total_bouts_soc_sol1 <- c(total_bouts_soc_sol1, rep(NA, max_length - length(total_bouts_soc_sol1)))
total_bouts_sol_sol1 <- c(total_bouts_sol_sol1, rep(NA, max_length - length(total_bouts_sol_sol1)))
total_bouts_soc_soc2 <- c(total_bouts_soc_soc2, rep(NA, max_length - length(total_bouts_soc_soc2)))
total_bouts_soc_sol2 <- c(total_bouts_soc_sol2, rep(NA, max_length - length(total_bouts_soc_sol2)))
total_bouts_sol_sol2 <- c(total_bouts_sol_sol2, rep(NA, max_length - length(total_bouts_sol_sol2)))
total_bouts_soc_soc3 <- c(total_bouts_soc_soc3, rep(NA, max_length - length(total_bouts_soc_soc3)))
total_bouts_soc_sol3 <- c(total_bouts_soc_sol3, rep(NA, max_length - length(total_bouts_soc_sol3)))
total_bouts_sol_sol3 <- c(total_bouts_sol_sol3, rep(NA, max_length - length(total_bouts_sol_sol3)))

plot_df <- data.frame(
  soc_soc1 = total_bouts_soc_soc1,
  soc_sol1 = total_bouts_soc_sol1,
  sol_sol1 = total_bouts_sol_sol1,
  soc_soc2 = total_bouts_soc_soc2,
  soc_sol2 = total_bouts_soc_sol2,
  sol_sol2 = total_bouts_sol_sol2,
  soc_soc3 = total_bouts_soc_soc3,
  soc_sol3 = total_bouts_soc_sol3,
  sol_sol3 = total_bouts_sol_sol3
)

colors <- c(
  "soc_soc (n = 30)" = "darkred",
  "soc_sol (n = 12)" = "steelblue",
  "sol_sol (n = 20)" = "darkgreen"
)

ggplot(data = plot_df) +
  geom_boxplot(aes(x = 1, y = soc_soc1, fill = "soc_soc (n = 30)"), alpha = 0.5, width = 0.4, outlier.shape = NA) +
  geom_boxplot(aes(x = 1.5, y = soc_sol1, fill = "soc_sol (n = 12)"), alpha = 0.5, width = 0.4, outlier.shape = NA) +
  geom_boxplot(aes(x = 2, y = sol_sol1, fill = "sol_sol (n = 20)"), alpha = 0.5, width = 0.4, outlier.shape = NA) +
  geom_boxplot(aes(x = 3, y = soc_soc2, fill = "soc_soc (n = 30)"), alpha = 0.5, width = 0.4, outlier.shape = NA) +
  geom_boxplot(aes(x = 3.5, y = soc_sol2, fill = "soc_sol (n = 12)"), alpha = 0.5, width = 0.4, outlier.shape = NA) +
  geom_boxplot(aes(x = 4, y = sol_sol2, fill = "sol_sol (n = 20)"), alpha = 0.5, width = 0.4, outlier.shape = NA) +
  geom_boxplot(aes(x = 5, y = soc_soc3, fill = "soc_soc (n = 30)"), alpha = 0.5, width = 0.4, outlier.shape = NA) +
  geom_boxplot(aes(x = 5.5, y = soc_sol3, fill = "soc_sol (n = 12)"), alpha = 0.5, width = 0.4, outlier.shape = NA) +
  geom_boxplot(aes(x = 6, y = sol_sol3, fill = "sol_sol (n = 20)"), alpha = 0.5, width = 0.4, outlier.shape = NA) +
  geom_jitter(aes(x = 1, y = soc_soc1, colour = "soc_soc (n = 30)"), alpha = 0.5, width = 0.2) +
  geom_jitter(aes(x = 1.5, y = soc_sol1, colour = "soc_sol (n = 12)"), alpha = 0.5, width = 0.2) +
  geom_jitter(aes(x = 2, y = sol_sol1, colour = "sol_sol (n = 20)"), alpha = 0.5, width = 0.2) +
  geom_jitter(aes(x = 3, y = soc_soc2, colour = "soc_soc (n = 30)"), alpha = 0.5, width = 0.2) +
  geom_jitter(aes(x = 3.5, y = soc_sol2, colour = "soc_sol (n = 12)"), alpha = 0.5, width = 0.2) +
  geom_jitter(aes(x = 4, y = sol_sol2, colour = "sol_sol (n = 20)"), alpha = 0.5, width = 0.2) +
  geom_jitter(aes(x = 5, y = soc_soc3, colour = "soc_soc (n = 30)"), alpha = 0.5, width = 0.2) +
  geom_jitter(aes(x = 5.5, y = soc_sol3, colour = "soc_sol (n = 12)"), alpha = 0.5, width = 0.2) +
  geom_jitter(aes(x = 6, y = sol_sol3, colour = "sol_sol (n = 20)"), alpha = 0.5, width = 0.2) +
  scale_color_manual(name = "Point Legend", values = colors) +
  scale_fill_manual(name = "Boxplot Legend", values = colors) +
  ggtitle("Total Interactions Every 5 Minutes") +
  labs(x = "Portion of Video (5 minute Increments)", y = "Total Interactions") +
  scale_x_continuous(breaks = c(1.5, 3.5, 5.5), labels = c(5, 10, 15)) +
  geom_vline(xintercept = c(2.5, 4.5), linetype = "dashed", color = "black")

# fourth, perform Kruskal-Wallis test
soc_soc1_df <- data.frame(total_bouts_soc_soc1, rep("soc_soc", times = length(total_bouts_soc_soc1)))
soc_sol1_df <- data.frame(total_bouts_soc_sol1, rep("soc_sol", times = length(total_bouts_soc_sol1)))
sol_sol1_df <- data.frame(total_bouts_sol_sol1, rep("sol_sol", times = length(total_bouts_sol_sol1)))
soc_soc2_df <- data.frame(total_bouts_soc_soc2, rep("soc_soc", times = length(total_bouts_soc_soc2)))
soc_sol2_df <- data.frame(total_bouts_soc_sol2, rep("soc_sol", times = length(total_bouts_soc_sol2)))
sol_sol2_df <- data.frame(total_bouts_sol_sol2, rep("sol_sol", times = length(total_bouts_sol_sol2)))
soc_soc3_df <- data.frame(total_bouts_soc_soc3, rep("soc_soc", times = length(total_bouts_soc_soc3)))
soc_sol3_df <- data.frame(total_bouts_soc_sol3, rep("soc_sol", times = length(total_bouts_soc_sol3)))
sol_sol3_df <- data.frame(total_bouts_sol_sol3, rep("sol_sol", times = length(total_bouts_sol_sol3)))
colnames(soc_soc1_df) <- c("Bouts", "Social_Contrast")
colnames(soc_sol1_df) <- c("Bouts", "Social_Contrast")
colnames(sol_sol1_df) <- c("Bouts", "Social_Contrast")
colnames(soc_soc2_df) <- c("Bouts", "Social_Contrast")
colnames(soc_sol2_df) <- c("Bouts", "Social_Contrast")
colnames(sol_sol2_df) <- c("Bouts", "Social_Contrast")
colnames(soc_soc3_df) <- c("Bouts", "Social_Contrast")
colnames(soc_sol3_df) <- c("Bouts", "Social_Contrast")
colnames(sol_sol3_df) <- c("Bouts", "Social_Contrast")

stats_df1 <- rbind(soc_soc1_df, soc_sol1_df, sol_sol1_df)
stats_df2 <- rbind(soc_soc2_df, soc_sol2_df, sol_sol2_df)
stats_df3 <- rbind(soc_soc3_df, soc_sol3_df, sol_sol3_df)

print(kruskal.test(Bouts ~ Social_Contrast, data = stats_df1))
print(dunn_test(Bouts ~ Social_Contrast, data = stats_df1))

print(kruskal.test(Bouts ~ Social_Contrast, data = stats_df2))
print(dunn_test(Bouts ~ Social_Contrast, data = stats_df2))

print(kruskal.test(Bouts ~ Social_Contrast, data = stats_df3))
print(dunn_test(Bouts ~ Social_Contrast, data = stats_df3))

# create plots of average duration of interactions #############################
# first, create vectors of average duration of interactions per social contrast
avg_dur_soc_soc <- numeric()
avg_dur_soc_sol <- numeric()
avg_dur_sol_sol <- numeric()
for (i in list_of_files) {
  full_filename <- paste("batch_degree\\", i, "_bouts.csv", sep = "")
  video_df <- read.delim(full_filename, header = TRUE, sep = ",", dec = ",")

  dur <- nrow(video_df) / length(unique(video_df$Bout))
  split_prefix <- strsplit(i, "_")[[1]]
  contrast <- paste(split_prefix[4:5], collapse = "_")
  if (contrast == "soc_soc") {
    avg_dur_soc_soc <- append(avg_dur_soc_soc, dur)
  } else if (contrast == "soc_sol") {
    avg_dur_soc_sol <- append(avg_dur_soc_sol, dur)
  } else {
    avg_dur_sol_sol <- append(avg_dur_sol_sol, dur)
  }
}

# second, make all vectors the same length
max_length <- max(length(avg_dur_soc_soc), length(avg_dur_soc_sol), length(avg_dur_sol_sol))

avg_dur_soc_soc <- c(avg_dur_soc_soc, rep(NA, max_length - length(avg_dur_soc_soc)))
avg_dur_soc_sol <- c(avg_dur_soc_sol, rep(NA, max_length - length(avg_dur_soc_sol)))
avg_dur_sol_sol <- c(avg_dur_sol_sol, rep(NA, max_length - length(avg_dur_sol_sol)))

# third, create box plots
plot_df <- data.frame(
  soc_soc = avg_dur_soc_soc / 20,
  soc_sol = avg_dur_soc_sol / 20,
  sol_sol = avg_dur_sol_sol / 20
)

colors <- c(
  "soc_soc (n = 30)" = "darkred",
  "soc_sol (n = 12)" = "steelblue",
  "sol_sol (n = 20)" = "darkgreen"
)

ggplot(data = plot_df) +
  geom_boxplot(aes(x = 1, y = soc_soc, fill = "soc_soc (n = 30)"), alpha = 0.5, width = 0.4, outlier.shape = NA) +
  geom_boxplot(aes(x = 2, y = soc_sol, fill = "soc_sol (n = 12)"), alpha = 0.5, width = 0.4, outlier.shape = NA) +
  geom_boxplot(aes(x = 3, y = sol_sol, fill = "sol_sol (n = 20)"), alpha = 0.5, width = 0.4, outlier.shape = NA) +
  geom_jitter(aes(x = 1, y = soc_soc, colour = "soc_soc (n = 30)"), alpha = 0.5, width = 0.2) +
  geom_jitter(aes(x = 2, y = soc_sol, colour = "soc_sol (n = 12)"), alpha = 0.5, width = 0.2) +
  geom_jitter(aes(x = 3, y = sol_sol, colour = "sol_sol (n = 20)"), alpha = 0.5, width = 0.2) +
  scale_color_manual(name = "Point Legend", values = colors) +
  scale_fill_manual(name = "Boxplot Legend", values = colors) +
  ggtitle("Avg Interaction Duration Per Social Contrast") +
  labs(x = "Social Contrast", y = "Avg Interaction Duration (seconds)") +
  scale_x_continuous(breaks = c(1, 2, 3), labels = c("soc_soc", "soc_sol", "sol_sol"))

# fourth, perform Kruskal-Wallis test
avg_dur_soc_soc_df <- data.frame(avg_dur_soc_soc, rep("soc_soc", times = length(avg_dur_soc_soc)))
avg_dur_soc_sol_df <- data.frame(avg_dur_soc_sol, rep("soc_sol", times = length(avg_dur_soc_sol)))
avg_dur_sol_sol_df <- data.frame(avg_dur_sol_sol, rep("sol_sol", times = length(avg_dur_sol_sol)))
colnames(avg_dur_soc_soc_df) <- c("Durs", "Social_Contrast")
colnames(avg_dur_soc_sol_df) <- c("Durs", "Social_Contrast")
colnames(avg_dur_sol_sol_df) <- c("Durs", "Social_Contrast")

stats_df <- rbind(avg_dur_soc_soc_df, avg_dur_soc_sol_df, avg_dur_sol_sol_df)

print(kruskal.test(Durs ~ Social_Contrast, data = stats_df))
print(dunn_test(Durs ~ Social_Contrast, data = stats_df))

# create plots of average velocities of each social contrast ###################
# first, store data for each social contrast
velocity_df_soc_soc <- data.frame(matrix(NA, nrow = 17998, ncol = 1))
velocity_df_soc_sol <- data.frame(matrix(NA, nrow = 17998, ncol = 1))
velocity_df_sol_sol <- data.frame(matrix(NA, nrow = 17998, ncol = 1))
for (i in unique(soc_soc_df$`Videoname Prefix`)) {
  full_filename <- paste("velocities\\", i, "_soc_soc_vels.csv", sep = "")
  video_df <- read.csv(full_filename, header = TRUE, sep = ",")
  video_df <- video_df[, !colnames(video_df) == "X"]

  if (is.null(colnames(video_df))) next

  velocity_df_soc_soc <- cbind(velocity_df_soc_soc, video_df)
}

for (i in unique(soc_sol_df$`Videoname Prefix`)) {
  full_filename <- paste("velocities\\", i, "_soc_sol_vels.csv", sep = "")
  video_df <- read.csv(full_filename, header = TRUE, sep = ",")
  video_df <- video_df[, !colnames(video_df) == "X"]

  if (is.null(colnames(video_df))) next

  velocity_df_soc_sol <- cbind(velocity_df_soc_sol, video_df)
}

for (i in unique(sol_sol_df$`Videoname Prefix`)) {
  full_filename <- paste("velocities\\", i, "_sol_sol_vels.csv", sep = "")
  video_df <- read.csv(full_filename, header = TRUE, sep = ",")
  video_df <- video_df[, !colnames(video_df) == "X"]

  if (is.null(colnames(video_df))) next

  velocity_df_sol_sol <- cbind(velocity_df_sol_sol, video_df)
}

# second, remove all NAs and average velocities across videos
velocity_df_soc_soc <- velocity_df_soc_soc[-(1:199), -1]
velocity_df_soc_sol <- velocity_df_soc_sol[-(1:199), -1]
velocity_df_sol_sol <- velocity_df_sol_sol[-(1:199), -1]

velocity_df_soc_soc[is.na(velocity_df_soc_soc)] <- 0
velocity_df_soc_sol[is.na(velocity_df_soc_sol)] <- 0
velocity_df_sol_sol[is.na(velocity_df_sol_sol)] <- 0

avg_vels_soc_soc <- rowMeans(velocity_df_soc_soc)
avg_vels_soc_sol <- rowMeans(velocity_df_soc_sol)
avg_vels_sol_sol <- rowMeans(velocity_df_sol_sol)

# third, create line graphs
plot_df <- data.frame(
  time = c(seq_along(avg_vels_soc_soc) / 1200),
  soc_soc = avg_vels_soc_soc * 0.0946 * 20,
  soc_sol = avg_vels_soc_sol * 0.0946 * 20,
  sol_sol = avg_vels_sol_sol * 0.0946 * 20
)

colors <- c(
  "soc_soc (n = 30)" = "darkred",
  "soc_sol (n = 12)" = "steelblue",
  "sol_sol (n = 20)" = "darkgreen"
)

# pixel to mm conversion: 56 mm/592 pixels = 0.0946 mm/pixel
ggplot(data = plot_df, aes(x = time)) +
  geom_line(aes(y = soc_soc, color = "soc_soc (n = 30)")) +
  geom_line(aes(y = soc_sol, color = "soc_sol (n = 12)"), linetype = "dotted") +
  geom_line(aes(y = sol_sol, color = "sol_sol (n = 20)"), linetype = "longdash") +
  scale_color_manual(values = colors) +
  coord_cartesian(ylim = c(0, 60)) +
  scale_x_continuous(expand = c(0, 0)) +
  ggtitle("Avg Velocities Per Minute") +
  labs(x = "Time (minutes)", y = "Average Velocity (mm/s)")

# create plots of average velocities every 5 minutes (6000 frames) #############
# first, average across bees per video, then per 5 minute interval
bee_avg_soc_soc <- data.frame(columns = (rep("temp", nrow(velocity_df_soc_soc))))
bee_avg_soc_sol <- data.frame(columns = (rep("temp", nrow(velocity_df_soc_sol))))
bee_avg_sol_sol <- data.frame(columns = (rep("temp", nrow(velocity_df_sol_sol))))
for (i in 1:(ncol(velocity_df_soc_soc) / 2)) {
  bee_avg_soc_soc[, i] <- rowMeans(velocity_df_soc_soc[, ((i * 2) - 1):(i * 2)])
}

for (i in 1:(ncol(velocity_df_soc_sol) / 2)) {
  bee_avg_soc_sol[, i] <- rowMeans(velocity_df_soc_sol[, ((i * 2) - 1):(i * 2)])
}

for (i in 1:(ncol(velocity_df_sol_sol) / 2)) {
  bee_avg_sol_sol[, i] <- rowMeans(velocity_df_sol_sol[, ((i * 2) - 1):(i * 2)])
}

bee_avg_soc_soc <- bee_avg_soc_soc[, -1]
bee_avg_soc_sol <- bee_avg_soc_sol[, -1]
bee_avg_sol_sol <- bee_avg_sol_sol[, -1]

avg_vels_soc_soc1 <- colMeans(bee_avg_soc_soc[1:5799, ])
avg_vels_soc_sol1 <- colMeans(bee_avg_soc_sol[1:5799, ])
avg_vels_sol_sol1 <- colMeans(bee_avg_sol_sol[1:5799, ])
avg_vels_soc_soc2 <- colMeans(bee_avg_soc_soc[6001:11799, ])
avg_vels_soc_sol2 <- colMeans(bee_avg_soc_sol[6001:11799, ])
avg_vels_sol_sol2 <- colMeans(bee_avg_sol_sol[6001:11799, ])
avg_vels_soc_soc3 <- colMeans(bee_avg_soc_soc[12001:17799, ])
avg_vels_soc_sol3 <- colMeans(bee_avg_soc_sol[12001:17799, ])
avg_vels_sol_sol3 <- colMeans(bee_avg_sol_sol[12001:17799, ])

# second, make all average velocity vectors the same length
max_length <- max(
  length(avg_vels_soc_soc1), length(avg_vels_soc_sol1), length(avg_vels_sol_sol1),
  length(avg_vels_soc_soc2), length(avg_vels_soc_sol2), length(avg_vels_sol_sol2),
  length(avg_vels_soc_soc3), length(avg_vels_soc_sol3), length(avg_vels_sol_sol3)
)

avg_vels_soc_soc1 <- c(avg_vels_soc_soc1, rep(NA, max_length - length(avg_vels_soc_soc1)))
avg_vels_soc_sol1 <- c(avg_vels_soc_sol1, rep(NA, max_length - length(avg_vels_soc_sol1)))
avg_vels_sol_sol1 <- c(avg_vels_sol_sol1, rep(NA, max_length - length(avg_vels_sol_sol1)))
avg_vels_soc_soc2 <- c(avg_vels_soc_soc2, rep(NA, max_length - length(avg_vels_soc_soc2)))
avg_vels_soc_sol2 <- c(avg_vels_soc_sol2, rep(NA, max_length - length(avg_vels_soc_sol2)))
avg_vels_sol_sol2 <- c(avg_vels_sol_sol2, rep(NA, max_length - length(avg_vels_sol_sol2)))
avg_vels_soc_soc3 <- c(avg_vels_soc_soc3, rep(NA, max_length - length(avg_vels_soc_soc3)))
avg_vels_soc_sol3 <- c(avg_vels_soc_sol3, rep(NA, max_length - length(avg_vels_soc_sol3)))
avg_vels_sol_sol3 <- c(avg_vels_sol_sol3, rep(NA, max_length - length(avg_vels_sol_sol3)))


# third, create boxplots
plot_df <- data.frame(
  soc_soc1 = avg_vels_soc_soc1 * 0.0946 * 20,
  soc_sol1 = avg_vels_soc_sol1 * 0.0946 * 20,
  sol_sol1 = avg_vels_sol_sol1 * 0.0946 * 20,
  soc_soc2 = avg_vels_soc_soc2 * 0.0946 * 20,
  soc_sol2 = avg_vels_soc_sol2 * 0.0946 * 20,
  sol_sol2 = avg_vels_sol_sol2 * 0.0946 * 20,
  soc_soc3 = avg_vels_soc_soc3 * 0.0946 * 20,
  soc_sol3 = avg_vels_soc_sol3 * 0.0946 * 20,
  sol_sol3 = avg_vels_sol_sol3 * 0.0946 * 20
)

colors <- c(
  "soc_soc (n = 30)" = "darkred",
  "soc_sol (n = 12)" = "steelblue",
  "sol_sol (n = 20)" = "darkgreen"
)

# pixel to mm conversion: 56 mm/592 pixels = 0.0946 mm/pixel
ggplot(data = plot_df) +
  geom_boxplot(aes(x = 1, y = soc_soc1, fill = "soc_soc (n = 30)"), alpha = 0.5, width = 0.4, outlier.shape = NA) +
  geom_boxplot(aes(x = 1.5, y = soc_sol1, fill = "soc_sol (n = 12)"), alpha = 0.5, width = 0.4, outlier.shape = NA) +
  geom_boxplot(aes(x = 2, y = sol_sol1, fill = "sol_sol (n = 20)"), alpha = 0.5, width = 0.4, outlier.shape = NA) +
  geom_boxplot(aes(x = 3, y = soc_soc2, fill = "soc_soc (n = 30)"), alpha = 0.5, width = 0.4, outlier.shape = NA) +
  geom_boxplot(aes(x = 3.5, y = soc_sol2, fill = "soc_sol (n = 12)"), alpha = 0.5, width = 0.4, outlier.shape = NA) +
  geom_boxplot(aes(x = 4, y = sol_sol2, fill = "sol_sol (n = 20)"), alpha = 0.5, width = 0.4, outlier.shape = NA) +
  geom_boxplot(aes(x = 5, y = soc_soc3, fill = "soc_soc (n = 30)"), alpha = 0.5, width = 0.4, outlier.shape = NA) +
  geom_boxplot(aes(x = 5.5, y = soc_sol3, fill = "soc_sol (n = 12)"), alpha = 0.5, width = 0.4, outlier.shape = NA) +
  geom_boxplot(aes(x = 6, y = sol_sol3, fill = "sol_sol (n = 20)"), alpha = 0.5, width = 0.4, outlier.shape = NA) +
  geom_jitter(aes(x = 1, y = soc_soc1, colour = "soc_soc (n = 30)"), alpha = 0.5, width = 0.2) +
  geom_jitter(aes(x = 1.5, y = soc_sol1, colour = "soc_sol (n = 12)"), alpha = 0.5, width = 0.2) +
  geom_jitter(aes(x = 2, y = sol_sol1, colour = "sol_sol (n = 20)"), alpha = 0.5, width = 0.2) +
  geom_jitter(aes(x = 3, y = soc_soc2, colour = "soc_soc (n = 30)"), alpha = 0.5, width = 0.2) +
  geom_jitter(aes(x = 3.5, y = soc_sol2, colour = "soc_sol (n = 12)"), alpha = 0.5, width = 0.2) +
  geom_jitter(aes(x = 4, y = sol_sol2, colour = "sol_sol (n = 20)"), alpha = 0.5, width = 0.2) +
  geom_jitter(aes(x = 5, y = soc_soc3, colour = "soc_soc (n = 30)"), alpha = 0.5, width = 0.2) +
  geom_jitter(aes(x = 5.5, y = soc_sol3, colour = "soc_sol (n = 12)"), alpha = 0.5, width = 0.2) +
  geom_jitter(aes(x = 6, y = sol_sol3, colour = "sol_sol (n = 20)"), alpha = 0.5, width = 0.2) +
  scale_color_manual(name = "Point Legend", values = colors) +
  scale_fill_manual(name = "Boxplot Legend", values = colors) +
  ggtitle("Avg Velocities Every 5 Minutes") +
  labs(x = "Portion of Video (5 minute Increments)", y = "Average Velocity (mm/s)") +
  scale_x_continuous(breaks = c(1.5, 3.5, 5.5), labels = c(5, 10, 15)) +
  geom_vline(xintercept = c(2.5, 4.5), linetype = "dashed", color = "black")

# fourth, perform Kruskal-Wallis test
avg_vels_soc_soc1_df <- data.frame(avg_vels_soc_soc1, rep("soc_soc", times = length(avg_vels_soc_soc1)))
avg_vels_soc_sol1_df <- data.frame(avg_vels_soc_sol1, rep("soc_sol", times = length(avg_vels_soc_sol1)))
avg_vels_sol_sol1_df <- data.frame(avg_vels_sol_sol1, rep("sol_sol", times = length(avg_vels_sol_sol1)))
avg_vels_soc_soc2_df <- data.frame(avg_vels_soc_soc2, rep("soc_soc", times = length(avg_vels_soc_soc2)))
avg_vels_soc_sol2_df <- data.frame(avg_vels_soc_sol2, rep("soc_sol", times = length(avg_vels_soc_sol2)))
avg_vels_sol_sol2_df <- data.frame(avg_vels_sol_sol2, rep("sol_sol", times = length(avg_vels_sol_sol2)))
avg_vels_soc_soc3_df <- data.frame(avg_vels_soc_soc3, rep("soc_soc", times = length(avg_vels_soc_soc3)))
avg_vels_soc_sol3_df <- data.frame(avg_vels_soc_sol3, rep("soc_sol", times = length(avg_vels_soc_sol3)))
avg_vels_sol_sol3_df <- data.frame(avg_vels_sol_sol3, rep("sol_sol", times = length(avg_vels_sol_sol3)))
colnames(avg_vels_soc_soc1_df) <- c("Vels", "Social_Contrast")
colnames(avg_vels_soc_sol1_df) <- c("Vels", "Social_Contrast")
colnames(avg_vels_sol_sol1_df) <- c("Vels", "Social_Contrast")
colnames(avg_vels_soc_soc2_df) <- c("Vels", "Social_Contrast")
colnames(avg_vels_soc_sol2_df) <- c("Vels", "Social_Contrast")
colnames(avg_vels_sol_sol2_df) <- c("Vels", "Social_Contrast")
colnames(avg_vels_soc_soc3_df) <- c("Vels", "Social_Contrast")
colnames(avg_vels_soc_sol3_df) <- c("Vels", "Social_Contrast")
colnames(avg_vels_sol_sol3_df) <- c("Vels", "Social_Contrast")

stats_df1 <- rbind(avg_vels_soc_soc1_df, avg_vels_soc_sol1_df, avg_vels_sol_sol1_df)
stats_df2 <- rbind(avg_vels_soc_soc2_df, avg_vels_soc_sol2_df, avg_vels_sol_sol2_df)
stats_df3 <- rbind(avg_vels_soc_soc3_df, avg_vels_soc_sol3_df, avg_vels_sol_sol3_df)

print(kruskal.test(Vels ~ Social_Contrast, data = stats_df1))
print(dunn_test(Vels ~ Social_Contrast, data = stats_df1))

print(kruskal.test(Vels ~ Social_Contrast, data = stats_df2))
print(dunn_test(Vels ~ Social_Contrast, data = stats_df2))

print(kruskal.test(Vels ~ Social_Contrast, data = stats_df3))
print(dunn_test(Vels ~ Social_Contrast, data = stats_df3))

# analysis of ovary dissections
setwd("G:\\My Drive\\KL\\Dissections")
master_data <- read.csv("LBAL_Master_Data_Sheet.csv")
master_data <- master_data[!(master_data$Remove. == "Yes"), ]

queen_df <- as.numeric(master_data$Ovarian.Index[master_data["Caste"] == "queen"])
worker_df <- as.numeric(master_data$Ovarian.Index[master_data["Caste"] == "worker"])
solitary_df <- as.numeric(master_data$Ovarian.Index[master_data["Caste"] == "solitary"])

max_length <- max(length(queen_df), length(worker_df), length(solitary_df))

queen_df <- c(queen_df, rep(NA, max_length - length(queen_df)))
worker_df <- c(worker_df, rep(NA, max_length - length(worker_df)))
solitary_df <- c(solitary_df, rep(NA, max_length - length(solitary_df)))

plot_df <- data.frame(
  queens = queen_df,
  workers = worker_df,
  solitary_reproductives = solitary_df
)

colors <- c(
  "queens (n = 63)" = "#501617",
  "workers (n = 44)" = "#F38AB3",
  "solitary reproductives (n = 45)" = "#355C67"
)

# make boxplot of ovarian development per caste
ggplot(data = plot_df) +
  geom_boxplot(aes(x = 1, y = queen_df, fill = "queens (n = 63)"), alpha = 0.5, width = 0.4, outlier.shape = NA) +
  geom_boxplot(aes(x = 2, y = worker_df, fill = "workers (n = 44)"), alpha = 0.5, width = 0.4, outlier.shape = NA) +
  geom_boxplot(aes(x = 3, y = solitary_df, fill = "solitary reproductives (n = 45)"), alpha = 0.5, width = 0.4, outlier.shape = NA) +
  geom_jitter(aes(x = 1, y = queen_df, colour = "queens (n = 63)"), alpha = 0.5, width = 0.2) +
  geom_jitter(aes(x = 2, y = worker_df, colour = "workers (n = 44)"), alpha = 0.5, width = 0.2) +
  geom_jitter(aes(x = 3, y = solitary_df, colour = "solitary reproductives (n = 45)"), alpha = 0.5, width = 0.2) +
  scale_x_continuous(breaks = c(1, 2, 3), labels = c("queens", "workers", "solitary reproductives")) +
  scale_color_manual(name = "Point Legend", values = colors) +
  scale_fill_manual(name = "Boxplot Legend", values = colors) +
  labs(x = "Caste", y = "Body-Size Corrected Ovary Development Index") +
  theme_classic()

# determine sample sizes of each social pairing
contrast_list <- list() # full list of all contrasts
nest_ID_list <- list()
partner_list <- list() # keep track of IDs you've already accounted for
video_df <- data.frame(
  videoname = character(),
  contrast = character(),
  nest_ID_contrast = character()
)
for (i in seq_len(nrow(master_data))) {
  if (master_data$Specimen.ID[i] %in% unlist(partner_list)) {
    next
  }

  partner_i <- master_data$CT.Partner.ID[i]

  caste_1 <- master_data$Caste[i]
  caste_2 <- master_data[master_data$Specimen.ID == partner_i, ]$Caste
  castes_1_and_2 <- c(caste_1, caste_2)
  castes_in_order <- castes_1_and_2[str_order(castes_1_and_2)]
  contrast <- paste(castes_in_order[1], castes_in_order[2], sep = "-")

  nest_ID_1 <- master_data$Nest.Letter[i]
  nest_ID_2 <- master_data[master_data$Specimen.ID == partner_i, ]$Nest.Letter
  nest_IDs_1_and_2 <- c(nest_ID_1, nest_ID_2)
  nest_IDs_in_order <- nest_IDs_1_and_2[str_order(nest_IDs_1_and_2)]
  nest_ID_contrast <- paste(nest_IDs_in_order[1], nest_IDs_in_order[2], sep = "-")

  contrast_list[i] <- contrast
  nest_ID_list[i] <- nest_ID_contrast
  partner_list[i] <- partner_i

  video_df[i, ] <- c(master_data$Videoname[i], contrast, nest_ID_contrast)
}
as.data.frame(table(unlist(contrast_list)))
video_df <- video_df[complete.cases(video_df), ]


write.csv(video_df, "video_df.csv")

# create bar graphs for social contrasts
# start with velocities
setwd("G:\\My Drive\\KL\\ICE 2024\\velocities")
video_list_que_que <- read.delim("list_of_files_queen_queen.txt", header = FALSE)
video_list_que_wor <- read.delim("list_of_files_queen_worker.txt", header = FALSE)
video_list_wor_wor <- read.delim("list_of_files_worker_worker.txt", header = FALSE)
video_list_que_sol <- read.delim("list_of_files_queen_solitary.txt", header = FALSE)
video_list_sol_sol <- read.delim("list_of_files_solitary_solitary.txt", header = FALSE)
video_list <- unlist(c(
  video_list_que_que, video_list_que_wor, video_list_wor_wor,
  video_list_que_sol, video_list_sol_sol
))
contrast_list <- c(
  rep("queen_queen", nrow(video_list_que_que)),
  rep("queen_worker", nrow(video_list_que_wor)),
  rep("worker_worker", nrow(video_list_wor_wor)),
  rep("queen_solitary", nrow(video_list_que_sol)),
  rep("solitary_solitary", nrow(video_list_sol_sol))
)

video_list_vels <- list()
for (i in seq_along(video_list)) {
  video_list_vels[i] <- paste(video_list[i], contrast_list[i], "vels.csv", sep = "_")
}
video_list_vels <- unlist(video_list_vels)

# now determine when bees are moving or not
motion_prop_list <- list()
for (i in seq_along(video_list_vels)) {
  vels <- read.csv(video_list_vels[i])

  frame_count <- 0
  for (j in 200:nrow(vels)) {
    # if either bee is moving less than 1 mm/s
    if (is.na(vels[j, 2])) {
      next
    } else if ((vels[j, 2] * 0.0946 * 20) < 1) {
      frame_count <- frame_count + 1
    }
  }

  motion_prop_list[i] <- frame_count / (nrow(vels) - 200)
}
motion_prop_list <- unlist(motion_prop_list)

# now look at head-to-head and head-to-body interactions
setwd("G:\\My Drive\\KL\\ICE 2024\\batch_degree")
video_list_hth <- list()
video_list_htb <- list()
for (i in seq_along(video_list)) {
  video_list_hth[i] <- paste(video_list[i], contrast_list[i], "hth_bouts.csv", sep = "_")
  video_list_htb[i] <- paste(video_list[i], contrast_list[i], "htb_bouts.csv", sep = "_")
}

video_list_hth <- unlist(video_list_hth)
video_list_htb <- unlist(video_list_htb)

hth_bout_list <- list()
htb_bout_list <- list()
for (i in seq_along(video_list_hth)) {
  if (file.exists(video_list_hth[i])) {
    hth <- read.csv(video_list_hth[i])
    hth_bout_list[i] <- length(unique(hth$Bout))
  } else {
    hth_bout_list[i] <- 0
  }

  if (file.exists(video_list_htb[i])) {
    htb <- read.csv(video_list_htb[i])
    htb_bout_list[i] <- length(unique(htb$Bout))
  } else {
    htb_bout_list[i] <- 0
  }
}
hth_bout_list <- unlist(hth_bout_list)
htb_bout_list <- unlist(htb_bout_list)
hth_bout_list[hth_bout_list == "NULL"] <- 0
htb_bout_list[htb_bout_list == "NULL"] <- 0

# perform lmm analysis here
factored_contrast_list <- factor(contrast_list, c("queen_solitary", "solitary_solitary", "worker_worker", "queen_worker", "queen_queen"))

lmm_df <- data.frame(
  motion_prop = motion_prop_list,
  hth_bouts = hth_bout_list,
  htb_bouts = htb_bout_list,
  contrast = factored_contrast_list,
  date = video_list,
  videoname = video_list
)

lmm_df <- lmm_df[match(video_df$videoname, lmm_df$videoname), ]

lmm_df["nest_ID_contrast"] <- video_df$nest_ID_contrast

for (i in seq_along(lmm_df$date)) {
  lmm_df$date[i] <- strsplit(lmm_df$date[i], "_")[[1]][1]
}

lmm_df$contrast <- as.factor(lmm_df$contrast)
lmm <- lme(motion_prop ~ contrast, random = list(date = ~1, nest_ID_contrast = ~1), data = lmm_df)
anova(lmm)
# lmm = lmer(motion_prop ~ contrast + (1| nest_ID_contrast), data = lmm_df)
summary(lmm)

# running glht()
post.hoc <- glht(lmm, linfct = mcp(contrast = "Tukey"))

# displaying the result table with summary()
summary(post.hoc)

# now plot
plot_df <- data.frame(
  motion_prop = motion_prop_list,
  hth_bouts = hth_bout_list,
  htb_bouts = htb_bout_list,
  contrast = contrast_list
)

motion_prop_que_que <- plot_df$motion_prop[plot_df$contrast == "queen_queen"]
motion_prop_que_wor <- plot_df$motion_prop[plot_df$contrast == "queen_worker"]
motion_prop_wor_wor <- plot_df$motion_prop[plot_df$contrast == "worker_worker"]
motion_prop_que_sol <- plot_df$motion_prop[plot_df$contrast == "queen_solitary"]
motion_prop_sol_sol <- plot_df$motion_prop[plot_df$contrast == "solitary_solitary"]
hth_bouts_que_que <- plot_df$hth_bouts[plot_df$contrast == "queen_queen"]
hth_bouts_que_wor <- plot_df$hth_bouts[plot_df$contrast == "queen_worker"]
hth_bouts_wor_wor <- plot_df$hth_bouts[plot_df$contrast == "worker_worker"]
hth_bouts_que_sol <- plot_df$hth_bouts[plot_df$contrast == "queen_solitary"]
hth_bouts_sol_sol <- plot_df$hth_bouts[plot_df$contrast == "solitary_solitary"]
htb_bouts_que_que <- plot_df$htb_bouts[plot_df$contrast == "queen_queen"]
htb_bouts_que_wor <- plot_df$htb_bouts[plot_df$contrast == "queen_worker"]
htb_bouts_wor_wor <- plot_df$htb_bouts[plot_df$contrast == "worker_worker"]
htb_bouts_que_sol <- plot_df$htb_bouts[plot_df$contrast == "queen_solitary"]
htb_bouts_sol_sol <- plot_df$htb_bouts[plot_df$contrast == "solitary_solitary"]

motion_prop_que_que <- c(motion_prop_que_que, rep(NA, length(motion_prop_que_wor) - length(motion_prop_que_que)))
motion_prop_wor_wor <- c(motion_prop_wor_wor, rep(NA, length(motion_prop_que_wor) - length(motion_prop_wor_wor)))
motion_prop_que_sol <- c(motion_prop_que_sol, rep(NA, length(motion_prop_que_wor) - length(motion_prop_que_sol)))
motion_prop_sol_sol <- c(motion_prop_sol_sol, rep(NA, length(motion_prop_que_wor) - length(motion_prop_sol_sol)))
hth_bouts_que_que <- c(hth_bouts_que_que, rep(NA, length(hth_bouts_que_wor) - length(hth_bouts_que_que)))
hth_bouts_wor_wor <- c(hth_bouts_wor_wor, rep(NA, length(hth_bouts_que_wor) - length(hth_bouts_wor_wor)))
hth_bouts_que_sol <- c(hth_bouts_que_sol, rep(NA, length(hth_bouts_que_wor) - length(hth_bouts_que_sol)))
hth_bouts_sol_sol <- c(hth_bouts_sol_sol, rep(NA, length(hth_bouts_que_wor) - length(hth_bouts_sol_sol)))
htb_bouts_que_que <- c(htb_bouts_que_que, rep(NA, length(htb_bouts_que_wor) - length(htb_bouts_que_que)))
htb_bouts_wor_wor <- c(htb_bouts_wor_wor, rep(NA, length(htb_bouts_que_wor) - length(htb_bouts_wor_wor)))
htb_bouts_que_sol <- c(htb_bouts_que_sol, rep(NA, length(htb_bouts_que_wor) - length(htb_bouts_que_sol)))
htb_bouts_sol_sol <- c(htb_bouts_sol_sol, rep(NA, length(htb_bouts_que_wor) - length(htb_bouts_sol_sol)))

plot_df <- data.frame(
  motion_prop_que_que = motion_prop_que_que * 100,
  motion_prop_que_wor = motion_prop_que_wor * 100,
  motion_prop_wor_wor = motion_prop_wor_wor * 100,
  motion_prop_que_sol = motion_prop_que_sol * 100,
  motion_prop_sol_sol = motion_prop_sol_sol * 100,
  hth_bouts_que_que = hth_bouts_que_que,
  hth_bouts_que_wor = hth_bouts_que_wor,
  hth_bouts_wor_wor = hth_bouts_wor_wor,
  hth_bouts_que_sol = hth_bouts_que_sol,
  hth_bouts_sol_sol = hth_bouts_sol_sol,
  htb_bouts_que_que = htb_bouts_que_que,
  htb_bouts_que_wor = htb_bouts_que_wor,
  htb_bouts_wor_wor = htb_bouts_wor_wor,
  htb_bouts_que_sol = htb_bouts_que_sol,
  htb_bouts_sol_sol = htb_bouts_sol_sol
)

# perform some t-tests
t.test(plot_df$hth_bouts_sol_sol, plot_df$htb_bouts_sol_sol)

colors <- c(
  "proportion of time spent moving (%)" = "#f7a278",
  "total head-to-head interactions" = "#be97c6",
  "total head-to-body interactions" = "#2e294e"
)

# make boxplots of ovarian development per caste
ggplot(data = plot_df) +
  geom_boxplot(aes(x = 1, y = motion_prop_que_que, fill = "proportion of time spent moving (%)"), alpha = 0.5, width = 0.4, outlier.shape = NA) +
  geom_boxplot(aes(x = 1.5, y = hth_bouts_que_que, fill = "total head-to-head interactions"), alpha = 0.5, width = 0.4, outlier.shape = NA) +
  geom_boxplot(aes(x = 2, y = htb_bouts_que_que, fill = "total head-to-body interactions"), alpha = 0.5, width = 0.4, outlier.shape = NA) +
  geom_boxplot(aes(x = 3, y = motion_prop_que_wor, fill = "proportion of time spent moving (%)"), alpha = 0.5, width = 0.4, outlier.shape = NA) +
  geom_boxplot(aes(x = 3.5, y = hth_bouts_que_wor, fill = "total head-to-head interactions"), alpha = 0.5, width = 0.4, outlier.shape = NA) +
  geom_boxplot(aes(x = 4, y = htb_bouts_que_wor, fill = "total head-to-body interactions"), alpha = 0.5, width = 0.4, outlier.shape = NA) +
  geom_boxplot(aes(x = 5, y = motion_prop_wor_wor, fill = "proportion of time spent moving (%)"), alpha = 0.5, width = 0.4, outlier.shape = NA) +
  geom_boxplot(aes(x = 5.5, y = hth_bouts_wor_wor, fill = "total head-to-head interactions"), alpha = 0.5, width = 0.4, outlier.shape = NA) +
  geom_boxplot(aes(x = 6, y = htb_bouts_wor_wor, fill = "total head-to-body interactions"), alpha = 0.5, width = 0.4, outlier.shape = NA) +
  geom_boxplot(aes(x = 7, y = motion_prop_que_sol, fill = "proportion of time spent moving (%)"), alpha = 0.5, width = 0.4, outlier.shape = NA) +
  geom_boxplot(aes(x = 7.5, y = hth_bouts_que_sol, fill = "total head-to-head interactions"), alpha = 0.5, width = 0.4, outlier.shape = NA) +
  geom_boxplot(aes(x = 8, y = htb_bouts_que_sol, fill = "total head-to-body interactions"), alpha = 0.5, width = 0.4, outlier.shape = NA) +
  geom_boxplot(aes(x = 9, y = motion_prop_sol_sol, fill = "proportion of time spent moving (%)"), alpha = 0.5, width = 0.4, outlier.shape = NA) +
  geom_boxplot(aes(x = 9.5, y = hth_bouts_sol_sol, fill = "total head-to-head interactions"), alpha = 0.5, width = 0.4, outlier.shape = NA) +
  geom_boxplot(aes(x = 10, y = htb_bouts_sol_sol, fill = "total head-to-body interactions"), alpha = 0.5, width = 0.4, outlier.shape = NA) +
  geom_jitter(aes(x = 1, y = motion_prop_que_que, colour = "proportion of time spent moving (%)"), alpha = 0.5, width = 0.2) +
  geom_jitter(aes(x = 1.5, y = hth_bouts_que_que, colour = "total head-to-head interactions"), alpha = 0.5, width = 0.2) +
  geom_jitter(aes(x = 2, y = htb_bouts_que_que, colour = "total head-to-body interactions"), alpha = 0.5, width = 0.2) +
  geom_jitter(aes(x = 3, y = motion_prop_que_wor, colour = "proportion of time spent moving (%)"), alpha = 0.5, width = 0.2) +
  geom_jitter(aes(x = 3.5, y = hth_bouts_que_wor, colour = "total head-to-head interactions"), alpha = 0.5, width = 0.2) +
  geom_jitter(aes(x = 4, y = htb_bouts_que_wor, colour = "total head-to-body interactions"), alpha = 0.5, width = 0.2) +
  geom_jitter(aes(x = 5, y = motion_prop_wor_wor, colour = "proportion of time spent moving (%)"), alpha = 0.5, width = 0.2) +
  geom_jitter(aes(x = 5.5, y = hth_bouts_wor_wor, colour = "total head-to-head interactions"), alpha = 0.5, width = 0.2) +
  geom_jitter(aes(x = 6, y = htb_bouts_wor_wor, colour = "total head-to-body interactions"), alpha = 0.5, width = 0.2) +
  geom_jitter(aes(x = 7, y = motion_prop_que_sol, colour = "proportion of time spent moving (%)"), alpha = 0.5, width = 0.2) +
  geom_jitter(aes(x = 7.5, y = hth_bouts_que_sol, colour = "total head-to-head interactions"), alpha = 0.5, width = 0.2) +
  geom_jitter(aes(x = 8, y = htb_bouts_que_sol, colour = "total head-to-body interactions"), alpha = 0.5, width = 0.2) +
  geom_jitter(aes(x = 9, y = motion_prop_sol_sol, colour = "proportion of time spent moving (%)"), alpha = 0.5, width = 0.2) +
  geom_jitter(aes(x = 9.5, y = hth_bouts_sol_sol, colour = "total head-to-head interactions"), alpha = 0.5, width = 0.2) +
  geom_jitter(aes(x = 10, y = htb_bouts_sol_sol, colour = "total head-to-body interactions"), alpha = 0.5, width = 0.2) +
  scale_x_continuous(breaks = c(1.5, 3.5, 5.5, 7.5, 9.5), labels = c("queen-queen", "queen-worker", "worker-worker", "queen-solitary", "solitary-solitary")) +
  scale_color_manual(name = "Point Legend", values = colors) +
  theme_classic() +
  scale_fill_manual(name = "Boxplot Legend", values = colors) +
  ggtitle("Phases 1 and 2") +
  labs(x = "Social Contrast", y = "")

# perform lmm analysis here
lmm_df <- data.frame(
  motion_prop = motion_prop_list,
  hth_bouts = hth_bout_list,
  htb_bouts = htb_bout_list,
  contrast = contrast_list,
  date = video_list,
  videoname = video_list
)

lmm_df <- lmm_df[match(video_df$videoname, lmm_df$videoname), ]

lmm_df["nest_ID_contrast"] <- video_df$nest_ID_contrast

for (i in seq_along(lmm_df$date)) {
  lmm_df$date[i] <- strsplit(lmm_df$date[i], "_")[[1]][1]
}

lmm_df <- lmm_df[as.numeric(lmm_df$date) < 20230627, ]

lmm_df$contrast <- as.factor(lmm_df$contrast)
lmm <- lme(motion_prop ~ contrast, random = list(date = ~1, nest_ID_contrast = ~1), data = lmm_df)
anova(lmm)
# lmm = lmer(motion_prop ~ contrast + (1| date) + (1| nest_ID_contrast), data = lmm_df)
summary(lmm)

# running glht()
post.hoc <- glht(lmm, linfct = mcp(contrast = "Tukey"))

# displaying the result table with summary()
summary(post.hoc)

# now plot
# create bar graphs for social contrasts before workers emerged
plot_df_with_vidname <- data.frame(
  motion_prop = motion_prop_list,
  hth_bouts = hth_bout_list,
  htb_bouts = htb_bout_list,
  contrast = contrast_list,
  videoname = video_list
)

for (i in seq_along(plot_df_with_vidname$videoname)) {
  plot_df_with_vidname$videoname[i] <- strsplit(plot_df_with_vidname$videoname[i], "_")[[1]][1]
}

plot_df <- plot_df_with_vidname[as.numeric(plot_df_with_vidname$videoname) < 20230627, ]
plot_df <- plot_df[!(plot_df$contrast) == "queen_worker", ]

motion_prop_que_que <- plot_df$motion_prop[plot_df$contrast == "queen_queen"]
motion_prop_que_sol <- plot_df$motion_prop[plot_df$contrast == "queen_solitary"]
motion_prop_sol_sol <- plot_df$motion_prop[plot_df$contrast == "solitary_solitary"]
hth_bouts_que_que <- plot_df$hth_bouts[plot_df$contrast == "queen_queen"]
hth_bouts_que_sol <- plot_df$hth_bouts[plot_df$contrast == "queen_solitary"]
hth_bouts_sol_sol <- plot_df$hth_bouts[plot_df$contrast == "solitary_solitary"]
htb_bouts_que_que <- plot_df$htb_bouts[plot_df$contrast == "queen_queen"]
htb_bouts_que_sol <- plot_df$htb_bouts[plot_df$contrast == "queen_solitary"]
htb_bouts_sol_sol <- plot_df$htb_bouts[plot_df$contrast == "solitary_solitary"]

motion_prop_que_que <- c(motion_prop_que_que, rep(NA, length(motion_prop_sol_sol) - length(motion_prop_que_que)))
motion_prop_que_sol <- c(motion_prop_que_sol, rep(NA, length(motion_prop_sol_sol) - length(motion_prop_que_sol)))
hth_bouts_que_que <- c(hth_bouts_que_que, rep(NA, length(hth_bouts_sol_sol) - length(hth_bouts_que_que)))
hth_bouts_que_sol <- c(hth_bouts_que_sol, rep(NA, length(hth_bouts_sol_sol) - length(hth_bouts_que_sol)))
htb_bouts_que_que <- c(htb_bouts_que_que, rep(NA, length(htb_bouts_sol_sol) - length(htb_bouts_que_que)))
htb_bouts_que_sol <- c(htb_bouts_que_sol, rep(NA, length(htb_bouts_sol_sol) - length(htb_bouts_que_sol)))

plot_df <- data.frame(
  motion_prop_que_que = motion_prop_que_que * 100,
  motion_prop_que_sol = motion_prop_que_sol * 100,
  motion_prop_sol_sol = motion_prop_sol_sol * 100,
  hth_bouts_que_que = hth_bouts_que_que,
  hth_bouts_que_sol = hth_bouts_que_sol,
  hth_bouts_sol_sol = hth_bouts_sol_sol,
  htb_bouts_que_que = htb_bouts_que_que,
  htb_bouts_que_sol = htb_bouts_que_sol,
  htb_bouts_sol_sol = htb_bouts_sol_sol
)

colors <- c(
  "proportion of time spent moving (%)" = "#f7a278",
  "total head-to-head interactions" = "#be97c6",
  "total head-to-body interactions" = "#2e294e"
)

# make boxplots of ovarian development per caste
ggplot(data = plot_df) +
  geom_boxplot(aes(x = 1, y = motion_prop_que_que, fill = "proportion of time spent moving (%)"), alpha = 0.5, width = 0.4, outlier.shape = NA) +
  geom_boxplot(aes(x = 1.5, y = hth_bouts_que_que, fill = "total head-to-head interactions"), alpha = 0.5, width = 0.4, outlier.shape = NA) +
  geom_boxplot(aes(x = 2, y = htb_bouts_que_que, fill = "total head-to-body interactions"), alpha = 0.5, width = 0.4, outlier.shape = NA) +
  geom_boxplot(aes(x = 3, y = motion_prop_que_sol, fill = "proportion of time spent moving (%)"), alpha = 0.5, width = 0.4, outlier.shape = NA) +
  geom_boxplot(aes(x = 3.5, y = hth_bouts_que_sol, fill = "total head-to-head interactions"), alpha = 0.5, width = 0.4, outlier.shape = NA) +
  geom_boxplot(aes(x = 4, y = htb_bouts_que_sol, fill = "total head-to-body interactions"), alpha = 0.5, width = 0.4, outlier.shape = NA) +
  geom_boxplot(aes(x = 5, y = motion_prop_sol_sol, fill = "proportion of time spent moving (%)"), alpha = 0.5, width = 0.4, outlier.shape = NA) +
  geom_boxplot(aes(x = 5.5, y = hth_bouts_sol_sol, fill = "total head-to-head interactions"), alpha = 0.5, width = 0.4, outlier.shape = NA) +
  geom_boxplot(aes(x = 6, y = htb_bouts_sol_sol, fill = "total head-to-body interactions"), alpha = 0.5, width = 0.4, outlier.shape = NA) +
  geom_jitter(aes(x = 1, y = motion_prop_que_que, colour = "proportion of time spent moving (%)"), alpha = 0.5, width = 0.2) +
  geom_jitter(aes(x = 1.5, y = hth_bouts_que_que, colour = "total head-to-head interactions"), alpha = 0.5, width = 0.2) +
  geom_jitter(aes(x = 2, y = htb_bouts_que_que, colour = "total head-to-body interactions"), alpha = 0.5, width = 0.2) +
  geom_jitter(aes(x = 3, y = motion_prop_que_sol, colour = "proportion of time spent moving (%)"), alpha = 0.5, width = 0.2) +
  geom_jitter(aes(x = 3.5, y = hth_bouts_que_sol, colour = "total head-to-head interactions"), alpha = 0.5, width = 0.2) +
  geom_jitter(aes(x = 4, y = htb_bouts_que_sol, colour = "total head-to-body interactions"), alpha = 0.5, width = 0.2) +
  geom_jitter(aes(x = 5, y = motion_prop_sol_sol, colour = "proportion of time spent moving (%)"), alpha = 0.5, width = 0.2) +
  geom_jitter(aes(x = 5.5, y = hth_bouts_sol_sol, colour = "total head-to-head interactions"), alpha = 0.5, width = 0.2) +
  geom_jitter(aes(x = 6, y = htb_bouts_sol_sol, colour = "total head-to-body interactions"), alpha = 0.5, width = 0.2) +
  scale_x_continuous(breaks = c(1.5, 3.5, 5.5), labels = c("queen-queen", "queen-solitary", "solitary-solitary")) +
  scale_color_manual(name = "Point Legend", values = colors) +
  theme_classic() +
  scale_fill_manual(name = "Boxplot Legend", values = colors) +
  ggtitle("Phase 1 Only") +
  labs(x = "Social Contrast", y = "")
