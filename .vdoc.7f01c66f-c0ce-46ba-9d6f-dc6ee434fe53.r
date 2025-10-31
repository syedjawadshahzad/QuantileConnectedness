#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
# Global knitr options: make figures show on GitHub
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.path = "man/figures/README-",
  out.width = "100%",
  dpi = 150
)
set.seed(1)
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
data(renewable_residuals)
renewable_small <- renewable_residuals[, 1:5]    # first 5 firms
colnames(renewable_small)
#
#
#
#
#
R2con_small <- R2ConnectednessQ(
  renewable_small,
  window.size = NULL,    # full sample
  nlag = 1,
  quantile = TRUE,
  q = 0.05,
  method = "pearson",
  shrink = TRUE,
  progbar = FALSE
)
names(R2con_small$TABLE)
#
#
#
#
#
Cnames_small <- colnames(renewable_small)
# simple grouping for 5 firms – adjust for your application
groups_small <- list(Group1 = 1:2, Group2 = 3:4, Group3 = 5)
node.size <- 20
#
#
#
g_overall_small <- net_plot(
  R2con_small$TABLE$Overall, Cnames_small,
  use_pmfg = FALSE, layout = "spring",
  groups = groups_small, div = 5,
  node.size = node.size, edge = 3,
  pie.colour = c("black","white")
)
g_overall_small
#
#
#
g_contemp_small <- net_plot(
  R2con_small$TABLE$Contemporaneous, Cnames_small,
  use_pmfg = FALSE, layout = "spring",
  groups = groups_small, div = 5,
  node.size = node.size, edge = 3,
  pie.colour = c("black","white")
)
g_contemp_small
#
#
#
g_lagged_small <- net_plot(
  R2con_small$TABLE$Lagged, Cnames_small,
  use_pmfg = FALSE, layout = "spring",
  groups = groups_small, div = 8,
  node.size = node.size, edge = 2,
  pie.colour = c("black","white")
)
g_lagged_small
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
dca_small <- R2ConnectednessQ(
  renewable_small,
  window.size = 200,
  nlag = 1,
  quantile = TRUE,
  q = 0.05,
  method = "pearson",
  shrink = TRUE,
  progbar = FALSE
)
TCI_small <- dca_small$TCI
head(TCI_small)
#
#
#
df_small <- cbind.data.frame(as.Date(rownames(TCI_small)), TCI_small[, 1:3, drop = FALSE])
colnames(df_small) <- c("date", "SOI", "SOIC", "SOIL")
summary(df_small)
#
#
#
library(ggplot2)

ymin <- 0; ymax <- 80

mytheme <- theme(
  panel.background = element_rect(fill = "transparent"),
  panel.border     = element_rect(fill = "transparent", color = "black", linewidth = 0.6),
  text             = element_text(size = 13, color = "black"),
  axis.text.x      = element_text(color = "black"),
  axis.text.y      = element_text(color = "black"),
  panel.grid.major = element_line(size = 0.1, colour = "grey75"),
  panel.grid.minor = element_line(size = 0.05, colour = "grey85"),
  legend.position  = "bottom",
  legend.title     = element_blank()
)

g_tci_small <- ggplot(df_small, aes(x = date)) +
  geom_line(aes(y = SOI),  color = "blue",  linewidth = 0.9) +
  geom_line(aes(y = SOIC), color = "green", linewidth = 0.9) +
  geom_line(aes(y = SOIL), color = "red",   linewidth = 0.9) +
  theme_minimal() +
  coord_cartesian(ylim = c(ymin, ymax)) +
  labs(y = "Connectedness", x = "Date") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  geom_vline(xintercept = as.Date("2020-03-01"), linetype = "dashed") +
  geom_vline(xintercept = as.Date("2021-01-01"), linetype = "dashed") +
  geom_vline(xintercept = as.Date("2022-02-24"), linetype = "dashed") +
  annotate("text", x = as.Date("2019-06-01"), y = ymax - 2,
           label = "COVID-19", size = 3.6, hjust = 0) +
  annotate("text", x = as.Date("2021-01-30"), y = ymin + 5,
           label = "Russia–Ukraine war", size = 3.6, hjust = 0) +
  mytheme

g_tci_small
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
