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
R2conL <- R2ConnectednessQ(
  renewable_residuals,
  window.size = NULL,    # full sample
  nlag = 1,
  quantile = TRUE,
  q = 0.05,
  method = "pearson",
  shrink = TRUE,
  progbar = FALSE
)
#
#
#
#
#
#
Cnames <- colnames(renewable_residuals)
groups <- list(Renewable = 1:7,
               Solar = 8:15,
               Wind = 16:26)

#
#
#
#
go <- net_plot(
  R2conL$TABLE$Overall, Cnames,
  use_pmfg = FALSE, layout = "spring",
  groups = groups, div = 5,
  node.size = 35, edge = 3,
  pie.colour = c("black","white")
)
plot(go)
#
#
#
#
gc <- net_plot(
  R2conL$TABLE$Contemporaneous, Cnames,
  use_pmfg = FALSE, layout = "spring",
  groups = groups, div = 5,
  node.size = 35, edge = 3,
  pie.colour = c("black","white")
)
plot(gc)
#
#
#
gl <- net_plot(
  R2conL$TABLE$Lagged, Cnames,
  use_pmfg = FALSE, layout = "spring",
  groups = groups, div = 8,
  node.size = 35, edge = 2,
  pie.colour = c("black","white")
)
plot(gl)
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
dca <- R2ConnectednessQ(
  renewable_residuals[,1:5],
  window.size = 200,
  nlag = 1,
  quantile = TRUE,
  q = 0.05,
  method = "pearson",
  shrink = TRUE,
  progbar = FALSE
)
TCI <- dca$TCI
head(TCI)
#
#
#
df <- cbind.data.frame(as.Date(rownames(TCI)), TCI[, 1:3, drop = FALSE])
colnames(df) <- c("date", "SOI", "SOIC", "SOIL")

#
#
#
library(ggplot2)

ymin <- 0; ymax <- 40

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

g_tci <- ggplot(df, aes(x = date)) +
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

g_tci
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
