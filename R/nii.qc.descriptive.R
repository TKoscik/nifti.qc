nii.qc.descriptive <- function(
  img.nii, img.vol=1L,
  mask.nii, mask.vol=1L, mask.dir="gt", mask.thresh=0) {

  img <- read.nii.volume(img.nii, img.vol)
  mask <- read.nii.volume(mask.nii, mask.vol)
  mask <- thresh.apply(mask, mask.dir, mask.thresh, "index.arr")
  img <- img[mask]
  summary <- list()
  summary$stats <- data.frame(
    mean = mean(img, na.rm=TRUE),
    sd = sd(img, na.rm=TRUE),
    median = median(img, na.rm=TRUE),
    mad = mad(img, na.rm=TRUE),
    skew = skewness(img, na.rm=TRUE),
    kurtosis = kurtosis(img, na.rm=TRUE),
    q05 = quantile(img, 0.05, na.rm=TRUE),
    q95 = quantile(img, 0.95, na.rm=TRUE))
  df <- data.frame(Intensity=img)
  summary$plot <- ggplot(df, aes(x=Intensity)) +
    theme_bw() +
    geom_density(size=1, fill="#646464", color="transparent") +
    labs(x="Intensity", y="Density") +
    geom_vline(xintercept=summary$stats$mean, linetype="dotted", size=1) +
    annotate(geom = "rect",
             xmin=summary$stats$mean - summary$stats$sd,
             xmax=summary$stats$mean + summary$stats$sd,
             ymin=-Inf, ymax=Inf, alpha=0.15, color="transparent") +
    annotate(geom = "rect", xmin=summary$stats$q05, xmax=summary$q95,
             ymin=-Inf, ymax=Inf, alpha=0.15, color="transparent")
  return(summary)
}
