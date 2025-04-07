library(tidyverse)
library(ggpubr)
library(rstatix)
library(readxl)
df<- read_excel("FINAL DATA.xlsx")
df
#summary stats
df %>% get_summary_stats(relr, type = "mean_sd")
df %>% get_summary_stats(relc, type = "mean_sd")
df %>% get_summary_stats(relb, type = "mean_sd")
# One-sample t-test
res <- t.test(df$relr, mu = 0)

# Printing the results
res 

# One-sample t-test
res <- t.test(df$relc, mu = 0)

# Printing the results
res 

# One-sample t-test
res <- t.test(df$relb, mu = 0)

# Printing the results
res 

#ESTIMATION ACCURACY NOT SIGNIFICANLTY DIFFERENT THAN 0
stat.test <- df %>% t_test(relr ~ 1, mu = 0)
stat.test#p=0.4
stat.test <- df %>% t_test(relc ~ 1, mu = 0)
stat.test#p=0.8
stat.test <- df %>% t_test(relb ~ 1, mu = 0)
stat.test#p=0.4

df %>% t_test(relr ~ 1, mu = 0, detailed = TRUE)
df %>% t_test(relc ~ 1, mu = 0, detailed = TRUE)
df %>% t_test(relb ~ 1, mu = 0, detailed = TRUE)

#effects size Cohen's d
df %>% cohens_d(relr ~ 1, mu = 0)
df %>% cohens_d(relc ~ 1, mu = 0)
df %>% cohens_d(relb ~ 1, mu = 0)

# Create green boxplot with mean and jitter
bxp <- ggboxplot(
  df$relr,
  width = 0.5,
  fill = "green",                 
  add = c("mean", "jitter"),
  ylab = "Relative error (%)",
  xlab = FALSE
)

# Add significance label
bxp + labs(
  subtitle = get_test_label(stat.test, detailed = TRUE)
)

bxp <- ggboxplot(
  df$relc,
  width = 0.5,
  fill = "blue",                 
  add = c("mean", "jitter"),
  ylab = "Relative error (%)",
  xlab = FALSE
)

# Add significance label
bxp + labs(
  subtitle = get_test_label(stat.test, detailed = TRUE)
)

bxp <- ggboxplot(
  df$relb,
  width = 0.5,
  fill = "red",                
  add = c("mean", "jitter"),
  ylab = "Relative error (%)",
  xlab = FALSE
)

# Add significance label
bxp + labs(
  subtitle = get_test_label(stat.test, detailed = TRUE)
)

#density plot

ggdensity(df, x = "relr", rug = TRUE, fill = "green") +
  coord_cartesian(xlim = c(min(df$relr, na.rm = TRUE), max(df$relr, na.rm = TRUE))) +
  stat_central_tendency(type = "mean", color = "black", linetype = "dashed") +  # sample mean
  geom_vline(xintercept = 0, color = "blue3", linetype = "dashed") +  # null hypothesis
  labs(
    x = "Relative Error (%)",
    subtitle = get_test_label(stat.test, detailed = TRUE)
  )

ggdensity(df, x = "relc", rug = TRUE, fill = "blue") +
  coord_cartesian(xlim = c(min(df$relc, na.rm = TRUE), max(df$relc, na.rm = TRUE))) +
  stat_central_tendency(type = "mean", color = "black", linetype = "dashed") +  # sample mean
  geom_vline(xintercept = 0, color = "blue3", linetype = "dashed") +  # null hypothesis
  labs(
    x = "Relative Error (%)",
    subtitle = get_test_label(stat.test, detailed = TRUE)
  )

ggdensity(df, x = "relb", rug = TRUE, fill = "red") +
  coord_cartesian(xlim = c(min(df$relb, na.rm = TRUE), max(df$relb, na.rm = TRUE))) +
  stat_central_tendency(type = "mean", color = "black", linetype = "dashed") +  # sample mean
  geom_vline(xintercept = 0, color = "blue3", linetype = "dashed") +  # null hypothesis
  labs(
    x = "Relative Error (%)",
    subtitle = get_test_label(stat.test, detailed = TRUE)
  )


#SUBJECTS 2 way MIXED ANOVA

#SUBJECT
ggboxplot(
  df,
  x = "subject", y = "totald",
  color = "subject", palette = "jco", add = "jitter"
) +
  labs(
    title = "Total Difference by subject Group",
    x = "subject",
    y = "Total Difference"
  ) +
  theme_minimal()

# Wide format
set.seed(123)
df %>% sample_n_by(subject, size = 1)

# Gather the columns long format.
# Convert into factor variables
df <- df %>%
  gather(key = "shape", value = "error", relr, relc, relb) %>%
  convert_as_factor(volunteer, shape)
# Inspect some random rows of the data by groups
set.seed(123)
df %>% sample_n_by(subject, shape, size = 1)

#summary stats
df %>%
  group_by(shape, subject) %>%
  get_summary_stats(error, type = "mean_sd")

#boxplot
bxp <- ggboxplot(
  df, x = "shape", y = "error",
  color = "subject", palette = "jco"
)
bxp

#outliers
df %>%
  group_by(shape, subject) %>%
  identify_outliers(error)

#nomrality - non normal = no game design/photo for relr and no subject studied for relc 
df %>%
  group_by(shape, subject) %>%
  shapiro_test(error)

#homogniety of variance HAVE MET
df %>%
  group_by(shape) %>%
  levene_test(error ~ subject)
#homogniety of covariance MET
box_m(df[, "error", drop = FALSE], df$subject)

# Two-way mixed ANOVA test = NO SIG
res.aov <- anova_test(
  data = df, dv = error, wid = volunteer,
  between = subject, within = shape
)
get_anova_table(res.aov)

#effect of subject on shape
one.way <- df %>%
  group_by(shape) %>%
  anova_test(dv = error, wid = volunteer, between = subject) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way

# Pairwise comparisons between group levels = no sig
pwc <- df %>%
  group_by(shape) %>%
  pairwise_t_test(error ~ subject, p.adjust.method = "bonferroni")
pwc

# Effect of shape on subject
one.way2 <- df %>%
  group_by(subject) %>%
  anova_test(dv = error, wid = volunteer, within = shape) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way2

# Pairwise comparisons between time points at each group levels = NO SIG
# Paired t-test is used because we have repeated measures by time
pwc2 <- df %>%
  group_by(subject) %>%
  pairwise_t_test(
    error ~ shape, paired = TRUE, 
    p.adjust.method = "bonferroni"
  ) %>%
  select(-df, -statistic, -p) 
pwc2

#pairwised paired t test for shape = no significance
df %>%
  pairwise_t_test(
    error ~ shape, paired = TRUE, 
    p.adjust.method = "bonferroni"
  )
#group = no sig
df %>%
  pairwise_t_test(
    error ~ subject, 
    p.adjust.method = "bonferroni"
  )
# Visualization: boxplots with p-values
pwc <- pwc %>% add_xy_position(x = "shape")
bxp + 
  stat_pvalue_manual(pwc, tip.length = 0, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.aov, detailed = TRUE),
    caption = get_pwc_label(pwc)
  )

#Same for ecological knowledge 
df$kowledge <- as.factor(df$kowledge)

# Wide format
set.seed(123)
df %>% sample_n_by(kowledge, size = 1)
# Gather the columns long format.
# Convert into factor variables
df <- df %>%
  gather(key = "shape", value = "error", relr, relc, relb) %>%
  convert_as_factor(volunteer, shape)
# Inspect some random rows of the data by groups
set.seed(123)
df %>% sample_n_by(kowledge, shape, size = 1)

#summary stats
df %>%
  group_by(shape, kowledge) %>%
  get_summary_stats(error, type = "mean_sd")

#boxplot
bxp <- ggboxplot(
  df, x = "shape", y = "error",
  color = "kowledge", palette = "jco"
)
bxp

#outliers
df %>%
  group_by(shape, kowledge) %>%
  identify_outliers(error)

#nomrality - non normal = relc for knowledge 1
df %>%
  group_by(shape, kowledge) %>%
  shapiro_test(error)

#homogniety of variance HAVE MET
df %>%
  group_by(shape) %>%
  levene_test(error ~ kowledge)

#homogniety of covariance MET
box_m(df[, "error", drop = FALSE], df$kowledge)

# Two-way mixed ANOVA test = NO SIG
res.aov <- anova_test(
  data = df, dv = error, wid = volunteer,
  between = kowledge, within = shape
)
get_anova_table(res.aov)

#effect of knowledge on shape
one.way <- df %>%
  group_by(shape) %>%
  anova_test(dv = error, wid = volunteer, between = kowledge) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way

# Pairwise comparisons between group levels = no sig
pwc <- df %>%
  group_by(shape) %>%
  pairwise_t_test(error ~ kowledge, p.adjust.method = "bonferroni")
pwc

# Effect of shape on subject
one.way2 <- df %>%
  group_by(kowledge) %>%
  anova_test(dv = error, wid = volunteer, within = shape) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way2

# Pairwise comparisons between time points at each group levels = NO SIG
# Paired t-test is used because we have repeated measures by time
pwc2 <- df %>%
  group_by(kowledge) %>%
  pairwise_t_test(
    error ~ shape, paired = TRUE, 
    p.adjust.method = "bonferroni"
  ) %>%
  select(-df, -statistic, -p) 
pwc2

#pairwised paired t test for shape = no significance
df %>%
  pairwise_t_test(
    error ~ shape, paired = TRUE, 
    p.adjust.method = "bonferroni"
  )
#group = no sig
df %>%
  pairwise_t_test(
    error ~ kowledge, 
    p.adjust.method = "bonferroni"
  )
# Visualization: boxplots with p-values
pwc <- pwc %>% add_xy_position(x = "shape")
bxp + 
  stat_pvalue_manual(pwc, tip.length = 0, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.aov, detailed = TRUE),
    caption = get_pwc_label(pwc)
  )




#AGE
#boxplot
ggboxplot(
  df,
  x = "age", y = "totald",
  color = "age", palette = "jco", add = "jitter"
) +
  labs(
    title = "Total Difference by Age Group",
    x = "Age Group",
    y = "Total Difference"
  ) +
  theme_minimal()
#variance = ALL MET
df$age <- as.factor(df$age)

levene_test(totald ~ age, data = df)
levene_test(relr ~ age, data = df)
levene_test(relc ~ age, data = df)
levene_test(relb ~ age, data = df)








# Wide format
set.seed(123)
df %>% sample_n_by(age, size = 1)
# Gather the columns long format.
# Convert into factor variables
df <- df %>%
  gather(key = "shape", value = "error", relr, relc, relb) %>%
  convert_as_factor(volunteer, shape)
# Inspect some random rows of the data by groups
set.seed(123)
df %>% sample_n_by(age, shape, size = 1)

#summary stats
df %>%
  group_by(shape, age) %>%
  get_summary_stats(error, type = "mean_sd")

#boxplot
df$age <- factor(df$age,
                 levels = c(1, 2, 2.5, 3, 4, 5, 6),
                 labels = c("18–25", "26–35", "36–45", "46–55", "56–65", "66–75", "76+")
)

bxp <- ggboxplot(
  df, x = "shape", y = "error",
  color = "age", palette = "jco"
) +
  labs(color = "Age Group")
bxp

#outliers
df %>%
  group_by(shape, kowledge) %>%
  identify_outliers(error)

df

#nomrality - non normal = relc for knowledge 1
ggqqplot(df, "error", ggtheme = theme_bw()) +
  facet_grid(shape ~ age)
#homogniety of variance HAVE MET
df %>%
  group_by(shape) %>%
  levene_test(error ~ age)

#homogniety of covariance MET
box_m(df[, "error", drop = FALSE], df$age)

# Two-way mixed ANOVA test = NO SIG
res.aov <- anova_test(
  data = df, dv = error, wid = volunteer,
  between = age, within = shape
)
get_anova_table(res.aov) # age

#effect of age on shape = only relr
one.way <- df %>%
  group_by(shape) %>%
  anova_test(dv = error, wid = volunteer, between = age) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way

# Pairwise comparisons between age levels = NO SIG
pwc <- df %>%
  group_by(shape) %>%
  pairwise_t_test(error ~ age, p.adjust.method = "bonferroni")
pwc

# Effect of shape on age = NO SIG
one.way2 <- df %>%
  group_by(age) %>%
  anova_test(dv = error, wid = volunteer, within = shape) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way2

# Pairwise comparisons between time points at each group levels = NO SIG
# Paired t-test is used because we have repeated measures by time
pwc2 <- df %>%
  group_by(age) %>%
  pairwise_t_test(
    error ~ shape, paired = TRUE, 
    p.adjust.method = "bonferroni"
  ) %>%
  select(-df, -statistic, -p) 
pwc2

#age = significance between 1 and 2, 1 and 2.5, 1 and 3, 2.5 and 3.
df %>%
  pairwise_t_test(
    error ~ age, 
    p.adjust.method = "bonferroni"
  )
# Visualization: boxplots with p-values
pwc <- pwc %>% add_xy_position(x = "age")
bxp + 
  stat_pvalue_manual(pwc, tip.length = 0, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.aov, detailed = TRUE),
    caption = get_pwc_label(pwc)
  )

# Add positions for the p-values
pwc2 <- pwc2 %>%
  add_xy_position(x = "age")

# Boxplot with p-values
ggboxplot(
  df,
  x = "age", y = "error",
  color = "age", palette = "jco", add = "jitter"
) +
  stat_pvalue_manual(pwc, hide.ns = TRUE, tip.length = 0.01) +
  labs(
    title = "Total Error by Age Group",
    subtitle = "Pairwise comparisons with Bonferroni adjustment",
    caption = "Only significant comparisons shown (p.adj < 0.05)"
  ) +
  theme_minimal()










# Pairwise comparisons with p-value adjustment
pwc <- df %>%
  pairwise_t_test(
    error ~ age,
    p.adjust.method = "bonferroni"
  )
pwc
pwc_sig <- pwc %>%
  filter(p.adj < 0.05)
pwc_sig <- pwc_sig %>%
  add_xy_position(x = "age")

ggboxplot(
  df, x = "age", y = "error",
  color = "age", palette = "jco", add = "jitter"
) +
  stat_pvalue_manual(
    pwc_sig,
    label = "p.adj",        
    tip.length = 0.01,
    hide.ns = TRUE
  ) +
  labs(
    title = "Significant Pairwise Comparisons of Error by Age Group",
    subtitle = "Bonferroni-adjusted p-values shown",
    caption = "Only comparisons with p.adj < 0.05 are displayed"
  ) +
  theme_minimal()





