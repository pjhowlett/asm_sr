library(meta)

# Microbiological TB prevalence

study <- c("Abeid 2022", "Rambiki 2020", "Moyo 2021", "Ohene 2021", "Moyo 2022", "Mbuya 2023")
n <- c(21136, 892, 422, 844, 3547, 330)
cases <- c(380, 46, 9, 23, 83, 20)

df_m <- data.frame(study, n, cases)

m.prop <- metaprop(event = cases,
                   n = n,
                   studlab = study,
                   data = df_m,
                   method = "GLMM",
                   sm = "PLOGIT",
                   fixed = FALSE,
                   random = TRUE,
                   hakn = TRUE,
                   prediction = TRUE,
                   pscale=100,
                   title = "Microbiological TB in ASM")

summary(m.prop)

tiff (file="~/Dropbox/PhD/Papers/Resp disease and ASM SR/TB_micro_forest.tiff",
     width=190, height=70, units = "mm", res = 600)
par(mfrow = c(3, 1))
forest.meta(m.prop, 
            digits = 1,
            sortvar = TE,
            prediction = FALSE,
            print.tau2 = FALSE,
            weight.study = "random",
            leftlabs = c("Study", "Microbiological TB", "N"), 
            rightlabs = c("(%)", "95% CI"))

dev.off()

tiff (file="~/Dropbox/PhD/Papers/Resp disease and ASM SR/TB_micro_funnel.tiff",
     width=190, height=70, units = "mm", res = 600)

funnel.meta(m.prop,
            xlim = c(-5,-1),
            studlab = TRUE)
# Add title
title("Funnel plot of microbiological TB in ASM")
dev.off()

# Microbiological TB prevalence - sensitivity without Ohene

study <- c("Abeid 2022", "Moyo 2021", "Ohene 2021", "Moyo 2022", "Mbuya 2023")
n <- c(21136, 422, 844, 3547, 330)
cases <- c(380, 9, 23, 83, 20)
df_ms <- data.frame(study, n, cases)

# Clinical reference TB prevalence

study <- c("Abeid 2022", "Moyo 2022", "Rambiki 2020", "Moyo 2021")
n <- c(21136, 3547, 892, 422)
cases <- c(630, 240, 152, 17)

df_c <- data.frame(study, n, cases)

m.prop <- metaprop(event = cases,
                   n = n,
                   studlab = study,
                   data = df_c,
                   method = "GLMM",
                   sm = "PLOGIT",
                   fixed = FALSE,
                   random = TRUE,
                   hakn = TRUE,
                   prediction = TRUE,
                   pscale=100,
                   title = "Clinical TB in ASM")

tiff (file="~/Dropbox/PhD/Papers/Resp disease and ASM SR/TB_clinical_forest.tiff",
     width=190, height=70, units = "mm", res = 600)

forest.meta(m.prop, 
            digits = 1,
            sortvar = TE,
            prediction = FALSE,
            print.tau2 = FALSE,
            weight.study = "random",
            leftlabs = c("Study", "Clinical TB", "N"), 
            rightlabs = c("(%)", "95% CI"))
dev.off()

tiff (file="~/Dropbox/PhD/Papers/Resp disease and ASM SR/TB_clinic_funnel.tiff",
     width=190, height=70, units = "mm", res = 300)

funnel.meta(m.prop,
            xlim = c(-5,-1),
            studlab = TRUE)
# Add title
title("Funnel plot of clinical TB in ASM")
dev.off()

# Silicosis prevalence 

study <- c("Moyo 2021", "Moyo 2022", "Souza 2017", "Tse 2007", "Mbuya 2023")
n <- c(464, 3547, 348, 583, 330)
cases <- c(52, 666, 129, 170, 99)

# study <- c("Moyo 2021", "Moyo 2022", "Souza 2017", "Tse 2007", "Mbuya 2023")
# n <- c(464, 3547, 348, 583, 330)
# cases <- c(52, 666, 129, 170, 99)

df <- data.frame(study, n, cases)

df$prev <- df$cases/df$n

m.prop <- metaprop(event = cases,
                   n = n,
                   studlab = study,
                   data = df,
                   method = "GLMM",
                   sm = "PLOGIT",
                   fixed = FALSE,
                   random = TRUE,
                   hakn = TRUE,
                   prediction = TRUE,
                   pscale=100,
                   title = "Silicosis in ASM")

summary(m.prop)

tiff (file="~/Dropbox/PhD/Papers/Resp disease and ASM SR/Silic_forest.tiff",
    width=190, height=70, units = "mm", res = 300)

forest.meta(m.prop, 
            digits = 1,
            sortvar = TE,
            prediction = FALSE,
            print.tau2 = FALSE,
            weight.study = "random",
            leftlabs = c("Study", "Silicosis cases", "N"), 
            rightlabs = c("(%)", "95% CI"))
dev.off()

tiff (file="~/Dropbox/PhD/Papers/Resp disease and ASM SR/Silic_funnel.tiff",
     width=190, height=70, units = "mm", res = 300)

funnel.meta(m.prop,
            xlim = c(-2.5,0),
            studlab = TRUE)
# Add title
title("Funnel plot of silicosis prevalence in ASM")
dev.off()


