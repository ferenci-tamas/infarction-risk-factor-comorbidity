library(data.table)
library(ggplot2)
theme_set(theme_bw())

RawData <- data.table(readxl::read_excel(
  "RawData.xlsx", sheet = 2, na = c("Nincs kitöltve", "Nem ismert"),
  guess_max = 1e6), check.names = TRUE)
RawData$X.9..Felvétel.időpontja <- as.Date(RawData$X.9..Felvétel.időpontja)
RawData$X.4..Születési.dátum <- as.Date(RawData$X.4..Születési.dátum)

RawData$Eletkor <- as.numeric(difftime(
  RawData$X.9..Felvétel.időpontja,
  RawData$X.4..Születési.dátum, units = "days"))/365.241
hist(RawData$Eletkor)

RawData <- RawData[X.9..Felvétel.időpontja >= as.Date("2014-01-01")]

RawData$Eletkor40alatt <- as.numeric(RawData$Eletkor <= 40)
RawData$Eletkor80felett <- as.numeric(RawData$Eletkor > 80)

for(i in 31:37) print(table(RawData[[i]]))
for(i in 31:37) RawData[[i]] <- as.numeric(RawData[[i]] != "Nem")

RawData$Dohanyzik <- as.numeric(RawData$X.35..Kórelőzményben.dohányzás == "Dohányzik")
RawData$DohanyzastAbbahagyta <- as.numeric(RawData$X.35..Kórelőzményben.dohányzás == "A dohányzást abbahagyta")
RawData$DohanyzikSoha <- as.numeric(RawData$X.35..Kórelőzményben.dohányzás == "Soha nem dohányzott")

RawData$BMI25felett <- ifelse(lubridate::year(RawData$X.9..Felvétel.időpontja) == 2014 &
                                lubridate::month(RawData$X.9..Felvétel.időpontja) <= 3,
                              NA, as.numeric(RawData$X.24.1..Testtömeg.index.BMI.értéke > 25))
RawData$BMI30felett <- ifelse(lubridate::year(RawData$X.9..Felvétel.időpontja) == 2014 &
                                lubridate::month(RawData$X.9..Felvétel.időpontja) <= 3,
                              NA, as.numeric(RawData$X.24.1..Testtömeg.index.BMI.értéke > 30))

RawData$FelvetelNum <- as.numeric(RawData$X.9..Felvétel.időpontja) - as.numeric(as.Date("2014-01-01"))

RawData$X.2..Neme <- ifelse(RawData$X.2..Neme == "Férfi", "Male", "Female")

nrow(RawData)
range(RawData$X.9..Felvétel.időpontja)
table(RawData$X.2..Neme)
prop.table(table(RawData$X.2..Neme))
table(RawData$X.92..Diagnózis)
prop.table(table(RawData$X.92..Diagnózis))

RawData[, .(.N, EletkorAtlag = mean(Eletkor), EletkorSzoras = sd(Eletkor)), .(X.92..Diagnózis, X.2..Neme)]
cube(RawData, .(.N, EletkorAtlag = round(mean(Eletkor), 1), EletkorSzoras = round(sd(Eletkor), 1)),
     c("X.92..Diagnózis", "X.2..Neme"), label = "Mindösszesen")

figs <- data.table(
  name = c(names(RawData)[c(33, 35, 37)], "Dohanyzik", "BMI30felett"),
  title = c("Hypertension", "Diabetes mellitus","Hyperlipidaemia",
            "Current smoker", "BMI above 30 kg/m^2"),
  titleHun = c("Kórelőzményben szereplő vagy a kezelés során megállapított hypertonia",
               "Kórelőzményben szereplő vagy a kezelés során megállapított diabetes",
               "Kórelőzményben szereplő hyperlipidaemia",
               "Dohányzik", "BMI 30 kg/m^2 felett")
)
figs <- rbind(data.table(
  name = c("Eletkor40alatt", "Eletkor80felett"),
  title = c("Age below 40 years", "Age above 80 years"),
  titleHun = c("40 év alatti életkor", "80 év feletti életkor")), figs)

set.seed(1)
EletkorSamp <- sample(RawData[X.9..Felvétel.időpontja >= "2024-01-01"]$Eletkor, 500)

predgrid <- CJ(var = c(names(RawData)[c(33, 35, 37)], "Dohanyzik", "BMI30felett"),
               X.2..Neme = c("Male", "Female"),
               X.92..Diagnózis = c("STEMI", "NSTEMI"))

res <- rbindlist(lapply(1:nrow(predgrid), function(i)
  cbind(as.data.table(marginaleffects::avg_predictions(
    mgcv::gam(as.formula(paste0(predgrid$var[i], " ~ s(FelvetelNum) + s(Eletkor)")),
              data = RawData[X.2..Neme == predgrid$X.2..Neme[i] &
                               X.92..Diagnózis == predgrid$X.92..Diagnózis[i]],
              family = binomial(link = "logit")),
    newdata = marginaleffects::datagrid(
      FelvetelNum = seq(0, max(RawData$FelvetelNum), length.out = 50),
      Eletkor = EletkorSamp),
    by = "FelvetelNum")),
    var = predgrid$var[i],
    X.2..Neme = predgrid$X.2..Neme[i],
    X.92..Diagnózis = predgrid$X.92..Diagnózis[i])))
res$X.9..Felvétel.időpontja <- as.Date(res$FelvetelNum + as.numeric(as.Date("2014-01-01")))
res <- merge(res, figs[, .(var = name, title)])

ggplot(res, aes(x = X.9..Felvétel.időpontja, y = estimate, ymin = conf.low,
                ymax = conf.high, group = title, color = title, fill = title)) +
  facet_grid(~ X.92..Diagnózis + X.2..Neme) +
  geom_line() +
  geom_ribbon(alpha = 0.2, linetype = 0, show.legend = FALSE) +
  scale_x_date(breaks = "year", date_labels = "%Y") +
  guides(fill = "none") +
  labs(x = "", y = "Age-adjusted prevalence [%]", color = "Risk factor") +
  scale_y_continuous(labels = scales::percent) +
  theme(legend.position = "bottom")
ggsave("Figure1.pdf", width = 16, height = 9, device = cairo_pdf)

RawData2 <- melt(RawData[
  , .(X.2..Neme, X.92..Diagnózis, X.9..Felvétel.időpontja,
      Hypertension = X.30..Kórelőzményben.vagy.a.kezelés.során.megállapított.hypertonia,
      Diabetes = X.32..Kórelőzményben..vagy.a.kezelés.során.megállapított.diabetes,
      Hyperlipidemia = X.34..Kórelőzményben.hyperlipidaemia,
      Smoking = Dohanyzik, Obesity = BMI30felett,
      Eletkor40alatt, Eletkor80felett)],
  id.vars = c("X.2..Neme", "X.92..Diagnózis", "X.9..Felvétel.időpontja"))

RawData2$FelvIdopont <- (as.numeric(RawData2$X.9..Felvétel.időpontja) - as.numeric(as.Date("2014-01-01")))/(365.25*10)

preddf <- data.frame(FelvIdopont = (as.numeric(c(as.Date("2014-01-01"), as.Date("2024-12-31"))) - as.numeric(as.Date("2014-01-01")))/(365.25*10))

res <- RawData2[, {
  fit <- glm(value ~ FelvIdopont, family = binomial(link = "logit"))
  as.list(c(exp(c(coef(fit)["FelvIdopont"], confint(fit)["FelvIdopont",])),
            predict(fit, preddf, type = "response")))
}, .(variable, X.2..Neme, X.92..Diagnózis)]

res$formatted <- paste0(round(res$`1`*100, 1), "% → ", round(res$`2`*100, 1),
                        "%; OR = ", round(res$FelvIdopont, 2), " (",
                        round(res$`2.5 %`, 2), " - ", round(res$`97.5 %`, 2), ")")

fwrite(dcast(res, variable ~ X.2..Neme + X.92..Diagnózis, value.var = "formatted"),
       "Trend.csv", dec = ",", sep =";", bom = TRUE)

plotter2 <- function(var) {
  predgrid2 <- CJ(FelvetelNum = 0:max(RawData$FelvetelNum),
                  X.2..Neme = c("Male", "Female"),
                  X.92..Diagnózis = c("STEMI", "NSTEMI"),
                  Eletkor = seq(50, 80, 10))
  
  fit <- mgcv::gam(as.formula(paste0(
    var, " ~ te(FelvetelNum, Eletkor, by = interaction(X.2..Neme, X.92..Diagnózis)) +",
    "X.2..Neme * X.92..Diagnózis")),
    data = RawData, family = binomial(link = "logit"))
  
  predgrid2 <- cbind(predgrid2, with(
    predict(fit, predgrid2, type = "link", se.fit = TRUE),
    data.table(pred = as.numeric(plogis(fit)),
               lwr = as.numeric(plogis(fit - qnorm(0.975)*se.fit)),
               upr = as.numeric(plogis(fit + qnorm(0.975)*se.fit)))))
  predgrid2$X.9..Felvétel.időpontja <- as.Date(predgrid2$FelvetelNum + as.numeric(as.Date("2014-01-01")))

  ggplot(predgrid2,
         aes(x = X.9..Felvétel.időpontja, y = pred, ymin = lwr, ymax = upr,
             color = factor(Eletkor), fill = factor(Eletkor))) +
    facet_grid(~ X.92..Diagnózis + X.2..Neme) + geom_line() +
    geom_ribbon(alpha = 0.2, linetype = 0) +
    labs(x = "", y = "Prevalence [%]", color = "Age [year]",
         fill = "Age [year]") +
    scale_x_date(expand = expansion(mult = c(0.2, 0.2))) +
    scale_y_continuous(labels = scales::percent) +
    geom_text(data = predgrid2[FelvetelNum == min(predgrid2$FelvetelNum)],
              aes(x = X.9..Felvétel.időpontja, y = pred,
                  color = factor(Eletkor),
                  label = paste0(round(pred * 100, 1), "%")),
              show.legend = FALSE, hjust = 1) +
    geom_text(data = predgrid2[FelvetelNum == max(predgrid2$FelvetelNum)],
              aes(x = X.9..Felvétel.időpontja, y = pred,
                  color = factor(Eletkor),
                  label = paste0(round(pred * 100, 1), "%")),
              show.legend = FALSE, hjust = -0.1)
}


cl <- parallel::makeCluster(parallelly::availableCores() - 1)
parallel::clusterEvalQ(cl, library(data.table))
parallel::clusterEvalQ(cl, {library(ggplot2); theme_set(theme_bw());})
parallel::clusterExport(cl, c("plotter2", "RawData"))

temp <- parallel::parLapply(
  cl, c("X.30..Kórelőzményben.vagy.a.kezelés.során.megállapított.hypertonia",
        "BMI30felett", "Dohanyzik", "X.34..Kórelőzményben.hyperlipidaemia",
        "X.32..Kórelőzményben..vagy.a.kezelés.során.megállapított.diabetes"),
  function(v) ggsave(paste0("", v, "_agespecific.pdf"), plotter2(v),
                     width = 16, height = 9, device = cairo_pdf))

parallel::stopCluster(cl)

RawData$RiskFactorSum <- RawData$X.30..Kórelőzményben.vagy.a.kezelés.során.megállapított.hypertonia +
  RawData$BMI30felett + RawData$Dohanyzik +
  RawData$X.34..Kórelőzményben.hyperlipidaemia +
  RawData$X.32..Kórelőzményben..vagy.a.kezelés.során.megállapított.diabetes

mean(!is.na(RawData$RiskFactorSum))

predgrid2 <- CJ(FelvetelNum = 0:max(RawData$FelvetelNum),
                X.2..Neme = c("Male", "Female"),
                X.92..Diagnózis = c("STEMI", "NSTEMI"),
                Eletkor = seq(50, 80, 10))

fit <- mgcv::gam(cbind(RiskFactorSum, 5 - RiskFactorSum) ~ te(FelvetelNum, Eletkor, by = interaction(X.2..Neme, X.92..Diagnózis)) + X.2..Neme * X.92..Diagnózis,
                 data = RawData, family = binomial(link = "logit"))

predgrid2 <- cbind(predgrid2, with(predict(fit, predgrid2, type = "link", se.fit = TRUE),
                                   data.table(pred = as.numeric(plogis(fit)*5),
                                              lwr = as.numeric(plogis(fit - qnorm(0.975)*se.fit)*5),
                                              upr = as.numeric(plogis(fit + qnorm(0.975)*se.fit)*5))))
predgrid2$X.9..Felvétel.időpontja <- as.Date(predgrid2$FelvetelNum + as.numeric(as.Date("2014-01-01")))

ggplot(predgrid2, aes(x = X.9..Felvétel.időpontja, y = pred, ymin = lwr,
                      ymax = upr, color = factor(Eletkor), fill = factor(Eletkor))) +
  facet_grid(~ X.92..Diagnózis + X.2..Neme) +
  scale_x_date(breaks = "year", date_labels = "%Y") +
  geom_line() +
  geom_ribbon(alpha = 0.2, linetype = 0) +
  labs(x = "", y = "Predicted number", color = "Age [year]", fill = "Age [year]") +
  theme(legend.position = "bottom")
ggsave("Figure2.pdf", width = 16, height = 9, device = cairo_pdf)

fit <- mgcv::gam(RiskFactorSum + 1 ~ te(FelvetelNum, Eletkor, by = interaction(X.2..Neme, X.92..Diagnózis)) + X.2..Neme * X.92..Diagnózis,
                 data = RawData, family = mgcv::ocat(R = 6))

predgrid2 <- CJ(FelvetelNum = 0:max(RawData$FelvetelNum),
                X.2..Neme = c("Male", "Female"),
                X.92..Diagnózis = c("STEMI", "NSTEMI"),
                Eletkor = seq(40, 90, 10))

predgrid2 <- cbind(predgrid2, predict(fit, predgrid2, type = "response"))
predgrid2 <- melt(predgrid2, id.vars = c("FelvetelNum", "X.2..Neme", "X.92..Diagnózis", "Eletkor"))
predgrid2$X.9..Felvétel.időpontja <- as.Date(predgrid2$FelvetelNum + as.numeric(as.Date("2014-01-01")))
predgrid2$variable <- as.numeric(substring(predgrid2$variable, 2)) - 1

ggplot(predgrid2, aes(x = X.9..Felvétel.időpontja, y = value, fill = factor(variable))) +
  facet_grid(X.92..Diagnózis + X.2..Neme ~ Eletkor) + geom_area() +
  scale_x_date(breaks = "2 year", date_labels = "%Y") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "", y = "Proportion [%]", fill = "Number of\nrisk factors") +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 1))
ggsave("Figure3.pdf", width = 16, height = 9, device = cairo_pdf)

ComplexUpset::upset(
  RawData[!is.na(RiskFactorSum),
          .(`Hypertension` = X.30..Kórelőzményben.vagy.a.kezelés.során.megállapított.hypertonia,
            `BMI above 30 kg/m^2` = BMI30felett,
            `Current smoker` = Dohanyzik,
            `Hyperlipidaemia` = X.34..Kórelőzményben.hyperlipidaemia,
            `Diabetes mellitus` = X.32..Kórelőzményben..vagy.a.kezelés.során.megállapított.diabetes,
            Eletkor, X.2..Neme, X.92..Diagnózis)],
  c("Hypertension", "BMI above 30 kg/m^2", "Current smoker", "Hyperlipidaemia",
    "Diabetes mellitus"),
  keep_empty_groups = TRUE, n_intersections = 20, name = "",
  base_annotations = list(
    "Intersection size" = (
      ComplexUpset::intersection_size() +
        ylab("Frequency of the combination"))),
  annotations = list(
    "Age distribution" = (
      ggplot(mapping = aes(y = Eletkor)) +
        geom_boxplot(outlier.shape = NA) +
        geom_hline(yintercept = median(RawData[!is.na(RiskFactorSum)]$Eletkor), linetype = "dashed", color = "red")
    ),
    "Distribution of gender and type of AMI" = (
      ggplot(mapping = aes(fill = paste0(X.2..Neme, ", ", X.92..Diagnózis))) +
        geom_bar(position = "fill") + scale_y_continuous(labels = scales::percent) +
        labs(y = "Distribution of gender and type of AMI", fill = "")
    )
  ))
ggsave("Figure4.pdf", width = 16*1.2, height = 9*1.2, device = cairo_pdf)