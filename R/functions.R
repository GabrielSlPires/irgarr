open.irga <- function(file, acao = FALSE) {
  x <- data.table::fread(file, skip = 10)
  y <- x[x$ACAO != "", ]
  y <- y[y$YYYYMMDD != "in", ]
  col_index <- seq(1,length(colnames(y)))
  col_index <- col_index[duplicated(colnames(y))]
  y <- dplyr::select(y, ,-all_of(col_index))
  write.csv(unique(y), "dummy.csv")
  y <- data.table::fread("dummy.csv")
  file.remove("dummy.csv")
  y <- y[, -1]
  if (acao) {
    print(table(y$ACAO))
  }
  return(y)
}

plot.curve <- function(x, #Tabela do IRGA aberta com a funcao open.irga()
                       day, #Estou usando o formato MM_DD
                       #Abaixo sempre enviem dois valores dentro de c()
                       acao = c("A_Ci", "A_PAR"), #Nome da coluna ACAO do IRGA para filtrar as curvas
                       oxygen = c(21, 2), #Valores de oxigenio
                       co2_400 = FALSE, #Deixar ponto de 400ppm de CO2 vermelho
                       leak = FALSE, #Possui dados do Leak?
                       leak_acao = "LEAK", #Nome do Leak em ACAO
                       leak_before = FALSE, #Deseja plotar dados sem leak em cinza?
                       save = TRUE, #deseja salvar?
                       file_format = "png" #Formato para salvar o grafico
                       ) {

  var_x <- c("Ci", "PARi")

    if (leak) {
      leak_corection <- dplyr::filter(x, ACAO == leak_acao)
      corection <- lm(leak_corection$Photo ~ leak_corection$CO2S)
      message(paste("The R^2 for leak correction is:",
                    summary(corection)$r.squared))
      a <- corection$coefficients[2]
      b <- corection$coefficients[1]
      x <- dplyr::mutate(x,
                         ci_old = Ci,
                         photo_old = Photo,
                         Leak = a * CO2S + b,
                         Photo = ifelse(ACAO == acao[1],
                                        Photo + Leak,
                                        Photo),
                         Ci = ifelse(ACAO == acao[1],
                                     ((CndCO2-Trans/2)*CO2S-Photo)/(CndCO2+Trans/2),
                                     Ci)
                         )
    }

  for (i in 1:2) {
    for (j in 1:2) {
      data <- dplyr::filter(x, ACAO == acao[i],
                            `Oxygen%` == oxygen[j])

      title <- paste0(day, ": ", acao[i], " ", oxygen[j], "%")

      p <- ggplot2::ggplot(data,
                           ggplot2::aes_string(x = var_x[i],
                                               y = "Photo",
                                               group = 1))

      if (leak_before & i == 1) {
        old <- ggplot2::aes_string(x = "ci_old",
                                   y = "photo_old",
                                   group = 2)

        p <- p + ggplot2::geom_point(old,
                              col = "grey60") +
          ggplot2::geom_line(old,
                             col = "grey60")
      }

      p <- p +
        ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(10)) +
        ggplot2::geom_point(color = ifelse(co2_400 & (data$CO2R > 399 & data$CO2R < 401),
                                    "red",
                                    "black")) +
        ggplot2::geom_line() +
        ggplot2::theme_bw() +
        ggplot2::ggtitle(title) +
        ggplot2::ylab("A") +
        ggplot2::theme(legend.position = "none")

      print(p)

      if (save) {
        dir.create("plots/", showWarnings = FALSE)
        ggplot2::ggsave(paste0("plots/",
                               day,
                               "_",
                               acao[i],
                               "_",
                               oxygen[j],
                               ".",
                               file_format),
               p,
               device = file_format)
      }
    }
  }
}
