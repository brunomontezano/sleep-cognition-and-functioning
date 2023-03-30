dados <- haven::read_sav("data/banco-conversao-16-10-20.sav") |>
  dplyr::filter(!is.na(mora_t2)) |>
  janitor::clean_names()

dados_tai <-
  haven::read_sav("data/banco-conversao-t1-t2-mario2.sav")

dados_limpos <- dados |>
  dplyr::mutate(
    dplyr::across(dplyr::starts_with("numletr"),
                  \(x) tidyr::replace_na(x, 0)),
    riscodesuicidioatual = tidyr::replace_na(riscodesuicidioatual, 1),
    grupos = dplyr::case_when(
      tb_erros == 3 ~ 'Conversion to BD',
      (mini_a08at_t2 == 1 |
         mini_a08atpa_t2 == 1 |
         mini_a15b_t2 == 1)
      &
        tb_erros != 3 ~ 'Depression at follow-up',
      (
        is.na(mini_a08at_t2) &
          is.na(mini_a08atpa_t2) &
          is.na(mini_a15b_t2)
      )
      &
        tb_erros != 3 ~ 'No mood episode at follow-up'
    ),
    anxiety_dic = dplyr::case_when(
      mini_f04c_t2 == 1 |
        mini_f04d_t2 == 1 |
        mini_p06_t2 == 1 |
        mini_g06_t2 == 1 |
        mini_g07_t2 == 1 ~ 1,
      .default = 0
    )
  ) |>
  dplyr::rowwise() |>
  dplyr::mutate(numletr_total = sum(dplyr::c_across(numletr01_t2:numletr021_t2))) |>
  dplyr::ungroup() |>
  dplyr::left_join(
    dados_tai |>
      dplyr::rename(rec = a02rec) |>
      dplyr::select(rec, lifetimepsychiatricmed_t2, maritalstatusdic_t2),
    by = "rec"
  )

psqi_plot <- ggstatsplot::ggbetweenstats(
  data = dados_limpos,
  x = grupos,
  y = psqi_total,
  type = "nonparametric",
  xlab = "Mood group at follow-up",
  ylab = "PSQI total score",
  results.subtitle = FALSE,
  centrality.label.args = list(size = 5, nudge_x = 0.4, segment.linetype = 4,
                               min.segment.length = 0, family = "Charter"),
  ggsignif.args = list(textsize = 5, tip_length = 0.01, na.rm = TRUE,
                       family = "Charter"),
  ggtheme = ggplot2::theme_minimal(base_size = 18,
                                   base_family = "Charter"),
  ggplot.component = ggplot2::labs(caption = NULL)
)

psqi_plot

fast_plot <- ggstatsplot::ggbetweenstats(
  data = dados_limpos,
  x = grupos,
  y = fast_total_t2,
  type = "nonparametric",
  xlab = "Mood group at follow-up",
  ylab = "FAST total score",
  results.subtitle = FALSE,
  centrality.label.args = list(size = 5, nudge_x = 0.4, segment.linetype = 4,
                               min.segment.length = 0, family = "Charter"),
  ggsignif.args = list(textsize = 5, tip_length = 0.01, na.rm = TRUE,
                       family = "Charter"),
  ggtheme = ggplot2::theme_minimal(base_size = 18,
                                   base_family = "Charter"),
  ggplot.component = ggplot2::labs(caption = NULL)
)

fast_plot

cobra_plot <- ggstatsplot::ggbetweenstats(
  data = dados_limpos,
  x = grupos,
  y = cobra_soma_t2_16itens,
  type = "nonparametric",
  xlab = "Mood group at follow-up",
  ylab = "COBRA total score",
  results.subtitle = FALSE,
  centrality.label.args = list(size = 5, nudge_x = 0.4, segment.linetype = 4,
                               min.segment.length = 0, family = "Charter"),
  ggsignif.args = list(textsize = 5, tip_length = 0.01, na.rm = TRUE,
                       family = "Charter"),
  ggtheme = ggplot2::theme_minimal(base_size = 18,
                                   base_family = "Charter"),
  ggplot.component = ggplot2::labs(caption = NULL)
)

cobra_plot

numletr_plot <- ggstatsplot::ggbetweenstats(
  data = dados_limpos,
  x = grupos,
  y = numletr_total,
  type = "nonparametric",
  xlab = "Mood group at follow-up",
  ylab = "Letter-number sequencing total score",
  results.subtitle = FALSE,
  centrality.label.args = list(size = 5, nudge_x = 0.4, segment.linetype = 4,
                               min.segment.length = 0, family = "Charter"),
  ggsignif.args = list(textsize = 5, tip_length = 0.01, na.rm = TRUE,
                       family = "Charter"),
  ggtheme = ggplot2::theme_minimal(base_size = 18,
                                   base_family = "Charter"),
  ggplot.component = ggplot2::labs(caption = NULL)
)

numletr_plot

dados_limpos |> dplyr::count(a20medicpsi_t2)

dados_limpos <- dados_limpos |> 
  dplyr::mutate(
    a20medicpsi_t2 = dplyr::na_if(a20medicpsi_t2, 3)
  )


## ADICIONAR MEDICACAO NOS MODELOS
bd <- dados_limpos |> 
      dplyr::filter(grupos == 'Conversion')

# Creating MDD in current episode subset
mdd_cur <- dados_limpos |> 
           dplyr::filter(grupos == 'Persistent')

# Creating euthymic MDD subset
mdd_eut <- dados_limpos |> 
           dplyr::filter(grupos == 'Remitted')
# BD group
summary(fast_bd_adj <- lm(fast_total_t2 ~ psqi_total + escolaridade + lifetimepsychiatricmed_t2 + hipnoticosabudep + a20medicpsic_t2, data = bd))
confint(fast_bd_adj)
summary(cobra_bd_adj <- lm(cobra_soma_t2_16itens ~ psqi_total + escolaridade + lifetimepsychiatricmed_t2 + a20medicpsic_t2, data = bd))
confint(cobra_bd_adj)
summary(numletr_bd_adj <- lm(numletr_total ~ psqi_total + escolaridade + hipnoticosabudep + a20medicpsic_t2, data = bd))
confint(numletr_bd_adj)

# Euthymic MDD group
summary(fast_mdd_eut_adj <- lm(fast_total_t2 ~ psqi_total + anxiety_dic + a20medicpsic_t2, data = mdd_eut))
confint(fast_mdd_eut_adj)
summary(cobra_mdd_eut_adj <-lm(cobra_soma_t2_16itens ~ psqi_total + anxiety_dic + a20medicpsic_t2, data = mdd_eut))
confint(cobra_mdd_eut_adj)
summary(numletr_mdd_eut_adj <- lm(numletr_total ~ psqi_total, data = mdd_eut))
confint(numletr_mdd_eut_adj)

# MDD in current episode group
summary(fast_mdd_cur_adj <- lm(fast_total_t2 ~ psqi_total + escolaridade + riscodesuicidioatual + anxiety_dic + a20medicpsic_t2, data = mdd_cur))
confint(fast_mdd_cur_adj)
summary(cobra_mdd_cur_adj <- lm(cobra_soma_t2_16itens ~ psqi_total + escolaridade + riscodesuicidioatual + a20medicpsic_t2, data = mdd_cur))
confint(cobra_mdd_cur_adj)
summary(numletr_mdd_cur_adj <- lm(numletr_total ~ psqi_total + escolaridade + riscodesuicidioatual, data = mdd_cur))
confint(numletr_mdd_cur_adj)

# plots psqi
set.seed(123)

dados_limpos |>
  dplyr::select(rec, grupos, psqi_total, fast_total_t2,
                dplyr::matches("^psqi_comp.*$")) |> 
  tidyr::pivot_longer(
    cols = dplyr::starts_with("psqi_comp"),
    names_to = "comp",
    names_prefix = "psqi_comp",
    values_to = "escore_comp"
  ) |> 
  dplyr::group_by(comp) |> 
  dplyr::mutate(
    cor_fast = cor.test(fast_total_t2, escore_comp, na.rm = TRUE)$estimate
  )

dados_limpos |>
  dplyr::select(rec, grupos, psqi_total, fast_total_t2,
                dplyr::matches("^psqi_comp.*$")) |> 
  tidyr::pivot_longer(
    cols = dplyr::starts_with("psqi_comp"),
    names_to = "comp",
    names_prefix = "psqi_comp",
    values_to = "escore_comp"
  ) |> 
  dplyr::group_by(comp) |> 
  dplyr::summarise(
    r = cor.test(escore_comp, fast_total_t2, na.rm = TRUE)$estimate,
    p_value = cor.test(escore_comp, fast_total_t2, na.rm = TRUE)$p.value
  ) |> 
  dplyr::mutate(signif = p_value < 0.05)

dados_limpos |>
  dplyr::select(rec, grupos, psqi_total, fast_total_t2,
                dplyr::matches("^psqi_comp.*$")) |> 
  tidyr::pivot_longer(
    cols = dplyr::starts_with("psqi_comp"),
    names_to = "comp",
    names_prefix = "psqi_comp",
    values_to = "escore_comp"
  ) |> 
  dplyr::mutate(
    comp = dplyr::case_when(
      comp == 1 ~ "Subjective sleep quality",
      comp == 2 ~ "Sleep latency",
      comp == 3 ~ "Sleep duration",
      comp == 4 ~ "Sleep efficiency",
      comp == 5 ~ "Sleep disturbance",
      comp == 6 ~ "Use of sleep medication",
      comp == 7 ~ "Daytime dysfunction"
    )
  ) |> 
  ggplot2::ggplot(ggplot2::aes(x = escore_comp, y = fast_total_t2)) +
  ggplot2::geom_jitter(height = 0, show.legend = TRUE) +
  ggpubr::stat_cor(method = "pearson", label.y = 68, label.x = 0.13,
                   p.digits = 0, size = 5, family = "Charter") +
  ggplot2::geom_smooth(method = "lm", show.legend = FALSE) +
  ggplot2::facet_wrap(~ comp) +
  ggplot2::labs(x = "Score at PSQI component",
                y = "FAST score") +
  ggplot2::theme_minimal(base_size = 18, base_family = "Charter")
