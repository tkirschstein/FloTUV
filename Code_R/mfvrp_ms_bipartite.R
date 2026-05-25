# =============================================================================
# MFVRP-MS als bipartites Matching-Problem
# Quelle: Wittig, Bierwirth, Kirschstein (2025)
#
# Struktur des Dual-Load-Problems:
#   - n Kunden werden in zwei gleich große Mengen N1 und N2 aufgeteilt
#   - Jede Tour bedient genau einen Kunden aus N1 und einen aus N2
#     (Dual-Load: jeder Kunde nutzt 50% der Nutzlast)
#   - Fahrzeugtypen: konventioneller LKW (c-truck) und Elektro-LKW (e-truck)
#   - Kunden haben individuelle Präferenzen: kostensensitiv (w=0) oder
#     emissionssensitiv (w=1)
#   - MFVRP-MS maximiert die mittlere Kundenzufriedenheit
# =============================================================================

library(tidyverse)
library(ompr)
library(ompr.roi)
library(ROI.plugin.glpk)

set.seed(4721)


# ---- 1. Fahrzeugparameter ---------------------------------------------------
# Quellen für Richtwerte: Syré et al. (2025), Icha & Lauf (2025)
# Emissionen als Well-to-Wheel (WTW) CO2-Äquivalente
veh <- list(
  type  = c("c-truck", "e-truck"),
  c_fix = c(314,  375),   # Fixkosten je Tour-Einsatz [€/Tour]
  c_var = c(0.57, 0.75),  # Variable Kosten [€/km]
  e_fix = c(  0,    0),   # Fixemissionen je Tour [kg CO2e] (hier 0)
  e_var = c(1.07, 0.44),  # Variable Emissionen [kg CO2e/km]
  v_max = c(6, 6)         # Maximale Fahrzeugeanzahl je Typ
)
n_types <- 2


# ---- 2. Kundeninstanz generieren --------------------------------------------
n <- 12   # Anzahl Kunden — muss gerade sein (Dual-Load-Bedingung)
stopifnot(n %% 2 == 0)

customers <- tibble(
  id   = 1:n,
  x    = runif(n, 0, 100),
  y    = runif(n, 0, 100),
  # Kundenpräferenz: 0 = kostensensitiv, 1 = emissionssensitiv
  pref = sample(c(0, 1), n, replace = TRUE, prob = c(0.5, 0.5))
)

# Depot (zentral im Untersuchungsgebiet)
depot <- c(x = 50, y = 50)

cat(sprintf(
  "Instanz: %d Kunden  | %d kostensensitiv, %d emissionssensitiv\n",
  n, sum(customers$pref == 0), sum(customers$pref == 1)
))


# ---- 3. Tourlängenmatrix ----------------------------------------------------
# Tourlänge = Depot → Kunde i → Kunde j → Depot  [km]
d2 <- function(x1, y1, x2, y2) sqrt((x1 - x2)^2 + (y1 - y2)^2)

customers <- customers |>
  mutate(d_depot = d2(x, y, depot["x"], depot["y"]))

# create data frame of all pairs of customers
pairs <- tidyr::crossing(
  customers |> select(id, x, y, d_depot) |> rename(i = id, xi = x, yi = y, d_depot_i = d_depot),
  customers |> select(id, x, y, d_depot) |> rename(j = id, xj = x, yj = y, d_depot_j = d_depot)
) |>
  filter(i < j)

# add tour length to pairs
pairs <- pairs |>
  mutate(tl = d_depot_i + d2(xi, yi, xj, yj) + d_depot_j)

# add to pairs total cost, total emissions as well as allocated cost and emissions for i and j
veh_tbl <- tibble(
  t     = seq_len(n_types),
  type  = veh$type,
  c_fix = veh$c_fix,
  c_var = veh$c_var,
  e_fix = veh$e_fix,
  e_var = veh$e_var
)

pairs <- pairs |>
  tidyr::crossing(veh_tbl) |>
  mutate(
    tc   = c_fix + c_var * tl,   # total cost  [€]
    te   = e_fix + e_var * tl,   # total emissions  [kg CO2e]
    ac_i = tc * d_depot_i /(d_depot_i+d_depot_j),               # allocated cost for i  (EN 16258, equal load)
    ae_i = te * d_depot_i /(d_depot_i+d_depot_j),               # allocated emissions for i
    ac_j = tc * d_depot_j /(d_depot_i+d_depot_j),               # allocated cost for j
    ae_j = te * d_depot_j /(d_depot_i+d_depot_j)                # allocated emissions for j
  )

# ---- 5. Zufriedenheitsschranken pro Kunde -----------------------------------
# Für jeden Kunden: bestes und schlechtestes mögliches Ergebnis
# über alle Tourpartner und Fahrzeugtypen

# Schranken für N1-Kunden (Minimum/Maximum über N2-Dimension und Typen)
bounds_N1 <- map(seq_len(n1), function(ii) list(
  c_lo = min(AC[ii, , ]), c_hi = max(AC[ii, , ]),
  e_lo = min(AE[ii, , ]), e_hi = max(AE[ii, , ])
))

# Schranken für N2-Kunden (Minimum/Maximum über N1-Dimension und Typen)
bounds_N2 <- map(seq_len(n2), function(jj) list(
  c_lo = min(AC[, jj, ]), c_hi = max(AC[, jj, ]),
  e_lo = min(AE[, jj, ]), e_hi = max(AE[, jj, ])
))


# ---- 6. OS-Matrix: kombinierte Kundenzufriedenheit [n1 × n2 × n_types] -----
# OS_i(tour) = (1-w_i) * (c_hi - c_i)/(c_hi - c_lo)
#            +    w_i  * (e_hi - e_i)/(e_hi - e_lo)
# OS(tour)   = OS_i + OS_j  (beide Kunden auf der Tour)

norm_sat <- function(val, lo, hi) {
  if (hi > lo) (hi - val) / (hi - lo) else 1.0
}

OS <- array(0, dim = c(n1, n2, n_types))

for (ii in seq_len(n1)) {
  w_i <- customers$pref[N1_idx[ii]]
  b_i <- bounds_N1[[ii]]
  for (jj in seq_len(n2)) {
    w_j <- customers$pref[N2_idx[jj]]
    b_j <- bounds_N2[[jj]]
    for (t in seq_len(n_types)) {
      os_i <- (1 - w_i) * norm_sat(AC[ii, jj, t], b_i$c_lo, b_i$c_hi) +
                   w_i  * norm_sat(AE[ii, jj, t], b_i$e_lo, b_i$e_hi)
      os_j <- (1 - w_j) * norm_sat(AC[ii, jj, t], b_j$c_lo, b_j$c_hi) +
                   w_j  * norm_sat(AE[ii, jj, t], b_j$e_lo, b_j$e_hi)
      OS[ii, jj, t] <- os_i + os_j
    }
  }
}


# ---- 7. Matching-Modell aufbauen und lösen ----------------------------------
# Entscheidungsvariablen: x[ii, jj, t] ∈ {0,1}
#   = 1, wenn Kunde ii ∈ N1 und Kunde jj ∈ N2 auf einer Tour mit Typ t zusammen
#
# Constraints:
#   (a) Jeder N2-Kunde wird genau einmal bedient
#   (b) Jeder N1-Kunde wird genau einmal bedient
#   (c) Flottenkapazität je Fahrzeugtyp
#
# Ziel MFVRP-MS: Gesamtzufriedenheit maximieren
#      MFVRP-TC: Gesamtkosten minimieren
#      MFVRP-TE: Gesamtemissionen minimieren

build_model <- function(obj_arr, sense) {
  MIPModel() |>
    add_variable(
      x[ii, jj, t],
      ii = 1:n1, jj = 1:n2, t = 1:n_types,
      type = "binary"
    ) |>
    # (a) Jeder N2-Kunde genau einmal
    add_constraint(
      sum_over(x[ii, jj, t], ii = 1:n1, t = 1:n_types) == 1,
      jj = 1:n2
    ) |>
    # (b) Jeder N1-Kunde genau einmal
    add_constraint(
      sum_over(x[ii, jj, t], jj = 1:n2, t = 1:n_types) == 1,
      ii = 1:n1
    ) |>
    # (c) Flottenkapazität
    add_constraint(
      sum_over(x[ii, jj, t], ii = 1:n1, jj = 1:n2) <= veh$v_max[t],
      t = 1:n_types
    ) |>
    set_objective(
      sum_over(obj_arr[ii, jj, t] * x[ii, jj, t],
               ii = 1:n1, jj = 1:n2, t = 1:n_types),
      sense = sense
    )
}

solve_mfvrp <- function(obj_arr, sense, label) {
  res <- solve_model(
    build_model(obj_arr, sense),
    with_ROI(solver = "glpk", verbose = FALSE)
  )
  asgn <- get_solution(res, x[ii, jj, t]) |>
    filter(value > 0.5) |>
    select(ii, jj, t)
  list(label = label, result = res, assignments = asgn)
}

sol_ms <- solve_mfvrp(OS, "max", "MFVRP-MS")
sol_tc <- solve_mfvrp(TC, "min", "MFVRP-TC")
sol_te <- solve_mfvrp(TE, "min", "MFVRP-TE")


# ---- 8. Vergleichsmetriken --------------------------------------------------
evaluate <- function(sol) {
  a <- sol$assignments
  total_os <- sum(map_dbl(seq_len(nrow(a)), \(k) OS[a$ii[k], a$jj[k], a$t[k]]))
  total_tc <- sum(map_dbl(seq_len(nrow(a)), \(k) TC[a$ii[k], a$jj[k], a$t[k]]))
  total_te <- sum(map_dbl(seq_len(nrow(a)), \(k) TE[a$ii[k], a$jj[k], a$t[k]]))
  n_e      <- sum(a$t == 2)
  tibble(
    Modell                         = sol$label,
    `Ø Zufriedenheit`              = round(total_os / n, 3),
    `Gesamtkosten [€]`             = round(total_tc, 1),
    `Gesamtemissionen [kg CO2e]`   = round(total_te, 1),
    `Touren mit e-truck`           = n_e
  )
}

comparison <- bind_rows(evaluate(sol_ms), evaluate(sol_tc), evaluate(sol_te))
print(comparison)


# ---- 9. Visualisierung der MFVRP-MS-Lösung ----------------------------------
plot_solution <- function(sol) {
  a <- sol$assignments |>
    mutate(
      x_i = customers$x[N1_idx[ii]], y_i = customers$y[N1_idx[ii]],
      x_j = customers$x[N2_idx[jj]], y_j = customers$y[N2_idx[jj]],
      id_i = N1_idx[ii],
      id_j = N2_idx[jj],
      vehicle = factor(veh$type[t], levels = veh$type)
    )

  # Routen als Segmente: Depot → i → j → Depot
  segs_depot_i <- a |> transmute(x = depot["x"], y = depot["y"], xend = x_i, yend = y_i, vehicle)
  segs_i_j     <- a |> transmute(x = x_i, y = y_i, xend = x_j, yend = y_j, vehicle)
  segs_j_depot <- a |> transmute(x = x_j, y = y_j, xend = depot["x"], yend = depot["y"], vehicle)

  segs <- bind_rows(segs_depot_i, segs_i_j, segs_j_depot)

  cust_plot <- customers |>
    mutate(
      gruppe  = if_else(id %in% N1_idx, "N1", "N2"),
      praef   = factor(pref, labels = c("kostensensitiv", "emissionssensitiv"))
    )

  ggplot() +
    geom_segment(
      data = segs,
      aes(x = x, y = y, xend = xend, yend = yend, color = vehicle),
      linewidth = 0.75, alpha = 0.8
    ) +
    geom_point(
      data = cust_plot,
      aes(x, y, shape = praef, fill = gruppe),
      size = 4, stroke = 0.6, color = "white"
    ) +
    annotate("point", x = depot["x"], y = depot["y"],
             shape = 22, size = 5, fill = "black", color = "white", stroke = 1) +
    annotate("text",  x = depot["x"], y = depot["y"] - 5,
             label = "Depot", size = 3, fontface = "bold") +
    geom_text(data = cust_plot, aes(x, y, label = id),
              color = "black", size = 2.5, fontface = "bold") +
    scale_color_manual(
      values = c("c-truck" = "#D4622A", "e-truck" = "#2B7BB9"),
      name = "Fahrzeugtyp"
    ) +
    scale_fill_manual(
      values = c("N1" = "#5B84C4", "N2" = "#E07B5D"),
      name = "Kundengruppe"
    ) +
    scale_shape_manual(
      values = c("kostensensitiv" = 21, "emissionssensitiv" = 24),
      name = "Kundenpräferenz"
    ) +
    guides(fill = guide_legend(override.aes = list(shape = 21, size = 4))) +
    labs(
      title    = paste0(sol$label, ": Tourenplan"),
      subtitle = sprintf(
        "n = %d Kunden | Ø Zufriedenheit: %.3f | Kosten: %.0f € | Emissionen: %.0f kg CO2e",
        n,
        sum(map_dbl(seq_len(nrow(a)), \(k) OS[a$ii[k], a$jj[k], a$t[k]])) / n,
        sum(map_dbl(seq_len(nrow(a)), \(k) TC[a$ii[k], a$jj[k], a$t[k]])),
        sum(map_dbl(seq_len(nrow(a)), \(k) TE[a$ii[k], a$jj[k], a$t[k]]))
      ),
      x = "x [km]", y = "y [km]"
    ) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "right")
}

plot_solution(sol_ms)
