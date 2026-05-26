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
library(Matrix)
library(Rglpk)
library(ggplot2)
library(patchwork)

set.seed(4721)

# ---- 1. Fahrzeugparameter ---------------------------------------------------
# Quellen für Richtwerte: Syré et al. (2025), Icha & Lauf (2025)
# Emissionen als Well-to-Wheel (WTW) CO2-Äquivalente
veh.data <- list(
  type  = c("c-truck", "e-truck"),
  c_fix = c(314,  375),   # Fixkosten je Tour-Einsatz [€/Tour]
  c_var = c(0.57, 0.75),  # Variable Kosten [€/km]
  e_fix = c(  0,    0),   # Fixemissionen je Tour [kg CO2e] (hier 0)
  e_var = c(1.07, 0.44)   # Variable Emissionen [kg CO2e/km]
)
n_types <- 2


instance.solve <- function(veh = veh.data, customers = cust.mat, depot = depot.coord, veh.cap = c(nrow(customers) / 2, nrow(customers) / 2)) {

  n <- nrow(customers)
  stopifnot(n %% 2 == 0)

  
  cat(sprintf(
    "Instanz: %d Kunden  | %d kostensensitiv, %d emissionssensitiv\n",
    n, sum(customers$pref == 0), sum(customers$pref == 1)
  ))

  # ---- 2. Tourlängenmatrix --------------------------------------------------
  # Tourlänge = Depot → Kunde i → Kunde j → Depot  [km]
  d2 <- function(x1, y1, x2, y2) sqrt((x1 - x2)^2 + (y1 - y2)^2)

  customers <- customers |>
    mutate(d_depot = d2(x, y, depot["x"], depot["y"]))

  # Alle Kundenpaare direkt mit combn (vermeidet n²-crossing + Filter)
  idx   <- combn(customers$id, 2)
  pairs <- tibble(i = idx[1, ], j = idx[2, ]) |>
    left_join(customers |> select(id, xi = x, yi = y, d_depot_i = d_depot), by = c("i" = "id")) |>
    left_join(customers |> select(id, xj = x, yj = y, d_depot_j = d_depot), by = c("j" = "id")) |>
    mutate(tl = d_depot_i + d2(xi, yi, xj, yj) + d_depot_j) |>
    left_join(customers |> select(id, pref_i = pref), by = c("i" = "id")) |>
    left_join(customers |> select(id, pref_j = pref), by = c("j" = "id"))

  # ---- 3. Kosten & Emissionen je Fahrzeugtyp --------------------------------
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
      tc   = c_fix + c_var * tl,                             # Gesamtkosten  [€]
      te   = e_fix + e_var * tl,                             # Gesamtemissionen  [kg CO2e]
      ac_i = tc * d_depot_i / (d_depot_i + d_depot_j),      # anteilige Kosten Kunde i  (EN 16258)
      ae_i = te * d_depot_i / (d_depot_i + d_depot_j),      # anteilige Emissionen Kunde i
      ac_j = tc * d_depot_j / (d_depot_i + d_depot_j),      # anteilige Kosten Kunde j
      ae_j = te * d_depot_j / (d_depot_i + d_depot_j)       # anteilige Emissionen Kunde j
    )

  # ---- 4. Zufriedenheitsschranken & -werte ----------------------------------
  # Für jeden Kunden: bestes und schlechtestes mögliches Ergebnis
  # über alle Tourpartner und Fahrzeugtypen (vektorisiert via group_by)
  bounds_tbl <- bind_rows(
    pairs |> select(cust = i, ac = ac_i, ae = ae_i),
    pairs |> select(cust = j, ac = ac_j, ae = ae_j)
  ) |>
    summarise(
      c_lo = min(ac), c_hi = max(ac),
      e_lo = min(ae), e_hi = max(ae),
      .by = cust
    )

  # OS_i = (1-w_i)*(c_hi - c_i)/(c_hi - c_lo) + w_i*(e_hi - e_i)/(e_hi - e_lo)
  # Vektorisierte Berechnung: join bounds, dann mutate
  pairs <- pairs |>
    left_join(bounds_tbl |> rename(i = cust, c_lo_i = c_lo, c_hi_i = c_hi,
                                             e_lo_i = e_lo, e_hi_i = e_hi), by = "i") |>
    left_join(bounds_tbl |> rename(j = cust, c_lo_j = c_lo, c_hi_j = c_hi,
                                             e_lo_j = e_lo, e_hi_j = e_hi), by = "j") |>
    mutate(
      os_i = (1 - pref_i) * if_else(c_hi_i > c_lo_i, (c_hi_i - ac_i) / (c_hi_i - c_lo_i), 1) +
               pref_i      * if_else(e_hi_i > e_lo_i, (e_hi_i - ae_i) / (e_hi_i - e_lo_i), 1),
      os_j = (1 - pref_j) * if_else(c_hi_j > c_lo_j, (c_hi_j - ac_j) / (c_hi_j - c_lo_j), 1) +
               pref_j      * if_else(e_hi_j > e_lo_j, (e_hi_j - ae_j) / (e_hi_j - e_lo_j), 1),
      os   = os_i + os_j
    )

  # ---- 5. Zuordnungsmatrix (Sparse) ----------------------------------------
  # assign_mat[r, k] = 1  ⟺  Kunde k ist in Tour r enthalten
  n_pairs  <- nrow(pairs)
  assign_mat <- sparseMatrix(
    i    = c(seq_len(n_pairs), seq_len(n_pairs)),
    j    = c(pairs$i, pairs$j),
    x    = 1L,
    dims = c(n_pairs, n)
  )

  # ---- 6. Direktes LP aufbauen und lösen (Rglpk, kein OMPR-Overhead) --------
  #
  # Variablen:  y[r] ∈ {0,1}  für r = 1,...,n_pairs
  # Constraints:
  #   (a) Jeder Kunde genau einmal:   t(assign_mat) %*% y == 1_n
  #   (b) Flottenkapazität C-truck:   sum_{r: c-truck} y[r] <= cap[1]
  #   (c) Flottenkapazität E-truck:   sum_{r: e-truck} y[r] <= cap[2]

  fleet_c <- Matrix(as.integer(pairs$type == "c-truck"), nrow = 1L, sparse = TRUE)
  fleet_e <- Matrix(as.integer(pairs$type == "e-truck"), nrow = 1L, sparse = TRUE)

  # Constraint-Matrix: (n+2) Zeilen × n_pairs Spalten
  A   <- rbind(t(assign_mat), fleet_c, fleet_e)
  dir <- c(rep("==", n), "<=", "<=")
  rhs <- c(rep(1L, n), veh.cap[1], veh.cap[2])

  solve_direct <- function(obj_col, maximize) {
    res     <- Rglpk::Rglpk_solve_LP(
      obj   = as.numeric(pairs[[obj_col]]),
      mat   = A,
      dir   = dir,
      rhs   = rhs,
      types = rep("B", n_pairs),
      max   = maximize
    )
    sel_idx <- which(res$solution > 0.5)
    list(result = res, tours = tibble(i = sel_idx))
  }

  sol_os <- solve_direct("os", maximize = TRUE)   # Zufriedenheit maximieren
  sol_tc <- solve_direct("tc", maximize = FALSE)  # Kosten minimieren
  sol_te <- solve_direct("te", maximize = FALSE)  # Emissionen minimieren

  list(
    sol.list = list(os = sol_os, tc = sol_tc, te = sol_te),
    instance = list(
      veh.cap    = veh.cap,
      customers  = customers,
      depot = depot,
      veh.data   = veh,
      tours      = pairs,
      assign.mat = assign_mat
    )
  )
}


# ---- 7. Kundeninstanz & Lösung ---------------------------------------------
n  <- 50
stopifnot(n %% 2 == 0)

cust.mat <- tibble(
  id   = 1:n,
  x    = runif(n, 0, 100),
  y    = runif(n, 0, 100),
  pref = sample(c(0, 1), n, replace = TRUE, prob = c(0.5, 0.5))
)

# Depot (zentral im Untersuchungsgebiet)
depot.coord <- c(x = 50, y = 50)


n.50 <- instance.solve()


# ---- 8. Visualisierung der MFVRP-MS-Lösung ----------------------------------
plot_solution <- function(sol, instance) {
  pairs     <- instance$tours
  customers <- instance$customers
  depot     <- c(x = 50, y = 50)
  n         <- nrow(customers)

  a <- pairs[sol$tours$i, ] |>
    select(i, j, type, os, tc, te)

  segs <- bind_rows(
    a |> transmute(x = depot["x"], y = depot["y"], xend = customers$x[i], yend = customers$y[i], vehicle = type),
    a |> transmute(x = customers$x[i], y = customers$y[i], xend = customers$x[j], yend = customers$y[j], vehicle = type),
    a |> transmute(x = customers$x[j], y = customers$y[j], xend = depot["x"], yend = depot["y"], vehicle = type)
  )

  cust_plot <- customers |>
    mutate(praef = factor(pref, labels = c("C", "E")))

  ggplot() +
    geom_segment(
      data = segs,
      aes(x = x, y = y, xend = xend, yend = yend, color = vehicle),
      linewidth = 0.75, alpha = 0.8
    ) +
    geom_point(
      data = cust_plot,
      aes(x, y, shape = praef),
      size = 7, stroke = 0.6, color = "black", fill = "lightgrey"
    ) +
    annotate("point", x = depot["x"], y = depot["y"],
             shape = 22, size = 5, fill = "black", color = "white", stroke = 1) +
    annotate("text",  x = depot["x"], y = depot["y"] - 5,
             label = "Depot", size = 5, fontface = "bold") +
    geom_text(data = cust_plot, aes(x, y, label = id),
              color = "black", size = 3.5, fontface = "bold") +
    scale_color_manual(
      values = c("c-truck" = "#D4622A", "e-truck" = "#2B7BB9"),
      name   = "Fahrzeugtyp"
    ) +
    scale_shape_manual(
      values = c("C" = 21, "E" = 24),
      name   = "Präferenz"
    ) +
    guides(fill = guide_legend(override.aes = list(shape = 21, size = 4))) +
    labs(
      title = sprintf(
        "n = %d | Ø Zufriedenheit: %.3f | Kosten: %.0f € | Emissionen: %.0f kg CO2e",
        n,
        sum(a$os) / n,
        sum(a$tc),
        sum(a$te)
      ),
      x = "x [km]", y = "y [km]"
    ) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "right")
}


p1 <- plot_solution(n.50$sol.list$os, n.50$instance)
p2 <- plot_solution(n.50$sol.list$tc, n.50$instance)
p3 <- plot_solution(n.50$sol.list$te, n.50$instance)

p1 / p2 / p3
