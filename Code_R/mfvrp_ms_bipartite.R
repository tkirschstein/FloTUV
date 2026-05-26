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


set.seed(4721)

# ---- 1. Fahrzeugparameter ---------------------------------------------------
# Quellen für Richtwerte: Syré et al. (2025), Icha & Lauf (2025)
# Emissionen als Well-to-Wheel (WTW) CO2-Äquivalente
veh.data <- list(
  type  = c("c-truck", "e-truck"),
  c_fix = c(314,  375),   # Fixkosten je Tour-Einsatz [€/Tour]
  c_var = c(0.57, 0.75),  # Variable Kosten [€/km]
  e_fix = c(  0,    0),   # Fixemissionen je Tour [kg CO2e] (hier 0)
  e_var = c(1.07, 0.44)  # Variable Emissionen [kg CO2e/km]
)
n <- 50

# ---- 2. Kundeninstanz generieren --------------------------------------------
stopifnot(n %% 2 == 0)

cust.mat <- tibble(
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
  n, sum(cust.mat$pref == 0), sum(cust.mat$pref == 1)
))



instance.solve <- function(veh = veh.data, customers = cust.mat , veh.cap = c(n/2,n/2)){
  library(tidyverse)
  library(ggplot2)
  library(ompr)
  library(ompr.roi)
  library(ROI.plugin.glpk)
  library(ROI.plugin.highs)
  library(Matrix)
  
  # Sub functions ####################################################
  
  build_model <- function(data= assign_mat, cap = rep(ncol(assign_mat)/2,2), veh.vec = pairs$type, data.obj = pairs, obj ="tc", sense = "min") {
    
    nT <- nrow(data)
    nC <- ncol(data)
    
    obj.vec <- as.numeric(unlist(data.obj[, obj]))
    
    c.truck.id <- which(veh.vec == "c-truck")
    e.truck.id <- which(veh.vec == "e-truck")
    
    MIPModel() |>
      add_variable(
        y[i], i = 1:nT, type = "binary"
      ) |>
      # (a) Jeder Kunde genau einmal
      add_constraint(
        sum_over(y[i] * data[i,j], i = 1:nT) == 1,
        j = 1:nC
      ) |>
      # (c) Flottenkapazität C-truck
      add_constraint(
        sum_over(y[i], i = c.truck.id) <= cap[1]
      ) |>
      # (c) Flottenkapazität E-truck
      add_constraint(
        sum_over(y[i], i = e.truck.id) <= cap[2]
      ) |>
      set_objective(
        sum_over(y[i] * obj.vec[i], i = 1:nT),
        sense = sense
      )
  }
  
  
  solve_mfvrp <- function(obj.var, sense, cap.vec = c(n/2,n/2) ) {
    res <- solve_model(
      build_model(obj = obj.var, sense = sense, cap = cap.vec),
      with_ROI(solver = "highs", verbose = TRUE)
    )
    asgn <- get_solution(res, y[i]) |>
      filter(value > 0.5) |>
      select(i)
    list(result = res, tours = asgn)
  }
  
  # Tourlänge = Depot → Kunde i → Kunde j → Depot  [km]
  d2 <- function(x1, y1, x2, y2) sqrt((x1 - x2)^2 + (y1 - y2)^2)
  
  
  
  # ---- 3. Tourlängenmatrix ----------------------------------------------------
  n = nrow(customers)
  
  customers <- customers |>
    mutate(d_depot = d2(x, y, depot["x"], depot["y"]))
  
  # create data frame of all pairs of customers
  idx <- combn(customers$id, 2)
  pairs <- tibble(i = idx[1,], j = idx[2,]) |>
    left_join(customers |> select(id, xi = x, yi = y, d_depot_i = d_depot), by = c("i" = "id")) |>
    left_join(customers |> select(id, xj = x, yj = y, d_depot_j = d_depot), by = c("j" = "id"))
  
  # add tour length to pairs
  pairs <- pairs |>
    mutate(tl = d_depot_i + d2(xi, yi, xj, yj) + d_depot_j)
  
  # add preference of customers i and j as variables pref_i and pref_j to pairs 
  pairs <- pairs |>
    left_join(customers |> select(id, pref) |> rename(i = id, pref_i = pref), by = "i") |>
    left_join(customers |> select(id, pref) |> rename(j = id, pref_j = pref), by = "j")
  
  
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
  
  # Schranken für Kunden (Minimum/Maximum über Dimension und Typen)
 bounds_tbl <- bind_rows(
  pairs |> select(cust = i, ac = ac_i, ae = ae_i),
  pairs |> select(cust = j, ac = ac_j, ae = ae_j)
) |>
  summarise(c_lo = min(ac), c_hi = max(ac),
            e_lo = min(ae), e_hi = max(ae),
            .by = cust)
  
  # calculate and add customer satisfaction
  
  pairs <- pairs |>
  left_join(bounds_tbl |> rename(i = cust, c_lo_i = c_lo, c_hi_i = c_hi,
                                           e_lo_i = e_lo, e_hi_i = e_hi), by = "i") |>
  left_join(bounds_tbl |> rename(j = cust, c_lo_j = c_lo, c_hi_j = c_hi,
                                           e_lo_j = e_lo, e_hi_j = e_hi), by = "j") |>
  mutate(
    os_i = (1 - pref_i) * if_else(c_hi_i > c_lo_i, (c_hi_i - ac_i)/(c_hi_i - c_lo_i), 1) +
             pref_i      * if_else(e_hi_i > e_lo_i, (e_hi_i - ae_i)/(e_hi_i - e_lo_i), 1),
    os_j = (1 - pref_j) * if_else(c_hi_j > c_lo_j, (c_hi_j - ac_j)/(c_hi_j - c_lo_j), 1) +
             pref_j      * if_else(e_hi_j > e_lo_j, (e_hi_j - ae_j)/(e_hi_j - e_lo_j), 1),
    os = os_i + os_j
  )
  
  # Zuordnungsmatrix: Zeilen = Touren (Pairs), Spalten = Kunden
  # Eintrag [r, k] = 1, wenn Kunde k in Tour r enthalten ist (k == i oder k == j)
  
  n_pairs <- nrow(pairs)
  row_idx <- c(seq_len(n_pairs), seq_len(n_pairs))
  col_idx <- c(pairs$i, pairs$j)
  assign_mat <- sparseMatrix(i = row_idx, j = col_idx,
                             x = 1L,
                             dims = c(n_pairs, n))
  
  # ---- 7. Optimierungs-Modell bauen und lösen ----------------------------------
  
  sol_os <- solve_mfvrp(obj.var = "os", sense = "max", cap.vec = veh.cap) # satisfaction optimal
  sol_tc <- solve_mfvrp(obj.var = "tc", sense = "min", cap.vec = veh.cap) # cost-optimal
  sol_te <- solve_mfvrp(obj.var = "te", sense = "min", cap.vec = veh.cap) # emission-optimal
  
return(
  list(
    sol.list = list(
      os = sol_os, 
      tc = sol_tc,
      te = sol_te),
    instance = list(
      veh.cap = veh.cap,
      customers = customers,
      veh.data = veh,
      tours= pairs,
      assign.mat = assign_mat ) 
    )
)

}


n.50 <- instance.solve()

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
  a <- sol$tours |>
    mutate(
      x_i = customers$x[pairs[i,"i"]], y_i = customers$y[pairs[i,"i"]],
      x_j = customers$x[pairs[i,"j"]], y_j = customers$y[pairs[i,"j"]],
      id_i = pairs[i,"i"],
      id_j = pairs[i,"j"],
      vehicle = pairs[i,"type"]
    )

  # Routen als Segmente: Depot → i → j → Depot
  segs_depot_i <- a |> transmute(x = depot["x"], y = depot["y"], xend = x_i, yend = y_i, vehicle)
  segs_i_j     <- a |> transmute(x = x_i, y = y_i, xend = x_j, yend = y_j, vehicle)
  segs_j_depot <- a |> transmute(x = x_j, y = y_j, xend = depot["x"], yend = depot["y"], vehicle)

  segs <- bind_rows(segs_depot_i, segs_i_j, segs_j_depot)

  cust_plot <- customers |>
    mutate(
      praef   = factor(pref, labels = c("C", "E"))
    )

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
      name = "Fahrzeugtyp"
    ) +
    scale_shape_manual(
      values = c("C" = 21, "E" = 24),
      name = "Präferenz"
    ) +
    guides(fill = guide_legend(override.aes = list(shape = 21, size = 4))) +
    labs(
      title    = sprintf(
        "n = %d Kunden | Ø Zufriedenheit: %.3f | Kosten: %.0f € | Emissionen: %.0f kg CO2e",
        n,
        sum(map_dbl(seq_len(nrow(a)), \(k) pairs[a$i[k], "os"])) / n,
        sum(map_dbl(seq_len(nrow(a)), \(k) pairs[a$i[k], "tc"])),
        sum(map_dbl(seq_len(nrow(a)), \(k) pairs[a$i[k], "te"]))
      ),
      x = "x [km]", y = "y [km]"
    ) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "right")
}



p1 <- plot_solution(n.50$sol.list$os)
p2 <- plot_solution(n.50$sol.list$tc)
p3 <- plot_solution(sol_te)

p1 / p2 / p3
