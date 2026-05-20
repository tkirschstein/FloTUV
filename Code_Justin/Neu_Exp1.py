import pandas as pd
import numpy as np
import gurobipy as gp
from gurobipy import GRB
import matplotlib
matplotlib.use("TkAgg")
import matplotlib.pyplot as plt

# === Parameter ===
max_type1 = 25   # C-Trucks
max_type2 = 25   # E-Trucks
file_path = "real100.xlsx"

# Excel einlesen
sheet_pairs = pd.read_excel(file_path, sheet_name=0)
sheet_customers = pd.read_excel(file_path, sheet_name=1)
customer_columns = [col for col in sheet_pairs.columns if col.startswith("Customer_")]
customers = [col.split("_")[1] for col in customer_columns]

# Ergebnisse speichern
results_diff_cost = []
results_diff_emis = []

for pref_share in np.arange(0, 1.05, 0.05):
    print(f"🔁 {int(pref_share * 100)}% emissionsorientiert")
    diffs_cost = []
    diffs_emis = []

    for run in range(10):
        # Präferenzen zuweisen
        pref_customers = np.random.choice(customers, size=int(pref_share * len(customers)), replace=False)
        prefs = {c: 1 if c in pref_customers else 0 for c in customers}

        # Paare aufbereiten
        valid_pairs = []
        for _, row in sheet_pairs.iterrows():
            name = row["Pair"]
            involved = [c for c in customers if row.get(f"Customer_{c}", 0) == 1]
            if len(involved) != 2:
                continue
            for vtype in [1, 2]:
                cost = row[f"Type_{vtype}_Cost"]
                emis = row[f"Type_{vtype}_Emission"]
                alloc = [(c,
                          row.get(f"Cost_Type_{vtype}_Cust_{c}", 0),
                          row.get(f"Emissions_Type_{vtype}_Cust_{c}", 0))
                         for c in involved]
                valid_pairs.append((name, tuple(involved), vtype, cost, emis, alloc))

        # Modelllösung
        def solve_model(mode):
            m = gp.Model()
            m.setParam("OutputFlag", 0)
            x = m.addVars(len(valid_pairs), vtype=GRB.BINARY)

            #  Constraints
            for c in customers:
                m.addConstr(gp.quicksum(x[i] for i, (_, pair, _, _, _, _) in enumerate(valid_pairs) if c in pair) == 1)
            m.addConstr(gp.quicksum(x[i] for i, (_, _, t, _, _, _) in enumerate(valid_pairs) if t == 1) <= max_type1)
            m.addConstr(gp.quicksum(x[i] for i, (_, _, t, _, _, _) in enumerate(valid_pairs) if t == 2) <= max_type2)

            #  Zielfunktion
            if mode == "satisfaction":
                terms = []
                for i, (_, pair, _, _, _, alloc) in enumerate(valid_pairs):
                    for c, c_cost, c_emis in alloc:
                        row_c = sheet_customers[sheet_customers["Customer_ID"] == int(c)].iloc[0]
                        # NEU: Max-Werte aus den neuen Spalten statt Solo_*:
                        min_c = row_c["Min_Allocated_Cost"]
                        max_c = row_c["Max_Allocated_Cost"]          # <<-- NEU
                        min_e = row_c["Min_Allocated_Emission"]
                        max_e = row_c["Max_Allocated_Emission"]       # <<-- NEU

                        cs = 1 - (c_cost - min_c) / (max_c - min_c) if max_c > min_c else 1
                        es = 1 - (c_emis - min_e) / (max_e - min_e) if max_e > min_e else 1
                        pref = prefs[c]
                        terms.append((pref * es + (1 - pref) * cs) * x[i])
                m.setObjective(gp.quicksum(terms), GRB.MAXIMIZE)

            elif mode == "cost":
                m.setObjective(gp.quicksum(valid_pairs[i][3] * x[i] for i in range(len(valid_pairs))), GRB.MINIMIZE)
            elif mode == "emission":
                m.setObjective(gp.quicksum(valid_pairs[i][4] * x[i] for i in range(len(valid_pairs))), GRB.MINIMIZE)

            m.optimize()
            return set(i for i in range(len(valid_pairs)) if x[i].X > 0.5)

        # Modelle lösen
        sol_sat = solve_model("satisfaction")
        sol_cost = solve_model("cost")
        sol_emis = solve_model("emission")

        # Differenzen berechnen
        diff_cost = len(sol_sat.symmetric_difference(sol_cost)) / len(customers) * 100
        diff_emis = len(sol_sat.symmetric_difference(sol_emis)) / len(customers) * 100

        diffs_cost.append(diff_cost)
        diffs_emis.append(diff_emis)

    # Mittelwerte speichern
    results_diff_cost.append({
        "Anteil_emissionsorientiert": int(pref_share * 100),
        "Ø Abweichung vs. Kostenminimierung [%]": np.mean(diffs_cost)
    })
    results_diff_emis.append({
        "Anteil_emissionsorientiert": int(pref_share * 100),
        "Ø Abweichung vs. Emissionsminimierung [%]": np.mean(diffs_emis)
    })

# Ergebnisse und Plot
df_cost = pd.DataFrame(results_diff_cost)
df_emis = pd.DataFrame(results_diff_emis)

plt.figure(figsize=(12, 6))

# Linien zeichnen
plt.plot(
    df_cost["Anteil_emissionsorientiert"],
    df_cost["Ø Abweichung vs. Kostenminimierung [%]"],
    color="blue", linestyle="-", linewidth=2.5, label="vs. MFVRP-TC"
)
plt.plot(
    df_emis["Anteil_emissionsorientiert"],
    df_emis["Ø Abweichung vs. Emissionsminimierung [%]"],
    color="green", linestyle="-", linewidth=2.5, label="vs. MFVRP-TE"
)

# Größere Achsenbeschriftungen und Legende
#plt.xlabel("Share of Emission-Oriented Customers [%]", fontsize=15, labelpad=10)
plt.ylabel("differing customer pairs [%]", fontsize=22, labelpad=10)
plt.xticks(fontsize=20)
plt.yticks(fontsize=20)
plt.grid(True, linestyle="--", alpha=0.6)
plt.legend(fontsize=13, loc="best")
plt.tight_layout()

# Speichern
plt.savefig("Test2.png", dpi=300, bbox_inches="tight")
plt.close()

print("✅ Plot gespeichert als 'Test2.png'")