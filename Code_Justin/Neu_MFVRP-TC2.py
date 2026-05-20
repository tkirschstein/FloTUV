import pandas as pd
import numpy as np
import gurobipy as gp
from gurobipy import GRB
import matplotlib.pyplot as plt

# 🔧 Parameter
max_type1 = 40
max_type2 = 10

# Daten laden
file_path = "real100.xlsx"
sheet_pairs = pd.read_excel(file_path, sheet_name=0)
sheet_customers = pd.read_excel(file_path, sheet_name=1)

customer_columns = [col for col in sheet_pairs.columns if col.startswith("Customer_")]
customers = [col.split("_")[1] for col in customer_columns]

results = []

for pref_share in np.arange(0, 1.05, 0.05):
    print(f"\n🔁 {int(pref_share * 100)} % emissionsorientierter Kunden")
    for run in range(10):
        # Zufällige Präferenzen
        pref_customers = np.random.choice(customers, size=int(pref_share * len(customers)), replace=False)
        prefs = {c: 1 if c in pref_customers else 0 for c in customers}

        #  Gültige Paare aufbauen
        valid_pairs = []
        for _, row in sheet_pairs.iterrows():
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
                valid_pairs.append((tuple(involved), vtype, cost, emis, alloc))

        #  Optimierungsmodell
        m = gp.Model()
        m.setParam("OutputFlag", 0)
        x = m.addVars(len(valid_pairs), vtype=GRB.BINARY)

        #  Nebenbedingungen
        for c in customers:
            m.addConstr(gp.quicksum(x[i] for i, (pair, _, _, _, _) in enumerate(valid_pairs) if c in pair) == 1)
        m.addConstr(gp.quicksum(x[i] for i, (_, t, _, _, _) in enumerate(valid_pairs) if t == 1) <= max_type1)
        m.addConstr(gp.quicksum(x[i] for i, (_, t, _, _, _) in enumerate(valid_pairs) if t == 2) <= max_type2)

        #  Zielfunktion: Minimierung der Gesamtkosten der ausgewählten Paare
        m.setObjective(gp.quicksum(valid_pairs[i][2] * x[i] for i in range(len(valid_pairs))), GRB.MINIMIZE)
        m.optimize()

        # Ergebnisse extrahieren
        z_cost = m.ObjVal if m.status == GRB.OPTIMAL else None
        total_emis = sum(valid_pairs[i][3] for i in range(len(valid_pairs)) if x[i].X > 0.5)

        #  Zufriedenheit (NEU: Max_... statt Solo_...)
        satisfaction = 0.0
        for i, (_, _, _, _, alloc) in enumerate(valid_pairs):
            if x[i].X < 0.5:
                continue
            for c, c_cost, c_emis in alloc:
                row_c = sheet_customers[sheet_customers["Customer_ID"] == int(c)].iloc[0]
                min_c = row_c["Min_Allocated_Cost"]
                max_c = row_c["Max_Allocated_Cost"]           # NEU
                min_e = row_c["Min_Allocated_Emission"]
                max_e = row_c["Max_Allocated_Emission"]        # NEU

                cs = 1 - (c_cost - min_c) / (max_c - min_c) if max_c > min_c else 1
                es = 1 - (c_emis - min_e) / (max_e - min_e) if max_e > min_e else 1
                pref = prefs[c]
                satisfaction += pref * es + (1 - pref) * cs

        results.append({
            "Anteil_emissionsorientiert": int(pref_share * 100),
            "Ø Zufriedenheit": satisfaction / len(customers),
            "Ø Gesamtkosten": z_cost,
            "Ø Gesamtemissionen": total_emis
        })

#  Mittelwerte & Ausgabe
df = pd.DataFrame(results)
means = df.groupby("Anteil_emissionsorientiert").mean().reset_index()
means.to_excel("MFVRP-TC3.xlsx", index=False)
print("✅ Ergebnisse gespeichert in 'MFVRP-TC3.xlsx'")

#  Plot
plt.figure(figsize=(12, 6))
plt.subplot(1, 3, 1)
plt.plot(means["Anteil_emissionsorientiert"], means["Ø Zufriedenheit"], marker='o')
plt.title("Ø Zufriedenheit")
plt.xlabel("Anteil emissionsorientierter Kunden [%]")
plt.ylabel("Ø Zufriedenheit")

plt.subplot(1, 3, 2)
plt.plot(means["Anteil_emissionsorientiert"], means["Ø Gesamtkosten"], marker='o', color='green')
plt.title("Ø Gesamtkosten")
plt.xlabel("Anteil emissionsorientierter Kunden [%]")

plt.subplot(1, 3, 3)
plt.plot(means["Anteil_emissionsorientiert"], means["Ø Gesamtemissionen"], marker='o', color='orange')
plt.title("Ø Gesamtemissionen")
plt.xlabel("Anteil emissionsorientierter Kunden [%]")

plt.tight_layout()
plt.suptitle("Einfluss emissionsorientierter Kunden – Kostenminimierung", fontsize=14, y=1.05)
plt.savefig("MFVRP-TC3.png", bbox_inches="tight", dpi=300)
print("📊 Plot gespeichert als 'MFVRP-TC3.png'")