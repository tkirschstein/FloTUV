import pandas as pd
import numpy as np
import gurobipy as gp
from gurobipy import GRB
import matplotlib.pyplot as plt

# Parameter: Anzahl der Fahrzeuge
max_type1 = 10  # C-Trucks
max_type2 = 40  # E-Trucks

# Excel einlesen
file_path = "real100.xlsx"
sheet_pairs = pd.read_excel(file_path, sheet_name=0)
sheet_customers = pd.read_excel(file_path, sheet_name=1)

# Kundenliste extrahieren
customer_columns = [col for col in sheet_pairs.columns if col.startswith("Customer_")]
customers = [col.split("_")[1] for col in customer_columns]

results = []

# Simulation für Präferenzanteile von 0 bis 100 %
for pref_share in np.arange(0, 1.05, 0.05):
    print(f"\n🔁 {int(pref_share * 100)} % emissionsorientierter Kunden")
    for run in range(100):  # Wiederholungen
        pref_customers = np.random.choice(customers, size=int(pref_share * len(customers)), replace=False)
        prefs = {c: 1 if c in pref_customers else 0 for c in customers}

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

        m = gp.Model()
        m.setParam("OutputFlag", 0)
        x = m.addVars(len(valid_pairs), vtype=GRB.BINARY)

        # Nebenbedingungen
        for c in customers:
            m.addConstr(gp.quicksum(x[i] for i, (_, pair, _, _, _, _) in enumerate(valid_pairs) if c in pair) == 1)
        m.addConstr(gp.quicksum(x[i] for i, (_, _, t, _, _, _) in enumerate(valid_pairs) if t == 1) <= max_type1)
        m.addConstr(gp.quicksum(x[i] for i, (_, _, t, _, _, _) in enumerate(valid_pairs) if t == 2) <= max_type2)

        # Zielfunktion: Zufriedenheit maximieren
        obj_terms = []
        for i, (_, pair, _, _, _, alloc) in enumerate(valid_pairs):
            for c, c_cost, c_emis in alloc:
                row_c = sheet_customers[sheet_customers["Customer_ID"] == int(c)].iloc[0]

                # 🔁 NEU: Verwende Min_... und Max_... statt Solo_...
                min_c, max_c = row_c["Min_Allocated_Cost"], row_c["Max_Allocated_Cost"]
                min_e, max_e = row_c["Min_Allocated_Emission"], row_c["Max_Allocated_Emission"]

                cs = 1 - (c_cost - min_c) / (max_c - min_c) if max_c > min_c else 1
                es = 1 - (c_emis - min_e) / (max_e - min_e) if max_e > min_e else 1
                pref = prefs[c]
                obj_terms.append((pref * es + (1 - pref) * cs) * x[i])

        m.setObjective(gp.quicksum(obj_terms), GRB.MAXIMIZE)
        m.optimize()

        z_value = m.ObjVal if m.status == GRB.OPTIMAL else None
        total_cost = sum(valid_pairs[i][3] for i in range(len(valid_pairs)) if x[i].X > 0.5)
        total_emis = sum(valid_pairs[i][4] for i in range(len(valid_pairs)) if x[i].X > 0.5)

        results.append({
            "Anteil_emissionsorientiert": int(pref_share * 100),
            "Ø Zufriedenheit": z_value / len(customers) if z_value is not None else None,
            "Ø Gesamtkosten": total_cost,
            "Ø Gesamtemissionen": total_emis
        })

# Ergebnisse aggregieren
df = pd.DataFrame(results)
means = df.groupby("Anteil_emissionsorientiert").mean().reset_index()

# Excel-Datei speichern
means.to_excel("MFVRP-CP1.xlsx", index=False)
print("\n✅ Ergebnisse gespeichert in 'MFVRP-CP1.xlsx'")

# Plot
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
plt.suptitle("Einfluss emissionsorientierter Kunden – Maximierung Zufriedenheit", fontsize=14, y=1.05)
plt.savefig("MFVRP-CP1.png", bbox_inches="tight", dpi=300)
print("📊 Plot gespeichert als 'MFVRP-CP1.png'")