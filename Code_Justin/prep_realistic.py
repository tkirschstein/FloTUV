import pandas as pd
import math
import itertools
import matplotlib.pyplot as plt

# Anzahl Kunden
num_customers = int(input("🔢 Wie viele Kunden sollen eingelesen werden? (max. 100): "))

# Datei einlesen
with open("rc101.txt", "r") as f:
    content = f.read()

# Koordinaten extrahieren
def extract_coordinates(content, section):
    start = content.split(section)[1].strip()
    lines = start.splitlines()
    coords = []
    for line in lines:
        for val in line.strip().split():
            try:
                coords.append(int(val))
            except ValueError:
                return coords
    return coords

x_all = extract_coordinates(content, "cx")
y_all = extract_coordinates(content, "cy")

# Skalierung der Koordinaten
x_all = [val * 4 for val in x_all]
y_all = [val * 4 for val in y_all]

depot = (x_all[0], y_all[0])
x = x_all[1:num_customers + 1]
y = y_all[1:num_customers + 1]
customer_ids = list(range(1, len(x) + 1))

print(f"📍 Depot-Koordinaten: {depot}")
print(f"✅ Kunden eingelesen: {len(x)}")

# Fahrzeugtypen mit Fixkosten, km-Kosten, Emissionsraten
vehicle_types = {
    'Type_1': {'fix_cost': 314.24, 'km_cost': 0.57, 'emission_rate': 1.07},
    'Type_2': {'fix_cost': 375.69, 'km_cost': 0.75, 'emission_rate': 0.44}
}

def euclidean(a, b):
    return math.hypot(a[0] - b[0], a[1] - b[1])

combinations = list(itertools.combinations(customer_ids, 2))
rows = []

for (i, j) in combinations:
    row = {'Pair': f"{i}-{j}"}
    for cid in customer_ids:
        row[f"Customer_{cid}"] = 1 if cid in (i, j) else 0

    coord_i = (x[i - 1], y[i - 1])
    coord_j = (x[j - 1], y[j - 1])
    total_distance = euclidean(depot, coord_i) + euclidean(coord_i, coord_j) + euclidean(coord_j, depot)

    dist_i = euclidean(depot, coord_i)
    dist_j = euclidean(depot, coord_j)
    total_luftlinie = dist_i + dist_j if (dist_i + dist_j) > 0 else 1
    share_i = dist_i / total_luftlinie
    share_j = dist_j / total_luftlinie

    for vt, v in vehicle_types.items():
        fix_share = v['fix_cost'] / 2
        var_cost = total_distance * v['km_cost']

        cost_i = fix_share + (var_cost * share_i)
        cost_j = fix_share + (var_cost * share_j)
        total_cost = cost_i + cost_j

        total_emission = total_distance * v['emission_rate']
        emission_i = total_emission * share_i
        emission_j = total_emission * share_j

        row[f"{vt}_Cost"] = round(total_cost, 2)
        row[f"{vt}_Emission"] = round(total_emission, 2)

        row[f'Cost_{vt}_Cust_{i}'] = round(cost_i, 2)
        row[f'Cost_{vt}_Cust_{j}'] = round(cost_j, 2)
        row[f'Emissions_{vt}_Cust_{i}'] = round(emission_i, 2)
        row[f'Emissions_{vt}_Cust_{j}'] = round(emission_j, 2)

    row[f'Pct_Share_Cust_{i}'] = round(share_i, 4)
    row[f'Pct_Share_Cust_{j}'] = round(share_j, 4)

    rows.append(row)

df = pd.DataFrame(rows)

# Koordinaten-DF & Solo-Berechnungen
coord_df = pd.DataFrame({'Customer_ID': customer_ids, 'X': x, 'Y': y})
solo_cost_e = []
solo_emission_c = []

for i in range(len(x)):
    coord = (x[i], y[i])
    rt_dist = 2 * euclidean(depot, coord)
    solo_cost = vehicle_types['Type_2']['fix_cost'] + rt_dist * vehicle_types['Type_2']['km_cost']
    solo_cost_e.append(round(solo_cost, 5))
    solo_emission = rt_dist * vehicle_types['Type_1']['emission_rate']
    solo_emission_c.append(round(solo_emission, 5))

coord_df['Solo_E_Cost'] = solo_cost_e
coord_df['Solo_C_Emission'] = solo_emission_c

# === Allokation: beste/worst Kosten- & Emissionskombination je Kunde ===
alloc_emission_all = {cid: [] for cid in customer_ids}
alloc_cost_all = {cid: [] for cid in customer_ids}

for _, row in df.iterrows():
    c1, c2 = map(int, row['Pair'].split('-'))
    for cid in (c1, c2):
        for vt in vehicle_types:
            emis = row.get(f"Emissions_{vt}_Cust_{cid}", float('inf'))
            cost = row.get(f"Cost_{vt}_Cust_{cid}", float('inf'))
            alloc_emission_all[cid].append((emis, row['Pair'], vt))
            alloc_cost_all[cid].append((cost, row['Pair'], vt))

best_emission_pair_all, min_emissions_all = [], []
best_cost_pair_all, min_costs_all = [], []

# NEU: Maxima
worst_emission_pair_all, max_emissions_all = [], []
worst_cost_pair_all, max_costs_all = [], []

for cid in customer_ids:
    # Minima
    min_em, em_pair, _ = min(alloc_emission_all[cid], key=lambda x: x[0])
    min_cost, cost_pair, _ = min(alloc_cost_all[cid], key=lambda x: x[0])
    best_emission_pair_all.append(em_pair)
    min_emissions_all.append(round(min_em, 5))
    best_cost_pair_all.append(cost_pair)
    min_costs_all.append(round(min_cost, 5))

    # Maxima
    max_em, worst_em_pair, _ = max(alloc_emission_all[cid], key=lambda x: x[0])
    max_cost, worst_cost_pair, _ = max(alloc_cost_all[cid], key=lambda x: x[0])
    worst_emission_pair_all.append(worst_em_pair)
    max_emissions_all.append(round(max_em, 5))
    worst_cost_pair_all.append(worst_cost_pair)
    max_costs_all.append(round(max_cost, 5))

coord_df['Best_Emission_Pair_AllTypes'] = best_emission_pair_all
coord_df['Min_Allocated_Emission'] = min_emissions_all
coord_df['Best_Cost_Pair_AllTypes'] = best_cost_pair_all
coord_df['Min_Allocated_Cost'] = min_costs_all

# Spalten für Maxima
coord_df['Worst_Emission_Pair_AllTypes'] = worst_emission_pair_all
coord_df['Max_Allocated_Emission'] = max_emissions_all
coord_df['Worst_Cost_Pair_AllTypes'] = worst_cost_pair_all
coord_df['Max_Allocated_Cost'] = max_costs_all

# Excel
with pd.ExcelWriter("real100_rc101.xlsx") as writer:
    df.to_excel(writer, sheet_name="Pairs", index=False)
    coord_df.to_excel(writer, sheet_name="Coordinates", index=False)

print("✅ Excel-Datei erfolgreich erstellt: real100.xlsx")

# Koordinaten mit Kundennummern
plt.figure(figsize=(10, 10))

# Kundenpunkte
plt.scatter(x, y, c='blue', label='customers', alpha=0.7)

# Depot
plt.scatter(*depot, c='red', marker='X', s=200, label='depot')

# Kundennummern direkt an die Punkte schreiben
for cid, x_coord, y_coord in zip(customer_ids, x, y):
    plt.annotate(
        str(cid),
        (x_coord, y_coord),
        textcoords="offset points",
        xytext=(5, 5),
        ha='left',
        fontsize=8
    )

# Depot beschriften
plt.annotate(
    "Depot",
    depot,
    textcoords="offset points",
    xytext=(8, 8),
    ha='left',
    fontsize=10,
    fontweight='bold',
    color='red'
)

plt.xlabel('X-coordinate')
plt.ylabel('Y-coordinate')
plt.title('Customer map with customer IDs')
plt.legend()
plt.grid(True)
plt.tight_layout()

# Bild speichern
plt.savefig("kundenkarte_mit_nummern_rc101.png", dpi=300)
plt.close()

print("🖼️ Bild erfolgreich gespeichert: kundenkarte_mit_nummern_rc101.png")
