"""
Installation
python version  : 3.13.15
1) create venv  : python -m venv path/to/.venv
2) activate venv: source path/to/.venv/bin/activate
3) install deps : pip install gurobipy numpy
4) execute code : python main.py
     --problem  : tspwp (with profits), tspclst (with clusters)
     --customers: number of customer locations incl depot
                : default: 10
     --data     : path to files holding customer related data (as csv).
                : default: "" (empty, meaning same folder)
     --soltime  : solution time
                : default: 1 minute
     --silent   : print (False) or hide (True) solver output
                : default: False

2 Steps:
1) python main.py --problem tspwp    # find best tours
2) python main.py --problem tspclst  # minimize dissatisfaction
"""

from __future__ import annotations

import csv
from argparse import ArgumentParser
from dataclasses import dataclass
from itertools import product
from pathlib import Path

import gurobipy as gp
import numpy as np

CUSTOMERS_NAME = "kunden-nicht-validiert.csv"
DISTANCES_NAME = "distances-zwischen-kunden-nicht-validiert.csv"
RESULTS_HEAD_TSPWP = (
    "Status",
    "Code",
    "SolTime",
    "MipGap",
    "TourSeq",
    "TourLng",
    "TourFuelCst",
    "TourCo2Cst",
    "Chosen",
    "Share",
    "ShrTourLng",
    "ShrTourFuelCst",
    "ShrTourCo2Cst",
)


RESULTS_HEAD_TSPCLST_TOUR = (
    "Cluster",
    "Truck",
    "TourSeq",
    "TourLng",
    "TourFuelCst",
    "TourCo2Cst",
)

RESULTS_HEAD_TSPCLST_CUSTOMERS = (
    "Cluster",
    "Customer",
    "Share",
    "ShareTourLng",
    "ShareTourFuelCst",
    "ShareTourCo2Cst",
    "Dissatisfaction",
)


def get_cli() -> ArgumentParser:
    parser = ArgumentParser(prog="cli")
    parser.add_argument(
        "--problem", type=str, required=True, choices=["tspwp", "tspclst"]
    )
    parser.add_argument("--customers", type=int, default=10)
    parser.add_argument("--data", type=str, default="")
    parser.add_argument("--soltime", type=int, default=1)
    parser.add_argument("--silent", action="store_true")
    return parser


@dataclass(frozen=True, slots=True)
class TruckProps:
    cls: str
    typ: str
    maxcapa: int
    # maxreach: int
    fcosts: float
    ecosts: float

    @property
    def clstyp(self) -> str:
        return f"{self.cls}-{self.typ}"


@dataclass(frozen=True, slots=True, init=False)
class CustomerData:
    locs: range
    custs: range
    custsnr: int
    dems: tuple[int, ...]
    cprefs: tuple[float, ...]
    eprefs: tuple[float, ...]
    dists: tuple[tuple[int, ...], ...]
    sdists: tuple[int, ...]  # also worst in tspclst

    def __init__(self, ttype: str, path: str, last: int) -> None:
        fields = {}
        # locations
        fields["locs"] = range(last)
        fields["custs"] = range(1, last)
        fields["custsnr"] = len(fields["custs"])

        # Demands and preferences
        data = np.loadtxt(
            f"{path}{CUSTOMERS_NAME}",
            dtype=np.int64,
            delimiter="\t",
            skiprows=1,
            usecols=[3, 4, 5],
        )[:last]
        fields["dems"] = tuple(data[:, 0].tolist())
        fields["cprefs"] = tuple(np.round(data[:, 1] / 100, 2).tolist())
        fields["eprefs"] = tuple(np.round(data[:, 2] / 100, 2).tolist())

        # distances
        dists = np.loadtxt(
            f"{path}{DISTANCES_NAME}", dtype=np.int64, delimiter="\t"
        )[:last, :last]
        fields["dists"] = tuple(dists.tolist())
        sdists = dists[0] + dists[:, 0]  # solo / single tour
        fields["sdists"] = tuple(sdists.tolist())

        # fill fields
        for key, val in fields.items():
            object.__setattr__(self, key, val)

    @property
    def worst(self) -> tuple[int, ...]:
        return self.sdists

    @property
    def best(self) -> tuple[tuple[int, ...], ...]:
        # 1, 0 - first customer, served by 0 class-type
        # 1, 0 - icev, small
        # 1, 1 - icev, large
        # 1, 2 - bev, small
        # 1, 3 - bev, large
        bests = ()
        for cls, typ in (
            ("icev", "small"),  # INFO don't change sequence
            ("icev", "large"),
            ("bev", "small"),
            ("bev", "large"),
        ):
            data = np.loadtxt(
                f"results-tspwp-{self.custsnr + 1}-{cls}-{typ}.csv",
                dtype=np.float64,
                delimiter="\t",
                skiprows=1,
                usecols=[10],
            )
            bests += (tuple(data.tolist()),)
        return bests


################################################################################
# TSP with profits


def get_tspwp(
    solver_params: dict[str, int],
    data: CustomerData,
    chosen: int,
    props: TruckProps,
) -> tuple[gp.Model, gp.tupledict, gp.tupledict]:
    # Initialize
    model = gp.Model("TSPWP")

    # solver settings
    for param, value in solver_params.items():
        model.setParam(param, value)

    # Location and Customers sets are passed in data

    # Decision Variables
    nodes = model.addVars(data.locs, vtype=gp.GRB.BINARY, name="nodes")
    arcs = tuple((i, j) for i, j in product(data.locs, data.locs) if i != j)
    edges = model.addVars(arcs, vtype=gp.GRB.BINARY, name="edges")
    # Parameters
    depot = 0  # always 0
    # dems, cprefs, eprefs, dists, sdists are passed in data

    # Assignment Constraints
    model.addConstr(nodes[0] == 1, name="ForceDepot")
    model.addConstr(nodes[chosen] == 1, name="ForceChosen")

    # Capa Constraints
    model.addConstr(
        gp.quicksum(data.dems[i] * nodes[i] for i in data.locs)
        <= props.maxcapa,
        name="MaximumLoad",
    )

    # TSP Constraints
    model.addConstrs(
        (
            gp.quicksum(edges[i, j] for j in data.locs if j != i) == nodes[i]
            for i in data.locs
        ),
        name="OnlyOneSuccessor",
    )
    model.addConstrs(
        (
            gp.quicksum(edges[j, i] for j in data.locs if j != i) == nodes[i]
            for i in data.locs
        ),
        name="OnlyOnePredecessor",
    )

    # Subtour elimination constraints
    last = len(data.custs)
    positions = model.addVars(data.custs, lb=0, ub=last, name="Position")
    model.addConstrs(
        (positions[i] >= nodes[i] for i in data.custs),
        name="PositionLowerBound",
    )
    model.addConstrs(
        (positions[i] <= last * nodes[i] for i in data.custs),
        name="PositionUpperBound",
    )
    model.addConstrs(
        (
            positions[i]
            - positions[j]
            + last * edges[i, j]
            + (last - 2) * edges[j, i]
            <= last - 1
            for i in data.custs
            for j in data.custs
            if i != j
        ),
        name="SubtourElimination",
    )

    # Calculate tour costs
    costs = model.addVar(lb=0, vtype=gp.GRB.INTEGER, name="TourCosts")
    model.addConstr(
        costs
        == gp.quicksum(data.dists[i][j] * edges[i, j] for i, j in edges.keys()),
        name="CalculateTourCosts",
    )
    # model.addConstr(costs <= props.maxreach, name="MaximumReach")

    # Linearization
    big_m = data.sdists[chosen]
    acosts = model.addVar(lb=0, ub=big_m, name="AllocatedCost")
    aux = model.addVars(data.custs, lb=0, name="AllocatedCostAux")
    model.addConstrs(
        (aux[i] <= big_m * nodes[i] for i in data.custs),
        name="AllocatedCostAux1UB",
    )
    model.addConstrs(
        (aux[i] <= acosts for i in data.custs), name="AllocatedCostAux2UB"
    )
    model.addConstrs(
        (aux[i] >= acosts - big_m * (1 - nodes[i]) for i in data.custs),
        name="AllocatedCostAux2LB",
    )
    model.addConstr(
        gp.quicksum(data.sdists[i] * aux[i] for i in data.custs)
        >= data.sdists[chosen] * costs,
        name="CalculateAllocatedCost",
    )
    model.setObjective(acosts, gp.GRB.MINIMIZE)
    model.update()
    return model, nodes, edges


def _get_tour_sequence(edges: gp.tupledict) -> str:
    # i - current, j - next
    successors = {i: j for (i, j), e in edges.items() if e.X > 0.5}
    tourseq: tuple[int, ...] = ()
    current = successors[0]
    while current != 0:
        tourseq += (current,)
        current = successors[current]
    return " ".join(map(str, tourseq))


def _get_share(data: CustomerData, nodes: gp.tupledict) -> float:
    shares: tuple[int, ...] = ()
    for loc in data.custs:
        if model.getVarByName(f"nodes[{loc}]").X > 0.5:
            shares += (data.sdists[loc],)
    return round(data.sdists[chosen] / sum(shares), 2)


def get_results_tspwp(
    model: gp.Model,
    nodes: gp.tupledict,
    edges: gp.tupledict,
    data: CustomerData,
    chosen: int,
    props: TruckProps,
) -> tuple[int | float | str, ...]:
    results = dict.fromkeys(RESULTS_HEAD_TSPWP, "NA")
    results["Code"] = model.Status
    results["SolTime"] = round(model.Runtime, 2)
    if model.SolCount == 0:
        results["Status"] = "not solved"
        return (tuple(results.values()),)
    results["Status"] = "solved"
    results["MipGap"] = model.MIPGap
    results["TourSeq"] = _get_tour_sequence(edges)
    results["TourLng"] = int(model.getVarByName("TourCosts").X)
    results["TourFuelCst"] = round(results["TourLng"] * props.fcosts, 2)
    results["TourCo2Cst"] = round(results["TourLng"] * props.ecosts, 2)
    results["Chosen"] = chosen
    results["Share"] = _get_share(data, nodes)
    results["ShrTourLng"] = round(model.getVarByName("AllocatedCost").X, 2)
    results["ShrTourFuelCst"] = round(results["ShrTourLng"] * props.fcosts, 2)
    results["ShrTourCo2Cst"] = round(results["ShrTourLng"] * props.ecosts, 2)
    return (tuple(results.values()),)


################################################################################
# Joint TSP with Clustering
def get_tspclst(
    solver_params: dict[str, int],
    data: CustomerData,
    ttrucks: dict[int, TruckProps],
) -> tuple[gp.Model, gp.tupledict, gp.tupledict]:
    # initialize
    model = gp.Model("TSPCLST")

    # solver settings
    for param, value in solver_params.items():
        model.setParam(param, value)

    # Sets; Location and Customers sets are passed in data
    clsts = range(1, len(data.custs) + 1)

    # Parameters
    last = len(data.custs)
    depot = 0
    arcs = tuple((i, j) for i, j in product(data.locs, data.locs) if i != j)

    # Decision Variables
    active = model.addVars(clsts, vtype=gp.GRB.BINARY, name="Active")
    vtrucks = model.addVars(
        clsts, ttrucks.keys(), vtype=gp.GRB.BINARY, name="Trucks"
    )
    nodes = model.addVars(data.custs, clsts, vtype=gp.GRB.BINARY, name="Nodes")
    edges = model.addVars(
        tuple((i, j, c) for i, j in arcs for c in clsts),
        vtype=gp.GRB.BINARY,
        name="Edges",
    )

    # Variables
    positions = model.addVars(data.custs, clsts, lb=0, ub=last, name="Position")
    lengths = model.addVars(clsts, lb=0, name="TourLength")
    shares = model.addVars(data.custs, clsts, lb=0, name="CustomerShare")
    share_dists = model.addVars(
        data.custs, clsts, lb=0, name="CustomerShareDistance"
    )
    unsat_m = model.addVars(data.custs, clsts, lb=0, name="CustomerUnsatM")
    unsat_e = model.addVars(data.custs, clsts, lb=0, name="CustomerUnsatE")
    unsat = model.addVars(data.custs, clsts, lb=0, name="CustomerUnsat")

    # Auxiliary Variables
    aux1 = model.addVars(data.custs, data.custs, clsts, lb=0, name="aux1")
    aux2 = model.addVars(
        ((v, i, j, c) for v in data.custs for i, j in arcs for c in clsts),
        lb=0,
        name="aux2",
    )

    # Assignment Constraints
    model.addConstrs(
        (
            gp.quicksum(vtrucks[c, t] for t in ttrucks.keys()) == active[c]
            for c in clsts
        ),
        name="SelectTruck",
    )
    model.addConstrs(
        (gp.quicksum(nodes[v, c] for c in clsts) == 1 for v in data.custs),
        name="AssignedOnce",
    )
    model.addConstrs(
        (nodes[v, c] <= active[c] for v in data.custs for c in clsts),
        name="OnlyInActive",
    )

    # Capa Constraints
    model.addConstrs(
        (
            gp.quicksum(data.dems[v] * nodes[v, c] for v in data.custs)
            <= gp.quicksum(
                ttrucks[t].maxcapa * vtrucks[c, t] for t in ttrucks.keys()
            )
            for c in clsts
        ),
        name="MaximumLoad",
    )

    # Routing Constraints
    model.addConstrs(
        (
            gp.quicksum(edges[v, j, c] for j in data.locs if j != v)
            == nodes[v, c]
            for v in data.custs
            for c in clsts
        ),
        name="OnlyOneSuccessor",
    )
    model.addConstrs(
        (
            gp.quicksum(edges[j, v, c] for j in data.locs if j != v)
            == nodes[v, c]
            for v in data.custs
            for c in clsts
        ),
        name="OnlyOnePredecessor",
    )

    # Routing Constraints - include depot
    model.addConstrs(
        (
            gp.quicksum(edges[0, j, c] for j in data.custs) == active[c]
            for c in clsts
        ),
        name="ActivateDepotStart",
    )
    model.addConstrs(
        (
            gp.quicksum(edges[i, 0, c] for i in data.custs) == active[c]
            for c in clsts
        ),
        name="ActivateDepotReturn",
    )

    # Route Calculation
    model.addConstrs(
        (
            lengths[c]
            == gp.quicksum(data.dists[i][j] * edges[i, j, c] for i, j in arcs)
            for c in clsts
        ),
        name="CalculateTourLength",
    )

    # Subtour elimination
    model.addConstrs(
        (positions[v, c] >= nodes[v, c] for v in data.custs for c in clsts),
        name="PositionLowerBound",
    )
    model.addConstrs(
        (
            positions[v, c] <= last * nodes[v, c]
            for v in data.custs
            for c in clsts
        ),
        name="PositionUpperBound",
    )
    model.addConstrs(
        (
            positions[i, c]
            - positions[j, c]
            + last * edges[i, j, c]
            + (last - 2) * edges[j, i, c]
            <= last - nodes[j, c]
            for i in data.custs
            for j in data.custs
            for c in clsts
            if i != j
        ),
        name="SubtourElimination",
    )

    # Share of customer v in cluster c
    model.addConstrs(
        (shares[v, c] <= nodes[v, c] for v in data.custs for c in clsts),
        name="ShareUpperbound",
    )
    model.addConstrs(
        (
            # v - customer whose share is calculated
            aux1[v, j, c] <= shares[v, c]
            for v in data.custs
            for j in data.custs
            for c in clsts
        ),
        name="Aux1UB1",
    )

    model.addConstrs(
        (
            # j - other customers contributing to the denominator
            aux1[v, j, c] <= nodes[j, c]
            for v in data.custs
            for j in data.custs
            for c in clsts
        ),
        name="Aux1UB2",
    )
    model.addConstrs(
        (
            aux1[v, j, c] >= shares[v, c] - (1 - nodes[j, c])
            for v in data.custs
            for j in data.custs
            for c in clsts
        ),
        name="ActivateAux1",
    )
    model.addConstrs(
        (
            gp.quicksum(data.worst[j] * aux1[v, j, c] for j in data.custs)
            == data.worst[v] * nodes[v, c]
            for v in data.custs
            for c in clsts
        ),
        name="CalculateShare",
    )

    # Tour Share of Customer i in cluster c
    model.addConstrs(
        (
            aux2[v, i, j, c] <= shares[v, c]
            for v in data.custs
            for i, j in arcs
            for c in clsts
        ),
        name="Aux2UB1",
    )
    model.addConstrs(
        (
            aux2[v, i, j, c] <= edges[i, j, c]
            for v in data.custs
            for i, j in arcs
            for c in clsts
        ),
        name="Aux2UB2",
    )
    model.addConstrs(
        (
            aux2[v, i, j, c] >= shares[v, c] - (1 - edges[i, j, c])
            for v in data.custs
            for i, j in arcs
            for c in clsts
        ),
        name="ActivateAux2",
    )
    model.addConstrs(
        (
            share_dists[v, c]
            == gp.quicksum(data.dists[i][j] * aux2[v, i, j, c] for i, j in arcs)
            for v in data.custs
            for c in clsts
        ),
        name="CalculateCustomerShareDistance",
    )

    # Customer Preferences
    for v in data.custs:
        for c in clsts:
            best = gp.quicksum(
                data.best[t][v - 1] * vtrucks[c, t] for t in ttrucks.keys()
            )
            model.addConstr(
                unsat_m[v, c]
                == (share_dists[v, c] - best * nodes[v, c])
                / (data.worst[v] - best),
                name=f"UnsatM[{v},{c}]",
            )
            model.addConstr(
                unsat_e[v, c]
                == (share_dists[v, c] - best * nodes[v, c])
                / (data.worst[v] - best),
                name=f"UnsatE[{v},{c}]",
            )
            model.addConstr(
                unsat[v, c]
                == data.cprefs[v] * unsat_m[v, c]
                + data.eprefs[v] * unsat_e[v, c],
                name=f"Unsat[{v},{c}]",
            )

    # Objective Function
    model.setObjective(
        gp.quicksum(unsat[v, c] for v in data.custs for c in clsts),
        gp.GRB.MINIMIZE,
    )
    model.update()
    return model, nodes, edges


def get_results_tspclst(
    model: gp.Model,
    nodes: gp.tupledict,
    edges: gp.tupledict,
    data: CustomerData,
    trucks: dict[int, TruckProps],
) -> tuple[tuple[tuple, ...], tuple[tuple, ...]]:

    tours = (RESULTS_HEAD_TSPCLST_TOUR,)
    custs = (RESULTS_HEAD_TSPCLST_CUSTOMERS,)

    if model.SolCount == 0:
        return tours, custs

    trng = range(len(trucks))

    clusters = {c for (_, c), node in nodes.items() if node.X > 0.5}

    for cluster in sorted(clusters):
        for tidx in trng:
            if model.getVarByName(f"Trucks[{cluster},{tidx}]").X > 0.5:
                break
        props = trucks[tidx]

        successors = {
            i: j
            for (i, j, c), edge in edges.items()
            if c == cluster and edge.X > 0.5
        }

        tour = [0]
        current = 0
        while True:
            current = successors[current]
            tour.append(current)
            if current == 0:
                break

        tour_length = model.getVarByName(f"TourLength[{cluster}]").X

        tours += (
            (
                cluster,
                props.clstyp,
                " ".join(map(str, tour)),
                round(tour_length, 2),
                round(tour_length * props.fcosts, 2),
                round(tour_length * props.ecosts, 2),
            ),
        )

        for customer in data.custs:
            if nodes[customer, cluster].X < 0.5:
                continue

            share = model.getVarByName(f"CustomerShare[{customer},{cluster}]").X
            share_dist = model.getVarByName(
                f"CustomerShareDistance[{customer},{cluster}]"
            ).X
            dissatisfaction = model.getVarByName(
                f"CustomerUnsat[{customer},{cluster}]"
            ).X

            custs += (
                (
                    cluster,
                    customer,
                    round(share, 2),
                    round(share_dist, 2),
                    round(share_dist * props.fcosts, 2),
                    round(share_dist * props.ecosts, 2),
                    round(dissatisfaction, 2),
                ),
            )

    return tours, custs


if __name__ == "__main__":
    args = get_cli().parse_args()
    data = CustomerData(args.problem, args.data, args.customers)
    # truck properties, don't change its sequence
    trucks = {
        # 12 - ubcapa, 740 - maxreach, 0.275 - fcosts, 0.051 - ecosts
        0: TruckProps("icev", "small", 12, 0.275, 0.051),  # maxreach 740
        1: TruckProps("icev", "large", 40, 0.459, 0.086),  # maxreach 2110
        2: TruckProps("bev", "small", 12, 0.3, 0.034),  # maxreach 185
        3: TruckProps("bev", "large", 40, 0.5, 0.055),  # maxreach 186
        # ("bev", "small"): TruckProps(12, 0.275, 0.034),  # maxreach 185
        # ("bev", "large"): TruckProps(40, 0.439, 0.055),  # maxreach 186
    }
    params = {"TimeLimit": args.soltime * 60, "SolutionNumber": 0}
    if args.silent:
        params = {"OutputFlag": 0} | params
    match args.problem:
        case "tspwp":
            for props in trucks.values():
                results = (RESULTS_HEAD_TSPWP,)
                for chosen in data.custs:
                    model, nodes, edges = get_tspwp(params, data, chosen, props)
                    # solve
                    model.optimize()
                    # save to results array
                    results += get_results_tspwp(
                        model, nodes, edges, data, chosen, props
                    )
                # export results array
                fname = f"{args.data}results-tspwp-{args.customers}-{props.clstyp}.csv"
                with open(fname, "w", newline="") as ffile:
                    writer = csv.writer(ffile, delimiter="\t")
                    writer.writerows(results)
            print("TSPWP solved!")
        case "tspclst":
            # replace with indexes
            model, nodes, edges = get_tspclst(params, data, trucks)
            model.optimize()
            tours, custs = get_results_tspclst(
                model, nodes, edges, data, trucks
            )
            fname = f"results-tspclst-{args.customers}-tours.csv"
            with open(fname, "w", newline="") as ffile:
                csv.writer(ffile, delimiter="\t").writerows(tours)
            fname = f"results-tspclst-{args.customers}-customers.csv"
            with open(fname, "w", newline="") as ffile:
                csv.writer(ffile, delimiter="\t").writerows(custs)
            print("TSPCLST solved!")
        case _:
            breakpoint()
