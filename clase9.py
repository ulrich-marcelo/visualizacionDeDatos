import pandas as pd
from pprint import pprint

arboles = pd.read_csv("data/arboles.csv")
data = pd.read_csv("data/infoArboles.csv", sep="\t")

arboles = arboles.loc[:, ["long", "lat", "nombre_cientifico"]]

total = pd.merge(arboles, data, how="right", left_on="nombre_cientifico",
                 right_on="Árbol (Nombre Científico)")


total.rename(columns={"nombre_cientifico": "Arbol"}, inplace=True)
total.drop(columns=["Árbol (Nombre Científico)"], inplace=True)

total.to_csv("data/arboles_completo.csv", sep="\t", index=False)
