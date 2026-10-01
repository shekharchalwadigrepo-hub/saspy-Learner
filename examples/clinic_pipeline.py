import pandas as pd
import numpy as np

patients = pd.read_csv("patients.csv")
adults = patients.query("age >= 18").copy()
adults["bmi"] = adults["weight"] / (adults["height"] ** 2)
adults["obese"] = np.where(adults["bmi"] >= 30, 1, 0)
adults = adults.loc[:, ["subject", "age", "bmi", "obese"]].copy()
patients_s = patients.sort_values(["site", "enrolled"], ascending=[True, False])
patients_s = patients_s.drop_duplicates(subset=["site"])
stats = patients.groupby(["site", "sex"], dropna=False)[["age", "bmi"]].agg(["count", "mean", "std", "min", "max"]).reset_index()
freq = pd.crosstab(patients["site"], patients["sex"], dropna=False)
both = demo.merge(labs, on="subject", how="inner")
adults.to_csv("adults.csv", index=False)
