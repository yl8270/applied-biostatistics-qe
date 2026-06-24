import numpy as np
import pandas as pd
from pathlib import Path

rng = np.random.default_rng(20240610)
N = 2400
caseid = np.arange(1, N+1)
# procedure: bypass more common
proc = rng.choice(['43770','43644'], size=N, p=[0.38,0.62])
proc_bypass = (proc=='43644').astype(int)
age = np.clip(rng.normal(44 + 2.5*proc_bypass, 10, N), 18, 75).round(0)
bmi = np.clip(rng.normal(45 + 2.0*proc_bypass, 6.5, N), 35, 75).round(1)
sex = rng.choice(['Female','Male'], size=N, p=[0.78,0.22])
race = rng.choice(['Non-Hispanic White','Non-Hispanic Black','Hispanic','Other/Unknown'], size=N, p=[0.62,0.18,0.12,0.08])
smoke = rng.choice(['No','Yes'], size=N, p=[0.86,0.14])
# ASA classes: mild/no vs severe/life threat/none
asa = []
for i in range(N):
    base = [0.06,0.50,0.38,0.05,0.01] # None assigned, 1,2,3,4
    if proc_bypass[i]:
        base = [0.05,0.45,0.40,0.08,0.02]
    if age[i] > 55:
        base = np.array(base) + np.array([-0.01,-0.07,0.02,0.04,0.02])
    base = np.clip(base, 0.001, None); base = base/np.sum(base)
    asa.append(rng.choice(['None assigned','1-No Disturb','2-Mild Disturb','3-Severe Disturb','4-Life Threat'], p=base))
asa=np.array(asa)
# OPTIME and LOS
op_time = np.clip(rng.normal(80 + 45*proc_bypass + 0.6*(bmi-45), 28, N), 25, 350).round(0)
# latent complication probability
asa_severe = np.isin(asa, ['3-Severe Disturb','4-Life Threat']).astype(int)
smoke_yes = (smoke=='Yes').astype(int)
age_c=(age-45)/10
bmi_c=(bmi-45)/5
lp = -3.4 + 0.75*proc_bypass + 0.23*age_c + 0.18*bmi_c + 0.45*asa_severe + 0.28*smoke_yes + 0.20*proc_bypass*age_c + 0.15*proc_bypass*bmi_c
p_any=1/(1+np.exp(-lp))
any_comp = rng.binomial(1, p_any)
# Individual complications with varied probabilities, conditional higher if any_comp
comp_vars = [
    'OUPNEUMO','REINTUB','PULEMBOL','FAILWEAN','OPRENAFL','RENAINSF','URNINFEC','CDARREST','CDMI','OTHBLEED','OTHDVT','NEURODEF','CNSCOMA','CNSCVA','OTHGRAFL','OTHSYSEP','OTHSESHOCK','SUPINFEC','WNDINFD','ORGSPCSSI','DEHIS'
]
base_rates = np.array([0.018,0.012,0.006,0.009,0.004,0.005,0.020,0.003,0.004,0.035,0.012,0.003,0.0015,0.002,0.002,0.009,0.004,0.025,0.010,0.008,0.006])
comp_mat = np.zeros((N,len(comp_vars)), dtype=int)
for j,br in enumerate(base_rates):
    lp_j = np.log(br/(1-br)) + 0.55*proc_bypass + 0.25*asa_severe + 0.12*age_c + 0.10*smoke_yes
    p = 1/(1+np.exp(-lp_j))
    comp_mat[:,j]=rng.binomial(1, p)
# force align with any_comp: if any_comp=1 and no individual complication, add one common complication
num0=comp_mat.sum(axis=1)
idx=np.where((any_comp==1)&(num0==0))[0]
if len(idx)>0:
    choices=rng.choice([0,8,9,15,17], size=len(idx), p=[0.20,0.08,0.32,0.12,0.28])
    comp_mat[idx, choices]=1
# Some may have complication despite any_comp simulated 0; derived any in R will be source of truth
num_comp = comp_mat.sum(axis=1)
# Mortality and reoperation
mort_lp = -6.0 + 0.6*proc_bypass + 0.7*(num_comp>0) + 0.45*asa_severe + 0.25*age_c
mort = rng.binomial(1, 1/(1+np.exp(-mort_lp)))
reop_lp = -4.2 + 0.65*proc_bypass + 1.25*(num_comp>0) + 0.25*asa_severe
reop = rng.binomial(1, 1/(1+np.exp(-reop_lp)))
# LOS skewed, at least 0/1 days
los_mean = 1.1 + 0.9*proc_bypass + 0.30*num_comp + 0.25*asa_severe + 0.1*bmi_c
los = rng.negative_binomial(n=2.2, p=2.2/(2.2+np.exp(np.log(np.clip(los_mean,0.2,None))))).astype(int) + 1
los = np.clip(los, 1, 35)
# string output like real data
out = pd.DataFrame({
    'CaseID': caseid,
    'bmi': bmi,
    'ageyrs': age.astype(int),
    'race_eth': race,
    'SEX': sex,
    'CPT': proc,
    'SMOKE': smoke,
    'ASACLAS': asa,
    'OPTIME': op_time.astype(int),
    'Mortality': np.where(mort==1,'Yes','No'),
    'TOTHLOS': los.astype(int),
    'REOPERATION': np.where(reop==1,'Yes','No'),
})
for j,v in enumerate(comp_vars):
    out[v]=np.where(comp_mat[:,j]==1,'Yes','No')
# Add missing codes: -99 numeric and NULL char
for col, rate in [('OPTIME',0.015),('TOTHLOS',0.01),('bmi',0.005),('ageyrs',0.004)]:
    mask=rng.random(N)<rate
    out.loc[mask,col]=-99
for col, rate in [('race_eth',0.01),('SEX',0.003),('SMOKE',0.006),('ASACLAS',0.007),('Mortality',0.004),('REOPERATION',0.004)]:
    mask=rng.random(N)<rate
    out.loc[mask,col]='NULL'
# rare missing in some complications
for col in comp_vars:
    mask=rng.random(N)<0.002
    out.loc[mask,col]='NULL'
# Save
root=Path('/mnt/data/qe2024_applied_nsqip')
out.to_csv(root/'data/raw/NSQIP_bmi_synthetic.csv', index=False)
# dictionary
labels={
'CaseID':'Case Identification Number','bmi':'BMI','ageyrs':'Age, years','race_eth':'Race/ethnicity','SEX':'Gender','CPT':'Procedure type, 43770 = Gastric Banding, 43644 = Gastric Bypass','SMOKE':'Current smoker within one year','ASACLAS':'ASA classification','OPTIME':'Total operation time, -99 = missing','Mortality':'Mortality','TOTHLOS':'Length of total hospital stay, days, -99 = missing','REOPERATION':'Reoperation'}
labels.update({v:v for v in comp_vars})
pd.DataFrame({'Variable':list(labels.keys()), 'Label':list(labels.values())}).to_csv(root/'data/dictionary/synthetic_data_dictionary.csv', index=False)
print(out.shape)
print(out.head())
