"""Extract only the RAW subfolder from each city's SEF zip into
raw_data/<City>/SEF_output/RAW (skips cities already extracted)."""

import glob
import os
import zipfile

for z in sorted(glob.glob(os.path.join("raw_data", "*", "SEF_*.zip"))):
    city = os.path.basename(os.path.dirname(z))
    dest = os.path.join("raw_data", city, "SEF_output", "RAW")
    if os.path.isdir(dest) and os.listdir(dest):
        print(city, "already extracted,", len(os.listdir(dest)), "files")
        continue
    os.makedirs(dest, exist_ok=True)
    zf = zipfile.ZipFile(z)
    members = [n for n in zf.namelist()
               if "/raw/" in n.lower() and n.lower().endswith(".tsv")]
    for m in members:
        target = os.path.join(dest, os.path.basename(m))
        with zf.open(m) as src, open(target, "wb") as out:
            out.write(src.read())
    print(city, len(members), "RAW files extracted")
