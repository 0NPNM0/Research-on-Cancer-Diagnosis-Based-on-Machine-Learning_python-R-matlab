# import pydicom as dicom
#
# dcm = dicom.read_file("/home/npnm/Desktop/Research-on-Cancer-Diagnosis-Based-on-Machine-Learning_python/Data/LIDC-IDRI-0001/01-01-2000-NA-NA-30178/3000566.000000-NA-03192/1-006.dcm")
# print(dcm)

from ucimlrepo import fetch_ucirepo
import pandas

# fetch dataset
lung_cancer = fetch_ucirepo(id=62)

# data (as pandas dataframes)
X = lung_cancer.data.features
y = lung_cancer.data.targets

# metadata
print(lung_cancer.metadata)

# variable information
print(lung_cancer.variables)