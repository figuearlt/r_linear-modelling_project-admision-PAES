#######################################
## CARGA DE DATOS AUTOMÁTICOS KAGGLE ##
#######################################

# Librerías
# Automatización de Carga:
install.packages("reticulate")
library(reticulate)



################################
# Carga Datos de Admisión PAES #
################################


reticulate::py_run_string("
import os
from kaggle.api.kaggle_api_extended import KaggleApi
#URL DEL DATASET:
url_dataset_paes =  'daniellopez01/admisionuescl'
os.environ['KAGGLE_CONFIG_DIR'] = os.path.expanduser('~/.kaggle')

api = KaggleApi()
api.authenticate()
api.dataset_download_files(url_dataset_paes, path='data/raw/datos_admision', unzip=True)
")

#---

###############################
# Carga Datos de Restaurantes #
###############################


reticulate::py_run_string("
import os

from kaggle.api.kaggle_api_extended import KaggleApi
url_dataset_rt = 'imtkaggleteam/fast-food-restaurants-across-america'
os.environ['KAGGLE_CONFIG_DIR'] = os.path.expanduser('~/.kaggle')

api = KaggleApi()
api.authenticate()
api.dataset_download_files(url_dataset_rt, path='data/raw/Restaurantes_Data', unzip=True)
")