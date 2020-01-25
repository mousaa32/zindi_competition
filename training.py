import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
plt.style.use('ggplot')

data_frame = pd.read_csv("/home/moussa/Documents/PROJET/IA/zindi competition/dataset/Train(copie).csv") 
# Preview the first 5 lines of the loaded data 
# print(data_frame.describe())

# #les jours ou il ya bcp de commande
# nmbre_de_commande=data_frame.groupby('Type de client')['jour_semaine(commande)'].count()
# # print(nmbre_de_commande)

# #les Types de client
# Type_de_client=data_frame.groupby('Type de client')['Type de client'].count()
# # print(Type_de_client)

# #les les Types de client par plateforme
# plateforme=data_frame.groupby(['Type de client','Platform Type'])['num_commande'].count()
# print(plateforme)

# #les les Types de client par plateforme
# plateforme=data_frame.groupby(['Type de client','Platform Type'])['num_commande'].count()
# # print(plateforme)

# #les les clients qui commendent le plus par plateforme
# plateforme=data_frame.groupby(['Type de client','Platform Type'])['num_commande'].count()
# print(plateforme)

jour_du_mois=data_frame.groupby('jour_du mois(commande)')['jour_du mois(commande)'].count()
jour_du_moi = jour_du_mois.sort_values()
plt.bar(jour_du_moi.index,jour_du_moi.values)


