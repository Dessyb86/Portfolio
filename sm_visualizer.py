#########################################################################################################
# File Name:        sm_visualizer.py                                                                    #
# Editor:           Patrick Barrett                                                                     #
# Last update:      May 1 , 2020                                                                       #
# Descrition :      This script reads a series of csv files containing soil moisture data and           #
#                   manipulates, performs quality control operations and visualize these data sets      #
#########################################################################################################

########################### User Inputs #########################

folder_location = '/Users/Dessyb/Dropbox/PPCR/SoilMoisture/' #name of the folder contaiing teh csv file for each station

stations = ['ATG_Cardi','ATG_Christian','BAR_Canefield_Nursery_A','BAR_Husbands_A','DMA_Pondcasse','DMA_Salisbury','GRD_Laura','GUY_Kairuni','GUY_Kuruduni','GUY_Uitvlugt','KNA_Stone_Castle','SLU_Doree','SVG_Jennings_A','SVG_Jennings_RG','SVG_Owia'] #list of station names

csv_locations = ['ATG_Cardi_17061771.csv','ATG_Christian_17061854.csv','BAR_Canefield_Nursery_(A)_13159596.csv','BAR_Husbands_(A)_13159643.csv','DMA_Pondcasse_15361384.csv','DMA_Salisbury_15461444.csv','GRD_Laura_12061649.csv','GUY_Kairuni_61558263.csv','GUY_Kuruduni_51658480.csv','GUY_Uitvlugt_68758300.csv','KNA_Stone_Castle_17362787.csv','SLU_Doree_13761063.csv','SVG_Jennings_(A)_13261162.csv','SVG_Jennings_(RG)_13261165.csv','SVG_Owia_13361174.csv'] #corresponding list of location of csv files for each station respectively

columns = 2

rows = 4

resolution = 600
##################### DO NOT TAMPER BEYOND THIS POINT #####################

import numpy as np
import math
import csv
import os
import pandas as pd
from datetime import datetime
from datetime import timedelta
import matplotlib.pyplot as plt
import matplotlib.pylab as pl
import sys
from matplotlib import image

for i in range(len(stations)): #for loop to iterate through stations to read in data and print a series of plots

    station_name = stations[i] #name of station

    file_name = folder_location + csv_locations[i] #location of the csv files for each station

    df = pd.read_csv(file_name,header=[0]) #read csv file into a pandas dataframe

    df['corrected_soil_moisture'] = np.where((df['corrected_soil_moisture'] > 100),np.nan,df['corrected_soil_moisture']) #removing extremely high values

    df['corrected_soil_moisture'] = np.where((df['corrected_soil_moisture'] <= 0),np.nan,df['corrected_soil_moisture']) # removing unrealistic negative and zero values

    date_time = [] #array to store datetime variable

    soil_moisture = [] #array to store soil mositure values

    date_time_7 = [] #array to store dates for thw weekly moving averages

    date_time_15 = [] #array to store dates for 15 day moving average

    soil_moisture_7 = [] #array to store the macthing soil mositure data corresponding to date_time_7

    soil_moisture_15 = [] #array to store the macthing coil mositure data corresponding to date_time_7

    for j in range(len(df['day'])):

        new = datetime.strptime(str(df['day'].iloc[j])+'/'+str(df['month'].iloc[j])+'/'+str(df['year'].iloc[j])+' '+str(df['hour'].iloc[j])+':'+str(df['minute'].iloc[j]),'%d/%m/%Y %H:%M') #converting year, month, day, hour and minutes into a single datetime variable

        date_time.append(new) #populating datetime array

    df['datetime'] = date_time #adding new datetime column to existing df dataframe

    soil_moisture = df['corrected_soil_moisture'] #storing soil moisture before removal of outliers
    
    Q1 = np.nanpercentile(soil_moisture,25) #25th percentile of the soil moisture data

    Q3 = np.nanpercentile(soil_moisture,75) #75th percentile of the soil moisture data

    IQR = Q3 - Q1 #interquartile range

    qc_df = df[(df.corrected_soil_moisture >= Q1 - 1.5 * IQR) & (df.corrected_soil_moisture <= Q3 + 1.5 * IQR)] #dataframe with outliers removed

    qc_date_time = qc_df['datetime']

    qc_soil_moisture = qc_df['corrected_soil_moisture']

    start_day = datetime.strptime(str(df['day'].iloc[0])+'/'+str(df['month'].iloc[0])+'/'+str(df['year'].iloc[0]),'%d/%m/%Y') #first day of within the data set

    start_week = start_day #start week marks the beginning of a 7 day period

    end_week = start_week + timedelta(days = 7) #end week marks the end of the 7 day period

    final_day = datetime.strptime(str(df['day'].iloc[-1])+'/'+str(df['month'].iloc[-1])+'/'+str(df['year'].iloc[-1]),'%d/%m/%Y') #final day of within the data set

    start_bi_week = start_day #start week marks the beginning of a 15 day period

    end_bi_week = start_week + timedelta(days = 15) #end_bi_week marks the end of the 15 day period

    while end_week <= final_day:

        date_time_7.append(end_week)

        week_slice = df[(df.datetime >= start_week) & (df.datetime < end_week)] #taking a 15 day slice of the existing dataframe

        missing_values = week_slice['corrected_soil_moisture'].isnull().sum() #number of missing values within the 15 day period

        no_values = len(week_slice['corrected_soil_moisture']) #total number of observations within the 15 day period

        start_week += timedelta(days = 1) #moving the start of the week to the following day

        end_week += timedelta(days = 1) #moving the end of the week to the following day

        if missing_values <= 0.1 * no_values: #ensures that less than 10% of the data in missing

            soil_moisture_7.append(np.mean(week_slice['corrected_soil_moisture']))

        else:

            soil_moisture_7.append(np.nan) #appends NaN if more than 10% of data is missing
    
    while end_bi_week <= final_day:

        date_time_15.append(end_bi_week) #appends the end of the 15 day period

        week_slice_15 = df[(df.datetime >= start_bi_week) & (df.datetime < end_bi_week)] #taking a 15 day slice of the existing dataframe

        missing_values = week_slice_15['corrected_soil_moisture'].isnull().sum() #number of missing values within the 15 day period

        no_values = len(week_slice_15['corrected_soil_moisture']) #total number of observations within the 15 day period

        start_bi_week += timedelta(days = 1) #moving the start of the 15 day period to the following day

        end_bi_week += timedelta(days = 1) #moving the end of the 15 day period to the following day

        if missing_values <= 0.1 * no_values: #ensuring less than 10% of the data is missing

            soil_moisture_15.append(np.mean(week_slice_15['corrected_soil_moisture']))

        else:

            soil_moisture_15.append(np.nan)

    #Creating Plots

    filename1 = 'soil_moisture_plots/'+stations[i]+'.png'

    filename2 = 'qc_soil_moisture_plots/'+stations[i]+'.png'

    filename3 = '7_day_mean_soil_moisture_plots/'+stations[i]+'.png'

    filename4 = '15_day_mean_soil_moisture_plots/'+stations[i]+'.png'

    plt.scatter(date_time, soil_moisture,s=0.2,linewidth=1) #create scatter plot for each station
    plt.title(stations[i]) #title of chart set as name of station
    plt.xlabel('Date') #labelling quantity being displayed on x-axis
    plt.ylabel('Soil Moisture') #labelling quantity being displsyed on y-axis
    plt.ylim(0,100) #fixes y axis between 0 and 100
    
    if not os.path.exists('soil_moisture_plots'): #creates folder if not existing
        os.makedirs('soil_moisture_plots')
        
    plt.savefig(filename1, dpi=resolution) #saving image
    plt.clf()
    plt.close #closing graph

    plt.scatter(qc_date_time, qc_soil_moisture,s=0.2,linewidth=1) #create scatter plot for each station
    plt.title(stations[i]) #title of chart set as name of station
    plt.xlabel('Date') #labelling quantity being displayed on x-axis
    plt.ylabel('Soil Moisture') #labelling quantity being displsyed on y-axis
    plt.ylim(0,100) #fixes y axis between 0 and 100
    
    if not os.path.exists('qc_soil_moisture_plots'): #creates folder if not existing
        os.makedirs('qc_soil_moisture_plots')
        
    plt.savefig(filename2, dpi=resolution) #saving image
    plt.clf()
    plt.close #closing graph

    plt.scatter(date_time_7, soil_moisture_7,s=0.2,linewidth=1) #create scatter plot for each station
    plt.title(stations[i]) #title of chart set as name of station
    plt.xlabel('Date') #labelling quantity being displayed on x-axis
    plt.ylabel('Soil Moisture') #labelling quantity being displsyed on y-axis
    plt.ylim(0,100) #fixes y axis between 0 and 100
    
    if not os.path.exists('7_day_mean_soil_moisture_plots'): #creates folder if not existing
        os.makedirs('7_day_mean_soil_moisture_plots')
        
    plt.savefig(filename3, dpi=resolution) #saving image
    plt.clf()
    plt.close #closing graph

    plt.scatter(date_time_15, soil_moisture_15,s=0.2,linewidth=1) #create scatter plot for each station
    plt.title(stations[i]) #title of chart set as name of station
    plt.xlabel('Date') #labelling quantity being displayed on x-axis
    plt.ylabel('Soil Moisture') #labelling quantity being displsyed on y-axis
    plt.ylim(0,100) #fixes y axis between 0 and 100
    
    if not os.path.exists('15_day_mean_soil_moisture_plots'): #creates folder if not existing
        os.makedirs('15_day_mean_soil_moisture_plots')
        
    plt.savefig(filename4, dpi=resolution) #saving image

    plt.clf()
    plt.close #closing graph

    print(stations[i]+' completed')

#Reading in plots to produce panes determined by rows and columns plots

set_plots = int(len(stations) / (columns * rows)) #number of 2 x 3 plots

if set_plots % (columns * rows) > 0: #determining if an extra plot needs to be created

    set_plots += 1


for i in range(set_plots): #iterating though the images

    plt.gcf()

    plt.figure(i+1) #initialising firgure
    plt.subplots_adjust(wspace=0.005, hspace=0.005)

    for j in range(1, columns*rows + 1):

        if (j + i * (rows * columns)) <= len(stations): #plots an empty plot if number of plots is not a multiple of 6

            img = image.imread('soil_moisture_plots/'+stations[(j - 1) + i * (rows * columns)]+'.png')       

            plt.imshow(img,aspect = 'auto')

            placement = 100 * rows + 10 * columns + j

            plt.subplot(placement)
            
            plt.box(on=None)

            plt.axis('off')

        else:

            img = np.random.randint(10, size=(10,10))

            plt.imshow(img,aspect = 'auto')

            placement = 100 * rows + 10 * columns + j

            plt.subplot(placement)
            
            plt.box(on=None)

            plt.axis('off')            


    saving_name = 'soil_moisture_plots/'+'compiled_plots_'+str(1+i) #creating name to save plot under

    plt.savefig(saving_name, dpi=resolution) #saving image
    plt.clf() #clearing plot
    plt.close() #closing graph

for i in range(set_plots): #iterating though the images

    plt.figure(i+1) #initialising firgure
    plt.subplots_adjust(wspace=0.005, hspace=0.005)

    for j in range(1, columns*rows + 1):

        if (j + i * (rows * columns)) <= len(stations): #plots an empty plot if number of plots is not a multiple of 6

            img = image.imread('qc_soil_moisture_plots/'+stations[(j - 1) + i * (rows * columns)]+'.png')       

            plt.imshow(img,aspect = 'auto')

            placement = 100 * rows + 10 * columns + j

            plt.subplot(placement)
            
            plt.box(on=None)

            plt.axis('off')

        else:

            img = np.random.randint(10, size=(10,10))

            plt.imshow(img,aspect = 'auto')

            placement = 100 * rows + 10 * columns + j

            plt.subplot(placement)
            
            plt.box(on=None)

            plt.axis('off')            


    saving_name = 'qc_soil_moisture_plots/'+'compiled_plots_'+str(1+i) #creating name to save plot under

    plt.savefig(saving_name, dpi=resolution) #saving image
    plt.clf() #clearing plot
    plt.close() #closing graph

for i in range(set_plots): #iterating though the images

    plt.figure(i+1) #initialising firgure
    plt.subplots_adjust(wspace=0.005, hspace=0.005)

    for j in range(1, columns*rows + 1):

        if (j + i * (rows * columns)) <= len(stations): #plots an empty plot if number of plots is not a multiple of 6

            img = image.imread('7_day_mean_soil_moisture_plots/'+stations[(j - 1) + i * (rows * columns)]+'.png')       

            plt.imshow(img,aspect = 'auto')

            placement = 100 * rows + 10 * columns + j

            plt.subplot(placement)
            
            plt.box(on=None)

            plt.axis('off')

        else:

            img = np.random.randint(10, size=(10,10))

            plt.imshow(img,aspect = 'auto')

            placement = 100 * rows + 10 * columns + j

            plt.subplot(placement)
            
            plt.box(on=None)

            plt.axis('off')            


    saving_name = '7_day_mean_soil_moisture_plots/'+'compiled_plots_'+str(1+i) #creating name to save plot under

    plt.savefig(saving_name, dpi=resolution) #saving image
    plt.clf() #clearing plot
    plt.close() #closing graph

for i in range(set_plots): #iterating though the images

    plt.figure(i+1) #initialising firgure
    plt.subplots_adjust(wspace=0.005, hspace=0.005)

    for j in range(1, columns*rows + 1):

        if (j + i * (rows * columns)) <= len(stations): #plots an empty plot if number of plots is not a multiple of 6

            img = image.imread('15_day_mean_soil_moisture_plots/'+stations[(j - 1) + i * (rows * columns)]+'.png')       

            plt.imshow(img,aspect = 'auto')

            placement = 100 * rows + 10 * columns + j

            plt.subplot(placement)
            
            plt.box(on=None)

            plt.axis('off')

        else:

            img = np.random.randint(10, size=(10,10))

            plt.imshow(img,aspect = 'auto')

            placement = 100 * rows + 10 * columns + j

            plt.subplot(placement)
            
            plt.box(on=None)

            plt.axis('off')            


    saving_name = '15_day_mean_soil_moisture_plots/'+'compiled_plots_'+str(1+i) #creating name to save plot under

    plt.savefig(saving_name, dpi=resolution) #saving image
    plt.clf() #clearing plot
    plt.close() #closing graph

