# Python Script to perform clustering on Klamath diversion pattern data.
# Also calculates diversion means
# Author: Jordan Lanini
# Date: 8/25/22

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import mpl_toolkits.mplot3d

from sklearn.cluster import KMeans
from sklearn.metrics import silhouette_score
from sklearn.preprocessing import StandardScaler
from sklearn import datasets


def ClusterAnalysis():  # Function to perform simple cluster analysis on the data.  Ultimately not used because no good patterns were apparent
    df2 = pd.concat([df_divsum, median], axis=1).dropna()
    df2 = pd.concat([df_class['UKLNI'], df_divsum, median], axis=1).dropna()
    data = df2.values.tolist()
    scaler = StandardScaler().fit(data)
    scale_data = scaler.transform(data)  # scale data for analysis
    inertias = []

    for i in range(1, 11):
        kmeans = KMeans(n_clusters=i)
        kmeans.fit(scale_data)
        inertias.append(kmeans.inertia_)

    # plt.plot(range(1, 11), inertias, marker='o')
    # plt.title('Elbow method')
    # plt.xlabel('Number of clusters')
    # plt.ylabel('Inertia')
    # # plt.show()
    # plt.close()
    kmeans = KMeans(n_clusters=3)
    kmeans.fit(scale_data)

    fignum = 1

    kmeans.fit(data)
    labels = kmeans.labels_
    # fig = plt.figure(fignum, figsize=(4, 3))
    # ax = fig.add_subplot(111, projection="3d", elev=28, azim=134)
    # ax.set_position([0, 0, 0.95, 1])
    # ax.scatter(df2[0], df2['UKLNI'], df2[1], c=labels.astype(float), edgecolor="k")
    #
    # ax.w_xaxis.set_ticklabels([])
    # ax.w_yaxis.set_ticklabels([])
    # ax.w_zaxis.set_ticklabels([])
    # ax.set_xlabel("Total Diversion")
    # ax.set_ylabel("UKLNI")
    # ax.set_zlabel("Median diversion day")
    # ax.set_title('KMeans ' + divname)
    # ax.dist = 12
    # fignum = fignum + 1
    # plt.close()
    # plt.scatter(df2[0], df2[1], c=kmeans.labels_)


inpath = ("C:\\Users\\JLanini\\Documents\\GitHub\\KROM\\Database\\")  # Path containing diversion data
figpath = ("C:\\Users\\JLanini\\OneDrive - DOI\\Klamath\\Scripts\Output\\")  # Path containing diversion data
# Dictionary for line plotting
color_dict = {'DRY': 'brown',
              'NORMAL': 'green',
              'WET': 'blue',
              0: 'brown',
              1: 'green',
              2: 'blue'}
if __name__ == '__main__':
    df_out = pd.DataFrame()  # create empty dataframe for exporting curves
    divlist = ['ACHO', 'ADDO Ag', 'MHPO - Spill', 'NOCO', 'ST48', 'FFF', 'LRDC',
               'LKNWR']  # list of diversion points to map
    divdict = {}
    df_class = pd.read_excel(inpath + 'KROM_DistributionPercent.xlsx', sheet_name='YearType', header=0,
                             index_col=0)  # Read in the classification data performed by BM
    rows = int(np.ceil(len(divlist) / 2))  # calculate the number of rows in the pot
    fig, axs = plt.subplots(nrows=rows, ncols=2, sharex=True, sharey=True, figsize=(10, 6.5),  # initialize the plot
                            squeeze=False)  # set up multipanel plots to show each diversion
    for divname in divlist:
        df = pd.read_excel(inpath + 'KROM_DistributionPercent.xlsx', sheet_name=divname, skiprows=range(6),
                           usecols='E:AS', header=0)  # Read in the individual diversion data
        divdict[divname] = df.clip(lower=0)  # set values less than zero to zero
        if divname == 'ACHO':  # if it's the first diversion, initialize dataframe
            df_divsum = divdict[divname].sum()
        else:
            df_divsum = df_divsum + divdict[divname].sum()  # cumulate diversions
    pltcount = 0
    for key in divdict:
        divdict[key].drop(columns=[2001, 2010, 2020, 2021], axis=1,
                        inplace=True)  # drop the last year due to missing data and anomalous years.
        # Years to drop determined by BM (9/6/22)
        if (key == 'LKNWR'):  # Special drop for LKNWR
            divdict[key].drop(columns=2014, axis=1,
                            inplace=True)  # 2014 only had one day of diversions, throwing everything off
        if (key == 'NOCO' or key == 'ADDO Ag'):  # split the data into two diversion periods for these PODs.
            df_fall = divdict[key].iloc[244:, ] / divdict[key].iloc[244:, ].sum()
            df_summer = divdict[key].iloc[:244, ] / divdict[key].iloc[
                                                    :244, ].sum()  # pull the values through day 245, starting on March 1.
            df_Percent = df_summer.append(df_fall)  #
        else:
            df_Percent = divdict[key] / divdict[key].sum()  # otherwise, calculate for the total
        median = df_Percent.cumsum().ge(0.5).idxmax()
        # ClusterAnalysis() #Perform cluster analysis
        count = 0

        df_out[key] = df_Percent.mean(axis=1) #export the mean of the percentages.  This is used in RiverWare
        for column, vals in df_Percent.iteritems(): #Plot each diversion trace
            # find the row and column to plot
            col = int(pltcount / rows)
            row = pltcount % rows
            print(row, col)
            # set the title to the diversion name
            axs[row][col].set_title(key, fontsize=10)
            #axs[row][col].plot(df_Percent.index.values, vals,
            #                   c=color_dict[labels[count]], linewidth=0.5)  # Plot the annual traces
            axs[row][col].plot(df_Percent.index.values, vals,
                               c='green', linewidth=0.2)  # Plot the annual traces

            count = count + 1
        axs[row][col].plot(df_out[key], c='Black', linewidth=2)  # Plot the average
        axs[row][col].set_ylim(0, .05)
        pltcount = pltcount + 1
        # if row == 3 and col == 1: #legend
        #     for label in np.unique(df_class['Baker Classification']):
        #         axs[row][col].plot(df_Percent.index.values, vals, label=df_class['Baker Classification'].iloc[count],
        #                            c=color_dict[label], linewidth=0.5)
        #     axs[row][col].legend(loc="lower right")

    # labels=[]
    # fig.legend([x, y], loc="upper right")
    # Set common labels
    fig.text(0.5, 0.04, 'Day of year (Mar-Feb)', ha='center', va='center')
    fig.text(0.06, 0.5, 'Percent of Total Diversion', ha='center', va='center', rotation='vertical')
    fig.suptitle('Annual and mean diversion patterns by day of year', fontsize=18)
    plt.savefig(figpath + '\\diversionpatterns2.png')
    df_out.to_csv(figpath + '\\patterns.csv')
    # df_out.plot()
    # plt.show()
