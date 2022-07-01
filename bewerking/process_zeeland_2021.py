# -*- coding: utf-8 -*-
"""
Created on Tue Jun  4 10:28:32 2019

@author: Cor
"""
import pandas as pd
import os
from math import log

#Import library with meteorological functions; now taken from working directory
#instead of from py library
import met_suitePET

# Set year here, or make interactive!
yyyy='2021'

# directory with datafiles
datadir = "/home/danke010/scratch/Zeeland/" + yyyy + "/"

#Get station name. Urban stations are 108, 109, 110, 111 or 112. For these
#stations, a correction for wind speed needs to be set for UTCI calculations.
#The urban stations are at a height of 3.5m. Wind speed for UTCI is needed at
#10m. We use a simple log-law for neutral conditions, assuming the roughness
#length is 1 m.
 
for station in ['108','109','110','111','112']:

    print('Processing station ',station)
    urban=(station=='108' or station=='109' or station=='110' or station=='111'or station=='112')
    
    if urban:
        Ufac=log(10/1.)/log(3.5/1.) 
    else:
        Ufac=1.

    #Name format of data files is set by following lines, but may of course be changed
    statdir = datadir + "Data " + station + "/"
    statfile = statdir + "Alldata_" + yyyy + "_" + station + "_screened.csv"
    outfile = statdir + "Data_" + yyyy + "_" + station + "_TCPET.csv"

    f1=open(statfile, 'r')
    f2=open(outfile,'w')
    
    #Read the meteorological data, while parsing date strings into datetime objects
    
    Meteo = pd.read_csv(f1,parse_dates=['DateTime'],dayfirst=True,na_values='#N/A')
    
    #infer_datetime_format; dayfirst=
    #Check some properties of Meteo: variable type and occurrence of null-values
    meteotypes = Meteo.dtypes
    identifynull = Meteo.isnull().any()
    quicklook = Meteo.describe()
    
#    print(meteotypes)
#    print(identifynull)
    print(quicklook)
    
    #If there appears to be a null value the location can be determined with:
    #Meteo.isnull(). Note that the results can be stored in an array object, which
    #may prove handy in further operations.
    
    #Convert to hourly data; not that everything is in UTC. Should that not be
    #the case anymore, a conversion for time zone is required.
    
    Meteo.index = Meteo['DateTime']
    Hourly = Meteo.resample('H').mean()
    Time = pd.to_datetime(Hourly.index)
    Tair = Hourly['temp']
    Hum = Hourly['rh']
    Kin = Hourly['rad']
    U = Hourly['speed']
    
    doy=Time.strftime('%j').astype(int)
    Hours  = Time.strftime('%H').astype(int)
    Minutes = Time.strftime('%M').astype(int)
    Dectime = Hours + Minutes/60.
    
    #Initialize lists
    #
    
    U10 = [None]*len(Hourly)
    fdif = [None]*len(Hourly)
    fdir = [None]*len(Hourly)
    Sol_dif=[None]*len(Hourly)
    Sol_dir=[None]*len(Hourly)
    LWin = [None]*len(Hourly)
    Tglobe=[None]*len(Hourly)
    mrt=[None]*len(Hourly)
    twb=[None]*len(Hourly)
    wbgt=[None]*len(Hourly)
    UTCI=[None]*len(Hourly)
    PET=[None]*len(Hourly)
    
    #Compute derived data, using the meteorological library met_suite
    # NOTE: LWdown is not yet implemented here!
    print("processing hourly data...")
    for i in range(len(Hourly)):
        
        cza        = met_suitePET.sin_solar_elev(lat=51.5,lon=3.75,DOY=doy[i],utc_dec=Dectime[i])
        
        fdif[i]       = met_suitePET.fr_diffuse(solar_down=Kin[i],DOY=doy[i])
        fdir[i]       = 1.-fdif[i]
       
        Tglobe[i]  = met_suitePET.calc_Tglobe(Ta=Tair[i],RH=Hum[i],Ua=Ufac*U[i],Solar=Kin[i],fdir=fdir[i],cza=cza)
        mrt[i]     = met_suitePET.Tmrt(Tglobe[i],Tair[i],U[i])
        UTCI[i]    = met_suitePET.UTCI(Tair[i],mrt[i],U[i],Hum[i])
        
        twb[i]     = met_suitePET.T_wb(Tair[i],Hum[i])
        wbgt[i]    = 0.7*twb[i] + 0.2*Tglobe[i] +0.1*Tair[i]
        tc, tsk, tcl, esw_real = met_suitePET.system(Tair[i], mrt[i], Hum[i], U[i], 80., 0.9, "standing","male",35.,75.,1.8)
        PET[i]     = met_suitePET.pet(tc,tsk,tcl,Tair[i], esw_real)
    
        #Inform on progress...
        
        if i%1000 == 0:
            print(i, "out of", len(Hourly))
    
    #...and create output...
    print("writing to file", outfile)
    
    out = pd.DataFrame({    'Year       ': Time.strftime('%y').astype(int),
                            'Month      ': Time.strftime('%m').astype(int),
                            'Day        ': Time.strftime('%d').astype(int),
                            'DOY        ': doy,
                            'Hour       ': Hours,
                            'Minutes    ': Minutes,
                            'Dectime    ': Dectime,
    #                        'LWd  [W/m2]': Obs.Longwave(),
                            'Tair    [C]': Tair,
                            'RH      [%]': Hum,
                            'Wind  [m/s]': U,
                            'Sol  [W/m2]': Kin,
                            'Dir  [W/m2]': fdir*Kin,
                            'Dif  [W/m2]': fdif*Kin,
                            'Twb    [°C]': twb,
                            'Tglobe [°C]': Tglobe,
                            'Tmrt   [°C]': mrt,
                            'WBGT   [°C]': wbgt,
                            'PET    [°C]': PET,
                            'UTCI   [°C]': UTCI})
    
    #...for specific years and months
    
    select=out.loc[out['Month      ']>=4].loc[out['Month      ']<=9].loc[out['Year       ']==int(yyyy[2:4])]
    select.to_csv(f2,index=False,line_terminator='\n')
    
    f1.close()
    f2.close()

print()
print("READY")


########## PROCESS KNMI STATIONS
# main code is same as above, but data are read in differently 
# and conversion of units is required

statfilenames = {'Vlissingen': 'uurgeg_310_2021-2030_Vlissingen.csv',
                 'Westdorpe': 'uurgeg_319_2021-2030-Westdorp.csv',
                 'Wilhelminadorp': 'uurgeg_323_2021-2030-wilhelmimadorp.csv'}

#for station in ['Vlissingen','Westdorpe','Wilhelminadorp']:
station='Wilhelminadorp'
Ufac=1.

#Name format of data files is set by following lines, but may of course be changed
statdir = datadir 
statfile = statdir + statfilenames[station]
outfile = statdir + "Data_" + yyyy + "_" + station + "_TCPET.csv"

f1=open(statfile, 'r')
f2=open(outfile,'w')

#Read the meteorological data, while parsing date strings into datetime objects

#Meteo = pd.read_csv(f1,parse_dates=['DateTime'],dayfirst=True,na_values='#N/A')
#Meteo = pd.read_csv(f1, header=31,parse_dates=[[1,2]])
Meteo = pd.read_csv(f1, header=31)

# remove first (empty) row
Meteo = Meteo.drop(labels=0,axis=0)
# change data type of date columns
Meteo = Meteo.astype({'YYYYMMDD': 'int32'})
Meteo = Meteo.astype({'   HH': 'int32'})

#pd.to_datetime(Meteo.YYYYMMDD,format="%Y%m%d")
#pd.to_datetime(Meteo['YYYYMMDD'],format="%Y%m%d")
Meteo['DateTime'] = pd.to_datetime(Meteo['YYYYMMDD'],format="%Y%m%d") + Meteo['   HH'].astype('timedelta64[h]')
Meteo.index = Meteo['DateTime']
# data are already hourly
#Hourly = Meteo.resample('H').mean()
Hourly = Meteo
Time = pd.to_datetime(Hourly.index)
Tair = Hourly['    T'] * 0.1
Hum = Hourly['    U'] 
Kin = Hourly['    Q']*(100*100) / (3600) # convert J/cm2g per hour to W/m2
U = Hourly['   FH'] * 0.1
doy=Time.strftime('%j').astype(int)
Hours  = Time.strftime('%H').astype(int)
Minutes = Time.strftime('%M').astype(int)
Dectime = Hours + Minutes/60.
    
#Initialize lists
U10 = [None]*len(Hourly)
fdif = [None]*len(Hourly)
fdir = [None]*len(Hourly)
Sol_dif=[None]*len(Hourly)
Sol_dir=[None]*len(Hourly)
LWin = [None]*len(Hourly)
Tglobe=[None]*len(Hourly)
mrt=[None]*len(Hourly)
twb=[None]*len(Hourly)
wbgt=[None]*len(Hourly)
UTCI=[None]*len(Hourly)
PET=[None]*len(Hourly)

#Compute derived data, using the meteorological library met_suite
# NOTE: LWdown is not yet implemented here!
print("processing hourly data...")
for i in range(len(Hourly)):
    
    cza        = met_suitePET.sin_solar_elev(lat=51.5,lon=3.75,DOY=doy[i],utc_dec=Dectime[i])
    
    fdif[i]       = met_suitePET.fr_diffuse(solar_down=Kin[i],DOY=doy[i])
    fdir[i]       = 1.-fdif[i]
   
    Tglobe[i]  = met_suitePET.calc_Tglobe(Ta=Tair[i],RH=Hum[i],Ua=Ufac*U[i],Solar=Kin[i],fdir=fdir[i],cza=cza)
    mrt[i]     = met_suitePET.Tmrt(Tglobe[i],Tair[i],U[i])
    UTCI[i]    = met_suitePET.UTCI(Tair[i],mrt[i],U[i],Hum[i])
    
    twb[i]     = met_suitePET.T_wb(Tair[i],Hum[i])
    wbgt[i]    = 0.7*twb[i] + 0.2*Tglobe[i] +0.1*Tair[i]
    tc, tsk, tcl, esw_real = met_suitePET.system(Tair[i], mrt[i], Hum[i], U[i], 80., 0.9, "standing","male",35.,75.,1.8)
    PET[i]     = met_suitePET.pet(tc,tsk,tcl,Tair[i], esw_real)
    
    #Inform on progress...
    
    if i%1000 == 0:
        print(i, "out of", len(Hourly))

#...and create output...
print("writing to file", outfile)

out = pd.DataFrame({    'Year       ': Time.strftime('%y').astype(int),
                        'Month      ': Time.strftime('%m').astype(int),
                        'Day        ': Time.strftime('%d').astype(int),
                        'DOY        ': doy,
                        'Hour       ': Hours,
                        'Minutes    ': Minutes,
                        'Dectime    ': Dectime,
#                        'LWd  [W/m2]': Obs.Longwave(),
                        'Tair    [C]': Tair,
                        'RH      [%]': Hum,
                        'Wind  [m/s]': U,
                        'Sol  [W/m2]': Kin,
                        'Dir  [W/m2]': fdir*Kin,
                        'Dif  [W/m2]': fdif*Kin,
                        'Twb    [°C]': twb,
                        'Tglobe [°C]': Tglobe,
                        'Tmrt   [°C]': mrt,
                        'WBGT   [°C]': wbgt,
                        'PET    [°C]': PET,
                        'UTCI   [°C]': UTCI})

#...for specific years and months

select=out.loc[out['Month      ']>=4].loc[out['Month      ']<=9].loc[out['Year       ']==int(yyyy[2:4])]
select.to_csv(f2,index=False,line_terminator='\n')

f1.close()
f2.close()

print()
print("READY")