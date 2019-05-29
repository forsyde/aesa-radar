# -*- coding: utf-8 -*-
"""
Created on Tue Apr 24 12:27:06 2019

@author: Timmy Sundström
"""


# This script will generate input data, AESA["InputData], to an AESA model in the format
# of a matrix with: 
# NoDataChannels radar elements
# NoRangeBinInEveryPulse samples per pulse
# NoPulsesPerFFTBatch number of pulse repetitions
#
# The output will be written to AESA_DATA.csv and each line will contain
# The I and Q sample of each radar element given as:
# I1, Q1, I2, Q2, I3, Q3...
# The first line will correspond to Range bin 1, pulse 1
# The second line will correspond to Range bin 2, pulse 1
# Total number of lines is NoRangeBinInEveryPulse * NoPulsesPerFFTBatch


import math
import numpy as np
import csv

AESA = dict()

#Radar Specific Configuration
AESA["Fradar"] = 10e9 # 10 Ghz X-band
AESA["Wavelength"] = 3e8 / AESA["Fradar"]
AESA["DistanceBetweenRadarElements"] = AESA["Wavelength"]/2

AESA["NoDataChannels"] = 16
AESA["NoRangeBinInEveryPulse"] = 1024
AESA["NoPulsesPerFFTBatch"] = 256
AESA["DataWidthIn"] = 16
AESA["Fsampling"] = 3e6
AESA["PulseWidth"] = 1e-6

AESA["NoInCubes"] = 2

# Definition of wave fronts angle Theta is given by figure 4
# Enumeration of radar elements if left to right
# That is, a wave front with positive theta hits the radar element with the
# highest index first

# Generate Input Data will all zeros of form AESA["InputData"][Channel][Rangebin][Pulse]
EmptyData = list(list() for i in range(0,AESA["NoDataChannels"]))
for i in range(0,AESA["NoDataChannels"]):
    EmptyData[i] = list(list() for j in range(0,AESA["NoRangeBinInEveryPulse"]))
    for j in range(0,AESA["NoRangeBinInEveryPulse"]):
        EmptyData[i][j] = list(0 for j in range(0,AESA["NoPulsesPerFFTBatch"]))         
AESA["InputData"] = EmptyData




###########################################################
#                                                         #
# Function to Generate Reflection from an object          #
#                                                         #
###########################################################
def GenerateObjectReflection(AESA, Distance, Angle, RelativeSpeed, SignalPower):
    # Distance in meters
    # Relative speed i m/s, positive relative speed means approaching object
    # Angle to object, given as Theta above
    # SignalPower given as relative to the full scale power

    # wd is 2*pi*doppler frequency
    wd = 2 * math.pi * 2 * RelativeSpeed / AESA["Wavelength"]
    print(wd)
    # A is the power of the reflected signal (-5 => 1/32 of fullscale)
    A = math.pow(2,SignalPower)
   
    # Large distances will fold to lower ones, assume infinite sequences
    # Otherwise the the first X pulses would be absent
    trefl_start = math.ceil((2*Distance/3e8)*AESA["Fsampling"]) % AESA["NoRangeBinInEveryPulse"]
    trefl_stop = math.ceil((2*Distance/3e8+AESA["PulseWidth"])*AESA["Fsampling"]) % AESA["NoRangeBinInEveryPulse"]

    print([trefl_start, trefl_stop])

    # Handling for distances at the edge of the 
    crossing_reflection = 0
    if (trefl_stop < trefl_start):
        crossing_reflection = 1
    
    # The initial phase of a full data cube will be random
    phi_start = 2*math.pi*np.random.randint(0,359)/360
    
    for PulseIterator in range(0, AESA["NoPulsesPerFFTBatch"]):
       for RangeBinIterator in range(0, AESA["NoRangeBinInEveryPulse"]):
           for ChannelIterator in range(0, AESA["NoDataChannels"]):
               ChannelDelay = -1*ChannelIterator*math.pi*math.sin(Angle)
               t = (RangeBinIterator + PulseIterator*AESA["NoRangeBinInEveryPulse"]) / AESA["Fsampling"]
               I = A*math.cos(wd*t + phi_start)
               Q = -A*math.sin(wd*t + phi_start)
               value = (I + 1j*Q)*(math.cos(ChannelDelay) + 1j*math.sin(ChannelDelay))
               #print(A, math.sqrt(value.real*value.real + value.imag*value.imag))

                  
             
               if (RangeBinIterator in range(trefl_start, trefl_stop)) and (not crossing_reflection):
                  AESA["InputData"][ChannelIterator][RangeBinIterator][PulseIterator] = AESA["InputData"][ChannelIterator][RangeBinIterator][PulseIterator] + value
               if not (RangeBinIterator in range(trefl_start, trefl_stop)) and (crossing_reflection):
                  AESA["InputData"][ChannelIterator][RangeBinIterator][PulseIterator] = AESA["InputData"][ChannelIterator][RangeBinIterator][PulseIterator] + value




###########################################################
#                                                         #
# Function to Generate Background Noise                   #
#                                                         #
###########################################################
def GenerateNoise(AESA, SignalPower):
    for PulseIterator in range(0, AESA["NoPulsesPerFFTBatch"]):
       for RangeBinIterator in range(0, AESA["NoRangeBinInEveryPulse"]):
           for ChannelIterator in range(0, AESA["NoDataChannels"]):
               valueI = np.random.normal(0, math.pow(2,SignalPower))   
               valueQ = np.random.normal(0, math.pow(2,SignalPower))
               value = valueI + 1j*valueQ
               AESA["InputData"][ChannelIterator][RangeBinIterator][PulseIterator] = AESA["InputData"][ChannelIterator][RangeBinIterator][PulseIterator] + value



###########################################################-
#                                                         #
# Add Objects and Noise to the Data                       #
#                                                         #
###########################################################

#Add object at distance 5.12km, Theta = pi/3.7, speed 52.2 m/s towards radar and full scale level -12dBFS (= 6.02*-2)
#GenerateObjectReflection(AESA, 15.12e3, math.pi/5, 52.2, -4) # GenerateObjectReflection(AESA, 5.12e3, math.pi/3.7, 52.2, -2)

#Add object at distance 199.9m, Theta = 0, speed 0 radar and full scale level -18dBFS (= 6.02*-3)
GenerateObjectReflection(AESA, 12e3, math.pi/3 + 2*(math.pi - 2*math.pi/3)/7, 0.94, -6)
GenerateObjectReflection(AESA, 13e3, math.pi/3 + 2*(math.pi - 2*math.pi/3)/7, 5*0.94, -6)
GenerateObjectReflection(AESA, 14e3, math.pi/3 + 2*(math.pi - 2*math.pi/3)/7, 10*0.94, -6)

GenerateObjectReflection(AESA, 15e3, math.pi/3 + 2*(math.pi - 2*math.pi/3)/7, -0.94, -2)
GenerateObjectReflection(AESA, 16e3, math.pi/3 + 2*(math.pi - 2*math.pi/3)/7, -2*0.94, -2)
GenerateObjectReflection(AESA, 17e3, math.pi/3 + 2*(math.pi - 2*math.pi/3)/7, -3*0.94, -2)

GenerateObjectReflection(AESA, 18e3, math.pi/3 + 2*(math.pi - 2*math.pi/3)/7, -20*0.94, -4)
GenerateObjectReflection(AESA, 19e3, math.pi/3 + 2*(math.pi - 2*math.pi/3)/7, -23*0.94, -4)
GenerateObjectReflection(AESA, 20e3, math.pi/3 + 2*(math.pi - 2*math.pi/3)/7, -26*0.94, -4)
GenerateObjectReflection(AESA, 21e3, math.pi/3 + 2*(math.pi - 2*math.pi/3)/7, -29*0.94, -4)

GenerateObjectReflection(AESA, 25e3, math.pi/3 + 2*(math.pi - 2*math.pi/3)/7, -15*0.94, -2)
GenerateObjectReflection(AESA, 25.4e3, math.pi/3 + 2.1*(math.pi - 2*math.pi/3)/7, -15*0.94, -4)
GenerateObjectReflection(AESA, 25.2e3, math.pi/3 + 2.2*(math.pi - 2*math.pi/3)/7, -15*0.94, -3)

#GenerateObjectReflection(AESA, 18.0e3, math.pi/3 + 0*(math.pi - 2*math.pi/3)/7, -20*0.94, -4)
#GenerateObjectReflection(AESA, 18.2e3, math.pi/3 + 1*(math.pi - 2*math.pi/3)/7, -20*0.94, -4)
#GenerateObjectReflection(AESA, 18.4e3, math.pi/3 + 2*(math.pi - 2*math.pi/3)/7, -20*0.94, -4)
#GenerateObjectReflection(AESA, 18.6e3, math.pi/3 + 3*(math.pi - 2*math.pi/3)/7, -20*0.94, -4)
#GenerateObjectReflection(AESA, 18.8e3, math.pi/3 + 4*(math.pi - 2*math.pi/3)/7, -20*0.94, -4)
#GenerateObjectReflection(AESA, 19.0e3, math.pi/3 + 5*(math.pi - 2*math.pi/3)/7, -20*0.94, -4)
#GenerateObjectReflection(AESA, 19.2e3, math.pi/3 + 6*(math.pi - 2*math.pi/3)/7, -20*0.94, -4)
#GenerateObjectReflection(AESA, 19.4e3, math.pi/3 + 7*(math.pi - 2*math.pi/3)/7, -20*0.94, -4)


#Add background noise with power -56 dBFS  (= 6.02*-7)
GenerateNoise(AESA, -3)


###########################################################
#                                                         #
# Output the data to a CSV file                           #
#                                                         #
###########################################################

#with open('AESA_DATA.csv', mode='w') as data_file:
#    data_writer = csv.writer(data_file, delimiter=',', quotechar='"', quoting=csv.QUOTE_MINIMAL)
#    for PulseIterator in range(0, AESA["NoPulsesPerFFTBatch"]):
#       for RangeBinIterator in range(0, AESA["NoRangeBinInEveryPulse"]):
#           WriteCSVRow = list()
#           for ChannelIterator in range(0, AESA["NoDataChannels"]):
#               # Output I and Q separately, scaled from 0 to (2^AESA["DataWidthIn"]-1) 
##               WriteCSVRow.append(math.floor(AESA["InputData"][ChannelIterator][RangeBinIterator][PulseIterator].real*pow(2,AESA["DataWidthIn"]-1) + pow(2,AESA["DataWidthIn"]-1)))
##               WriteCSVRow.append(math.floor(AESA["InputData"][ChannelIterator][RangeBinIterator][PulseIterator].imag*pow(2,AESA["DataWidthIn"]-1) + pow(2,AESA["DataWidthIn"]-1)))
#
#               WriteCSVRow.append(AESA["InputData"][ChannelIterator][RangeBinIterator][PulseIterator].real)
#               WriteCSVRow.append(AESA["InputData"][ChannelIterator][RangeBinIterator][PulseIterator].imag)
#
#           data_writer.writerow(WriteCSVRow)
           
#with open('AESA_DATA.csv', mode='w') as data_file:
#    
#    WriteArray = list()
#    
#    data_writer = csv.writer(data_file, delimiter=',', quotechar='"', quoting=csv.QUOTE_MINIMAL)
#    for PulseIterator in range(0, AESA["NoPulsesPerFFTBatch"]):
#       for RangeBinIterator in range(0, AESA["NoRangeBinInEveryPulse"]):
#           WriteCSVRow = list()
#           for ChannelIterator in range(0, AESA["NoDataChannels"]):
               # Output I and Q separately, scaled from 0 to (2^AESA["DataWidthIn"]-1) 
#               WriteCSVRow.append(math.floor(AESA["InputData"][ChannelIterator][RangeBinIterator][PulseIterator].real*pow(2,AESA["DataWidthIn"]-1) + pow(2,AESA["DataWidthIn"]-1)))
#               WriteCSVRow.append(math.floor(AESA["InputData"][ChannelIterator][RangeBinIterator][PulseIterator].imag*pow(2,AESA["DataWidthIn"]-1) + pow(2,AESA["DataWidthIn"]-1)))

with open('AESA_DATA_UNSCALED.csv', mode='w') as data_file:
    data_writer = csv.writer(data_file, delimiter=' ', quotechar='"', quoting=csv.QUOTE_MINIMAL)
    for CubeIterator in range(0, AESA["NoInCubes"]):
        for PulseIterator in range(0, AESA["NoPulsesPerFFTBatch"]):
            for RangeBinIterator in range(0, AESA["NoRangeBinInEveryPulse"]):
                WriteCSVRow = list()
                for ChannelIterator in range(0, AESA["NoDataChannels"]):
                    # Output I and Q separately, scaled from 0 to (2^AESA["DataWidthIn"]-1) 
                    WriteCSVRow.append('{:f}'.format(AESA["InputData"][ChannelIterator][RangeBinIterator][PulseIterator].real))
                    WriteCSVRow.append('{:f}'.format(AESA["InputData"][ChannelIterator][RangeBinIterator][PulseIterator].imag))
                data_writer.writerow(WriteCSVRow)
