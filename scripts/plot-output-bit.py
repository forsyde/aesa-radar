#!/bin/python3

import matplotlib.pyplot as plt
import os
from matplotlib import colors
import numpy as np
import argparse

class ObjectScanner:
   def __init__(self, scope=False, vmax=0, x=0, y=0):
      self.scope=scope
      self.vmax=vmax
      self.x=x
      self.y=y
      self.prevy=0
      self.prevoffset=0

   def annotate(self,ax):
      xoffset = 25 
      yoffset = -10
      ax.annotate("{:.1f}".format(self.vmax), xy=(self.x, self.y),
                  xytext=(self.x+xoffset,self.y+yoffset), fontsize='12',
                  arrowprops=dict(facecolor='red',arrowstyle='->'))
      
   def update(self, vmax, x, y):
      self.vmax=vmax
      self.x=x
      self.y=y 
      
parser = argparse.ArgumentParser(description='Plots the AESA signal processing data.')
parser.add_argument('inpath', nargs=1, type=str,  metavar='PATH',
                    help='Plots the AESA antenna input. Expects path to antenna data')
parser.add_argument('-t', '--threshold', nargs='?', type=float,  metavar='VAL', default=15,
                    help='Threshold value for detected objects. If 0 then detection values are not annotated. Default: 15')
parser.add_argument('-b', '--beam', nargs='?', type=int,  metavar='VAL', default=2,
                    help='Beam which needs to be plotted. Default: 2')
parser.add_argument('-f', '--first', nargs='?', type=int,  metavar='VAL', default=0,
                    help='Beam which needs to be plotted. Default: 0')
parser.add_argument('-l', '--last', nargs='?', type=int,  metavar='VAL', default=1023,
                    help='Beam which needs to be plotted. Default: 1023')

args = parser.parse_args()
     
nbeams=8
nbins=1024
   
data = np.genfromtxt(args.inpath[0], delimiter=" ")
beam=data[args.beam*nbins:(1+args.beam)*nbins]

# print (beam)

fig, axs = plt.subplots(1, 1, figsize=(3.5,2.5))
images = []
# for i in range(nbeams):
images.append(axs.imshow(beam, cmap='PuBuGn', aspect='equal', interpolation="nearest"))
axs.grid(False)
axs.label_outer()
# axs.set_title('beam ' + str(i))
axs.set_ylabel("Rage (bin)")   
axs.set_xlabel("Relative Speed (Doppler channel)")
axs.tick_params(which='major', labelsize=10);
axs.set_ylim(args.last,args.first)

      
vmin = min(image.get_array().min() for image in images)
vmax = max(image.get_array().max() for image in images)
norm = colors.Normalize(vmin=vmin, vmax=vmax)
for im in images:
    im.set_norm(norm) 

if args.threshold:
    scanner=ObjectScanner()
    for y in range(len(beam)):
        for x in range(len(beam[y])):
            if scanner.scope:
               if x>scanner.x+2 and y>scanner.y+2 and scanner.vmax>=beam[y][x]:
                  scanner.annotate(axs)
                  scanner.scope=False
               if scanner.vmax<beam[y][x]:
                  scanner.update(beam[y][x],x,y)
            else:
               if beam[y][x] > args.threshold:
                  scanner.update(beam[y][x],x,y)
                  scanner.scope=True

# fig.colorbar(images[nbeams-1], ax=axs[nbeams], orientation='vertical', fraction=.1)

# fig.colorbar(images[nbeams-1], ax=axs.ravel().tolist(), shrink=0.7, fraction=.1)

plt.yticks(rotation=0,fontsize=10);
plt.xticks(fontsize=10);
plt.tight_layout()

filename=os.path.splitext(os.path.basename(args.inpath[0]))[0] + '_s.pdf'
plt.savefig(filename)
plt.show()
