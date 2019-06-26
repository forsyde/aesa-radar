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
      if self.y - self.prevy < 20:
         self.prevoffset=self.prevoffset+20
      else:
         self.prevoffset=0
      self.prevy=self.y
      xoffset = 50 if self.x<128 else (-95)
      yoffset=-5+self.prevoffset
      ax.annotate("{:.1f}".format(self.vmax), xy=(self.x, self.y),
                  xytext=(self.x+xoffset,self.y+yoffset), fontsize='8',
                  arrowprops=dict(facecolor='red',arrowstyle='->'))
      
   def update(self, vmax, x, y):
      self.vmax=vmax
      self.x=x
      self.y=y 
      
parser = argparse.ArgumentParser(description='Plots the AESA signal processing data.')
parser.add_argument('inpath', nargs=1, type=str,  metavar='PATH',
                    help='Plots the AESA antenna input. Expects path to antenna data')
parser.add_argument('-t', '--threshold', nargs='?', type=int,  metavar='VAL', default=15,
                    help='Threshold value for detected objects. If 0 then detection values are not annotated. Default: 15')

args = parser.parse_args()
     
nbeams=8
nbins=1024
   
data = np.genfromtxt(args.inpath[0], delimiter=" ")
beams=[]
for i in range(nbeams):
   beams.append(data[i*nbins:(i+1)*nbins-1])
   
fig, axs = plt.subplots(1, nbeams, figsize=(7*nbeams/4,7), sharey=True)
images = []
for i in range(nbeams):
   images.append(axs[i].imshow(beams[i], cmap='PuBuGn', aspect='equal', interpolation="nearest"))
   axs[i].grid(False)
   axs[i].label_outer()
   axs[i].set_title('beam ' + str(i))

      
vmin = min(image.get_array().min() for image in images)
vmax = max(image.get_array().max() for image in images)
norm = colors.Normalize(vmin=vmin, vmax=vmax)
for im in images:
    im.set_norm(norm)

if args.threshold:
   for i in range(nbeams):
      scanner=ObjectScanner()
      for y in range(len(beams[i])):
         for x in range(len(beams[i][y])):
            if scanner.scope:
               if x>scanner.x+2 and y>scanner.y+2 and scanner.vmax>=beams[i][y][x]:
                  scanner.annotate(axs[i])
                  scanner.scope=False
               if scanner.vmax<beams[i][y][x]:
                  scanner.update(beams[i][y][x],x,y)
            else:
               if beams[i][y][x] > args.threshold:
                  scanner.update(beams[i][y][x],x,y)
                  scanner.scope=True


fig.colorbar(images[nbeams-1], ax=axs[nbeams-1], orientation='vertical', fraction=.1)

plt.yticks(rotation=0,fontsize=10);
plt.xticks(fontsize=12);
plt.tight_layout()

filename=os.path.splitext(os.path.basename(args.inpath[0]))[0] + '.pdf'
plt.savefig(filename)
plt.show()
