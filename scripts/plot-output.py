#!/bin/python3

import matplotlib.pyplot as plt
import os
from matplotlib import colors
import numpy as np
import argparse

# class ObjectScanner:
#    def __init__(self, threshold, scope=False, vmax=0, x=0, y=0):
#       self.scope=scope
#       self.vmax=vmax
#       self.x=x
#       self.y=y
#       self.threshold=threshold

#    def detect(self,x,y,val):
#       if self.scope:
#          if x>self.x+2 and y>self.y+2 and self.vmax >= val:
#             self.scope=False
#          return True
#          if currmax<val:
#             self.vmax=val
#             self.x=x
#             self.y=y
#       else:
#          if val > self.threshold:
#             self.scope=True
#             self.x=x
#             self.y=y
#             self.vmax=val
#       return False

#    def annotate(self,ax,x,y):
#       yoffset=0
#       if y >507 and y< 515 :
#          yoffset = +20
#       if y > 501 and y < 505:
#          yoffset= -20
#          ax.annotate("{:.1f}".format(self.vmax), xy=(self.x, self.y),
#                      xytext=(self.x-70,self.y+yoffset), fontsize='10',
#                      arrowprops=dict(facecolor='red', arrowstyle='-'))
      
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
   
fig, axs = plt.subplots(1, nbeams, figsize=(8*nbeams/4,8), sharey=True)
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

fig.colorbar(images[nbeams-1], ax=axs[nbeams-1], orientation='vertical', fraction=.1)

if args.threshold:
   for i in range(nbeams):
       scope=False
       currmax=0
       currx=0
       curry=0
       for y in range(len(beams[i])):
          for x in range(len(beams[i][y])):
             if scope:
               if x>currx+2 and y>curry+2 and currmax>=beams[i][y][x]:
                  yoffset=0
                  if y >507 and y< 515 :
                     yoffset = +20
                  if y > 501 and y < 505:
                     yoffset= -20
                  axs[i].annotate("{:.1f}".format(currmax), xy=(currx, curry), xytext=(currx-70,curry+yoffset),
                                  fontsize='10' , arrowprops=dict(facecolor='red', arrowstyle='-')
                     )
                  scope=False
               if currmax<beams[i][y][x]:
                  currmax=beams[i][y][x]
                  currx=x
                  curry=y
             else:
                if beams[i][y][x] > args.threshold:
                   scope=True
                   currx=x
                   curry=y
                   currmax=beams[i][y][x]

plt.yticks(rotation=0,fontsize=10);
plt.xticks(fontsize=12);
plt.tight_layout()

filename=os.path.splitext(os.path.basename(args.inpath[0]))[0] + '.pdf'
plt.savefig(filename)
plt.show()
