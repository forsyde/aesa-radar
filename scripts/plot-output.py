#!/bin/python3

import matplotlib.pyplot as plt
import os
from matplotlib import colors
import numpy as np
import seaborn as sns
import argparse

parser = argparse.ArgumentParser(description='Plots the AESA signal processing data.')
parser.add_argument('-r', '--radar', nargs=1, type=str,  metavar='PATH',
                    help='Plots the radar output. Expects path to AESA signal processing output')
parser.add_argument('-a', '--antenna', nargs=1, type=str,  metavar='PATH',
                    help='Plots the AESA antenna input. Expects path to antenna data')

args = parser.parse_args()

###############
## INPUT PLOT
###############

if args.antenna:
   frow=250
   lrow=375
   data = np.genfromtxt(args.antenna[0], delimiter=" ")[:lrow]

   reals=np.transpose([i[0::2] for i in data])
   imags=np.transpose([i[1::2] for i in data])
   pdata=[reals,imags]

   fig, axs = plt.subplots(2, 1, figsize=(6,4))
   images = []
   for i in range(2):
      # Generate data with a range that varies from one plot to the next.
      images.append(axs[i].imshow(pdata[i], cmap="Blues", aspect='equal', interpolation="nearest"))
      axs[i].grid(None)
      axs[i].label_outer()
      axs[i].set_xlim(frow,lrow)

   axs[0].set_ylabel("real (I)")
   axs[1].set_ylabel("imag (Q)")   
   axs[1].set_xlabel("samples")

   # Find the min and max of all colors for use in setting the color scale.
   vmin = min(image.get_array().min() for image in images)
   vmax = max(image.get_array().max() for image in images)
   norm = colors.Normalize(vmin=vmin, vmax=vmax)
   for im in images:
      im.set_norm(norm)
         
   fig.colorbar(images[0], ax=axs[0], orientation='horizontal', fraction=.1)
   filename=os.path.splitext(os.path.basename(args.antenna[0]))[0] + '.pdf'
   plt.savefig(filename, bbox_inches='tight')
   plt.show()

###############
## OUTPUT PLOT
###############

if args.radar:
   nbeams=8
   nbins=1024
   # with open(args.radar[0]) as fp:  
   #    line = fp.readline()
   #    rangeBins = []
   #    beams = []
   #    while line:
   #       line = fp.readline()
   #       if (len(line)>1):
   #          window=[float(s) for s in line.strip().split(' ')]
   #          rangeBins.append(window)
   #       else:
   #          if (rangeBins):
   #             beam.append(rangeBins)
   #          rangeBins=[]
   #    print(len(beams))

   data = np.genfromtxt(args.radar[0], delimiter=" ")
   beams=[]
   for i in range(nbeams):
      beams.append(data[i*nbins:(i+1)*nbins-1])

   print(beams[0][254:259])
      
   vmin= min([min(rbin) for rbin in data])
   vmax= max([max(rbin) for rbin in data])
   # vmin= min([min([min (row) for row in beam]) for beam in beams])
   # vmax= max([max([max (row) for row in beam]) for beam in beams])
   
   fig, axs = plt.subplots(1, nbeams, figsize=(8*nbeams/2,10))
   images = []
   for i in range(nbeams):
      images.append(axs[i].imshow(beams[i], vmin=vmin, vmax=vmax, cmap="Blues", aspect='equal', interpolation="nearest"))
      axs[i].grid(None)
      axs[i].label_outer()

   fig.colorbar(images[nbeams-1], ax=axs[nbeams-1], orientation='vertical', fraction=.1)
      
   # fig, (ax1,ax2,ax3,ax4,ax5,ax6,ax7,ax8,axcb) = plt.subplots(1,9, figsize=(40,10), gridspec_kw={'width_ratios':[1,1,1,1,1,1,1,1,0.08]})
   # ax1.get_shared_y_axes().join(ax2,ax3,ax4,ax5,ax6,ax7,ax8)
   # axes = [ax1,ax2,ax3,ax4,ax5,ax6,ax7,ax8,axcb]
   # heatms = []
   # for i in list(range(7)):
   #    x = beams[i+offs]
   #    y=np.array([np.array(xi) for xi in x])
   #    g = sns.heatmap(y, vmin=0, vmax=maxv, square=True, ax=axes[i],cbar=False)
   #    heatms.append(g)
   #    g.set_title('beam ' + str(i))

   # y=np.array([np.array(xi) for xi in beams[7+offs]])
   # g = sns.heatmap(y, vmin=0, vmax=maxv, square=True, ax=ax8, cbar_ax=axcb)
   # heatms.append(g)
   # g.set_title('beam 8')

   # for i in list(range(7)):  
   #    heatms[i+1].set_ylabel('')
   #    heatms[i+1].set_xlabel('')
   #    heatms[i+1].set_yticks([])
   
   plt.yticks(rotation=0,fontsize=10);
   plt.xticks(fontsize=12);
   plt.tight_layout()

   filename=os.path.splitext(os.path.basename(args.radar[0]))[0] + '.pdf'
   plt.savefig(filename)
   plt.show()
