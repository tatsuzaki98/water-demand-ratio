#define colors Blue-Red
#http://macroscope.world.coocan.jp/ja/edu/computer/grads/rgbcolor.html
#https://hackmd.io/@h2tg95D2RP2ed-D8u-49Mg/S1moBqaRr

from matplotlib.colors import ListedColormap, BoundaryNorm
import numpy as np


#========== copy define_colors() or use def 'make_cmap(number of splits)' ==========
#                                                   splits should be odd number 5 ~ 23

def define_colors():
 def make_col(RGB255):
  color=np.array(RGB255)
  color=color/255.e0
  RGB1=color.tolist()
  return RGB1
 
 #Red
 r1=make_col([42,17,5])          #dark red
 r2=make_col([76,23,9])
 r3=make_col([112,27,11])
 r4=make_col([151,28,11])
 r5=make_col([191,25,8])
 r6=make_col([233,14,3])
 r7=make_col([255,59,29])
 r8=make_col([255,110,74])
 r9=make_col([255,150,117])
 r10=make_col([255,186,161])
 r11=make_col([255,212,207])     #pale red
 
 #Blue
 b1=make_col([27,15,67])         #dark blue
 b2=make_col([35,16,136])
 b3=make_col([27,10,211])
 b4=make_col([55,26,255])
 b5=make_col([100,60,255])
 b6=make_col([132,89,255])
 b7=make_col([157,116,255])
 b8=make_col([180,143,255])
 b9=make_col([201,171,255])
 b10=make_col([220,198,255])
 b11=make_col([238,227,255])     #pale blue
 
 #Green
 g1=make_col([16,27,11])         #dark green
 g2=make_col([23,47,18])
 g3=make_col([28,68,21])
 g4=make_col([32,90,24])
 g5=make_col([35,113,25])
 g6=make_col([36,137,26])
 g7=make_col([36,162,25])
 g8=make_col([34,187,23])
 g9=make_col([28,214,19])
 g10=make_col([15,240,9])
 g11=make_col([139,255,116])     #pale green
 
 #print(type(r1),r1)
 
 #make color map
 cmap = ListedColormap([b3,b4,b5,b6,b7,b8,b9,r9,r8,r7,r6,r5,r4,r3]) #sample , 14colors , 15splits
 ##cmap = ListedColormap([b4,b5,b6,b7,b8,b9,r9,r8,r7,r6,r5,r4]) #sample , 14colors , 13splits
 ##cmap.set_under(b3) ; cmap.set_over(r3)
 #bounds = np.linspace(0,100,15)      #Essentially when plot
 #norm = BoundaryNorm(bounds,cmap.N)  #Essentially when plot

 return r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11,\
        b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11

#plot
#sc = plt.scatter(x,y,cmap=cmap,norm=norm,vmin=min(bounds),vmax=max(bounds) )
#cbar = plt.colorbar(sc,aspect=30,shrink=0.7,extend='both')
#==============================================================

def make_cmap1(s) :
 r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11,b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11 = define_colors()
 cmap = ListedColormap([b4,b5,b6,b7,b8,b9,r9,r8,r7,r6,r5,r4]) #sample , 14colors , 13splits
 cmap.set_under(b3) ; cmap.set_over(r3)
 if s == 5 :
  cmap = ListedColormap([b8,b11,r11,r8])
  cmap.set_under(b5) ; cmap.set_over(r5)
 elif s == 7 :
  cmap = ListedColormap([b5,b8,b11,r11,r8,r5])
  cmap.set_under(b2) ; cmap.set_over(r2)
 elif s == 9 :
  cmap = ListedColormap([b5,b7,b9,b11,r11,r9,r7,r5])
  cmap.set_under(b3) ; cmap.set_over(r3)
 elif s == 11 :
  cmap = ListedColormap([b3,b5,b7,b9,b11,r11,r9,r7,r5,r3])
  cmap.set_under(b1) ; cmap.set_over(r1)
 elif s == 13 :
  cmap = ListedColormap([b5,b6,b7,b8,b9,b10,r10,r9,r8,r7,r6,r5])
  cmap.set_under(b4) ; cmap.set_over(r4)
 elif s == 15 :
  cmap = ListedColormap([b4,b5,b6,b7,b8,b9,b10,r10,r9,r8,r7,r6,r5,r4])
  cmap.set_under(b3) ; cmap.set_over(r3)
 elif s == 17 :
  cmap = ListedColormap([b3,b4,b5,b6,b7,b8,b9,b10,r10,r9,r8,r7,r6,r5,r4,r3])
  cmap.set_under(b2) ; cmap.set_over(r2)
 elif s == 19 :
  cmap = ListedColormap([b3,b4,b5,b6,b7,b8,b9,b10,b11,r11,r10,r9,r8,r7,r6,r5,r4,r3])
  cmap.set_under(b2) ; cmap.set_over(r2)
 elif s == 21 :
  cmap = ListedColormap([b2,b3,b4,b5,b6,b7,b8,b9,b10,b11,r11,r10,r9,r8,r7,r6,r5,r4,r3,r2])
  cmap.set_under(b1) ; cmap.set_over(r1)

 return cmap

def make_cmap2(s) :
 r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11,b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11 = define_colors()
 cmap = ListedColormap([b3,b4,b5,b6,b7,b8,b9,r9,r8,r7,r6,r5,r4,r3]) #sample , 14colors , 15splits
 if s == 5 :
  cmap = ListedColormap([b6,b11,r11,r6])
 elif s == 7 :
  cmap = ListedColormap([b5,b8,b11,r11,r8,r5])
 elif s == 9 :
  cmap = ListedColormap([b2,b5,b8,b11,r11,r8,r5,r2])
 elif s == 11 :
  cmap = ListedColormap([b3,b5,b7,b9,b11,r11,r9,r7,r5,r3])
 elif s == 13 :
  cmap = ListedColormap([b1,b3,b5,b7,b9,b11,r11,r9,r7,r5,r3,r1])
 elif s == 15 :
  cmap = ListedColormap([b4,b5,b6,b7,b8,b9,b10,r10,r9,r8,r7,r6,r5,r4])
 elif s == 17 :
  cmap = ListedColormap([b3,b4,b5,b6,b7,b8,b9,b10,r10,r9,r8,r7,r6,r5,r4,r3])
 elif s == 19 :
  cmap = ListedColormap([b2,b3,b4,b5,b6,b7,b8,b9,b10,r10,r9,r8,r7,r6,r5,r4,r3,r2])
 elif s == 21 :
  cmap = ListedColormap([b2,b3,b4,b5,b6,b7,b8,b9,b10,b11,r11,r10,r9,r8,r7,r6,r5,r4,r3,r2])
 elif s == 23 :
  cmap = ListedColormap([b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11,r11,r10,r9,r8,r7,r6,r5,r4,r3,r2,r1])

 return cmap