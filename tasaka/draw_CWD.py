import numpy as np
import matplotlib.pyplot as plt
import matplotlib.patches as patches
import cartopy.crs as ccrs
import cartopy.feature as cfeature
import sys
from cartopy.mpl.ticker import LongitudeFormatter, LatitudeFormatter,LatitudeLocator,LongitudeLocator
import gdal
import color_BR
from matplotlib.colors import ListedColormap, BoundaryNorm
import namelist
#===============set param===============
isy = 2000 ; iey = 2012
ctitle = '(With Dam)'
suffix = '_dam'

mx = namelist.mx
my = namelist.my
resl = namelist.resl
xmin = namelist.xmin ; xmax = namelist.xmax
ymin = namelist.ymin ; ymax = namelist.ymax
cname = namelist.cname ; cresl = namelist.cresl
damx=[21,31,33,31,34,30,27,29]
damy=[13,20,23,24,23,21,18,17]

resolution:int=2 #0:rough 1:middle 2:hi
undef=np.nan
lakes_10m = cfeature.NaturalEarthFeature('physical', 'lakes', '10m',edgecolor='black',facecolor='none')
states_10m  = cfeature.NaturalEarthFeature('cultural', 'admin_1_states_provinces_lines', '10m',edgecolor='black',facecolor='none')
xloc = np.arange(135.5,136.5,0.5) 
yloc = np.arange(34.5,36.0,0.5) 
#=======================================
data = open('./output/CWD_Yodo3min'+suffix+'_'+str(isy)+'-'+str(iey)+'.bin','rb')
d0 = np.fromfile(data,np.float32)
d = np.where(d0<0.,np.nan,d0)
d = d.reshape(my,mx)
d = np.flipud(d)

d0 = d0.reshape(my,mx)
d0 = np.flipud(d0)
d0 = np.where( d0 == 1.e0 , 2. , d0 )


def make_col(RGB255):
  color=np.array(RGB255)
  color=color/255.e0
  RGB1=color.tolist()
  return RGB1
g5=make_col([35,113,25])
g8=make_col([34,187,23])

cmap = color_BR.make_cmap2(11)
cmap = cmap.reversed()
cmap.set_over(g8)
bounds = np.arange(0,1.1,0.1)
norm = BoundaryNorm(bounds,cmap.N)  #Essentially when plot

############################################################
map_src = gdal.Open('./map/Basin_No_Yodo.tif',gdal.GA_ReadOnly)
map_data = map_src.GetRasterBand(1)
map_array0 = map_data.ReadAsArray()
map_array = np.array(map_array0)
map_array = np.where(map_array<-8,np.nan,map_array)
map_array = np.where(map_array==860604.,1.e7,map_array)

xmin2:float=135.00 ; xmax2:float=137.00
ymin2:float=34.00  ; ymax2:float=36.00
mx2:int=18000 ; my2:int=18000
reslx2 = (xmax2-xmin2) / float(mx2)
resly2 = (ymax2-ymin2) / float(my2)
x2 = np.arange(xmin2+reslx2/2.e0,xmax2,reslx2)
y2 = np.arange(ymax2-resly2/2.e0,ymin2,-resly2)
print(mx2,len(x2))
print(my2,len(y2))
#############################################################

fig = plt.figure(figsize=(12,9), dpi=300)
ax = plt.axes(projection=ccrs.PlateCarree())
ax.set_extent([xmin,xmax,ymin,ymax])
plt.title('CWD'+ctitle, fontsize=30)
ax.coastlines(resolution='10m')
plt.imshow(d[:,:], extent=[xmin,xmax,ymin,ymax], interpolation='nearest', transform=ccrs.PlateCarree(), cmap=cmap, norm=norm)
plt.colorbar(extend='max' ,shrink=1.0, ticks=bounds, orientation='vertical')
plt.contour(x2, y2, map_array, levels=[1.e7-1], colors=['white'], transform=ccrs.PlateCarree(), linewidth=3)
#----------------------------------make grid lines---------------------------------------
ax.coastlines(resolution='10m',linewidth=3)
gl = ax.gridlines(xlocs=xloc, ylocs=yloc, crs=ccrs.PlateCarree(), draw_labels=True, linewidth=1, color='gray', alpha=0.5, linestyle='--')
gl.top_labels = False
gl.bottom_labels = True
gl.right_labels = False
gl.left_labels = True
gl.xlines = True
gl.ylines = True
gl.xlabel_style = {'size':10,'color':'black'}
gl.ylabel_style = {'size':10,'color':'red'}
gl.xlocator = LongitudeLocator()
gl.ylocator = LatitudeLocator()
gl.xformatter = LongitudeFormatter()
gl.yformatter = LatitudeFormatter()
ax.add_feature(lakes_10m, linewidth=1.5)
ax.add_feature(states_10m, linewidth=2, linestyle='dashdot')
#----------------------------------------------------------------------------------------
for y in range(0,my):
  for x in range(0,mx):
    if d0[y,x] > -6000. and d0[y,x] < -5000. :     
      lat=ymax-float(y)*resl-resl*0.5
      lon=xmin+float(x)*resl+resl*0.5
      plt.scatter(lon, lat, s=200, transform=ccrs.PlateCarree(), color='limegreen', marker='*',zorder=2) # No water demand
      bblank = True
if bblank :
  plt.figtext(0.7, 0.9, r'$\star$ No water demand', fontsize='15', ha='center', va='center', color='limegreen')
#----- urban water line options Yodo(23,17)  
lat = ymax - float(35-17+1-1)*resl - resl*0.5
lon = xmin + float(23-1)*resl + resl*0.5
plt.scatter(lon, lat, s=150, transform=ccrs.PlateCarree(), color='yellow', marker='<',zorder=2) # Yodo point

for k in range(0,len(damx)):
 lat=ymax-float(damy[k]-1)*resl-resl*0.5 #上端だから-
 lon=xmin+float(damx[k]-1)*resl+resl*0.5 #左端だから+
 plt.scatter(lon,lat,s=100,transform=ccrs.PlateCarree(),color='orange')#dam
 plt.figtext(0.7,0.925,r'$\bullet$ Dam Location',fontsize='15',ha='center',va='center',color='orange')
plt.figtext(0.7,0.925,r'$\bullet$ Dam Location',fontsize='15',ha='center',va='center',color='orange')

plt.savefig('./CWD'+suffix+'.png')
#plt.savefig('./CWD.png',bbox_inches='tight' )
plt.cla()
plt.clf()
plt.close()

#sys.exit()

for year in range(isy,iey+1):
  print( year )       
  data = open('./output/CWD_Yodo3min'+suffix+'_'+str(year)+'.bin','rb')
  d0 = np.fromfile(data,np.float32)
  d = np.where(d0<0.,np.nan,d0)
  d = d.reshape(my,mx)
  d = np.flipud(d)
  d0 = d0.reshape(my,mx)
  d0 = np.flipud(d0)
  d0 = np.where( d0 == 1.e0 , 2. , d0 )

  fig = plt.figure(figsize=(12,9), dpi=300)
  ax = plt.axes(projection=ccrs.PlateCarree())
  ax.set_extent([xmin,xmax,ymin,ymax])
  plt.title('CWD'+ctitle+str(year), fontsize=30)
  ax.coastlines(resolution='10m')
  plt.imshow(d[:,:], extent=[xmin,xmax,ymin,ymax], interpolation='nearest', transform=ccrs.PlateCarree(), cmap=cmap, norm=norm)
  plt.colorbar(extend='max' ,shrink=1.0, ticks=bounds, orientation='vertical')
#  plt.contour(x2, y2, map_array, levels=[1.e7-1], colors=['white'], transform=ccrs.PlateCarree(), linewidth=3)
  #----------------------------------make grid lines---------------------------------------
  ax.coastlines(resolution='10m',linewidth=3)
  gl = ax.gridlines(xlocs=xloc, ylocs=yloc, crs=ccrs.PlateCarree(), draw_labels=True, linewidth=1, color='gray', alpha=0.5, linestyle='--')
  gl.top_labels = False
  gl.bottom_labels = True
  gl.right_labels = False
  gl.left_labels = True
  gl.xlines = True
  gl.ylines = True
  gl.xlabel_style = {'size':10,'color':'black'}
  gl.ylabel_style = {'size':10,'color':'red'}
  gl.xlocator = LongitudeLocator()
  gl.ylocator = LatitudeLocator()
  gl.xformatter = LongitudeFormatter()
  gl.yformatter = LatitudeFormatter()
  ax.add_feature(lakes_10m, linewidth=1.5)
  ax.add_feature(states_10m, linewidth=2, linestyle='dashdot')
  #----------------------------------------------------------------------------------------
  for y in range(0,my):
    for x in range(0,mx):
      if d0[y,x] > -6000. and d0[y,x] < -5000. :     
        lat=ymax-float(y)*resl-resl*0.5
        lon=xmin+float(x)*resl+resl*0.5
        plt.scatter(lon, lat, s=200, transform=ccrs.PlateCarree(), color='limegreen', marker='*',zorder=2) # No water demand
        bblank = True
  if bblank :
    plt.figtext(0.7, 0.9, r'$\star$ No water demand', fontsize='15', ha='center', va='center', color='limegreen')
  #----- urban water line options Yodo(23,17)  
  lat = ymax - float(35-17+1-1)*resl - resl*0.5
  lon = xmin + float(23-1)*resl + resl*0.5
  plt.scatter(lon, lat, s=150, transform=ccrs.PlateCarree(), color='yellow', marker='<',zorder=2) # Yodo point
  plt.savefig('./fig/CWD'+suffix+'_'+str(year)+'.png')
  plt.cla()
  plt.clf()
  plt.close()