import numpy as np
import matplotlib.pyplot as plt
import matplotlib.patches as patches
import cartopy.crs as ccrs
import cartopy.feature as cfeature
import sys
import matplotlib.ticker as ticker
from cartopy.mpl.ticker import LongitudeFormatter, LatitudeFormatter,LatitudeLocator,LongitudeLocator
import gdal
from matplotlib.colors import ListedColormap, BoundaryNorm
import namelist
#===============set param===============
isy_list = [1950,1980,2010,2040,2070]
iey_list = [1979,2009,2039,2069,2099]
suffix = '_comp'

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
cmap = ListedColormap(['white',b11,b10,b9,b8,b7,b6,b5,b4,b3,b2])
cmap.set_under(r8) ; cmap.set_over(g8)
#bounds =  np.linspace(0,1.0,11)
bounds = [0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0]
bounds = np.append(-0.001,bounds)
norm = BoundaryNorm(bounds,cmap.N) 
print(bounds)
cbar_num_format = ticker.FuncFormatter(lambda y, _: '{:.16g}'.format(y))

############################################################
map_src = gdal.Open('/lake5/tasaka/Master/Yodo/WR/map/Basin_No_Yodo.tif',gdal.GA_ReadOnly)
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
#############################################################

for t in range(0,5):
  isy = isy_list[t]      
  iey = iey_list[t]      

  data = open('./output/CWD_Yodo3min_dam'+'_'+str(isy)+'-'+str(iey)+'.bin','rb')
  d0 = np.fromfile(data,np.float32)
  d = np.where(d0<0.,np.nan,d0)
  
  d0 = d0.reshape(my,mx)
  d0 = np.flipud(d0)
  dd = d.reshape(my,mx)
  dd = np.flipud(dd)    #dam
  
  data = open('./output/CWD_Yodo3min_natural'+'_'+str(isy)+'-'+str(iey)+'.bin','rb')
  d = np.fromfile(data,np.float32)
  d = np.where(d<0.,np.nan,d)
  
  dn = d.reshape(my,mx)
  dn = np.flipud(dn)    #no dam
  
  d_dif = np.zeros((my,mx))
  d_dif[:,:] = np.nan
  # dd : with dam , dn : no dam
  for y in range(0,my):
   for x in range(0,mx):
    if dn[y,x] == 1.e0:
     #d_dif[y,x] = 100.e0
     d_dif[y,x] = np.nan
    elif dd[y,x] > dn[y,x]:
     d_dif[y,x] = (dd[y,x]-dn[y,x]) / (1.e0-dn[y,x])
     if d_dif[y,x] < 1.e-3:
       d_dif[y,x] = np.nan
    elif dn[y,x] > dd[y,x] :
     d_dif[y,x] = dd[y,x] - dn[y,x]
    else:
     d_dif[y,x] = np.nan 
  
  fig = plt.figure(figsize=(12,9), dpi=300)
  ax = plt.axes(projection=ccrs.PlateCarree())
  ax.set_extent([xmin,xmax,ymin,ymax])
  plt.title('CWD Infra Effect'+' '+str(isy)+'-'+str(iey), fontsize=25)
  ax.coastlines(resolution='10m')
  plt.imshow(d_dif[:,:], extent=[xmin,xmax,ymin,ymax], interpolation='nearest', transform=ccrs.PlateCarree(), cmap=cmap, norm=norm)
  plt.colorbar(extend='both' ,shrink=1.0, ticks=bounds, orientation='vertical')
  plt.contour(x2, y2, map_array, levels=[1.e7-1], colors=['gray'], transform=ccrs.PlateCarree(), linewidth=3)
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
  plt.savefig('./CWD'+suffix+'_'+str(isy)+'-'+str(iey)+'.png')
  #plt.savefig('./CWD.png',bbox_inches='tight' )
  plt.cla()
  plt.clf()
  plt.close()
  
  continue
  
  for year in range(isy,iey+1):
    print( year )       
    data = open('./output/CWD_Yodo3min_dam'+'_'+str(year)+'.bin','rb')
    d0 = np.fromfile(data,np.float32)
    d = np.where(d0<0.,np.nan,d0)
  
    d0 = d0.reshape(my,mx)
    d0 = np.flipud(d0)
    dd = d.reshape(my,mx)
    dd = np.flipud(dd)    #dam
  
    data = open('./output/CWD_Yodo3min_nodam'+'_'+str(year)+'.bin','rb')
    d = np.fromfile(data,np.float32)
    d = np.where(d<0.,np.nan,d)
  
    dn = d.reshape(my,mx)
    dn = np.flipud(dn)    #no dam
  
    d_dif = np.zeros((my,mx))
    d_dif[:,:] = np.nan
  # dd : with dam , dn : no dam
    for y in range(0,my):
     for x in range(0,mx):
      if dn[y,x] == 1.e0:
       #d_dif[y,x] = 100.e0
       d_dif[y,x] = np.nan
      elif dd[y,x] > dn[y,x]:
       d_dif[y,x] = (dd[y,x]-dn[y,x]) / (1.e0-dn[y,x])
       if d_dif[y,x] < 1.e-3:
         d_dif[y,x] = np.nan
      elif dn[y,x] > dd[y,x] :
       d_dif[y,x] = dd[y,x] - dn[y,x]
      else:
       d_dif[y,x] = np.nan 
  
    fig = plt.figure(figsize=(12,9), dpi=300)
    ax = plt.axes(projection=ccrs.PlateCarree())
    ax.set_extent([xmin,xmax,ymin,ymax])
    plt.title('CWD Dam Effect'+str(year), fontsize=30)
    ax.coastlines(resolution='10m')
    plt.imshow(d_dif[:,:], extent=[xmin,xmax,ymin,ymax], interpolation='nearest', transform=ccrs.PlateCarree(), cmap=cmap, norm=norm)
    plt.colorbar(extend='max' ,shrink=1.0, ticks=bounds, orientation='vertical')
    plt.contour(x2, y2, map_array, levels=[1.e7-1], colors=['gray'], transform=ccrs.PlateCarree(), linewidth=3)
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
    plt.savefig('./fig/CWD'+suffix+'_'+str(year)+'.png')
    plt.cla()
    plt.clf()
    plt.close()