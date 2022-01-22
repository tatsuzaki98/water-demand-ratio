from PIL import Image
#https://note.nkmk.me/python-pillow-gif/

isy = 2000 ; iey = 2012
suffix = '_comp'

files=[]

for year in range(isy,iey+1):
  file=Image.open('./fig/CWD'+suffix+'_'+str(year)+'.png')
  files.append(file)

files[0].save('./CWD'+suffix+'.gif',save_all=True, append_images=files[1:], loop=0, duration=500,optimize=False)
#files は要するにアルバム。アルバムの1枚目 [0] に対し、append_images で2枚目以降を追加していく
#optimize : 似たような画像があったらスキップするか。基本的に False
#loop : 何回繰り返すか。 0 にすると無限に続く
#duration : 各画像の時間。単位はミリ秒