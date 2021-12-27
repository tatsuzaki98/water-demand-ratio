from PIL import Image
#https://note.nkmk.me/python-pillow-gif/

isy_list = [1950,1980,2010,2040,2070]
iey_list = [1979,2009,2039,2069,2099]
suffix = '_comp'

files=[]

for t in range(0,5):
  isy = isy_list[t]      
  iey = iey_list[t]      
  file=Image.open('./CWD'+suffix+'_'+str(isy)+'-'+str(iey)+'.png')
  files.append(file)

files[0].save('./CWD'+suffix+'_term.gif',save_all=True, append_images=files[1:], loop=0, duration=800,optimize=False)
#files は要するにアルバム。アルバムの1枚目 [0] に対し、append_images で2枚目以降を追加していく
#optimize : 似たような画像があったらスキップするか。基本的に False
#loop : 何回繰り返すか。 0 にすると無限に続く
#duration : 各画像の時間。単位はミリ秒