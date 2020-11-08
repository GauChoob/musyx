NUMBER_OF_MACROS = None
NUMBER_OF_SONGS = None
assert NUMBER_OF_MACROS is not None
assert NUMBER_OF_SONGS is not None

ID_MACRO = 0
ID_ADSR = 1
ID_SAMPLE = 2
ID_SONG = 3
ID_SFX = 4
def transform_id(id,type):
    # The offsets can be modified, but 0 is an invalid Input id so it has to be at least +1
    if(type == ID_MACRO):
        id = NUMBER_OF_MACROS-id
    if(type == ID_ADSR):
        id += 1
    if(type == ID_SAMPLE):
        id += 1
    if(type == ID_SONG):
        id = NUMBER_OF_SONGS-id
    if(type == ID_SFX):
        id += 1
    return id
    
    

def littledata_to_word(file,position):
    return file[position]+file[position+1]*256
    
def bigdata_to_word(file,position):
    return file[position]*256+file[position+1]
    
def twobytearray(value):
    assert abs(value) < 256**2 #Make sure value is not too big
    value = value % 256**2 #Fix negative numbers
    return bytearray([value % 256,(value - (value % 256))//256])
    
